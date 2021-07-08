{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Helpers to deal with Postgres in test suites
module NejlaCommon.Test.Postgres
  ( Migrate
  , withTestDB
  , cleanDB
  , DBApiSpec
  , TestDone -- Don't except constructor
  , specApi
  , ConnectInfo(..)
  , dbTestConnectInfo
  ) where

import qualified Control.Monad.Catch               as Ex
import           Control.Monad.IO.Unlift           ( MonadUnliftIO )
import           Control.Monad.Logger
import           Control.Monad.Reader

import qualified Data.ByteString.Char8             as BS
import           Data.Maybe

import           Database.Persist.Sql
                 ( ConnectionPool, SqlBackend )
import qualified Database.Persist.Sql              as P
import qualified Database.PostgreSQL.Simple        as Postgres

import           NejlaCommon.Persistence
import           NejlaCommon.Persistence.Migration
import qualified NejlaCommon.Persistence.Migration as Migration
import           NejlaCommon.Test.Logging          ( loggingToChan )

import           Network.Wai                       ( Application )

import           System.Environment                ( lookupEnv )
import           System.IO                         ( stderr )

import           Test.Hspec
                 ( SpecWith, aroundWith, hspec )

type Migrate = Migration.M ()

-- | Set up a connection to the testing database. Re-tries connecting to the
-- database and once successful cleans it out and runs the migration. It will
-- also change all constraints to DEFERRABLE.
--
-- /NB/: THIS FUNCTION IS DESTRUCTIVE! It will delete you database schema and
-- overwrite it. This is necessary for the tests to function properly, but can
-- lead to data loss if used on a production database
--
-- Example
-- >  conf <- loadConf "my-app"
-- >  conInfo <- getDBConnectInfo conf
-- >  withTestDB conInfo 3 (mapM_ script migrations) $ \pool -> do
-- >     «run tests...»
withTestDB
  :: (MonadLoggerIO m, MonadUnliftIO m, Ex.MonadCatch m)
  => ConnectInfo
  -> Int -- ^ Maximum number of connections in the pool
  -> Migration.M () -- ^ Migration to run once after connection is established
  -> (ConnectionPool -> m a)
  -> m a
withTestDB ci cs doMigrate f = withDBPool ci cs dbSetup $ \pool -> f pool
  where
    dbSetup :: Migration.M ()
    dbSetup = do
      resetDB
      doMigrate
      makeConstraintsDeferrable

    resetDB =
      P.rawExecute [sql|
        SET client_min_messages TO ERROR;
        DROP SCHEMA IF EXISTS _meta CASCADE;
        DROP SCHEMA public CASCADE;
        CREATE SCHEMA public;
        GRANT ALL ON SCHEMA public TO postgres;
        GRANT ALL ON SCHEMA public TO public;
        COMMENT ON SCHEMA public IS 'standard public schema';
        RESET client_min_messages;
        |]
                   []

    -- Iterate over all foreign constraints and make them deferrable (so we can
    -- DELETE them without having to worry about the order we do it in)
    makeConstraintsDeferrable =
      P.rawExecute [sql|
          DO $$
            DECLARE
                statements CURSOR FOR
                    SELECT c.relname AS tab, con.conname AS con
                    FROM pg_constraint con
                    INNER JOIN pg_class c
                      ON con.conrelid = c.oid
                    WHERE con.contype='f';
            BEGIN
                FOR row IN statements LOOP
                    EXECUTE 'ALTER TABLE ' ||  quote_ident(row.tab) ||
                            ' ALTER CONSTRAINT ' || quote_ident(row.con) ||
                            ' DEFERRABLE;' ;
                END LOOP;
            END;
          $$;
      |]
                   []

-- Run DELETE FROM on all relations after setting all constraints to deferred
cleanDB :: MonadIO m => ReaderT SqlBackend m ()
cleanDB = P.rawExecute cleanDBSql []
  where
    cleanDBSql =
      [sql|
         SET client_min_messages TO ERROR;
         SET CONSTRAINTS ALL DEFERRED;
         DO $$
         DECLARE
             statements CURSOR FOR
                 SELECT tablename FROM pg_tables
                 WHERE schemaname = 'public';
         BEGIN
             FOR stmt IN statements LOOP
                 EXECUTE 'DELETE FROM ' || quote_ident(stmt.tablename)
                   || ';';
             END LOOP;
         END;
         $$;
         RESET client_min_messages;
        |]

type DBApiSpec st = SpecWith ((ConnectionPool, st), Application)

-- | Dummy type to ensure that passed test is run
data TestDone = TestDone

-- | Hspec helper. Sets up a database connection via withTestDB, clearing out
-- the database before every test
--
-- The callback function is run around every test. It's given the live database
-- pool (already migrated and cleaned) and a continuation it should be
-- running. The slightly complicated type is so that it can perform setup and
-- teardown of additional resources. Note that the continuation might throw
-- exceptions, so resource management should be done with bracket.
--
-- Example: Say we want to create a temporary directory for each test and clean it up afterwards, then we would write:
--
-- > withTest _pool runTest = do
-- >   withSystemTempDirectory "example" $ \path ->
-- >     liftIO $ runTest path (mkApp path)
-- >
-- > specApi connInfo migrate withTest spec
specApi :: ConnectInfo -- ^ Database connection info
        -> Migration.M () -- ^ Migration script to run once
        -> (ConnectionPool
            -> (st -> Application -> IO TestDone)
            -> LoggingT IO TestDone)
        -- ^ Setup and teardown of Application around each test
        -> DBApiSpec st -- ^ Tests to run
        -> IO ()
specApi ci migration withMkApp spec = loggingToChan 20 $ \getLogs -> do
  logFun <- askLoggerIO
  withTestDB ci 5 migration $ \pool -> lift . hspec $
    aroundWith (\s () ->
                runLoggingT (do
                               let s' st app =
                                     s ((pool, st), app) >> return TestDone
                               -- Drain logs so we don't get logs from previous tests
                               _ <- liftIO getLogs
                               P.runSqlPool cleanDB pool
                               _ <- Ex.onException (withMkApp pool s') $ do
                                 liftIO (mapM_ (BS.hPutStrLn stderr) =<< getLogs)
                               return ()) logFun) spec

-- | Read database connection info from environment variables, reverting to
-- defaults if unset.
--
-- Recognized variables (default):
-- DB_HOST     ("localhost")
-- DB_USER     ("postgres")
-- DB_DATABASE ("postgres")
-- DB_PASSWORD ("")
-- DB_PORT     (5432)
dbTestConnectInfo :: IO ConnectInfo
dbTestConnectInfo = do
  dbHost <- getEnv "DB_HOST" "localhost"
  dbUser <- getEnv "DB_USER" "postgres"
  dbDatabase <- getEnv "DB_DATABASE" "postgres"
  dbPassword <- getEnv "DB_PASSWORD" ""
  dbPort <- getEnv' "DB_PORT" 5432
  return Postgres.ConnectInfo
         { Postgres.connectPort     = dbPort
         , Postgres.connectHost     = dbHost
         , Postgres.connectUser     = dbUser
         , Postgres.connectDatabase = dbDatabase
         , Postgres.connectPassword = dbPassword
         }
  where
    getEnv name def = do
      mbE <- lookupEnv name
      return $ fromMaybe def mbE

    getEnv' name def = do
      mbE <- lookupEnv name
      case mbE of
          Nothing -> return def
          Just r -> case reads r of
              [ (e, _) ] -> return e
              _ -> error $ "Could not read " <> name <> ", value" <> show r
                <> " not understood"
