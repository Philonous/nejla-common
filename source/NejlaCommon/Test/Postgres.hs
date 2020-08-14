{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Helpers to deal with Postgres in test suites

module NejlaCommon.Test.Postgres
  ( module NejlaCommon.Test.Postgres
  , ConnectInfo(..)
  )

where

import qualified Control.Monad.Catch               as Ex
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString.Char8             as BS
import           Database.Persist.Sql              (SqlBackend, ConnectionPool)
import qualified Database.PostgreSQL.Simple        as Postgres
import           NejlaCommon.Persistence
import           NejlaCommon.Persistence.Migration
import           Network.Wai                       (Application)
import           System.Environment                (lookupEnv)
import           System.IO                         (stderr)
import           Test.Hspec                  ( Example(..), SpecWith
                                             , beforeWith, aroundWith
                                             , hspec
                                             )

import qualified Database.Persist.Sql              as P

import           NejlaCommon.Test.Logging          (loggingToChan)


type Migrate = ReaderT SqlBackend (LoggingT IO) ()

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
withTestDB :: ConnectInfo
           -> Int -- ^ Maximum number of connections in the pool
           -> Migrate -- ^ Migration to run once after connection is established
           -> (ConnectionPool -> IO a) -> LoggingT IO a
withTestDB ci cs doMigrate f =
  withDBPool ci cs doMigrate $ \pool -> do
    liftIO $ do
      _ <- runLoggingT (runPoolRetry pool dbSetup) (\_ _ _ _ -> return ())
      f pool
  where
    dbSetup = do
      resetDB
      doMigrate
      makeConstraintsDeferrable
    resetDB = P.rawExecute
      [sql|
        SET client_min_messages TO ERROR;
        DROP SCHEMA IF EXISTS _meta CASCADE;
        DROP SCHEMA public CASCADE;
        CREATE SCHEMA public;
        GRANT ALL ON SCHEMA public TO postgres;
        GRANT ALL ON SCHEMA public TO public;
        COMMENT ON SCHEMA public IS 'standard public schema';
        RESET client_min_messages;
        |] []
    -- Iterate over all foreign constraints and make them deferrable (so we can
    -- DELETE them without having to worry about the order we do it in)
    makeConstraintsDeferrable = P.rawExecute
      [sql|
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
      |] []


-- Run DELETE FROM on all relations after setting all constraints to deferred
cleanDB :: MonadIO m => ReaderT SqlBackend m ()
cleanDB = P.rawExecute cleanDBSql []
  where
    cleanDBSql = [sql|
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

type DBApiSpec st = SpecWith (st, Application)

-- | Hspec helper. Sets up a database connection via withTestDB, clearing out the database before every test
--
-- The callback function will be called on each test. It's passed the connection pool
specApi :: ConnectInfo -- ^ Database connection info
        -> Migrate -- ^ Migration script to run once
        -> (ConnectionPool
             -> ((st -> Application -> IO ()) -> LoggingT IO ()))
          -- ^ Setup and teardown of Application around each test
        -> DBApiSpec st -- ^ Tests to run
        -> IO ()
specApi ci migration withMkApp spec =
  loggingToChan 20 $ \getLogs -> do
  logFun <- askLoggerIO
  withTestDB ci 5 migration $ \pool ->
    hspec $ aroundWith ( \s () -> runLoggingT (do
      -- Drain logs so we don't get logs from previous tests
      _ <- liftIO $ getLogs
      P.runSqlPool cleanDB pool
      Ex.catch (withMkApp pool $ curry s) $ \(_ :: Ex.SomeException) -> do
        liftIO (mapM_ (BS.hPutStrLn stderr) =<< getLogs)


      return ()
                                              ) logFun

                )
     spec

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
  return Postgres.ConnectInfo { Postgres.connectPort = dbPort
                              , Postgres.connectHost = dbHost
                              , Postgres.connectUser = dbUser
                              , Postgres.connectDatabase = dbDatabase
                              , Postgres.connectPassword = dbPassword
                              }
  where
    getEnv name def = do
      mbE <- lookupEnv name
      return $ case mbE of
                 Nothing -> def
                 Just e -> e
    getEnv' name def = do
      mbE <- lookupEnv name
      case mbE of
        Nothing -> return def
        Just r -> case reads r of
                    [(e,_)] -> return e
                    _ -> error $ "Could not read " <> name
                                  <> ", value" <> show r <> " not understood"
