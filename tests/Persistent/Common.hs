{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Persistent.Common where

import Control.Concurrent
import Control.Lens
import Control.Monad
import qualified Control.Monad.Catch as Ex
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.ByteString (ByteString)
import Data.Default
import Data.IORef
import Data.Singletons
#if MIN_VERSION_singletons(3,0,0)
import           Data.Ord.Singletons
#else
import           Data.Singletons.Prelude.Ord
#endif
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Database.Esqueleto as E
import Database.Persist.Postgresql
import Database.Persist.TH
import qualified Database.PostgreSQL.Simple as Postgres
import NejlaCommon
import NejlaCommon.Test
import System.Environment
import System.IO
import Test.Hspec
import UnliftIO (MonadUnliftIO)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

Foo
  class Int
  value Int
  deriving Eq Show
|]

defaultConnectionString :: ByteString
defaultConnectionString =
  "host=localhost port=5555 dbname=postgres user=postgres"

run ::
  ( MonadIO m,
    MonadUnliftIO m,
    ('NejlaCommon.ReadCommitted <= level) ~ 'True
  ) =>
  Sing level ->
  Int ->
  ConnectionPool ->
  ReaderT SqlBackend IO a ->
  m a
run lvl i pool m = liftIO . runApp lvl conf pool () . withLevel $ db' m
  where
    conf :: SqlConfig
    conf = def & numRetries .~ i

withDB :: (ConnectionPool -> IO a) -> IO ConnectionPool -> IO a
withDB f getPool = runStderrLoggingT $ do
  pool <- liftIO getPool
  runPoolRetry pool $ do
    resetDB
    _ <- runMigrationSilent migrateAll
    _ <-
      insert
        Foo
          { fooClass = 1,
            fooValue = 3
          }
    _ <-
      insert
        Foo
          { fooClass = 2,
            fooValue = 5
          }
    return ()
  liftIO $ f pool

resetCommands :: [Text]
resetCommands =
  [ "SET client_min_messages TO WARNING",
    "DROP SCHEMA public CASCADE",
    "CREATE SCHEMA public",
    "GRANT ALL ON SCHEMA public TO postgres",
    "GRANT ALL ON SCHEMA public TO public",
    "COMMENT ON SCHEMA public IS 'standard public schema'",
    "DROP SCHEMA IF EXISTS _meta CASCADE"
  ]

resetDB :: (MonadIO m) => ReaderT SqlBackend m ()
resetDB = forM_ resetCommands $ \c -> rawExecute c []

commit :: (MonadIO m) => ReaderT SqlBackend m ()
commit = transactionSave

getSum :: (MonadIO m, MonadFail m) => Int -> SqlReadT m Rational
getSum i = do
  [Value res] <- E.select $ E.from $ \foo -> do
    E.where_ $ foo E.^. FooClass E.==. E.val i
    return $ coalesceDefault [E.sum_ $ foo E.^. FooValue] (val 0)
  return res

--------------------------------------------------------------------------------
-- Thread synchronization helpers ----------------------------------------------
--------------------------------------------------------------------------------
data Baton
  = Baton
  { number :: Int,
    we, them :: MVar ()
  }

takeBaton :: (MonadIO m) => Baton -> m ()
takeBaton baton = liftIO $ do
  takeMVar $ we baton

passBaton :: (MonadIO m) => Baton -> m ()
passBaton baton = liftIO $ do
  putMVar (them baton) ()

-- | A mutex with two handles. Yielding the mutex (passing the baton) allows the
-- other thread to run until the baton is passed back
--
-- Initially, neither handle holds the baton (start by passBaton to one of them)
mkBatons :: (MonadIO m) => m (Baton, Baton)
mkBatons = liftIO $ do
  sem1 <- newEmptyMVar
  sem2 <- newEmptyMVar
  return (Baton 1 sem1 sem2, Baton 2 sem2 sem1)

-- | Restart the transaction by throwing a re-tryeable SQL-error
restart :: App () Privileged NejlaCommon.ReadCommitted a
restart = do
  Ex.throwM $
    Postgres.SqlError
      { Postgres.sqlState = "40001", -- SerializationFailure, should lead to restart
        Postgres.sqlExecStatus = Postgres.NonfatalError,
        Postgres.sqlErrorMsg = "Test: restart",
        Postgres.sqlErrorDetail = "Test: Error thrown to restart transaction",
        Postgres.sqlErrorHint = mempty
      }

-- Create and pass an action that restarts the transaction a certain number of
-- times
withRestarts ::
  (MonadIO m) =>
  Int ->
  (App () 'Privileged 'NejlaCommon.ReadCommitted () -> m b) ->
  m b
withRestarts count f = do
  countRef <- liftIO $ newIORef count
  f $ restartIf countRef
  where
    restartIf ref = do
      count <-
        liftIO $
          atomicModifyIORef ref (\x -> if x > 0 then (x - 1, x) else (x, x))
      when (count > 0) restart

type DBSpec = SpecWith ConnectionPool

dbSpec :: DBSpec -> SpecWith ()
dbSpec = sequential . aroundAll withPool . aroundWith cleanDB
  where
    withPool f = do
      mbConStr <- lookupEnv "TEST_DB_CONNECTION"
      let connectionString = case mbConStr of
            Nothing -> defaultConnectionString
            Just str -> Text.encodeUtf8 (Text.pack str)
      loggingToChan 500 $ \getLogs ->
        withPostgresqlPool connectionString 3 $
          \pool -> liftIO $ f (pool, getLogs)
    cleanDB f (pool, getLogs) = do
      -- Clean out existing logs before running test case
      _ <- getLogs
      -- Log to stderr directly, since those are connectivity problems rather
      -- than test failures
      runStderrLoggingT $ runPoolRetry pool resetDB
      Ex.catchAll (f pool) $ \e -> do
        getLogs >>= mapM_ BS8.putStrLn
        Ex.throwM e

withDefaultSetup :: DBSpec -> DBSpec
withDefaultSetup = beforeWith migrated
  where
    migrated pool = runStdoutLoggingT $ runPoolRetry pool $ do
      _ <- runMigrationSilent migrateAll
      _ <-
        insert
          Foo
            { fooClass = 1,
              fooValue = 3
            }
      _ <-
        insert
          Foo
            { fooClass = 2,
              fooValue = 5
            }
      return pool
