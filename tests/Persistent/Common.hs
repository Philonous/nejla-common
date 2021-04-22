{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Persistent.Common where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import qualified Control.Monad.Catch         as Ex
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.ByteString             (ByteString)
import           Data.Default
import           Data.IORef
import           Data.Singletons
import           Data.Singletons.Prelude.Ord
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Text
import           Database.Esqueleto          as E
import           Database.Persist.Postgresql
import           Database.Persist.TH
import qualified Database.PostgreSQL.Simple  as Postgres
import           System.Environment
import           System.IO
import           UnliftIO                    (MonadUnliftIO)

import           NejlaCommon

share [ mkPersist sqlSettings
      , mkMigrate "migrateAll"
      ]
      [persistLowerCase|

Foo
  class Int
  value Int
  deriving Eq Show
|]

defaultConnectionString :: ByteString
defaultConnectionString = "host=localhost port=5432 dbname=postgres user=postgres"

run :: ( MonadIO m, MonadUnliftIO m
       , ('NejlaCommon.ReadCommitted <= level) ~ 'True) =>
        Sing level
     -> Int
    -> ConnectionPool
    -> ReaderT SqlBackend IO a
    -> m a
run lvl i pool m = liftIO
             . runApp lvl conf pool ()
             . withLevel $ db' m
  where
    conf :: SqlConfig
    conf = def & numRetries .~ i

withDB :: (ConnectionPool -> IO b) -> IO b
withDB f = do
  mbDebug <- liftIO $ lookupEnv "DEBUG"
  let debug = case mbDebug of
                Nothing -> False
                Just{} -> True
  withDB' debug f

withDB' :: Bool
        -> (ConnectionPool -> IO a)
        -> IO a
withDB' debug (f :: ConnectionPool -> IO a) = do
  case debug of
    False -> runNoLoggingT go
    True -> runStderrLoggingT go
  where
    go :: (MonadIO m, MonadLogger m, MonadUnliftIO m, Ex.MonadCatch m) => m a
    go = do
      mbConStr <- liftIO $ lookupEnv "TEST_DB_CONNECTION"
      let connectionString = case mbConStr of
                               Nothing -> defaultConnectionString
                               Just str -> Text.encodeUtf8 (Text.pack str)
      withPostgresqlPool connectionString 3 $ \pool -> do
        runPoolRetry pool $ do
          resetDB
          runMigrationSilent migrateAll
          _ <- insert Foo { fooClass = 1, fooValue = 3}
          _ <- insert Foo { fooClass = 2, fooValue = 5}
          return ()
        liftIO $ f pool


resetCommands :: [Text]
resetCommands = [ "SET client_min_messages TO WARNING;"
                , "DROP SCHEMA public CASCADE;"
                , "CREATE SCHEMA public;"
                , "GRANT ALL ON SCHEMA public TO postgres;"
                , "GRANT ALL ON SCHEMA public TO public;"
                , "COMMENT ON SCHEMA public IS 'standard public schema';"
                ]

resetDB :: MonadIO m => ReaderT SqlBackend m ()
resetDB = forM_ resetCommands $ \c -> rawExecute c []

commit :: MonadIO m => ReaderT SqlBackend m ()
commit = transactionSave

getSum :: (MonadIO m, MonadFail m) =>
          Int
       -> ReaderT SqlBackend m Rational
getSum i = do
  [Value res] <- E.select . E.from $ \foo -> do
    E.where_ $ foo E.^. FooClass E.==. E.val i
    return $ coalesceDefault [E.sum_ $ foo E.^. FooValue] (val 0)
  return res

--------------------------------------------------------------------------------
-- Thread synchronization helpers ----------------------------------------------
--------------------------------------------------------------------------------

data Baton = Baton { number :: Int
                   , we, them :: MVar ()}

takeBaton :: MonadIO m => Baton -> m ()
takeBaton baton = liftIO $ do
  takeMVar $ we baton

passBaton :: MonadIO m => Baton -> m ()
passBaton baton = liftIO $ do
  putMVar (them baton) ()

-- | A mutex with two handles. Yielding the mutex (passing the baton) allows the
-- other thread to run until the baton is passed back
--
-- Initially, neither handle holds the baton (start by passBaton to one of them)
mkBatons :: MonadIO m => m (Baton, Baton)
mkBatons = liftIO $ do
  sem1 <- newEmptyMVar
  sem2 <- newEmptyMVar
  return (Baton 1 sem1 sem2, Baton 2 sem2 sem1)

-- | Restart the transaction by throwing a re-tryeable SQL-error
restart :: App () Privileged NejlaCommon.ReadCommitted a
restart = do
  Ex.throwM $
    Postgres.SqlError
    { Postgres.sqlState       = "40001" -- SerializationFailure, should lead to restart
    , Postgres.sqlExecStatus  = Postgres.NonfatalError
    , Postgres.sqlErrorMsg    = "Test: restart"
    , Postgres.sqlErrorDetail = "Test: Error thrown to restart transaction"
    , Postgres.sqlErrorHint   = mempty
    }

withRestarts :: MonadIO m =>
  Int
  -> (App () 'Privileged 'NejlaCommon.ReadCommitted () -> m b)
  -> m b
withRestarts count f = do
  countRef <- liftIO $ newIORef count
  f $ restartIf countRef
  where
    restartIf ref = do
      count <- liftIO $ atomicModifyIORef ref (\x -> if x > 0 then (x-1, x) else (x, x))
      when (count > 0) restart
