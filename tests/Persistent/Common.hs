{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Persistent.Common where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.ByteString (ByteString)
import Data.Default
import Data.Singletons
import Data.Singletons.Prelude.Ord
import Data.Text (Text)
import Database.Esqueleto as E
import Database.Persist.Postgresql
import Database.Persist.TH
import System.Environment
import System.IO

import NejlaCommon

share [ mkPersist sqlSettings
      , mkMigrate "migrateAll"
      ]
      [persistLowerCase|

Foo
  class Int
  value Int
  deriving Eq Show
|]

connectionString :: ByteString
connectionString = "dbname=nejlacommon-test"

run :: (MonadIO m, ('ReadCommitted :<= level) ~ 'True) =>
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
    go :: (MonadIO m, MonadLogger m, MonadBaseControl IO m) => m a
    go = do
      withPostgresqlPool connectionString 3 $ \pool -> do
        -- Setup database
        run readCommitted 0 pool $ do
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

getSum :: (MonadIO m) =>
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

yieldBaton :: MonadIO m => Baton -> m ()
yieldBaton baton = liftIO $ do
  putMVar (them baton) ()

passBaton :: MonadIO m => Baton -> m ()
passBaton baton = do
  yieldBaton baton
  takeBaton baton

mkBatons :: MonadIO m => m (Baton, Baton)
mkBatons = liftIO $ do
  sem1 <- newMVar ()
  sem2 <- newEmptyMVar
  return (Baton 1 sem1 sem2, Baton 2 sem2 sem1)
