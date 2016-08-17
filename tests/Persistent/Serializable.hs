{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Persistent.Serializable where

import           Control.Concurrent.Async
import           Control.Monad.Trans
import           Database.Persist
import           Database.Persist.Sql
import qualified Database.PostgreSQL.Simple as Postgres
import           Persistent.Common

import           Test.Hspec.Expectations
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.TH

import NejlaCommon

--------------------------------------------------------------------------------
-- Serialization failure -------------------------------------------------------
--------------------------------------------------------------------------------

-- | Orchestrate a serialization failure
--
-- See <https://www.postgresql.org/docs/9.5/static/transaction-iso.html#XACT-SERIALIZABLE>
serializableError :: MonadIO m =>
                     Bool -- ^ Enable retries
                  -> ConnectionPool
                  -> m ()
serializableError retry pool = do
    (b1, b2) <- mkBatons
    liftIO $ withAsync (thread1 b1) $ \a1 -> do
      link a1
      withAsync (thread2 b2) $ \a2 -> do
        link a2
        _ <- wait a2
        yieldBaton b2
        passBaton b2
        _ <- wait a1
        return ()
    return ()

  where
    retries = if retry then 1 else 0
    runThread f = run serializable retries pool f
    thread1 baton = runThread $ do
      takeBaton baton
      s <- getSum 1
      passBaton baton
      _ <- insert Foo {fooClass = 2, fooValue = round s}
      commit
      yieldBaton baton
    thread2 baton = runThread $ do
      takeBaton baton
      s <- getSum 2
      _ <- insert Foo {fooClass = 1, fooValue = round s}
      commit
      yieldBaton baton

-- | Check that we really are producing a serialization failure
case_serializable_error :: IO ()
case_serializable_error =
  (withDB  $ serializableError False)
            `shouldThrow` (\e -> Postgres.sqlState e == "40001")

-- | Check that we successfully retry the transaction when a serialization
-- failure occurs
case_serializable_retries :: IO ()
case_serializable_retries = withDB $ \pool -> liftIO $ do
  serializableError True pool
  ls <- run readCommitted 0 pool $ selectList [] []
  (entityVal <$>  ls)
    `shouldBe` [ Foo 1 3
               , Foo 2 5
               , Foo 1 5
               , Foo 2 8
               ]

tests :: TestTree
tests = $testGroupGenerator
