{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Persistent.Serializable where

import           Control.Concurrent.Async
import           Control.Exception          ( throw )
import qualified Control.Monad.Catch        as Ex
import           Control.Monad.Trans

import           Data.Typeable              ( typeOf )

import           Database.Persist
import           Database.Persist.Sql
import qualified Database.PostgreSQL.Simple as Postgres

import           NejlaCommon

import           Persistent.Common

import           Test.Hspec.Expectations
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.TH

--------------------------------------------------------------------------------
-- Serialization failure -------------------------------------------------------
--------------------------------------------------------------------------------
-- | Orchestrate a serialization failure
--
-- See <https://www.postgresql.org/docs/9.5/static/transaction-iso.html#XACT-SERIALIZABLE>
serializableError :: MonadIO m
                  => Bool -- ^ Enable retries
                  -> ConnectionPool
                  -> m ()
serializableError retry pool = do
  -- "Baton" is a
  (b1, b2) <- mkBatons
  liftIO $ withAsync (thread1 b1) $ \a1 -> do
    link a1
    withAsync (thread2 b2) $ \a2 -> do
      link a2
      passBaton b2 -- This gives the baton to b1!

      -- Thread 1 will fail with a serialization error, since thread 2 "sneaks
      -- in"
      -- If we don't allow retries, we just jump out of here with an exception
      -- Otherwise thread 1 will start again
      _ <- wait a2
      -- Now thread 2 is finished and thread 1 can try again. We need to keep
      -- passing it the baton so it can make progress (since thread 1 isn't
      -- there to do it any more)
      passBaton b2
      takeBaton b2
      passBaton b2

      -- Thread 1 can now succeed
      _ <- wait a1
      return ()
  return ()
  where
    retries = if retry then 1 else 0

    runThread f = run serializable retries pool f

    -- This thread fails with a serialization error
    thread1 baton = runThread $ do
      takeBaton baton
      s <- getSum 1
      passBaton baton
      -- Here thread two changes the data we just read, leading to a
      -- serialization anomaly
      takeBaton baton
      _ <- insert Foo
                  { fooClass = 2
                  , fooValue = round s
                  }
      commit
      passBaton baton

    thread2 baton = runThread $ do
      takeBaton baton
      s <- getSum 2
      _ <- insert Foo
                  { fooClass = 1
                  , fooValue = round s
                  }
      commit
      passBaton baton

-- | Check that we really are producing a serialization failure
case_serializable_error :: IO ()
case_serializable_error =
  withDB (serializableError False)
  `shouldThrow` (\(ExceptionInLinkedThread _ mbE) ->
                 case Ex.fromException mbE of
                     Just (DBError mbPgError)
                       | Just e <- Ex.fromException mbPgError ->
                         Postgres.sqlState e == "40001"
                     _ -> throw mbE)

-- | Check that we successfully retry the transaction when a serialization
-- failure occurs
case_serializable_retries :: IO ()
case_serializable_retries = withDB $ \pool -> liftIO $ do
  serializableError True pool
  ls <- run readCommitted 0 pool $ selectList [] []
  (entityVal <$> ls) `shouldBe` [ Foo 1 3, Foo 2 5, Foo 1 5, Foo 2 8 ]

tests :: TestTree
tests = $testGroupGenerator
