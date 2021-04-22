{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Persistent.DelayedIO where

import           Control.Lens
import qualified Control.Monad.Catch     as Ex
import           Data.IORef
import           Test.Hspec.Expectations
import           Test.Tasty
import           Test.Tasty.TH           (testGroupGenerator)
import           Test.Tasty.HUnit
import           Data.Default            (def)

import           NejlaCommon

import           Persistent.Common

inc ref = atomicModifyIORef' ref (\x -> (x + 1, ()))

sqlConf :: Int -> SqlConfig
sqlConf n = def & numRetries .~ n


case_delayedIO_runs_after = do
  countRef <- newIORef 0
  withDB $ \pool -> runApp readCommitted (sqlConf 3) pool () $
    delayIO $ inc countRef
  count <- readIORef countRef
  count `shouldBe` 1


case_delayedIO_runs_only_once = withRestarts 2 $ \restart -> do
  countRef <- newIORef 0
  withDB $ \pool -> runApp readCommitted (sqlConf 3) pool () $ do
    delayIO $ inc countRef
    restart
  count <- readIORef countRef
  count `shouldBe` 1

case_delayedIO_runs_not_on_error = withRestarts 1 $ \restart -> do
  countRef <- newIORef 0
  Ex.handle (\(e :: PersistError) -> return ()) $
    withDB $ \pool -> runApp readCommitted (sqlConf 0) pool () $ do
      delayIO $ inc countRef
      restart
  count <- readIORef countRef
  count `shouldBe` 0

push x ref = atomicModifyIORef' ref (\xs -> (xs ++ [x], ()))


case_delayedIO_runs_multiple_in_order = withRestarts 1 $ \restart -> do
  ref <- newIORef []
  Ex.handle (\(e :: PersistError) -> return ()) $
    withDB $ \pool -> runApp readCommitted (sqlConf 3) pool () $ do
      delayIO $ push 1 ref
      delayIO $ push 2 ref
      delayIO $ push 3 ref
      restart
  res <- readIORef ref
  res `shouldBe` [1,2,3]


tests :: TestTree
tests = $testGroupGenerator
