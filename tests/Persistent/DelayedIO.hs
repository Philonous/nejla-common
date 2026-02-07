{-# LANGUAGE ScopedTypeVariables #-}

module Persistent.DelayedIO where

import Control.Lens
import qualified Control.Monad.Catch as Ex
import Data.Default (def)
import Data.IORef
import NejlaCommon
import Persistent.Common
import Test.Hspec

inc :: IORef Int -> IO ()
inc ref = atomicModifyIORef' ref (\x -> (x + 1, ()))

sqlConf :: Int -> SqlConfig
sqlConf n = def & numRetries .~ n

push :: Int -> IORef [Int] -> IO ()
push x ref = atomicModifyIORef' ref (\xs -> (xs ++ [x], ()))

spec :: DBSpec
spec = withDefaultSetup $ describe "delayed IO" $ do
  it "runs after the transaction finishes" $ \pool -> do
    countRef <- newIORef 0
    runApp readCommitted (sqlConf 3) pool () $
      delayIO $
        inc countRef
    count <- readIORef countRef
    count `shouldBe` 1

  it "runs only once even if the transaction restarts" $ \pool ->
    withRestarts 2 $ \restart -> do
      countRef <- newIORef 0
      runApp readCommitted (sqlConf 3) pool () $ do
        delayIO $ inc countRef
        restart
      count <- readIORef countRef
      count `shouldBe` 1

  it "doesn't run when the transaction fails" $ \pool ->
    withRestarts 1 $ \restart -> do
      countRef <- newIORef 0
      Ex.handle (\(_e :: PersistError) -> return ()) $
        runApp readCommitted (sqlConf 0) pool () $ do
          delayIO $ inc countRef
          restart
      count <- readIORef countRef
      count `shouldBe` 0
  it "runs multiple delayed actions in order " $ \pool ->
    withRestarts 1 $ \restart -> do
      ref <- newIORef []
      Ex.handle (\(_e :: PersistError) -> return ()) $
        runApp readCommitted (sqlConf 3) pool () $ do
          delayIO $ push 1 ref
          delayIO $ push 2 ref
          delayIO $ push 3 ref
          restart
      res <- readIORef ref
      res `shouldBe` [1, 2, 3]
