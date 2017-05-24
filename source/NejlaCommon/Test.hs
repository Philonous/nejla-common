{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NejlaCommon.Test where

import qualified Control.Exception         as Ex
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans       (MonadIO(..))
import qualified Data.Aeson                as Aeson
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import           Data.Data                 (Proxy(..))
import           Data.Monoid
import           Network.HTTP.Types.Status

import           Network.Wai.Test
import           Test.HUnit.Lang           (FailureReason(..), HUnitFailure(..))
import qualified Test.Hspec                as HSpec
import qualified Test.Hspec.Wai            as Wai
import           Test.Hspec.Wai            hiding (post, put)

failure :: MonadIO m => String -> m a
failure msg = liftIO . Ex.throwIO $ HUnitFailure Nothing (Reason msg)

infix 1 `shouldBe`
-- | shouldBe lifted to MonadIO
shouldBe :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
shouldBe x y = liftIO $ HSpec.shouldBe x y

-- | Lik Test.Hspec.Wai.post, but sets Content-Type to json
postJ :: BS.ByteString -> BSL.ByteString -> WaiSession SResponse
postJ path bd =
  Wai.request "POST" path [("Content-Type", "application/json")] bd

-- | Lik Test.Hspec.Wai.put, but sets Content-Type to json
putJ :: BS.ByteString -> BSL.ByteString -> WaiSession SResponse
putJ path bd =
  Wai.request "PUT" path [("Content-Type", "application/json")] bd

infix 1 `shouldParseAs`
-- | Check that the
shouldParseAs :: (MonadIO m, Aeson.FromJSON a) => BSL.ByteString -> Proxy a -> m a
shouldParseAs bs (prx :: Proxy t) = do
  case Aeson.eitherDecode bs `withType` prx of
    Left e -> failure $ "Could not decode json " <> show bs <> " : " <> e
    Right r -> return r
  where
    withType :: Either String a -> proxy a -> Either String a
    withType x _ = x

infix 1 `shouldParseAs_`
shouldParseAs_ :: (MonadIO m, Aeson.FromJSON a) => BSL.ByteString -> Proxy a -> m ()
shouldParseAs_  bs prx = do
  _ <- bs `shouldParseAs` prx
  return ()

body :: Lens' SResponse BSL.ByteString
body = lens simpleBody (\x y -> x{simpleBody = y} )

checkStatusCode ::(MonadIO f) => Int -> Int -> SResponse -> f ()
checkStatusCode from' to' res = do
  let scode = (statusCode . simpleStatus $ res)
      smessage = (statusMessage . simpleStatus $ res)
  unless (from' <= scode && scode < to')
    . failure $ "Expected status code between 200 and 299 but got "
    <> show scode <> " (" <> show smessage <> ")"
    <> "\nBody: " <> show (simpleBody res)

infix 1 `shouldReturnA`
shouldReturnA :: (Aeson.FromJSON a, MonadIO m) => m SResponse -> Proxy a -> m a
shouldReturnA f prx = do
  res <- f
  checkStatusCode 200 300 res
  res ^. body `shouldParseAs` prx

infix 1 `shouldReturnA_`
shouldReturnA_ :: (Aeson.FromJSON a, MonadIO m) => m SResponse -> Proxy a -> m ()
shouldReturnA_ f prx = do
  _ <- f `shouldReturnA` prx
  return ()
