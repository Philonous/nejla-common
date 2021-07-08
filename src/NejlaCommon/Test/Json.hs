{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NejlaCommon.Test.Json where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text

import           Test.Hspec.Wai.JSON

-- | Newtype wrapper to handle endpoints that return JSON values
newtype JSON = JSON Aeson.Value
    deriving newtype ( Eq )

instance Show JSON where
  show (JSON x) = Text.unpack . Text.decodeUtf8 . BSL.toStrict $ Aeson.encode x

instance Aeson.FromJSON JSON where
  parseJSON x = return $ JSON x

instance FromValue JSON where
  fromValue x = JSON x
