{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NejlaCommon.Test.Json where

import qualified Data.Aeson          as Aeson
import           Test.Hspec.Wai.JSON

-- | Newtype wrapper to handle endpoints that return JSON values
newtype JSON = JSON Aeson.Value
  deriving newtype (Eq, Show)

instance Aeson.FromJSON JSON where
  parseJSON x = return $ JSON x

instance FromValue JSON where
  fromValue x = JSON x
