{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module JSON where

import qualified Data.Aeson as Aeson
import GHC.Generics
import NejlaCommon.JSON
import Test.Hspec

data TestJsonStruct
  = TestJsonStruct
  { testJsonStructFoo :: Int,
    testJsonStructBar :: String,
    testJsonStructQuux :: Bool
  }
  deriving (Generic, Show, Eq)

spec :: Spec
spec = describe "json / AsObject" $ do
  it "parses from json" $ do
    let input = "{\"foo\": 133, \"bar\":\"hello world\", \"quux\":false}"
    Aeson.decode input
      `shouldBe` Just
        ( AsObject
            TestJsonStruct
              { testJsonStructFoo = 133,
                testJsonStructBar = "hello world",
                testJsonStructQuux = False
              }
        )
  it "roundtrips a value" $ do
    let input =
          AsObject
            TestJsonStruct
              { testJsonStructFoo = 133,
                testJsonStructBar = "hello world",
                testJsonStructQuux = False
              }
    Aeson.decode (Aeson.encode input) `shouldBe` Just input
