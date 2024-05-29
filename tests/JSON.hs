{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module JSON where

import qualified Data.Aeson              as Aeson
import           GHC.Generics

import           NejlaCommon.JSON

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Hspec.Expectations
import           Test.Tasty.TH

data TestJsonStruct =
  TestJsonStruct
  { testJsonStructFoo :: Int
  , testJsonStructBar :: String
  , testJsonStructQuux :: Bool
  }
  deriving (Generic, Show, Eq)

case_structLikeDeserialize = do
  let input = "{\"foo\": 133, \"bar\":\"hello world\", \"quux\":false}"
  Aeson.decode input `shouldBe`
    Just (AsObject TestJsonStruct
            { testJsonStructFoo = 133
            , testJsonStructBar = "hello world"
            , testJsonStructQuux = False
            })

case_structLikeSerializeDeserialize = do
  let input = AsObject TestJsonStruct
              { testJsonStructFoo = 133
              , testJsonStructBar = "hello world"
              , testJsonStructQuux = False
              }
  Aeson.decode (Aeson.encode input) `shouldBe` Just input

tests = $testGroupGenerator
