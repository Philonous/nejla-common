module Main where

import qualified Config

import qualified Logging

import qualified Logstash

import qualified Persistent

import           Test.Tasty

tests =
  testGroup "tests"
            [ Persistent.tests
            , Logging.tests
              -- , Logstash.tests
            , Config.tests
            ]

main = defaultMain tests
