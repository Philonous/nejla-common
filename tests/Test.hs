module Main where

import           Test.Tasty

import qualified Persistent
import qualified Logging
import qualified Logstash
import qualified Config

tests = testGroup "tests" [ Persistent.tests
                          , Logging.tests
                          -- , Logstash.tests
                          , Config.tests
                          ]


main = defaultMain tests
