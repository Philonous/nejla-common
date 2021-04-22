module Main where

import           Test.Tasty

import qualified Persistent as Persistent
import qualified Logging as Logging
import qualified Logstash as Logstash
import qualified Config as Config

tests = testGroup "tests" [ Persistent.tests
                          , Logging.tests
                          -- , Logstash.tests
                          , Config.tests
                          ]


main = defaultMain tests
