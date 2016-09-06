module Main where

import           Test.Tasty

import qualified Persistent as Persistent
import qualified Logging as Logging
import qualified Logstash as Logstash

tests = testGroup "tests" [ Persistent.tests
                          , Logging.tests
                          , Logstash.tests
                          ]


main = defaultMain tests
