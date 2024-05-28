module Main where

import qualified Config
import qualified Logging
import qualified Logstash
import qualified Persistent
import qualified JSON

import           Test.Tasty

tests =
  testGroup "tests"
            [ Persistent.tests
            , Logging.tests
              -- , Logstash.tests
            , Config.tests
            , JSON.tests
            ]

main = defaultMain tests
