module Main where

import Test.Tasty

import qualified Config
import qualified Logging
import qualified Logstash
import qualified Persistent

tests =
    testGroup
        "tests"
        [ Persistent.tests
        , Logging.tests
        , -- , Logstash.tests
          Config.tests
        ]

main = defaultMain tests
