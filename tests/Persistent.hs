module Persistent where

import Test.Tasty

import qualified Persistent.DelayedIO as DelayedIO
import qualified Persistent.Serializable as Serializable

tests :: TestTree
tests =
    testGroup
        "persistent"
        [ Serializable.tests
        , DelayedIO.tests
        ]
