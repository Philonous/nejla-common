module Persistent where

import qualified Persistent.DelayedIO    as DelayedIO
import qualified Persistent.Serializable as Serializable

import           Test.Tasty

tests :: TestTree
tests = testGroup "persistent" [ Serializable.tests, DelayedIO.tests ]
