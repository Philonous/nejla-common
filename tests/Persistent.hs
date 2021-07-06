module Persistent where

import           Test.Tasty

import qualified Persistent.Serializable as Serializable
import qualified Persistent.DelayedIO as DelayedIO

tests :: TestTree
tests = testGroup "persistent" [ Serializable.tests
                               , DelayedIO.tests
                               ]
