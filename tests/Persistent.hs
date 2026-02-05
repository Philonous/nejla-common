module Persistent where

import qualified Persistent.DelayedIO as DelayedIO
import qualified Persistent.Serializable as Serializable
import Test.Hspec
import Persistent.Common (dbSpec)

spec :: Spec
spec = dbSpec $ describe "Peristent" $ do
  Serializable.spec
  DelayedIO.spec
