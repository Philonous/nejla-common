module Persistent where

import Persistent.Common (dbSpec)
import qualified Persistent.DelayedIO as DelayedIO
import qualified Persistent.Migrations as Migrations
import qualified Persistent.Serializable as Serializable
import Test.Hspec

spec :: Spec
spec = do
  dbSpec $ describe "Peristent" $ do
    Serializable.spec
    DelayedIO.spec
    Migrations.spec
  Migrations.consistencySpec
