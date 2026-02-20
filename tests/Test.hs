module Main where

import qualified Config
import qualified Helpers
import qualified JSON
import qualified Logging
import qualified Persistent
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    Config.spec
    Helpers.spec
    JSON.spec
    Logging.spec
    Persistent.spec
