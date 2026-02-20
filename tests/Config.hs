{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import Control.Monad.Logger
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText
import Formatting
import NejlaCommon.Config
import System.Environment
import System.IO
import qualified System.IO.Temp as Temp
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

--------------------------------------------------------------------------------
-- Conf file -------------------------------------------------------------------
--------------------------------------------------------------------------------
mkConfig :: String -> LText.Text
mkConfig = format ("testing{\n  value = " % string % " \n}")

withConf :: FilePath -> String -> (Config -> IO b) -> IO b
withConf confFile value f = do
  let c = mkConfig value
  LText.writeFile confFile $ mkConfig value
  setEnv "CONF_PATH" confFile
  conf <- loadConf ""
  res <- f conf
  unsetEnv "CONF_PATH"
  return res

withConfFile :: (FilePath -> IO a) -> IO a
withConfFile f = do
  Temp.withSystemTempFile "test.conf" $ \filename h -> do
    hClose h
    f filename

--------------------------------------------------------------------------------
-- Environment Variables -------------------------------------------------------
--------------------------------------------------------------------------------
envName :: String
envName = "NEJLACOMMON_TESTING"

withEnv :: String -> String -> IO b -> IO b
withEnv env v f = do
  setEnv env v
  res <- f
  unsetEnv env
  return res

instance Arbitrary Text where
  arbitrary = Text.pack <$> arbitrary

  shrink = map Text.pack . shrink . Text.unpack

test ::
  (Show s, Show t, Arbitrary t) =>
  -- | How the value should be rendered to a String for
  -- storage in the conf file
  (t -> String) ->
  -- | How the value should be rendered to a String for
  -- storage in the env variable
  (t -> String) ->
  -- | The config retrieval function to test
  (String -> Text -> Either Text t -> Config -> LoggingT IO s) ->
  -- | How the returned value should be compared to the
  -- original value (in case of maybe etc)
  (s -> t -> Bool) ->
  IO ()
test toConf toEnv getter checkGetter = withConfFile $ \filename -> do
  quickCheck $ \v' -> monadicIO $ do
    -- Config File
    let cf = toConf v'
    res1 <- run . withConf filename cf $ get
    assert (checkGetter res1 v')
    -- Environment
    let ev = toEnv v'
    -- res2 <- run $ do
    --   conf <- loadConf ""
    --   withEnv envName ev $ get conf
    -- assert (checkGetter res2 v')
    return ()
  where
    get conf =
      runStderrLoggingT $ getter envName "testing.value" (Left "value") conf

spec :: Spec
spec = describe "config" $ do
  it "reads an integer list" $ do
    test shw shw getConf' (==)
  it "reads a bool value" $ do
    test sw sw getConfBool (==)
  where
    shw :: [Integer] -> String
    shw = show
    sw = map Char.toLower . show
