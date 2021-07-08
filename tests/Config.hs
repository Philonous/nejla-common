{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import           Control.Monad.Logger

import qualified Data.Char               as Char
import           Data.Configurator       as Conf
import           Data.Monoid
import           Data.Text               ( Text )
import qualified Data.Text               as Text
import qualified Data.Text.Lazy          as LText
import qualified Data.Text.Lazy.IO       as LText

import           Formatting

import           NejlaCommon.Config

import           System.Environment
import           System.IO
import qualified System.IO.Temp          as Temp

import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.HUnit        ( testCase )
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

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

test :: (Show s, Show t, Arbitrary t)
     => (t -> String) -- ^ How the value should be rendered to a String for
                      -- storage in the conf file
     -> (t -> String) -- ^ How the value should be rendered to a String for
                      -- storage in the env variable
     -> (String -> Text -> Either Text t -> Config -> LoggingT IO s)
     -- ^ The config retrieval function to test
     -> (s -> t -> Bool) -- ^ How the returned value should be compared to the
                         -- original value (in case of maybe etc)
     -> IO ()
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

case_readable_integer_list :: IO ()
case_readable_integer_list = test shw shw getConf' (==)
  where
    shw :: [Integer] -> String
    shw = show

-- case_readable_string :: IO ()
-- case_readable_string = test cf ev getConf (==)
--   where
--     cf :: Text -> String
--     cf = ((\t -> "\"" <> t <> "\"") . Text.unpack)
--     ev :: Text -> String
--     ev = Text.unpack
case_Bool :: IO ()
case_Bool = test sw sw getConfBool (==)
  where
    sw = map Char.toLower . show

tests :: TestTree
tests = $testGroupGenerator
