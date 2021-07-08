{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Test interaction between parsing output and logstash
module Logstash where

import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Reader

import qualified Data.Aeson              as Aeson
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.HashMap.Strict     as HMap
import           Data.Monoid
import           Data.Text               ( Text )
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text

import           Logging

import           NejlaCommon

import           Shelly

import           Test.Hspec.Expectations
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.TH

-- Set the default string type to Text so we don't get ambiguity errors
default ( Text )

exampleInput :: Text
exampleInput = "[Debug#test] test123\n[Debug#test] test345\n"

logstashConfNames :: [Text]
logstashConfNames =
  [ "1-input-stdin.conf", "2-filter-parse-logs.conf", "3-output-stdout.conf" ]

mkLogsstashConfPaths :: Text -> Text -> (Text, Text)
mkLogsstashConfPaths pwd name =
  (pwd <> "/tests/logstash/conf.d/" <> name, "/etc/logstash/conf.d/" <> name)

confVolume :: Text -> Text -> [Text]
confVolume pwd name =
  [ "-v"
  , let (lPath, dPath) = mkLogsstashConfPaths pwd name
    in lPath <> ":" <> dPath
  ]

checkConfFiles :: Sh ()
checkConfFiles = do
  workdir <- toTextIgnore <$> pwd
  forM_ logstashConfNames $ \name -> do
    let (path, _) = mkLogsstashConfPaths workdir name
    unlessM (test_f $ fromText path) . terror $
      "File" <> path <> " does not exist"

docker :: Text -> [Text] -> Sh Text
docker cmnd args = command "docker" [ cmnd ] args

logstash :: [Text] -> Sh Text
logstash parameters = do
  workdir <- toTextIgnore <$> pwd
  docker "run" $
    concat [ [ "--rm", "-i" ]
           , concat $ confVolume workdir <$> logstashConfNames
           , [ "elk_logstash", "logstash", "-f", "/etc/logstash/conf.d" ]
           , parameters
           ]

runLogstash :: MonadIO m => IORefLogger a -> m [Text]
runLogstash f = shelly . silently $ do
  checkConfFiles
  logs <- liftIO $ captureLogs f
  setStdin . Text.unlines $ Text.decodeUtf8 <$> logs
  out <- logstash [ "-w", "1", "--quiet", "-l", "/dev/null" ]
  stripped
    <- case Text.stripPrefix "Sending logstash logs to /dev/null.\n" out of
        Nothing -> terror "Expected output prefix \
                         \ \"Sending logstash logs to /dev/null.\\n\""
        Just s -> return s
  return $ splitLines stripped
  where
    splitLines lines = case Text.breakOn "}{" lines of
        (rest, "") -> [ rest ]
        (first, last) -> (first <> "}") : splitLines (Text.drop 1 last)

case_logstash :: IO ()
case_logstash = do
  res <- runLogstash $ do
    logEvent logTest1
    logWarnNS "database" "foo"
  let rows = Aeson.decode . BSL.fromStrict . Text.encodeUtf8 <$> res
  case rows of
      [ Just (Aeson.Object row1), Just (Aeson.Object row2) ] -> do
        withRow row1 $ do
          "event" `shouldDecodeTo` ("test" :: Text)
          "time" `shouldDecodeTo` log1TestTime
          "level" `shouldDecodeTo` (5 :: Int)
          "foo" `shouldDecodeTo` False
          "bar" `shouldDecodeTo` True
          "parsed_json" `shouldDecodeTo` True
        withRow row2 $ do
          "type" `shouldDecodeTo` ("logs" :: Text)
          "component" `shouldDecodeTo` ("database" :: Text)
          "level" `shouldDecodeTo` (4 :: Int)
          "message" `shouldDecodeTo` ("foo" :: Text)
      _ -> assertFailure $ "Expected 1 row, instead got "
        <> unlines (Text.unpack <$> res)
  where
    shouldDecodeTo x y = ReaderT $ \o -> case HMap.lookup x o of
        Nothing -> assertFailure $ "could not find element" <> show x
        Just v -> case Aeson.fromJSON v of
            Aeson.Error e ->
              assertFailure $ "Could not decode " <> show x <> " because " <> e
            Aeson.Success v -> v `shouldBe` y

    withRow row m = runReaderT m row

tests :: TestTree
tests = $testGroupGenerator
