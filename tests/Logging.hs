{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Logging where

import           Control.Lens hiding ((.=), elements)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Default
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time
import           NejlaCommon
import           System.Log.FastLogger
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

import           Persistent.Common

instance Arbitrary UTCTime where
    arbitrary =
        do randomYear <- choose (1900, 2100) :: Gen Integer
           randomMonth <- choose (1, 12) :: Gen Int
           let lastDay = gregorianMonthLength randomYear randomMonth
           randomDay <- choose (1, lastDay) :: Gen Int
           randomTime <- choose (0, 86401) :: Gen Int
           return $ UTCTime (fromGregorian randomYear randomMonth randomDay)
                            (fromIntegral randomTime)

instance Arbitrary Text where
  arbitrary = Text.pack <$> arbitrary
  shrink = map Text.pack . shrink . Text.unpack

instance Arbitrary LogLevel where
  arbitrary = elements [ LevelDebug
                       , LevelInfo
                       , LevelWarn
                       , LevelError
                       , LevelOther "Critical"
                       , LevelOther "Fatal"
                       ]

instance Arbitrary LogRow where
  arbitrary = LogRow <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> (Aeson.object <$> listOf
                           ((,) <$> (arbitrary `suchThat` (`notElem`
                                                            [ "type"
                                                            , "time"
                                                            , "log_level"
                                                            ]
                                                          )
                                    )
                                <*> (Aeson.toJSON  <$> (arbitrary :: Gen Text))
                           ))

--------------------------------------------------------------------------------
-- Log Row JSON instances ------------------------------------------------------
--------------------------------------------------------------------------------

prop_logRow_json_roundtrip :: LogRow -> Bool
prop_logRow_json_roundtrip =
  \logRow -> Aeson.decode (Aeson.encode logRow) == Just (logRow :: LogRow)


--------------------------------------------------------------------------------
-- logEvent --------------------------------------------------------------------
--------------------------------------------------------------------------------

data LogTest = LogTest { logTestTime  :: Maybe UTCTime
                       , logTestLevel :: LogLevel
                       , logTestType  :: Text
                       , logTestPayload :: Aeson.Value
                       } deriving (Show, Eq)

instance IsLogEvent LogTest where
  toLogEvent lt = event "test" & details .~ logTestPayload lt
                               & time    .~ logTestTime lt
                               & level   .~ logTestLevel lt
                               & type'   .~ logTestType lt

log1TestTime :: UTCTime
log1TestTime = read "2016-08-17 13:52:03.998382 UTC"

logTest1 :: LogTest
logTest1 = LogTest { logTestTime    = Just log1TestTime
                   , logTestLevel   = LevelInfo
                   , logTestType    = "test"
                   , logTestPayload = Aeson.object [ ("foo" Aeson..= False)
                                                   , ("bar" Aeson..= True)
                                                   ]
                   }
case_log :: IO ()
case_log = withDB $ \pool -> runApp readCommitted def pool () $ do
  logEvent LogTest{ logTestTime  = Nothing
                  , logTestLevel = LevelInfo
                  , logTestType  = "testtype"
                  , logTestPayload = object [ "foo" .= (3 :: Int)]
                  }

--------------------------------------------------------------------------------
-- MonadLogger -----------------------------------------------------------------
--------------------------------------------------------------------------------

-- Monad for capturing logged messages in an IORef
newtype IORefLogger a = IORefLogger (ReaderT (IORef [BS.ByteString]) IO a)
                        deriving (Monad, Functor, Applicative, MonadIO)

instance MonadLogger IORefLogger where
  monadLoggerLog loc logSource logLevel msg = IORefLogger . ReaderT $ \ref -> do
    let logStr = defaultLogStr loc logSource logLevel (toLogStr  msg)
    modifyIORef ref (fromLogStr logStr :)

captureLogs :: IORefLogger a -> IO [ByteString]
captureLogs (IORefLogger f) = do
  ref <- newIORef []
  _ <- runReaderT f ref
  logs <- readIORef ref
  return $ reverse logs


tests :: TestTree
tests = $testGroupGenerator
