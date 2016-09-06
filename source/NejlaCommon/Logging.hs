-- Copyright © 2014-2015 Lambdatrade AB. All rights reserved.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module NejlaCommon.Logging
  -- @TODO: Explicit export. Don't export LogEvent constructor!
where

import qualified Control.Exception as Ex
import           Control.Lens hiding ((.=))
import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import           Data.Data
import qualified Data.HashMap.Strict as HMap
import           Data.IORef
import qualified Data.List as List
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.IO as Text
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock (getCurrentTime)
import           GHC.Generics
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import           System.IO

import           NejlaCommon.Helpers

--------------------------------------------------------------------------------
-- Logging type class ----------------------------------------------------------
--------------------------------------------------------------------------------

-- | Standardized logging row. This is a helper type to construct the JSON in
-- log messages
data LogRow = LogRow { logRowTime    :: !UTCTime
                     , logRowEvent   :: !Text
                     , logRowSource  :: !Text
                     , logRowDetails :: !Value
                     } deriving (Show, Eq)

instance ToJSON LogRow where
  toJSON lr =
    let v = toJSON $ logRowDetails lr
        commons = [ "time"  .= logRowTime lr
                  , "event" .= logRowEvent lr
                  , "source" .= logRowSource lr
                  ]
    in case v of
         Object o ->
           Object $ o <> HMap.fromList commons
         _ -> object $ [ "details" .= v ] <> commons

instance FromJSON LogRow where
  parseJSON = withObject "log row" $ \o -> do
    tp <- o .: "event"
    time <- o .: "time"
    source <- o .: "source"
    mbPayload <- o .:? "details"
    payload <- case mbPayload of
                 Nothing -> parseJSON . Object $
                             o // "event"
                               // "time"
                               // "source"
                 Just pl -> return pl
    return LogRow { logRowTime    = time
                  , logRowEvent   = tp
                  , logRowDetails = payload
                  , logRowSource  = source
                  }
      where
         infixl 8 //
         o // k = HMap.delete k o

-- | An Event to be logged. Use the 'event' constructor to create and update
-- using record syntax or lenses (see 'event' for more details)
data LogEvent =
  LogEvent { logEventType :: !Text -- ^ What sort of event this is (e.g. "login
                                   -- failed").
           , logEventLevel :: !LogLevel -- ^ Log level
           , logEventSource :: !Text -- ^ Component that is logging the
                                       -- event. Can be left at "" if not
                                       -- relevant
           , logEventTime :: !(Maybe UTCTime)
             -- ^ Time the event happened. Can usually be left at Nothing and
             -- will then be filled in with the current system time
           , logEventDetails :: !Aeson.Value
           } deriving (Show, Eq, Generic)

makeLensesWith camelCaseFields' ''LogEvent

-- | Construct an event given it's type. Use with record updates or lenses to
-- set the remaining fields
--
-- Sets the following default values:
--
-- level = LevelInfo
-- source = ""
-- time = Nothing
-- Details = object []
--
-- Example:
--
-- > (event "database connection failure")
-- >   { logEventLevel = LevelError
-- >   , logEventDetails = toJSON errorDetails
-- >   }
--
-- Or using Lenses:
--
-- > event "database connection failure"
-- >   & level .~ LevelError
-- >   & details .~ toJSON errorDetails
event :: Text -> LogEvent
event tp = LogEvent { logEventType = tp
                    , logEventLevel = LevelInfo
                    , logEventSource = ""
                    , logEventTime = Nothing
                    , logEventDetails = Aeson.object []
                    }

-- | Like event, but also sets the details
eventDetails :: ToJSON a => Text -> a -> LogEvent
eventDetails tp v = event tp & details .~ toJSON v

-- | If you already have data types that you want to log (e.g. exceptions),
-- defining an instance once might be more convenient than converting them to
-- logEvents in multiple places
class IsLogEvent a where
  toLogEvent :: a -> LogEvent

instance IsLogEvent LogEvent where
  toLogEvent = Prelude.id

-- | Log an event. Accepts a LogEvent or a value that's an instance of the
-- IsLogEvent class
logEvent :: (MonadIO m, MonadLogger m, IsLogEvent a) =>
            a
         -> m ()
logEvent (toLogEvent -> lEvent)= do
  row <- liftIO toLogRow
  logWithoutLoc "json_event" (lEvent ^. level) $ encodeText row

  where
    logWithoutLoc = monadLoggerLog defaultLoc
    defaultLoc :: Loc
    defaultLoc = Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)
    encodeText = Text.decodeUtf8 . BSL.toStrict . Aeson.encode
    toLogRow :: IO LogRow
    toLogRow = do
      time' <- case lEvent ^. time of
        Nothing -> getCurrentTime
        Just t -> return t
      return LogRow{ logRowTime    = time'
                   , logRowEvent   = lEvent ^. type'
                   , logRowDetails = lEvent ^. details
                   , logRowSource  = lEvent ^. source
                   }

--------------------------------------------------------------------------------
-- Request/Response log --------------------------------------------------------
--------------------------------------------------------------------------------

-- | Helper data type for logging HTTP requests
data LogHeader = LogHeader{ logHeaderName  :: !Text
                          , logHeaderValue :: !Text
                          } deriving (Show, Typeable, Data, Generic)

-- | Convert a HTTP header to a loggable header
toLogHeaders :: [(CI.CI ByteString, ByteString)] -> [LogHeader]
toLogHeaders = fmap toHeader
  where
  toHeader (bsn, bsv) = LogHeader{ logHeaderName = tDecode $ CI.foldedCase bsn
                                 , logHeaderValue = tDecode bsv
                                 }
  tDecode = Text.decodeUtf8With Text.lenientDecode

Aeson.deriveJSON (aesonTHOptions "logHeader") ''LogHeader

-- | Data type for logging HTTP requests
data RequestLog =
  RequestLog { requestLogMethod       :: !Text
             , requestLogPath         :: ![Text]
             , requestLogQuery        :: !Text
             , requestLogHeaders      :: ![LogHeader]
             , requestLogRequestBody  :: !(Maybe Text)
             , requestLogResponseCode :: !Int
             , requestLogResponseBody :: !(Maybe Text)
             , requestLogIP           :: !(Maybe Text)
             } deriving (Show, Typeable, Data, Generic)

Aeson.deriveJSON (aesonTHOptions "requestLog") ''RequestLog

instance IsLogEvent RequestLog where
  toLogEvent = eventDetails "request"

-- | Middleware for logging http requests and responses.
--
-- /NB/ The entirety of the request and response bodies are logged, which can be
-- very large
logHttpCalls :: (RequestLog -> IO ())
               ->  Wai.Middleware
logHttpCalls logRequest app request' respond = do
    -- We can't use (Wai.strictRequestBody request) because that consumes the
    -- request body. TODO: Figure this out
    (reqB, reqBody) <- do
        body <- getBody (Wai.requestBody request') BS.empty
        bdRef <- newIORef body
        let rBody = do
                bd <- readIORef bdRef
                writeIORef bdRef BS.empty
                return  bd
        return (rBody, if BS.null body then Nothing else Just body)
    let request = request'{Wai.requestBody = reqB}
    rr <- app request $ \response -> do
        body <- responseToText response
        logRequest
          RequestLog { requestLogMethod       = bst $ Wai.requestMethod request
                     , requestLogPath         = Wai.pathInfo request
                     , requestLogQuery        = bst $ Wai.rawQueryString request
                     , requestLogHeaders      =
                         toLogHeaders $ Wai.requestHeaders request
                     , requestLogRequestBody         = bst <$> reqBody
                     , requestLogResponseCode =
                         HTTP.statusCode $ Wai.responseStatus response
                     , requestLogResponseBody = body
                     , requestLogIP = bst <$> (List.lookup "X-Real-IP"
                                                $ Wai.requestHeaders request)
                     }
        respond response
    return rr
  where
    getBody nextChunk acc = do
        chunk <- nextChunk
        if BS.null chunk
            then return acc
            else getBody nextChunk (acc <> chunk)
    bst = Text.decodeUtf8With Text.lenientDecode
    responseToText resp = do
      ref <- newIORef []
      case Wai.responseToStream resp of
       (_, _, f) -> f $ \sb -> sb (\chunk -> modifyIORef ref (chunk:))
                                  (return ())
      chunks <- List.reverse <$> readIORef ref
      let txt :: Text
          txt = Text.decodeUtf8With Text.lenientDecode
                  . BSL.toStrict . BS.toLazyByteString $ mconcat chunks
      return $ Just txt

--------------------------------------------------------------------------------
-- Critical Event --------------------------------------------------------------
--------------------------------------------------------------------------------

data CriticalEvent =
  CriticalEvent
    { criticalEventTime      :: !UTCTime
    , criticalEventSystem    :: !Text
    , criticalEventCondition :: !Text
    , criticalEventContext   :: !Text
    , criticalEventDetails   :: !Text
    } deriving (Show, Typeable, Data, Generic)

catchMiddleware :: (CriticalEvent -> IO ()) -> Wai.Middleware
catchMiddleware lEvent app = \req cont ->
    Ex.catch (app req cont)
        (\e -> do
              now <- getCurrentTime
              Text.hPutStrLn stderr $ "[Error] Unhandled exception: "
                                      <> showText (e :: Ex.SomeException)
              Ex.catch ( lEvent $
                  CriticalEvent
                    { criticalEventTime = now
                    , criticalEventSystem = "API"
                    , criticalEventCondition = "unhandled exception"
                    , criticalEventContext = ""
                    , criticalEventDetails = showText e
                    })
                  (\e' -> Text.hPutStrLn stderr $
                         "[Error] Exception while trying to write to Critical Event log: "
                         <> showText (e' :: Ex.SomeException))
              cont (Wai.responseBuilder HTTP.status500 [] ""))
