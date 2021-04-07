{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module NejlaCommon.Logging
  -- @TODO: Explicit export. Don't export LogEvent constructor!
where

import qualified Control.Exception        as Ex
import           Control.Lens             hiding ((.=))
import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.TH            as Aeson
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Builder  as BS
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.CaseInsensitive     as CI
import           Data.Data
import qualified Data.HashMap.Strict      as HMap
import           Data.IORef
import qualified Data.List                as List
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.IO             as Text
import           Data.Time.Clock          ( UTCTime, getCurrentTime )
import           GHC.Generics
import qualified Network.HTTP.Types       as HTTP
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           System.IO
import qualified System.Log.FastLogger    as FastLogger
import qualified System.Process           as Process

import           NejlaCommon.Helpers

--------------------------------------------------------------------------------
-- Logging type class ----------------------------------------------------------
--------------------------------------------------------------------------------

-- LogRow and LogEvent are very similar and should maybe be joined.
-- The difference is that LogRow has times resolved

-- | Standardized logging row. This is a helper type to construct the JSON in
-- log messages
data LogRow = LogRow { logRowTime    :: !UTCTime
                     , logRowEvent   :: !Text
                     , logRowSource  :: !Text
                     , logRowLevel   :: !LogLevel
                     , logRowDetails :: !Value
                     } deriving (Show, Eq)

instance ToJSON LogRow where
  toJSON lr =
    let v = toJSON $ logRowDetails lr
        commons = [ "time"  .= logRowTime lr
                  , "event" .= logRowEvent lr
                  , "source" .= logRowSource lr
                  , "level" .= fromLogLevel (logRowLevel lr)
                  ]
    in case v of
         Object o ->
           Object $ o <> HMap.fromList commons
         _ -> object $ [ "details" .= v ] <> commons
    where
      fromLogLevel :: LogLevel -> Text
      fromLogLevel LevelDebug = "DEBUG"
      fromLogLevel LevelInfo = "INFO"
      fromLogLevel LevelWarn = "WARN"
      fromLogLevel LevelError = "ERROR"
      fromLogLevel (LevelOther other) = other

instance FromJSON LogRow where
  parseJSON = withObject "log row" $ \o -> do
    tp <- o .: "event"
    time <- o .: "time"
    source <- o .: "source"
    mbPayload <- o .:? "details"
    lvl <- toLogLevel <$> o .: "level"
    payload <- case mbPayload of
                 Nothing -> parseJSON . Object $
                             o // "event"
                               // "time"
                               // "source"
                               // "level"
                 Just pl -> return pl
    return LogRow { logRowTime    = time
                  , logRowEvent   = tp
                  , logRowDetails = payload
                  , logRowSource  = source
                  , logRowLevel   = lvl
                  }
      where
         infixl 8 //
         o // k = HMap.delete k o
         toLogLevel :: Text -> LogLevel
         toLogLevel txt = case matchLogLevel $ Text.toUpper txt of
                            Just lvl -> lvl
                            Nothing -> LevelOther txt
         matchLogLevel "DEBUG" = Just LevelDebug
         matchLogLevel "INFO"  = Just LevelInfo
         matchLogLevel "WARN"  = Just LevelWarn
         matchLogLevel "ERROR" = Just LevelError
         matchLogLevel _   = Nothing


instance ToLogStr LogRow where
  toLogStr s = toLogStr $ Aeson.encode s

-- | Use fast-logger / monad-logger log function to log log row
fromLogFun :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
           -> LogRow
           -> IO ()
fromLogFun logfun row =
  logfun defaultLoc (logRowSource row) (logRowLevel row) (toLogStr row)
  where
    defaultLoc :: Loc
    defaultLoc = Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)


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
logEvent (toLogEvent -> lEvent) = do
  row <- liftIO toLogRow
  logWithoutLoc "json_event" (lEvent ^. level) row

  where
    logWithoutLoc = monadLoggerLog defaultLoc
    defaultLoc :: Loc
    defaultLoc = Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)
    toLogRow :: IO LogRow
    toLogRow = do
      time' <- case lEvent ^. time of
        Nothing -> getCurrentTime
        Just t -> return t
      return LogRow{ logRowTime    = time'
                   , logRowEvent   = lEvent ^. type'
                   , logRowDetails = lEvent ^. details
                   , logRowSource  = lEvent ^. source
                   , logRowLevel   = lEvent ^. level
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
             , requestLogResponseHeaders :: ![LogHeader]
             , requestLogResponseBody :: !(Maybe Text)
             , requestLogIP           :: !(Maybe Text)
             } deriving (Show, Typeable, Data, Generic)

Aeson.deriveJSON (aesonTHOptions "requestLog") ''RequestLog

instance IsLogEvent RequestLog where
  toLogEvent = eventDetails "request"

instance FastLogger.ToLogStr RequestLog where
  toLogStr = toLogStr . (<> "\n") . Aeson.encode

-- | Middleware for logging http requests and responses.
--
-- /NB/ The entirety of the request and response bodies are logged, which can be
-- very large
logHttpCalls :: (LogRow -> IO ())
             ->  Wai.Middleware
logHttpCalls logRequest app request' respond = do
    (reqB, reqBody) <- do
        body <- getBody (Wai.getRequestBodyChunk request') BS.empty
        bdRef <- newIORef body
        let rBody = do
                bd <- readIORef bdRef
                writeIORef bdRef BS.empty
                return  bd
        return (rBody, if BS.null body then Nothing else Just body)
    let request = request'{Wai.requestBody = reqB}
    rr <- app request $ \response -> do
        body <- responseToText response
        now <- getCurrentTime
        let reqLog =
              RequestLog
              { requestLogMethod       = bst $ Wai.requestMethod request
              , requestLogPath         = Wai.pathInfo request
              , requestLogQuery        = bst $ Wai.rawQueryString request
              , requestLogHeaders      =
                  toLogHeaders $ Wai.requestHeaders request
              , requestLogRequestBody         = bst <$> reqBody
              , requestLogResponseCode =
                  HTTP.statusCode $ Wai.responseStatus response
              , requestLogResponseHeaders = toLogHeaders $ Wai.responseHeaders response
              , requestLogResponseBody = body
              , requestLogIP = bst <$> (List.lookup "X-Real-IP"
                                         $ Wai.requestHeaders request)
              }

            logRow =
              LogRow
              { logRowTime = now
              , logRowEvent = "http full request"
              , logRowSource = "logHttpCalls"
              , logRowDetails = toJSON reqLog
              , logRowLevel = LevelInfo
              }
        logRequest logRow
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

-- | Log to a file
--
-- Logged data includes all headers, request an response bodies, HTTP response
-- codes and network addresses.
--
-- A timestamp (according to the provided format) is prefixed to the
-- filename. When the timestamp changes a new file is created and the old file
-- is compressed using xc
--
-- This function assumes that an 'xz' executable is in path
--
-- Example usage:
-- > run = withFileLogger "/var/log/myserver.log" "%F" $ \logger -> do
-- >   Warp.run (logHttpCalls logger) myApp
withFileLogger :: ToLogStr log =>
                  FilePath -- ^ Base name of the log files, a timestamp is
                                -- prepended to the base name
               -> Text -- ^ Timestamp format (e.g. "%F" for date). Same
                       -- format as stftime, see
                       -- https://linux.die.net/man/3/strftime
               -> ((log -> IO ()) -> IO a) -- ^ Callback
               -> IO a
withFileLogger path format f = do
  let logSpec = FastLogger.TimedFileLogSpec
        { FastLogger.timed_log_file = path
        , FastLogger.timed_timefmt = Text.encodeUtf8 format
        , FastLogger.timed_same_timeframe = (==)
        , FastLogger.timed_post_process = \file ->
            Process.callProcess "xz" [file]
        }
      logConfig = FastLogger.LogFileTimedRotate logSpec 4096

  FastLogger.withFastLogger logConfig $ \logger -> do
    f (logger . toLogStr)

-- Logging of Exceptions

data ExceptionEvent = ExceptionEvent
  { exceptionEventException :: !Text
  , exceptionEventDescription :: !Text
  , exceptionEventMethod :: !Text
  , exceptionEventPath :: !Text
  } deriving Show

Aeson.deriveJSON (aesonTHOptions "exceptionEvent") '' ExceptionEvent

-- | Log all unhandled Exceptions as JSON.
-- Expects a "logger" functions that tells it where to actually print the LogRow.
-- Use e.g. 'withFileLogger' or 'askLoggerIO' together with 'fromLogFun' to get one
--
-- Produces JSON for easy parsing
--
-- Fields:
-- [@exception@]: Type of the exception (as produced by 'typof')
-- [@description@]: String representation of the exception (as produced by show)
-- [@method@]: HTTP method of the request (or "N/A" if not available)
-- [@path@]: HTTP request path (or "server" if exception happened outside a request)
-- [@time@]: ISO 8601 formatted timestamp
-- [@event@]: Always "unhandled exception" (helps parsing the log message)
-- [@level@]: Always "ERROR"
-- [@source@]: Always "webserver"
--
-- > {
-- >  "event": "unhandled exception",
-- >  "exception": "ErrorCall",
-- >  "path": "/crash",
-- >  "time": "2021-01-12T15:47:08.496182106Z",
-- >  "method": "GET",
-- >  "source": "webserver",
-- >  "level": "ERROR",
-- >  "description": "crash!"
-- > }
logOnException :: (LogRow -> IO ()) -> Warp.Settings -> Warp.Settings
logOnException logFunction = Warp.setOnException $ \mbReq (Ex.SomeException e) -> do
    now <- getCurrentTime
    let (method, path) = case mbReq of
                Nothing -> ("N/A", "server")
                Just req -> (Text.decodeUtf8With Text.lenientDecode
                              (Wai.requestMethod req)
                            , Text.decodeUtf8With Text.lenientDecode
                              (Wai.rawPathInfo req)
                            )
    let evt =
          ExceptionEvent
          { exceptionEventException = Text.pack $ show (typeOf e)
          , exceptionEventDescription = Text.pack $ show e
          , exceptionEventMethod = method
          , exceptionEventPath = path
          }
        row =
          LogRow
          { logRowTime = now
          , logRowEvent = "unhandled exception"
          , logRowSource = "webserver"
          , logRowDetails = toJSON evt
          , logRowLevel = LevelError
          }
    logFunction row
