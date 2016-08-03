-- Copyright © 2014-2015 Lambdatrade AB. All rights reserved.

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module NejlaCommon.Persistence.Logging where

import           Control.Applicative
import qualified Control.Exception as Ex
import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import           Data.Data
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
import           System.IO (stderr)

import           NejlaCommon.Helpers

--------------------------------------------------------------------------------
-- Logging type class ----------------------------------------------------------
--------------------------------------------------------------------------------

-- | Standardized logging row
data LogRow = LogRow { logRowTime    :: !UTCTime
                     , logRowType    :: !Text
                     , logRowPayload :: !Value
                     } deriving Show

-- | Create a log row
toLogRow :: LogMessage a => a -> IO LogRow
toLogRow v = do
  now <- getCurrentTime
  return LogRow{ logRowTime    = now
               , logRowType    = messageType v
               , logRowPayload = Aeson.toJSON v
               }

class Aeson.ToJSON a => LogMessage a  where
  messageType :: a -> Text

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

instance LogMessage RequestLog where
  messageType _ = "request"

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
      let txt = Text.decodeUtf8With Text.lenientDecode
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
catchMiddleware logEvent app = \req cont ->
    Ex.catch (app req cont)
        (\e -> do
              now <- getCurrentTime
              Text.hPutStrLn stderr $ "[Error] Unhandled exception: "
                                      <> showText (e :: Ex.SomeException)
              Ex.catch ( logEvent $
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
