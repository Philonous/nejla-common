{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | A Servant combinator for streaming raw bytes with a runtime-chosen
-- content type and optional @Content-Disposition@ (e.g. file downloads),
--
-- Example usage:
--
-- @
-- type API = "report" 'Servant.API.:>' 'ByteStream' ''GET' 200
--
-- server :: 'Server' API
-- server = pure 'ByteStream'
--   { byteStreamContentDisposition = Just "attachment; filename=\\"report.csv\\""
--   , byteStreamContentType        = "text/csv"
--   , byteStreamSource             = 'Conduit.sourceFile' "report.csv"
--   }
-- @
module NejlaCommon.Servant.ByteStream where

import Conduit (ConduitT, ResourceT)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder  as BB
import Data.Data
import qualified Data.OpenApi as OpenApi
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import GHC.TypeLits
import Network.HTTP.Types
import Network.Wai (Request, Response, requestMethod, responseStream)
import Servant.API (OctetStream, toSourceIO)
import Servant.API.Verbs
import Servant.Conduit ()
import Servant.OpenApi (HasOpenApi (..), toOpenApi)
import Servant.Server
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO
import Servant.Server.Internal.RouteResult
import Servant.Server.Internal.Router
import qualified Servant.Types.SourceT as S

-- | Endpoint type and handler return value: a streamed response body.
data ByteStream (method :: StdMethod) (status :: Nat)
  = ByteStream
  { -- | Optional @Content-Disposition@, e.g. @"attachment; filename=\"x.csv\""@.
    byteStreamContentDisposition :: Maybe Text,
    -- | @Content-Type@ header value
    byteStreamContentType :: Text,
    -- | Body producer; chunks are written and flushed as they arrive.
    byteStreamSource :: ConduitT () ByteString (ResourceT IO) ()
  }

-- | Serve a 'ByteStream' endpoint.
instance
  {-# OVERLAPPABLE #-}
  (ReflectMethod method, KnownNat status) =>
  HasServer (ByteStream method status) context
  where
  type
    ServerT (ByteStream method status) m =
      m (ByteStream method status)

  route Proxy _ = streamRouter method status
    where
      -- Type-level method/status reified for runtime checks.
      method = reflectMethod (Proxy :: Proxy method)
      status :: Status
      status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)

  -- No extra context
  hoistServerWithContext _ _ nt = nt

-- | Builds the leaf router: checks the method, runs the handler, then streams
-- its conduit out via WAI's 'responseStream'.
streamRouter ::
  Method ->
  Status ->
  Delayed env (Handler (ByteStream method status)) ->
  Router' env (Request -> (RouteResult Response -> IO r) -> IO r)
streamRouter method status action = leafRouter $ \env request respond ->
  runAction
    (action `addMethodCheck` methodCheck method request)
    env
    request
    respond
    $ \stream ->
      let contentHeader =
            ( "Content-Type",
              Text.encodeUtf8 $ byteStreamContentType stream
            )
          -- Conduit -> SourceT; kStepLBS feeds steps to a continuation.
          S.SourceT kStepLBS = toSourceIO $ byteStreamSource stream
       in Route
            $ responseStream
              status
              ( contentHeader
                  : case byteStreamContentDisposition stream of
                    Nothing -> []
                    Just dispo ->
                      [ ( "Content-Disposition",
                          Text.encodeUtf8 dispo
                        )
                      ]
              )
            $ \write flush -> do
              -- Drain the source, flushing after each chunk so clients see
              -- data promptly. Errors abort the (already-started) response.
              let loop S.Stop = flush
                  loop (S.Error err) = fail err
                  loop (S.Skip s) = loop s
                  loop (S.Effect ms) = ms >>= loop
                  loop (S.Yield lbs s) = do
                    write (BB.byteString lbs)
                    flush
                    loop s
              kStepLBS loop
  where
    -- 405 unless the request method matches (HEAD is accepted for GET).
    methodCheck method request
      | allowedMethod method request = return ()
      | otherwise = delayedFail err405
    allowedMethodHead method request =
      method == methodGet && requestMethod request == methodHead
    allowedMethod method request =
      allowedMethodHead method request || requestMethod request == method

instance
  {-# OVERLAPPING #-}
  (KnownNat status, Typeable method) =>
  HasOpenApi (ByteStream method status)
  where
  toOpenApi _ =
    toOpenApi
      ( Proxy
          @( Verb
               'GET
               status
               '[OctetStream]
               (ByteStream method status)
           )
      )

instance
  (Typeable method, KnownNat code) =>
  OpenApi.ToSchema (ByteStream method code)
  where
  declareNamedSchema _ =
    return $ OpenApi.NamedSchema Nothing OpenApi.binarySchema
