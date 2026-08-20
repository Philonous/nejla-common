{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Scaffolding for API tests
module NejlaCommon.Test.Api
  ( -- * Session context
    Ctx,
    ctxPayload,
    getPayload,

    -- * Request tracing
    addTrace,
    amendTrace,
    traceRequest,
    requestVia,

    -- * Resource acquisition
    Setup,
    runSetup,
    withLinkedAsync,

    -- * The harness
    LogFunc,
    WithApp (..),
    withPool,
    mkWithApp,
    reportFailures,
  )
where

import Control.Monad (when)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Logger
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO as Text
import qualified Database.Persist.Postgresql as P
import qualified NejlaCommon.Persistence.Migration as Migration
import NejlaCommon.Test
  ( cleanDB,
    dbTestConnectInfo,
    loggingToChan,
    withTestDB,
  )
import Network.HTTP.Types (Header, statusCode)
import Network.Wai.Test
  ( SResponse,
    simpleBody,
    simpleStatus,
  )
import Test.Hspec.Wai (WaiSession)
import qualified Test.Hspec.Wai as Wai
import UnliftIO
import qualified UnliftIO.Exception as Ex

--------------------------------------------------------------------------------
-- Session context ------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The session context: a framework-owned backtrace slot plus whatever the
-- application wants to carry.
--
-- The constructor is deliberately not exported. 'mkWithApp' hands the per-test
-- 'Setup' the partially applied constructor, which is the only way to build a
-- 'Ctx' -- so the backtrace the tracing functions write to is necessarily the
-- one that gets printed on failure.
data Ctx a
  = Ctx
  { ctxBacktrace :: IORef [Text],
    ctxPayload :: a
  }

instance Functor Ctx where
  fmap f c = c {ctxPayload = f (ctxPayload c)}

-- | The application-specific part of the current session context.
getPayload :: WaiSession (Ctx a) a
getPayload = ctxPayload <$> Wai.getState

--------------------------------------------------------------------------------
-- Request tracing ------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Push a new entry onto the backtrace.
addTrace :: Text -> WaiSession (Ctx a) ()
addTrace str = do
  ref <- ctxBacktrace <$> Wai.getState
  liftIO $ atomicModifyIORef' ref (\btr -> (str : btr, ()))

-- | Add a bullet point to the most recent backtrace entry.
amendTrace :: Text -> WaiSession (Ctx a) ()
amendTrace str = do
  ref <- ctxBacktrace <$> Wai.getState
  liftIO $ atomicModifyIORef' ref $ \case
    -- The originals matched irrefutably on (b : bs) and would have thrown here.
    [] -> ([str], ())
    (b : bs) -> (b <> "\n    * " <> str : bs, ())

-- | Run a request, recording the method, URI, headers and a caller-supplied
-- description of the body, and then the status (plus the body, for errors).
traceRequest ::
  (Show desc) =>
  -- | Method
  ByteString ->
  -- | Path
  ByteString ->
  -- | Description of the request body
  desc ->
  [Header] ->
  WaiSession (Ctx a) SResponse ->
  WaiSession (Ctx a) SResponse
traceRequest method uri ctx headers f = do
  addTrace $
    "request: "
      <> Text.decodeUtf8 method
      <> " "
      <> Text.decodeUtf8 uri
      <> "\n    * Context: "
      <> Text.pack (show ctx)
      <> Text.concat
        ( headers <&> \(hdr, val) ->
            "\n    * "
              <> Text.pack (show hdr)
              <> ": "
              <> abridge (Text.decodeUtf8 val)
        )
  response <- f
  let scode = statusCode (simpleStatus response)
  amendTrace $ "Response " <> Text.pack (show scode)
  let body = Text.decodeUtf8With lenientDecode (BSL.toStrict $ simpleBody response)
  when (scode >= 400 && not (Text.null body)) $
    amendTrace $
      "ResponseBody " <> body
  return response
  where
    abridge txt
      | Text.length txt > 50 = Text.take 20 txt <> "[...]" <> Text.takeEnd 20 txt
      | otherwise = txt

-- | A traced request over an arbitrary transport. @perform@ is the thing that
-- actually issues the request ('Wai.request', a multipart sender, ...); the
-- header actions are sequenced in the session first, so they can draw on
-- per-session state such as a nonce pool.
requestVia ::
  (Show body) =>
  ( ByteString ->
    ByteString ->
    [Header] ->
    body ->
    WaiSession (Ctx a) SResponse
  ) ->
  -- | Method
  ByteString ->
  -- | Path
  ByteString ->
  [WaiSession (Ctx a) Header] ->
  body ->
  WaiSession (Ctx a) SResponse
requestVia perform method path mkHeaders body = do
  headers <- sequence mkHeaders
  traceRequest method path body headers $ perform method path headers body

--------------------------------------------------------------------------------
-- Resource acquisition --------------------------------------------------------
--------------------------------------------------------------------------------

-- | Continuation-passing resource acquisition. Like @ContT@, but polymorphic in
-- the answer type, so a chain of @withFoo@-style brackets can still be run at
-- whatever result type the caller wants -- which is exactly what 'WithApp'
-- needs.
newtype Setup a = Setup {runSetup :: forall r. (a -> IO r) -> IO r}

instance Functor Setup where
  fmap f (Setup g) = Setup $ \k -> g (k . f)

instance Applicative Setup where
  pure x = Setup ($ x)
  Setup f <*> Setup x = Setup $ \k -> f (\f' -> x (k . f'))

instance Monad Setup where
  Setup x >>= f = Setup $ \k -> x (\a -> runSetup (f a) k)

instance MonadIO Setup where
  liftIO m = Setup (m >>=)

-- | Spawn a background thread for the duration of the test and link it, so that
-- a worker dying fails the test rather than hanging it.
withLinkedAsync :: IO a -> Setup (Async a)
withLinkedAsync act =
  Setup $ \k -> withAsync act $ \asnc -> link asnc >> k asnc

--------------------------------------------------------------------------------
-- The harness -----------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The type of 'askLoggerIO'.
type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

-- | Acquire a migrated test database and a handle for draining captured logs.
withPool ::
  (MonadUnliftIO m, MonadCatch m) =>
  -- | Log channel bound
  Int ->
  Migration.M () ->
  (IO [ByteString] -> P.ConnectionPool -> LoggingT m a) ->
  m a
withPool logBound migration f = do
  conInfo <- liftIO dbTestConnectInfo
  loggingToChan logBound $ \getLogs ->
    filterLogger (\source _level -> source /= "SQL-stats") $
      withTestDB conInfo 5 migration $ \pool ->
        f getLogs pool

-- | On failure, dump the captured logs and the request backtrace before
-- rethrowing.
reportFailures :: IO [ByteString] -> IORef [Text] -> IO a -> IO a
reportFailures getLogs btRef =
  Ex.handle $ \(e :: Ex.SomeException) -> do
    getLogs >>= mapM_ BS8.putStrLn
    readIORef btRef >>= \case
      [] -> putStrLn "Empty backtrace"
      backtrace -> do
        putStrLn "\nBacktrace (from most recent):"
        mapM_ Text.putStrLn ["  - " <> t | t <- backtrace]
    putStrLn $ "\nError: " ++ show e
    Ex.throwIO e

-- | @r@ is whatever the individual test cases are handed.
data WithApp r
  = WithApp (IO [ByteString]) (forall a. (r -> IO a) -> IO a)

-- | Build the test harness.
--
-- The pool and the logging function are created once, up front; the 'Setup'
-- returned by @mkRunner@ runs afresh before each test, after the database has
-- been reset. It is handed a @a -> Ctx a@ with which to wrap its
-- application-specific payload; that is the only way to build a 'Ctx', so the
-- backtrace cannot get lost.
--
-- This is in continuation-passing style because 'withPool' releases the pool
-- once we return.
mkWithApp ::
  (MonadUnliftIO m, MonadCatch m) =>
  -- | Log channel bound
  Int ->
  Migration.M () ->
  (LogFunc -> P.ConnectionPool -> (a -> Ctx a) -> Setup r) ->
  (WithApp r -> LoggingT m res) ->
  m res
mkWithApp logBounds migration mkRunner k =
  withPool logBounds migration $ \getLogs pool -> do
    -- This runs _once_, before all the tests
    logFunc <- askLoggerIO
    k $ WithApp getLogs $ \use -> do
      -- This runs before _every_ test
      P.runSqlPool cleanDB pool
      btRef <- newIORef []
      runSetup (mkRunner logFunc pool (Ctx btRef)) $ \r -> do
        -- Drain the log channel so we don't get logs from previous tests
        _ <- getLogs
        reportFailures getLogs btRef $ use r
