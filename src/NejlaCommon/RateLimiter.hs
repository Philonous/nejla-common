{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Self-tuning rate limiter using additive increase / multiplicative decrease
--
-- A rate limiter that discovers an acceptable request rate, in the
-- spirit of TCP congestion control: every successful action nudges the rate up
-- additively, every failed one cuts it by a fixed factor.
--
-- \"Failure\" means /any/ exception escaping the action. Protocols that signal
-- throttling without throwing — an HTTP 429, say — must be turned into an
-- exception by the caller, otherwise the limiter will read the rejection as
-- headroom and speed up. Exceptions are rethrown to the caller unchanged after
-- the rate has been adjusted.
--
-- == Usage
--
-- Treat any non-2xx response as a signal to back off:
--
-- @
-- import Network.HTTP.Client
-- import Network.HTTP.Types (statusCode)
-- import qualified RateLimiter as RL
--
-- config :: RL.Config
-- config =
--   RL.Config
--     { minRate = 1,        -- never slower than 1 request\/second
--       maxRate = 33,       -- never faster than 33 requests\/second
--       reduceFactor = 0.8, -- on failure, drop to 80% of current rate
--       addRate = 1,        -- on success, climb by 1 request\/second per second
--       gracePeriod = 3     -- wait 3s after a failure before climbing again
--     }
--
-- main :: IO ()
-- main = do
--   manager <- newManager defaultManagerSettings
--   req <- parseRequest "http:\/\/localhost:8080\/api"
--   limiter <- RL.rateLimiter config
--   forM_ [1 .. 100 :: Int] $ \\_ ->
--     handleAny (\\e -> putStrLn $ "giving up on this one: " <> show e) $
--       RL.run limiter $
--         withResponse req manager $ \\res -> do
--           let status = statusCode $ responseStatus res
--           when (status >= 300) $
--             throwIO $ HttpExceptionRequest req (StatusCodeException (void res) "")
--           print status
-- @

module NejlaCommon.RateLimiter where

import Control.Monad
import Data.Fixed (Pico)
import Data.Time.Clock
  ( NominalDiffTime,
    UTCTime,
    addUTCTime,
    diffUTCTime,
    getCurrentTime,
    nominalDiffTimeToSeconds,
    secondsToNominalDiffTime,
  )
import UnliftIO
import UnliftIO.Concurrent (threadDelay)

data Inner
  = Inner
  { lastRequest :: UTCTime,
    -- | Per second
    currentRate :: Pico,
    graceUntil :: Maybe UTCTime
  }

data Config = Config
  { -- | Minimum request rate per second
    minRate :: Pico,
    -- | Maximum request rate per second
    maxRate :: Pico,
    -- | Factor to reduce rate by when the action fails (< 1, e.g. 1/2)
    reduceFactor :: Pico,
    -- | How long to wait until we start increasing the rate again
    -- after having been throttled (in seconds)
    gracePeriod :: NominalDiffTime,
    -- | Amount to increase the rate per second while requests succeed
    addRate :: Pico
  }

data RateLimiter
  = RateLimiter
  { inner :: MVar Inner,
    config :: Config,
    -- | For testing
    getCurrentTime :: IO UTCTime
  }

rateLimiter :: Config -> IO RateLimiter
rateLimiter config = do
  now <- getCurrentTime
  inner <-
    newMVar
      Inner
        { lastRequest = now,
          currentRate = config.maxRate,
          graceUntil = Nothing
        }
  return RateLimiter {inner, config, getCurrentTime}

waitUntil :: (MonadIO m) => UTCTime -> UTCTime -> m ()
waitUntil now until = do
  when (now < until) $ do
    let delay = until `diffUTCTime` now
        delaymusecs = round $ 1E6 * nominalDiffTimeToSeconds delay
    threadDelay delaymusecs

nextInner ::
  Config ->
  -- | Was the previous run successful?
  Bool ->
  -- | Current time
  UTCTime ->
  Inner ->
  Inner
nextInner config True now inner =
  -- Check if we should increase rate again
  let graceUntil =
        case inner.graceUntil of
          Nothing -> Nothing
          Just t
            | t <= now -> Nothing
            | otherwise -> inner.graceUntil
      currentRate = case graceUntil of
        Nothing ->
          -- Increase rate at the configured rate, weighted so that
          -- the rate increases linearly per second
          let add =
                config.addRate
                  * min
                    1
                    ( nominalDiffTimeToSeconds $
                        now `diffUTCTime` inner.lastRequest
                    )
           in min
                config.maxRate
                (inner.currentRate + add)
        -- Still in grace time
        Just {} -> inner.currentRate
   in Inner
        { lastRequest = now,
          currentRate,
          graceUntil
        }
nextInner config False now inner =
  Inner
    { lastRequest = now,
      currentRate =
        max
          config.minRate
          (inner.currentRate * config.reduceFactor),
      graceUntil = Just $ config.gracePeriod `addUTCTime` now
    }

run :: (MonadUnliftIO m) => RateLimiter -> m a -> m a
run limiter f = do
  res <- modifyMVar limiter.inner $ \inner -> do
    let next =
          addUTCTime
            ( secondsToNominalDiffTime $
                recip inner.currentRate
            )
            inner.lastRequest
    now <- liftIO limiter.getCurrentTime
    waitUntil now next
    let actual = max now next
    (inner, x) <-
      tryAny f >>= \case
        Right x ->
          return
            ( nextInner limiter.config True actual inner,
              Right x
            )
        Left e ->
          return
            ( nextInner limiter.config False actual inner,
              Left e
            )
    return (inner, x)
  case res of
    Left e -> throwIO e
    Right x -> return x
