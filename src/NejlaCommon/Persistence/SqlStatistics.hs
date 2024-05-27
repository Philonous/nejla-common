{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Collect statistics of executed SQL queries.
--
-- It is important to know which SQL queries where executed and how long they
-- took individually and in aggregate. This module provides a way to /hook/ into
-- persistent backends and record the time each query took.
--
-- To get started, you can just wrap your 'App' actions with 'logSqlStats'. It
-- will hook the SQL backend, collect the query statistics and log them, e.g. like so:
-- > App.run pool conf ctx (logSqlStatistics m)
module NejlaCommon.Persistence.SqlStatistics where

import qualified Control.Foldl                    as Foldl
import           Control.Lens
import           Control.Monad
import qualified Control.Monad.Catch              as Ex
import           Control.Monad.Logger             as Log
import           Control.Monad.Reader

import qualified Data.Aeson                       as Aeson
import           Data.IORef
import qualified Data.List                        as List
import           Data.Map.Strict                  ( Map )
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       ( fromMaybe )
import           Data.String.Interpolate.IsString ( i )
import           Data.Text                        ( Text )
import           Data.Time.Clock                  ( getCurrentTime )
import qualified Data.Time.Clock                  as Time

import qualified Database.Persist.Sql             as P
import           Database.Persist.SqlBackend      ( setConnHooks
                                                  , emptySqlBackendHooks )
import           Database.Persist.SqlBackend.Internal ( SqlBackendHooks(..) )

import qualified NejlaCommon.Persistence          as NC
import           NejlaCommon.Persistence          ( App(..) )

import           Text.Printf                      ( printf )

-- | Individual sample of a query and its execution time.
data QueryTime =
  QueryTime
  { queryTimeQuery :: Text
  , queryTimeTime  :: Time.NominalDiffTime
  }

makeLensesWith camelCaseFields ''QueryTime

-- | How to fold a sequence of Query Timings into desired statistics
--
-- (See https://www.youtube.com/watch?v=6a5Ti0r8Q2s for an introduction to Foldl)
type StatsFold stats = Foldl.Fold QueryTime stats

-- | Add hooks to an SqlBackend to collect query execution statistics, remove
-- hooks once the function returns
--
-- You provide it with a Fold (aka reducer), i.e. a function that calculates the
-- aggregate statistics you are interested in from the individual samples. For
-- example, you might want to know how often a particular query ran, its maximum
-- execution time or something more complex like averages and standard
-- deviations.
--
-- The callback is passed an IO action that reads the statistics so they can be
-- used in an exception handler. this also allows fetching of intermediate
-- statistics
--
-- /NB/ the backend is /modified/ before the callback function is run and the
-- modifications are undone one the functions returns. This means that using the
-- Backend concurrently can have unintended side effects.
backendWithStats
  :: MonadIO m
  => StatsFold stats -- ^ Fold describes how to calculate statistics. See for example 'foldStats'
  -> P.SqlBackend -- ^ Backend to hook into
  -> (IO stats
      -> P.SqlBackend
      -> m a) -- ^ Callback (what to do with the hooked backend)
  -> m a
backendWithStats (Foldl.Fold fadd fempty fextract) con k = do
  statsRef <- liftIO $ newIORef fempty
  let update x = atomicModifyIORef statsRef $ \stats -> (fadd stats x, ())

  let readStats = fextract <$> readIORef statsRef
  let conHooks =
        emptySqlBackendHooks
        { hookGetStatement = \_backend statementText stmnt ->
            return $ hookedStatement update statementText stmnt
        }
  -- Call inner function with the read function and the hooked statement map
  k readStats (setConnHooks conHooks con)
  where
    -- | Execute action f while registering the query and the execution time
    withStats :: MonadIO m => (QueryTime -> IO ()) -> Text -> m a -> m a
    withStats addSample stmt f = do
      before <- liftIO Time.getCurrentTime
      res <- f
      after <- liftIO Time.getCurrentTime
      let tdiff = after `Time.diffUTCTime` before
      liftIO $ addSample (QueryTime
                          { queryTimeQuery = stmt
                          , queryTimeTime  = tdiff
                          })
      return res

    hookedStatement addSample statementText stmt = do
      stmt
        { P.stmtExecute = \values -> do
            withStats addSample statementText (P.stmtExecute stmt values)
        , P.stmtQuery   = \values -> do
            withStats addSample statementText (P.stmtQuery stmt values)
        }

--------------------------------------------------------------------------------
-- Default Statistics ----------------------------------------------------------
--------------------------------------------------------------------------------
-- | Query statistics. Knowing how often a query ran, the total executation time
-- and the longest run gives us enough information to debug many performance
-- problems.
data Stats =
  Stats
  { statsCount     :: Int -- ^ Number of times this statement was executed
  , statsTotalTime :: Time.NominalDiffTime -- ^ Total duration spent in this query
  , statsMaxTime   :: Time.NominalDiffTime -- ^ Longest run of this query
  }

makeLensesWith camelCaseFields ''Stats

-- | Default / example fold how to calculate statistics from individual query
-- execution times. Collects for each executed query (as Text) the runtime statistics
foldStats :: StatsFold (Map Text Stats)
foldStats = Foldl.groupBy (view query) . lmap (view time) $ do
  -- Make use of Foldl's Applicative instance.
  count <- Foldl.length
  total <- Foldl.sum
  max <- fromMaybe 0 <$> Foldl.maximum
  return $ Stats
    { statsCount     = count
    , statsTotalTime = total
    , statsMaxTime   = max
    }

-- | Logging the calculated statistics
logQueryStats :: (MonadIO m, MonadLogger m)
              => Text -- ^ Endpoint
              -> Bool -- Break down stats by query
              -> Map Text Stats
              -> m ()
logQueryStats endpoint breakdown stats = do
  now <- liftIO getCurrentTime
  let tCount        = sumOf (each . count) stats
      tUnique       = Map.size stats
      tTime         = sumOf (each . totalTime) stats
      tTimePerQuery = if tCount > 0 then tTime / fromIntegral tCount else 0
      tLongestQuery = fromMaybe 0 $ maximumOf (each . maxTime) stats
  when breakdown $ do
    let queries = List.sortOn (view (_2 . totalTime)) $ Map.toList stats

    forM_ queries $ \(query, stat) -> do
      Log.logDebugNS "SQL-stats" $ "  > " <> query
      Log.logDebugNS "SQL-stats"
                     [i| Ran #{stat ^. count} times, total=#{stat ^. totalTime}, max=#{stat ^. maxTime})|]
  Log.logInfoNS "SQL-stats"
                [i|{"request":#{Aeson.encode endpoint}, "timestamp": #{Aeson.encode now}, "queries":#{tCount}, "unique":#{tUnique}, "totalTime":#{tDiff tTime}, "avgTime":#{tDiff tTimePerQuery}, "maxTime":#{tDiff tLongestQuery}}|]
  return ()
  where
    tDiff :: RealFrac a => a -> String
    tDiff d = printf "%.3f" (realToFrac d :: Double)

-- | One-stop shop for just getting some statistics.
-- Hook the SQL backend, collect count, sum and maximum execution time and log them
logSqlStatistics :: Text -- ^ Context to log (e.g. the request endpoint)
                 -> App priv tl st a
                 -> App priv tl st a
logSqlStatistics ctx (NC.App m) = do
  st <- NC.App ask
  backendWithStats foldStats
                   (st ^. NC.connection)
                   (\getStats con ->
                    NC.App (ReaderT $ \_ ->
                            runReaderT m (st & NC.connection .~ con))
                    `Ex.finally` (logQueryStats ctx True =<< liftIO getStats))
