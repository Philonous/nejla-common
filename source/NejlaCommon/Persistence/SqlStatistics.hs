{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ApplicativeDo #-}

-- | Collect statistics of executed SQL queries.
module NejlaCommon.Persistence.SqlStatistics where

import qualified Control.Exception                as Ex
import qualified Control.Foldl                    as Foldl
import           Control.Lens
import           Control.Monad
import           Control.Monad.Logger             as Log
import           Control.Monad.Trans
import qualified Data.Aeson                       as Aeson
import           Data.IORef
import qualified Data.List                        as List
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromMaybe)
import           Data.Ord
import           Data.String.Interpolate.IsString (i)
import           Data.Text                        (Text)
import           Data.Time.Clock                  (getCurrentTime)
import qualified Data.Time.Clock                  as Time
import qualified Database.Persist.Sql             as P
import           Text.Printf                      (printf)

import qualified NejlaCommon.Persistence          as NC
import           NejlaCommon.Persistence          (App(..))


data Stats = Stats
  { statsCount :: Int -- ^ Number of times this statement is executed
  , statsTotalTime :: Time.NominalDiffTime -- ^ Total duration spent in this query
  , statsMaxTime :: Time.NominalDiffTime -- ^ Longest run of this query
  }

makeLensesWith camelCaseFields ''Stats

data QueryTime = QueryTime
  { queryTimeQuery :: Text
  , queryTimeTime  :: Time.NominalDiffTime
  }
makeLensesWith camelCaseFields ''QueryTime

-- | How to fold a sequence of Query Timings into desired statistics
type StatsFold stats = Foldl.Fold QueryTime stats

-- | Default / example fold how to calculate statistics from individual query
-- execution times
foldStats :: StatsFold (Map Text Stats)
foldStats  = Foldl.groupBy (view query) . lmap (view time) $ do
  count <- Foldl.length
  total <- Foldl.sum
  max <- fromMaybe 0 <$> Foldl.maximum
  return $ Stats { statsCount = count
                 , statsTotalTime = total
                 , statsMaxTime = max
                 }

logQueryStats :: (MonadIO m, MonadLogger m)
              => Text -- ^ Endpoint
              -> (Map Text Stats)
              -> Bool -- Break down stats by query
              -> m ()
logQueryStats endpoint stats breakdown = do
  now <- liftIO $ getCurrentTime
  let tCount = sumOf (each . count) stats
      tUnique = Map.size stats
      tTime = sumOf (each . totalTime) stats
      tTimePerQuery = if tCount > 0
                      then tTime / (fromIntegral tCount)
                      else  0
      tLongestQuery = fromMaybe 0 $ maximumOf (each . maxTime) stats
  when breakdown $ do
    let queries = List.sortBy (comparing $ view (_2 . totalTime))
                    $ Map.toList stats

    forM_ (queries) $ \(query, stat) -> do
      Log.logDebugNS "SQL-stats" $ "  > " <> query
      Log.logDebugNS "SQL-stats" [i| Ran #{stat ^. count} times, total=#{stat ^. totalTime}, max=#{stat ^. maxTime})|]
  Log.logInfoNS "SQL-stats" [i|{"request":#{Aeson.encode endpoint}, "timestamp": #{Aeson.encode now}, "queries":#{tCount}, "unique":#{tUnique}, "totalTime":#{tDiff tTime}, "avgTime":#{tDiff tTimePerQuery}, "maxTime":#{tDiff tLongestQuery}}|]
  return ()
  where
    tDiff :: RealFrac a => a -> String
    tDiff d = printf "%.3f" (realToFrac d :: Double)

-- | Add hooks to an SqlBackend to collect query execution statistics, remove
-- hooks once the function returns
backendWithStats ::
  MonadIO m => StatsFold stats -- ^ Fold describes how to calculate statistics
            -> P.SqlBackend
            -> (P.SqlBackend -> m a)
            -> m (stats, a)
backendWithStats (Foldl.Fold fadd fempty fextract) con k = do
  statsRef <- liftIO $ newIORef fempty
  let update x = atomicModifyIORef statsRef $ \stats -> (fadd stats x, ())
  -- Save statements before we add hooks so we can undo them
  unhookedStatements <- liftIO $ newIORef =<< readIORef (P.connStmtMap con)

  -- Hook all existing statements
  liftIO $ modifyIORef' (P.connStmtMap con) $ itraversed %@~ (hookedStatement update)

  -- Add hooking to newly created statements
  let prepare statementText = do
        stmt <- P.connPrepare con statementText
        -- Save new statement before hooking
        modifyIORef' unhookedStatements $ Map.insert statementText stmt
        -- Hook into the execute and query calls to register statistics
        return $ hookedStatement update statementText stmt

  -- Call inner function with hooked statement map
  res <- k con{ P.connPrepare = prepare }

  -- Reinstate unhooked statements
  liftIO $ writeIORef (P.connStmtMap con) =<< readIORef unhookedStatements
  -- We don't need bracket because exceptions during statement execution
  -- invalidate the connection anyway

  stats <- liftIO $  fextract <$> readIORef statsRef
  return (stats, res)
  where
    -- | Execute action f while registering the query and the execution time
    withStats :: MonadIO m =>
                 (QueryTime -> IO ())
              -> Text
              -> m a
              -> m a
    withStats addSample stmt f = do
      before <- liftIO $ Time.getCurrentTime
      res <- f
      after <- liftIO $ Time.getCurrentTime
      let tdiff = after `Time.diffUTCTime` before
      liftIO $ addSample (QueryTime{queryTimeQuery = stmt
                                   , queryTimeTime = tdiff
                                   })
      return res

    hookedStatement addSample statementText stmt = do
      stmt{ P.stmtExecute = \values -> do
              withStats addSample statementText (P.stmtExecute stmt values)
          , P.stmtQuery = \values -> do
              withStats addSample statementText (P.stmtQuery stmt values)
          }
