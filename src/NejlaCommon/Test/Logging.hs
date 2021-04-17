{-# LANGUAGE LambdaCase #-}

module NejlaCommon.Test.Logging where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBChan
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.ByteString               (ByteString)


-- | Give an action access to it's own logs.
loggingToChan ::
     MonadIO m
  => Int -- ^ Number of logs to keep
  -> (IO [ByteString] -- Action that retrieves logs (once)
       -> LoggingT m a)
  -> m a
loggingToChan bound m = do
  chan <- liftIO $ newTBChanIO (max 1 bound)
  runLoggingT (m $ dumpLogs chan) $ channelLogger chan
  where
    channelLogger chan  loc src lvl str = atomically $ do
        -- Drop oldest log line when channel is full
        full <- isFullTBChan chan
        when full $ void $ readTBChan chan
        writeTBChan chan $ (fromLogStr $ defaultLogStr loc src lvl str)
    dumpLogs chan = do
      atomically (untilM (readTBChan chan) (isEmptyTBChan chan))
    untilM m p = do
      p >>= \case
        True -> return []
        False -> do
          x <- m
          (x:) <$> untilM m p
