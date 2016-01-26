{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Lambdatrade.WAI where

import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text as Text
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Parse

multipartHandlerOverride :: (MonadIO m, MonadBaseControl IO m) =>
                            Method
                         -> [Text]
                         -> ((Response -> ResponseReceived)
                             -> [Param]
                             -> [File FilePath]
                             -> ResourceT m a)
                         -> (Request -> (Response -> ResponseReceived) -> m a)
                         -> Request
                         -> (Response -> ResponseReceived)
                         -> m a
multipartHandlerOverride method path handler app req sendRes
    | requestMethod req == method
    , pathInfo req == path
    =  runResourceT $ do
          iState <- getInternalState
          let backend = tempFileBackEnd iState
          (params, files) <- liftIO $ parseRequestBody backend req
          handler sendRes params files
    | otherwise = app req sendRes
