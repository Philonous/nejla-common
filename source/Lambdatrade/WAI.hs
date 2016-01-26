{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Lambdatrade.WAI where

import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Parse


-- | Given a method and a path, this middleware will write each part of the
-- (multipart) request body of requests to the according method and request path
-- to a file and pass on the filename of the _first_ part in the request body
--
-- Example usage with rest-core:
--
-- >>>  Warp.run port $ multipartHandlerOverride "POST"
-- >>>                $ apiToApplication run api
--
multipartHandlerOverride :: Method
                         -> [Text]
                         -> Middleware
multipartHandlerOverride method path app req sendRes
    | requestMethod req == method
    , pathInfo req == path
    =  runResourceT $ do
          iState <- getInternalState
          let backend = tempFileBackEnd iState
          (params, files) <- liftIO $ parseRequestBody backend req
          let filename = case files of
                          [] -> ""
                          ((_, FileInfo{fileContent = fn}):_) -> fn
              req' = req{requestBody = return . Text.encodeUtf8
                                              $ Text.pack filename}

          liftIO $ app req' sendRes
    | otherwise = app req sendRes
