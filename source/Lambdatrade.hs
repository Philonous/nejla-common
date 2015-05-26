-- Copyright © 2014-2015 Lambdatrade AB. All rights reserved.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | In addition to the entities below, this module provides a 'PersistField'
-- and a 'PersistFieldSql' instance of 'UUID'. It also provides 'FromJSON',
-- 'ToJSON', 'JSONSchema' and 'PathPiece' instances of 'UUID'.
module Lambdatrade (mkGenericJSON, withPool) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson
import Data.Char
import Data.JSON.Schema
import Data.Monoid
import Data.UUID
import Database.Persist.Postgresql
import Database.Persist.Sql
import Database.Persist.TH
import Generics.Generic.Aeson
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Environment
import Web.PathPieces

import Data.ByteString (ByteString)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS

instance PersistField UUID where
    toPersistValue = toPersistValue . BS.concat . BSL.toChunks . toByteString
    fromPersistValue = \x -> fromPersistValue x >>= \v ->
        case fromByteString $ BSL.fromChunks [v] of
            Nothing -> Left $ TS.concat ["Invalid UUID: ", TS.pack (show v)]
            Just u -> Right u

instance PersistFieldSql UUID where
    sqlType _ = SqlBlob

instance ToJSON UUID where
    toJSON = toJSON . toString

instance FromJSON UUID where
    parseJSON = maybe mzero return . fromString <=< parseJSON

instance JSONSchema UUID where
    schema uuid = schema $ fmap (TS.pack . show) uuid

instance PathPiece UUID where
    fromPathPiece = fromString . TS.unpack
    toPathPiece = TS.pack . toString

-- | Acquires the database password (from the @DB_PASSWORD@ environment
-- variable) and creates a PostgreSQL connection pool with the specified number
-- of threads.
withPool :: Int -> (ConnectionPool -> LoggingT IO b) -> IO b
withPool n f = do
    dbPassword <- TS.encodeUtf8 . TS.pack <$> getEnv "DB_PASSWORD"
    (runStderrLoggingT . withPostgresqlPool (connectionString dbPassword) n) f
  where
    connectionString passwd = BS.intercalate " "
                              $ [ "host"     .= "database"
                                , "user"     .= "lambdatrade"
                                , "dbname"   .= "lambdatrade"
                                , "password" .= passwd
                                ]
    k .= v = k <> "=" <> v

-- | Create "trivial" instances for FromJSON, ToJSON, JSONSchema. The Instance
-- members are set to gparseJsonWithSettings, gtoJsonWithSettings and
-- gSchemaWithSettings respectively with options set to strip the type name as a
-- prefix.
mkGenericJSON tp = do -- TODO: Rename to "mkGenericJson", probably.
    t <- tp
    -- If the type is a simple type, strip its name from the field names
    let prefix =
            case t of
             ConT tpnm -> Just . downcase $ nameBase tpnm
             _ -> Nothing
        settings = [| Settings{stripPrefix = $(lift prefix)} |]
    fj <- [d| instance FromJSON $tp where
                 parseJSON = gparseJsonWithSettings $settings |]
    tj <- [d| instance ToJSON $tp where
                 toJSON = gtoJsonWithSettings $settings |]
    sc <- [d| instance JSONSchema  $tp where
                 schema = gSchemaWithSettings $settings |]
    return . concat $ [ fj
                      , tj
                      , sc
                      ]
  where
    downcase [] = []
    downcase (c:cs) = toLower c : cs
