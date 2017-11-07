{-# LANGUAGE FlexibleContexts #-}
-- Copyright © 2014-2016 Nejla AB. All rights reserved.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | In addition to the entities below, this module provides the following
-- 'UUID' instances: 'PersistField', 'PersistFieldSql', 'FromJSON', 'ToJSON',
-- 'JSONSchema', 'PathPiece', 'Info' as well as 'FromHttpApiData' and
-- 'ToHttpApiData'.
module NejlaCommon ( module NejlaCommon.Wai
                   , module NejlaCommon.Persistence
                   , module NejlaCommon.Logging
                   , DerivedData(..)
                   , WithField(..)
                   , derivedType
                   , mkGenericJson
                   , mkJsonType
                   , formatUTC
                   , parseUTC
                   , withPool
                   ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Trans hiding (lift)
import           Control.Monad.Trans.Control
import           Data.Aeson
import           Data.Data
import           Data.Default
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock
import           Data.Time.Format
import qualified Data.UUID as UUID
import           Database.Persist.Postgresql
import           GHC.Generics (Generic)
import           GHC.TypeLits
import           Generics.Generic.Aeson
import           Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax as TH
import           Web.PathPieces

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HMap
import qualified Data.JSON.Schema as Schema
import qualified Data.List as L
import qualified Data.Text as TS
import qualified Rest.Types.Info as Rest

import           NejlaCommon.Config
import           NejlaCommon.Helpers
import           NejlaCommon.Persistence
import           NejlaCommon.Logging
import           NejlaCommon.Wai


instance PersistField UUID.UUID where
    toPersistValue = toPersistValue . UUID.toString
    fromPersistValue x = case x of
        PersistDbSpecific bs ->
            case UUID.fromASCIIBytes bs of
             Nothing -> Left $ "Invalid UUID: " <> TS.pack (show bs)
             Just u -> Right u
        PersistText txt ->
            case UUID.fromString $ TS.unpack txt of
             Nothing -> Left $ "Invalid UUID: " <> TS.pack (show txt)
             Just u -> Right u
        e -> Left $ "Can not convert to uuid: " <> TS.pack (show e)

instance PersistFieldSql UUID.UUID where
    sqlType _ = SqlOther "uuid"

instance Schema.JSONSchema UUID.UUID where
    schema uuid = Schema.schema $ fmap (TS.pack . show) uuid

instance PathPiece UUID.UUID where
    fromPathPiece = UUID.fromString . TS.unpack
    toPathPiece = TS.pack . UUID.toString

instance Rest.Info UUID.UUID where
    describe _ = "uuid"

-- | Acquires the database password (from the 'DB_PASSWORD' environment
-- variable) and creates a PostgreSQL connection pool with the specified number
-- of threads.
--
-- The function will try to read and use the following environment variables and
-- config options respectively:
--
-- * "DB_HOST" and "db.host" (defaults to "database")
-- * "DB_USER" and "db.user" (defaults to "postgres)
-- * "DB_DATABASE" and "db.database" (defaults to empty)
-- * "DB_PASSWORD" and "db.password" (defaults to empty)
-- withPool :: Config
--          -> Int -- ^ Number of connections to open
--          -> (ConnectionPool -> LoggingT IO b)
--          -> IO b
withPool :: (MonadIO m, MonadBaseControl IO m, MonadLogger m) =>
            Config
         -> Int
         -> (ConnectionPool -> m b)
         -> m b
withPool conf n f = do
    dbHost <- getConf "DB_HOST" "db.host" (Right "database") conf
    dbUser <- getConf "DB_USER" "db.user" (Right "postgres") conf
    dbDatabase <- getConfMaybe "DB_DATABASE" "db.database" conf
    dbPassword <- getConfMaybe "DB_PASSWORD" "db.password" conf
    dbPort <- getConfMaybe "DB_PORT" "db.port" conf
    let connectionString =
          BS.intercalate " "
          . catMaybes
            $ [ "host"     ..= Just dbHost
              , "user"     ..= Just dbUser
              , "dbname"   ..= dbDatabase
              , "password" ..= dbPassword
              , "port"     ..= dbPort
              ]
    $logDebug $ "Using connection string: \""
                <> Text.decodeUtf8 connectionString <> "\""

    withPostgresqlPool connectionString n f
  where
    k ..= (Just v) = Just $ k <> "=" <> Text.encodeUtf8 v
    _ ..= Nothing = Nothing

-- | Create "trivial" instances for 'FromJSON', 'ToJSON', 'JSONSchema'. The
-- instance members are set to 'gparseJsonWithSettings', 'gtoJsonWithSettings'
-- and 'gSchemaWithSettings' respectively with options set to strip the type
-- name as a prefix.
mkGenericJson :: Q Type -> Q [Dec]
mkGenericJson tp = do
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
    sc <- [d| instance Schema.JSONSchema  $tp where
                 schema = Schema.gSchemaWithSettings $settings |]
    return . concat $ [ fj
                      , tj
                      , sc
                      ]

-- | 'mkJsonType' is 'derivedType' in combination with 'mkGenericJSON'.
mkJsonType :: TH.Name -> DerivedData -> Q [Dec]
mkJsonType name dd = do
    tp@(DataD _ name' _ _ _ _:_) <- derivedType name dd
    instances <- mkGenericJson (return $ ConT name')
    return $ tp ++ instances

-- | See 'derivedType'.
data DerivedData = DD { derivedPrefix :: String
                      , removeFields :: [String]
                      , optionalFields :: [String]
                      , derive :: Cxt
                      }

instance Default DerivedData where
    def = DD { derivedPrefix = ""
             , removeFields = []
             , optionalFields = []
             , derive =
                 ConT <$>
                 [''Show, ''Eq, ''Data, ''Typeable, ''Generic ]
             }

-- | Create a derived type. Takes a type name and adds the 'derivedPrefix' value
-- to the type name, the constructor name and all the fields, removes the fields
-- in 'removeFields' (can be given either by their full (original) name or
-- without the (original) prefix), and add deriving clause for the class names
-- given in the 'derive' value.
--
-- Also creates a function 'fromDerivedType' that takes the omitted fields (in
-- the order as they are given in the original type declaration) and returns a
-- value of the original type.
--
-- Example:
--
-- @
--     derivedType ''Foo def{ derivedPrefix = "foo", removeFields = ["bar"]}
--     -> fromAddFoo :: Int -> AddFoo -> Foo
-- @
--
-- (The above assumes that the field bar is of type 'Int'.)
derivedType :: TH.Name -> DerivedData -> Q [Dec]
derivedType tname DD{ derivedPrefix = pre
                    , removeFields = rf
                    , optionalFields = mf
                    , derive = derive
                    } = do
    info <- reify tname
    let uPre = upcase pre
        removePre = downcase $ nameBase tname
    case info of
     TyConI (DataD [] name [] _ [RecC cName cFields] _) -> do
         let filterField excluded (nm, _, _) =
                 let name = nameBase nm
                 in and [ name `notIn` excluded
                        , downcase (fromMaybe ""
                                    (L.stripPrefix removePre name))
                          `notIn` excluded
                        ]
             (keptFields, removedFields') = L.partition (filterField rf)
                                             cFields
             (fullFields', maybeFields') = L.partition (filterField mf)
                                             keptFields
             fullFields = fst3 <$> fullFields'
             maybeFields = fst3 <$> maybeFields'
             removedFields = fst3 <$> removedFields'
             isStrict = Bang NoSourceUnpackedness SourceStrict
             cs = [ ( mkName $ pre <> upcase (nameBase nm) , isStrict , tp)
                  | (nm, _, tp) <- fullFields' ]
             ms = [ ( mkName $ pre <> upcase (nameBase nm) , isStrict
                                                           , AppT (ConT ''Maybe)
                                                                  tp)
                  | (nm, _, tp) <- maybeFields' ]
             cName' = (mkName $ uPre ++ nameBase name)
             dt = DataD [] (mkName $ uPre <> nameBase cName) []
                          Nothing [RecC cName' (cs ++ ms)] derive
             fromFunName = mkName $ concat [ "from"
                                           , upcase pre
                                           , nameBase name
                                           ]
             toFunName = mkName $ concat [ "to"
                                           , upcase pre
                                           , nameBase name
                                           ]
             fst3 (x, _, _) = x
         case compare (length rf) (length removedFields) of
          LT -> reportWarning "filtered too many fields "
          EQ -> return ()
          GT -> reportWarning $ "Could not find all fields to remove for "
                                <> show name <> " ("
                                <> show (length removedFields)
                                <> " filtered of "
                                <> show (length rf)
                                <> "; "
                                <> show (length rf - length removedFields)
                                <> "remain)"
         case compare (length mf) (length maybeFields) of
          LT -> reportWarning "made too many fields optional"
          EQ -> return ()
          GT -> reportWarning $ "Could not find all fields to make optional for "
                                <> show name <> " ("
                                <> show (length maybeFields)
                                <> " optional of "
                                <> show (length mf)
                                <> "; "
                                <> show (length mf - length maybeFields)
                                <> "remain)"

         freeParams <- forM removedFields $ newName . nameBase
         maybeParams <- forM maybeFields $ newName . nameBase
         constrParams <- forM cs $ \(nm, _, _) -> newName $ nameBase nm
         let
             pats = (VarP <$> freeParams ++ maybeParams )
                    ++ [RecP cName' (zip (fst3 <$> cs) (VarP <$> constrParams))]
             bd = RecConE cName (zip removedFields (VarE <$> freeParams)
                                 ++ zip maybeFields (VarE <$> maybeParams)
                                 ++ zip fullFields (VarE <$> constrParams))
             injFun = FunD fromFunName [Clause pats (NormalB bd) []]
             projPat = RecP cName (zip fullFields
                                       (VarP <$> constrParams)
                                   ++ zip maybeFields
                                       (VarP <$> maybeParams)
                                  )
             projBody = RecConE cName'
                          $ zip (fst3 <$> cs) (VarE <$> constrParams)
                          ++ zip (fst3 <$> ms) (AppE (ConE 'Just) . VarE
                                                     <$> maybeParams)
             projFun = FunD toFunName [Clause [projPat] (NormalB projBody ) []]
         return [dt, injFun, projFun]

     _ -> error "mkAddCall only works on single-record-constructor types"

-- Associated data declaration in IsResource class.
-- A bug in ghc prevents this from being used for now.
-- derivedType' :: TH.Name -> DerivedData -> Q [Dec]
-- derivedType' name DD{ derivedPrefix = pre
--                     , removeFields = rf
--                     , derive = derive
--                     } = do
--     info <- reify name
--     let uPre = upcase pre
--         removePre = takeWhile isLower . downcase $ nameBase name
--     case info of
--      TyConI (DataD [] name [] _ [RecC cName cFields] _) ->
--           let cs = [ ( mkName $ pre <> upcase name
--                     , s, tp)
--                   | (nm, s, tp) <- cFields
--                   , name <- [nameBase nm]
--                   , name `notIn` rf
--                     -- Drop the type name as a prefix
--                   , downcase (fromMaybe "" (L.stripPrefix removePre name))
--                        `notIn` rf
--                   ]
--          in return $ [DataInstD [] (mkName "AddResource") [ConT name] Nothing
--                         [RecC cName cFields] []]
--      _ -> error "mkAddCall only works on single-record-constructor types"

-- Used by derivedType{,'}. Not exported.
notIn :: Eq a => a -> [a] -> Bool
notIn x xs = x `notElem` xs

-- | Extends a given type with an extra field and derives 'FromJSON', 'ToJSON'
-- and 'JSONSchema' instances.
--
-- Usage:
--
-- @
--     WithField [name of field as a string] [type of field] [type to extend]
-- @

data WithField (name :: Symbol) fieldType baseType =
    WithField { withFieldField :: fieldType
              , withFieldBase :: baseType
              } deriving (Show)

instance (KnownSymbol name, ToJSON fieldType, ToJSON baseType) =>
         ToJSON (WithField name fieldType baseType) where
  toJSON wf =
      let fName = TS.pack $ symbolVal (Proxy :: Proxy name)
      in case toJSON $ withFieldBase wf of
               Object o ->
                   case toJSON $ withFieldField wf of
                    Null -> Object o
                    v -> Object $ o <> HMap.singleton fName v
               _ -> error "WithField.toJSON: base field does not yield object"

instance (KnownSymbol name, FromJSON fieldType, FromJSON baseType) =>
         FromJSON (WithField name fieldType baseType) where
  parseJSON = withObject "object" $ \o -> do
      let fName = TS.pack $ symbolVal (Proxy :: Proxy name)
      fv <- o .:? fName
      f <- case fv of
       Nothing -> parseJSON Null
       Just v -> parseJSON v
      b <- parseJSON (Object $ HMap.delete fName o)
      return WithField{ withFieldField = f
                      , withFieldBase = b
                      }

instance ( KnownSymbol name
         , Schema.JSONSchema fieldType
         , Schema.JSONSchema baseType
         ) => Schema.JSONSchema (WithField name fieldType baseType) where
  schema prx =
    let fName = TS.pack $ symbolVal (Proxy :: Proxy name)
    in case Schema.schema (withFieldBase <$> prx) of
         Schema.Object fs
             -> Schema.Object (fs ++ [Schema.Field fName False
                                      (Schema.schema
                                       (withFieldBase <$> prx))])
         _ -> Schema.Any

-- | Produces an \"ISO\" (ISO 8601) string.
formatUTC :: UTCTime -> String
formatUTC = formatTime defaultTimeLocale "%FT%T%QZ"

-- | Parses an \"ISO\" (ISO 8601) string.
parseUTC :: String -> Maybe UTCTime
parseUTC t = parseTimeM True defaultTimeLocale "%FT%T%QZ" t
             <|> parseTimeM True defaultTimeLocale "%FT%T%Q%z" t
             <|> parseTimeM True defaultTimeLocale "%F" t
