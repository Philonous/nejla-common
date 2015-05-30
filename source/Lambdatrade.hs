-- Copyright © 2014-2015 Lambdatrade AB. All rights reserved.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | In addition to the entities below, this module provides the following
-- 'UUID' instances: 'PersistField', 'PersistFieldSql', 'FromJSON', 'ToJSON',
-- 'JSONSchema', and 'PathPiece'.
module Lambdatrade ( DerivedData(..)
                   , derivedType
                   , mkGenericJSON
                   , mkJsonType
                   , withPool
                   ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson
import Data.Char
import Data.Data
import Data.Default
import Data.JSON.Schema
import Data.Maybe
import Data.Monoid
import Data.UUID
import Database.Persist.Postgresql
import Database.Persist.Sql
import Database.Persist.TH
import Generics.Generic.Aeson
import GHC.Generics
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Environment
import Web.PathPieces
import Data.ByteString (ByteString)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as L
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
mkGenericJSON :: Q Type -> Q [Dec]
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

-- | mkJsonType is derivedType + mkGenericJSON.
mkJsonType :: Name -> DerivedData -> Q [Dec]
mkJsonType name dd = do
    tp@(DataD _ name _ _ _:_) <- derivedType name dd
    instances <- mkGenericJSON (return $ ConT name)
    return $ tp ++ instances

data DerivedData = DD { derivedPrefix :: String
                      , removeFields :: [String]
                      , optionalFields :: [String]
                      , derive :: [Name]
                      }

instance Default DerivedData where
    def = DD { derivedPrefix = ""
             , removeFields = []
             , optionalFields = []
             , derive = [''Show, ''Eq, ''Data, ''Typeable, ''Generic ]
             }

-- | Create a derived type. Takes a type name and adds the derivedPrefix to the
-- type name, the constructor name and all the fields, removes the fields in
-- removeFields (can be given either by their full (original) name or without
-- the (original) prefix), and add deriving clause for the class names given in
-- derive
--
-- Also creates a function from<DerivedType> that takes the omitted fields (in
-- the order as they are given in the original type declaration) and returns a
-- value of the original type.
--
-- Example: derivedType ''Foo def{ derivedPrefix = "foo", removeFields = ["bar"]}
--
-- -> fromAddFoo :: Int -> AddFoo -> Foo
-- (Assuming that the field bar ist of type Int)
derivedType :: Name -> DerivedData -> Q [Dec]
derivedType tname DD{ derivedPrefix = pre
                    , removeFields = rf
                    , optionalFields = mf
                    , derive = derive
                    } = do
    info <- reify tname
    let uPre = upcase pre
        removePre = downcase $ nameBase tname
    case info of
     TyConI (DataD [] name [] [RecC cName cFields] _) -> do
         let filterField excluded (nm, _, _) =
                 let name = nameBase nm
                 in and [ name `notIn` excluded
                        , downcase (fromMaybe ""
                                    (L.stripPrefix removePre name))
                          `notIn` excluded
                        ]
             addFieldPrefix fs = [ ( mkName $ pre <> upcase (nameBase nm)
                                   , IsStrict
                                   , tp)
                                 | (nm, _, tp) <- fs
                                 ]
             (keptFields, removedFields') = L.partition (filterField rf)
                                             cFields
             (fullFields', maybeFields') = L.partition (filterField mf)
                                             keptFields
             fullFields = fst3 <$> fullFields'
             maybeFields = fst3 <$> maybeFields'
             removedFields = fst3 <$> removedFields'
             cs = [ ( mkName $ pre <> upcase (nameBase nm) , IsStrict , tp)
                  | (nm, _, tp) <- fullFields' ]
             ms = [ ( mkName $ pre <> upcase (nameBase nm) , IsStrict
                                                           , AppT (ConT ''Maybe)
                                                                  tp)
                  | (nm, _, tp) <- maybeFields' ]
             cName' = (mkName $ uPre ++ nameBase name)
             dt = DataD [] (mkName $ uPre <> (nameBase cName)) []
                          [RecC cName' (cs ++ ms)] derive
             fromFunName = mkName $ concat [ "from"
                                           , (upcase pre)
                                           , nameBase name
                                           ]
             toFunName = mkName $ concat [ "to"
                                           , (upcase pre)
                                           , nameBase name
                                           ]
             fst3 (x, _, _) = x
         case compare (length rf) (length removedFields) of
          LT -> reportWarning "filtered too many fields "
          EQ -> return ()
          GT -> reportWarning $ "Could not find all fields to remove for "
                                <> show name <> " ("
                                <> (show $ length removedFields)
                                <> " filtered of "
                                <> (show $ length rf)
                                <> "; "
                                <> (show (length rf - length removedFields))
                                <> "remain)"
         case compare (length mf) (length maybeFields) of
          LT -> reportWarning "made too many fields optional"
          EQ -> return ()
          GT -> reportWarning $ "Could not find all fields to make optional for "
                                <> show name <> " ("
                                <> (show $ length maybeFields)
                                <> " optional of "
                                <> (show $ length mf)
                                <> "; "
                                <> (show (length mf - length maybeFields))
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
             projPat = RecP cName (zip (fullFields)
                                       (VarP <$> constrParams)
                                   ++ zip (maybeFields)
                                       (VarP <$> maybeParams)
                                  )
             projBody = RecConE cName'
                          $ (zip (fst3 <$> cs) (VarE <$> constrParams))
                          ++ (zip (fst3 <$> ms) (AppE (ConE 'Just) . VarE
                                                     <$> maybeParams))
             projFun = FunD toFunName [Clause [projPat] (NormalB projBody ) []]
         return [dt, injFun, projFun]

     _ -> error "mkAddCall only works on single-record-constructor types"

-- Associated data declaration in IsResource class.
-- A bug in ghc prevents this from being used for now.
derivedType' :: Name -> DerivedData -> Q [Dec]
derivedType' name DD{ derivedPrefix = pre
                    , removeFields = rf
                    , derive = derive
                    } = do
    info <- reify name
    let uPre = upcase pre
        removePre = takeWhile isLower . downcase $ nameBase name
    case info of
     TyConI (DataD [] name [] [RecC cName cFields] _) ->
          let cs = [ ( mkName $ pre <> upcase name
                    , s, tp)
                  | (nm, s, tp) <- cFields
                  , name <- [nameBase nm]
                  , name `notIn` rf
                    -- Drop the type name as a prefix
                  , downcase (fromMaybe "" (L.stripPrefix removePre name))
                       `notIn` rf
                  ]
         in return $ [DataInstD [] (mkName "AddResource") [ConT name]
                        [RecC cName cFields] []]
     _ -> error "mkAddCall only works on single-record-constructor types"

-- Used by derivedType{,'} and mkGenericJSON. Not exported.
downcase :: [Char] -> [Char]
downcase [] = []
downcase (c:cs) = toLower c : cs

-- Used by derivedType{,'}. Not exported.
upcase :: [Char] -> [Char]
upcase [] = []
upcase (c:cs) = toUpper c : cs

-- Used by derivedType{,'}. Not exported.
notIn :: Eq a => a -> [a] -> Bool
notIn x xs = not (x `elem` xs)
