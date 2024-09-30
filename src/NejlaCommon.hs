{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | In addition to the entities below, this module provides the following
-- 'UUID' instances: 'PersistField', 'PersistFieldSql', 'FromJSON', 'ToJSON',
-- 'JSONSchema', 'PathPiece', 'Info' as well as 'FromHttpApiData' and
-- 'ToHttpApiData'.
module NejlaCommon
  ( module NejlaCommon.Wai
  , module NejlaCommon.Persistence
  , module NejlaCommon.Logging
  , module NejlaCommon.JSON
  , DerivedData(..)
  , WithField(..)
  , derivedType
  , formatUTC
  , parseUTC
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.String             (fromString)

import           Data.Aeson
import           Data.Data
import           Data.Default
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap       as HMap
#else
import qualified Data.HashMap.Strict     as HMap
#endif
import qualified Data.List               as L
import           Data.Maybe
import qualified Data.Text               as Text
import           Data.Time.Clock
import           Data.Time.Format
import           Data.OpenApi.Lens       hiding (patch)
import           Data.OpenApi.Schema     as Schema
import           Control.Lens

import           GHC.Generics (Generic)
import           GHC.TypeLits


import           Language.Haskell.TH     as TH

import           NejlaCommon.Helpers
import           NejlaCommon.JSON
import           NejlaCommon.Logging
import           NejlaCommon.Persistence
import           NejlaCommon.Wai


-- | See 'derivedType'.
data DerivedData =
  DD
  { derivedPrefix  :: String
  , removeFields   :: [String]
  , optionalFields :: [String]
  , derive         :: [DerivClause]
  }

instance Default DerivedData where
  def =
    DD
    { derivedPrefix  = ""
    , removeFields   = []
    , optionalFields = []
    , derive         = pure . DerivClause Nothing $
        ConT <$> [ ''Show, ''Eq, ''Data, ''Typeable, ''Generic ]
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
derivedType tname
            DD{ derivedPrefix = pre
              , removeFields = rf
              , optionalFields = mf
              , derive = derive
              } = do
  info <- reify tname
  let uPre      = upcase pre
      removePre = downcase $ nameBase tname
  case info of
      TyConI (DataD [] name [] _ [ RecC cName cFields ] _) -> do
        let filterField excluded (nm, _, _) =
              let name = nameBase nm
              in and [ name `notIn` excluded
                     , downcase (fromMaybe "" (L.stripPrefix removePre name))
                       `notIn` excluded
                     ]
            (keptFields, removedFields') = L.partition (filterField rf) cFields
            (fullFields', maybeFields') =
              L.partition (filterField mf) keptFields
            fullFields = fst3 <$> fullFields'
            maybeFields = fst3 <$> maybeFields'
            removedFields = fst3 <$> removedFields'
            isStrict = Bang NoSourceUnpackedness SourceStrict
            cs =
              [ (mkName $ pre <> upcase (nameBase nm), isStrict, tp)
              | (nm, _, tp) <- fullFields'
              ]
            ms =
              [ ( mkName $ pre <> upcase (nameBase nm)
                , isStrict
                , AppT (ConT ''Maybe) tp
                )
              | (nm, _, tp) <- maybeFields'
              ]
            cName' = mkName (uPre ++ nameBase name)
            dt =
              DataD []
                    (mkName $ uPre <> nameBase cName)
                    []
                    Nothing
                    [ RecC cName' (cs ++ ms) ]
                    derive
            fromFunName = mkName $ concat [ "from", upcase pre, nameBase name ]
            toFunName = mkName $ concat [ "to", upcase pre, nameBase name ]
            fst3 (x, _, _) = x
        case compare (length rf) (length removedFields) of
            LT -> reportWarning "filtered too many fields "
            EQ -> return ()
            GT -> reportWarning $ "Could not find all fields to remove for "
              <> show name <> " (" <> show (length removedFields)
              <> " filtered of " <> show (length rf) <> "; "
              <> show (length rf - length removedFields) <> "remain)"
        case compare (length mf) (length maybeFields) of
            LT -> reportWarning "made too many fields optional"
            EQ -> return ()
            GT -> reportWarning $
              "Could not find all fields to make optional for " <> show name
              <> " (" <> show (length maybeFields) <> " optional of "
              <> show (length mf) <> "; "
              <> show (length mf - length maybeFields) <> "remain)"

        freeParams <- forM removedFields $ newName . nameBase
        maybeParams <- forM maybeFields $ newName . nameBase
        constrParams <- forM cs $ \(nm, _, _) -> newName $ nameBase nm
        let pats     =
              (VarP <$> freeParams ++ maybeParams)
              ++ [ RecP cName' (zip (fst3 <$> cs) (VarP <$> constrParams)) ]
            bd       =
              RecConE cName
                      (zip removedFields (VarE <$> freeParams)
                       ++ zip maybeFields (VarE <$> maybeParams)
                       ++ zip fullFields (VarE <$> constrParams))
            injFun   = FunD fromFunName [ Clause pats (NormalB bd) [] ]
            projPat  =
              RecP cName
                   (zip fullFields (VarP <$> constrParams)
                    ++ zip maybeFields (VarP <$> maybeParams))
            projBody =
              RecConE cName' $ zip (fst3 <$> cs) (VarE <$> constrParams)
              ++ zip (fst3 <$> ms) (AppE (ConE 'Just) . VarE <$> maybeParams)
            projFun  =
              FunD toFunName [ Clause [ projPat ] (NormalB projBody) [] ]
        return [ dt, injFun, projFun ]

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
  WithField
  { withFieldField :: fieldType
  , withFieldBase  :: baseType
  }
    deriving ( Show )

instance (KnownSymbol name, ToJSON fieldType, ToJSON baseType)
  => ToJSON (WithField name fieldType baseType) where
  toJSON wf =
    let fName = fromString $ symbolVal (Proxy :: Proxy name)
    in case toJSON $ withFieldBase wf of
           Object o -> case toJSON $ withFieldField wf of
               Null -> Object o
               v -> Object $ o <> HMap.singleton fName v
           _ -> error "WithField.toJSON: base field does not yield object"

instance (KnownSymbol name, FromJSON fieldType, FromJSON baseType)
  => FromJSON (WithField name fieldType baseType) where
  parseJSON = withObject "object" $ \o -> do
    let fName = fromString $ symbolVal (Proxy :: Proxy name)
    fv <- o .:? fName
    f <- case fv of
        Nothing -> parseJSON Null
        Just v -> parseJSON v
    b <- parseJSON (Object $ HMap.delete fName o)
    return WithField
           { withFieldField = f
           , withFieldBase  = b
           }

-- | Ensures that `Maybe` fields aren't set as required fields
--
-- You can make your own type optional by adding an overlapping instance for
-- it that sets `isOptional` to true:
--
-- > instance {-# OVERLAPPING #-} MaybeOptional MyOptionalType where
-- >   isOptional _ = True
class MaybeOptional a where
  isOptional :: proxy a -> Bool

instance {-# OVERLAPPING #-} MaybeOptional (Maybe a) where
  isOptional _ = True

instance {-# OVERLAPPABLE #-} MaybeOptional a where
  isOptional _ = False

instance (KnownSymbol name, ToSchema fieldType, MaybeOptional fieldType
         , ToSchema baseType) =>
         ToSchema (WithField name fieldType baseType) where
  declareNamedSchema _ = do
    ref <- declareSchemaRef (Proxy @fieldType)
    baseSchema <- declareNamedSchema (Proxy @baseType)
    let fName = Text.pack $ symbolVal (Proxy @name)
    return $ baseSchema
      & schema . properties . at fName ?~ ref
      & if isOptional (Proxy :: Proxy fieldType)
        then Prelude.id
        else schema . required %~ (fName:)


-- | Produces an \"ISO\" (ISO 8601) string.
formatUTC :: UTCTime -> String
formatUTC = formatTime defaultTimeLocale "%FT%T%QZ"

-- | Parses an \"ISO\" (ISO 8601) string.
parseUTC :: String -> Maybe UTCTime
parseUTC t =
  parseTimeM True defaultTimeLocale "%FT%T%QZ" t
  <|> parseTimeM True defaultTimeLocale "%FT%T%Q%z" t
  <|> parseTimeM True defaultTimeLocale "%F" t

-- | Newtype for via-deriving openAPI schema specifications for record types
--
-- - Expects field names to be prefixed with the name of the type
-- - field names will be in camelCase
--
-- Use like this:
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DerivingVia #-}
--
