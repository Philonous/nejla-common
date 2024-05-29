{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NejlaCommon.JSON where

import           Control.Applicative          ((<|>))
import           Control.Lens                 hiding (from, to)
import qualified Data.Char                    as Char
import           Data.Data                    (Typeable, Proxy(..))
import           Data.Kind
import qualified Data.List                    as List
import qualified Data.OpenApi                 as OpenApi
import           Data.OpenApi.Internal.Schema (GToSchema)
import           Data.OpenApi.Schema          as Schema
import qualified Data.Text                    as Text
import           GHC.Generics
import           GHC.TypeLits
import           Web.HttpApiData              ( ToHttpApiData(..)
                                              , FromHttpApiData(..))

import           Data.Aeson                   as Aeson

import           NejlaCommon.Helpers

-- data Foo = Foo { fooBar :: Int, fooQuux :: Bool}
--    deriving (Generic)
--    deriving ToSchema via (AsObject Foo)
newtype AsObject a = AsObject a
  deriving newtype (Show, Eq, Ord)

instance (Typeable a, Generic a, GToSchema (Rep a)
         , Rep a ~ M1 d m m1
         , Datatype m
         )
  => ToSchema (AsObject a) where
  -- declareNamedSchema :: Proxy a -> Declare (Definitions Schema) NamedSchema
  declareNamedSchema _prx =
    -- Assume that the field prefix is the (lower case) name of the type
    -- E.g. data Foo = Bar {fooQuux :: Int}
    -- => field name is "quux"
    let prf = downcase $ datatypeName (M1 Proxy :: M1 d m Proxy m1)
    in genericDeclareNamedSchema
       (opts prf) (Proxy @a)
    where
      opts prf =
        defaultSchemaOptions {
        Schema.fieldLabelModifier = downcase . withoutPrefix prf
        }

structLikeAesonOptions :: String -> Options
structLikeAesonOptions prf =
  defaultOptions
  { Aeson.fieldLabelModifier = downcase . withoutPrefix prf
  }

instance (Generic a, GToJSON' Value Zero (Rep a)
         , Rep a ~ M1 d m m1
         , Datatype m
         )
  => ToJSON (AsObject a) where
  toJSON (AsObject x) =
    let prf = downcase $ datatypeName (M1 Proxy :: M1 d m Proxy m1)
    in genericToJSON (structLikeAesonOptions prf) x

instance ( Generic a, GFromJSON Zero (Rep a)
         , Rep a ~ M1 d m m1
         , Datatype m
         )
  => FromJSON (AsObject a) where
  parseJSON v =
    let prf = downcase $ datatypeName (M1 Proxy :: M1 d m Proxy m1)
    in AsObject <$> genericParseJSON (structLikeAesonOptions prf) v

-- Enums -----------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Genericly derive JSON instances for Enum-like types
-- use with »deriving via«
--
-- Example
--   data Role = RoleAdmin | RoleUser | Foobar
--     deriving (Generic)
--     deriving (ToJSON, FromJSON) via (AsEnum Role)
--
--   > encode RoleAdmin
--   "\"\admin"\"

-- Constructor to (json) string
constrToString :: [Char]
               -> [Char]
               -> [Char]
constrToString prefix str =
  downCase $
  case List.stripPrefix prefix str of
    Nothing -> str
    Just "" -> str
    Just stripped -> stripped
  where
    downCase [] = []
    downCase (c:cs) = Char.toLower c : cs

-- ToJSON
---------

class EnumToJSON f where
  enumToJSON :: f p -> String

instance KnownSymbol str => EnumToJSON (C1 ('MetaCons str a 'False) U1) where
  enumToJSON _ = symbolVal (Proxy :: Proxy str)

instance (EnumToJSON f, EnumToJSON g) => EnumToJSON (f :+: g) where
  enumToJSON (L1 x) = enumToJSON x
  enumToJSON (R1 y) = enumToJSON y

instance (KnownSymbol typeName, EnumToJSON f)
  => EnumToJSON (M1 D ('MetaData typeName m p x) f) where
  enumToJSON (M1 x) =
    let baseName = enumToJSON x
    in constrToString (symbolVal (Proxy :: Proxy typeName)) baseName


enumToJSON' ::(Generic a, EnumToJSON (Rep a)) => a -> Value
enumToJSON' x = String . Text.pack $ enumToJSON (from x)



-- FromJSON
-----------

class EnumFromJSON f where
  enumParseJSON :: String -- ^ Prefix
                -> String
                -> Maybe (f p)

instance KnownSymbol str => EnumFromJSON (C1 ('MetaCons str a 'False) U1) where
  enumParseJSON pref txt = if
    constrToString pref (symbolVal (Proxy :: Proxy str))
    == txt
    then Just (M1 U1)
    else Nothing


instance (EnumFromJSON f, EnumFromJSON g) => EnumFromJSON (f :+: g) where
  enumParseJSON pref txt =
        L1 <$> enumParseJSON pref txt
    <|> R1 <$> enumParseJSON pref txt

instance (KnownSymbol typeName, EnumFromJSON f)
  => EnumFromJSON (M1 D ('MetaData typeName m p x) f) where
  enumParseJSON _ txt = M1 <$> enumParseJSON (symbolVal (Proxy :: Proxy typeName)) txt


newtype AsEnum a = AsEnum a

instance (Generic a, EnumToJSON (Rep a)) => ToJSON (AsEnum a) where
  toJSON (AsEnum x) = enumToJSON' x

instance (Generic a, EnumFromJSON (Rep a)) => FromJSON (AsEnum a) where
  parseJSON = withText "string" $ \txt ->
    case enumParseJSON "" (Text.unpack txt) of
      Nothing -> mempty
      Just v -> return $ AsEnum $ to v

-- HttpApiData
--------------

instance (Generic a, EnumFromJSON (Rep a)) => FromHttpApiData (AsEnum a) where
  parseUrlPiece txt = case enumParseJSON "" (Text.unpack txt) of
                        Nothing -> Left $ "Could not parse value " <> txt
                        Just r -> Right $ AsEnum (to r)

instance (Generic a, EnumToJSON (Rep a)) => ToHttpApiData (AsEnum a) where
  toUrlPiece (AsEnum a) = Text.pack $ enumToJSON (from a)


-- Schema
---------

class EnumSchema (f :: Type -> Type) where
  enumSchema :: Proxy f
             -> String -- ^ Prefix
             -> [String]

instance (KnownSymbol str) => EnumSchema (C1 ('MetaCons str a 'False) U1) where
  enumSchema _ pref =
    [constrToString pref (symbolVal (Proxy :: Proxy str))]

instance (EnumSchema f, EnumSchema g) => EnumSchema (f :+: g) where
  enumSchema _ prefix =
    enumSchema (Proxy :: Proxy f) prefix ++ enumSchema (Proxy :: Proxy g) prefix

instance (KnownSymbol typeName, EnumSchema f)
      => EnumSchema (M1 D ('MetaData typeName m p x) f) where
  enumSchema _ _ =
    let prefix = symbolVal (Proxy :: Proxy typeName)
    in enumSchema (Proxy :: Proxy f) prefix



-- Enum Schema -----------------------------------------------------------------

boundedEnumParamSchema ::
     forall a. (Bounded a, Enum a, ToJSON a)
  => Proxy a
  -> OpenApi.Schema
boundedEnumParamSchema _ =
  mempty & OpenApi.type_ ?~ OpenApi.OpenApiString
         & OpenApi.enum_ ?~ (toJSON <$> ([minBound .. maxBound] :: [a]))

instance (Bounded a, Enum a, ToJSON a)
  => OpenApi.ToParamSchema (AsEnum a) where
  toParamSchema _ = boundedEnumParamSchema (Proxy @a)

boundedEnumSchema ::
     forall a. (Bounded a, Enum a, ToJSON a)
  => Proxy a
  -> OpenApi.NamedSchema
boundedEnumSchema prx = do
  OpenApi.NamedSchema Nothing $ boundedEnumParamSchema prx

instance (Bounded a, Enum a, ToJSON a, Typeable a)
  => OpenApi.ToSchema (AsEnum a) where
 declareNamedSchema _ = return $ boundedEnumSchema (Proxy @a)
