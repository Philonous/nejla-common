{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

#if MIN_VERSION_persistent(2,11,0)
{-# LANGUAGE ViewPatterns #-}
#endif

-- | Compatibility shim for dealing with changing APIs
module NejlaCommon.Persistence.Compat where

import Data.ByteString (ByteString)
import Database.Persist (
    FieldAttr (FieldAttrMaybe),
    PersistValue (PersistLiteral, PersistLiteralEscaped),
 )

-- Persistent 2.11 changed the type of FieldDef.fieldAttrs from Text to an ADT
#if MIN_VERSION_persistent(2,11,0)

hasFieldAttrMaybe :: [FieldAttr] -> Bool
hasFieldAttrMaybe fs = FieldAttrMaybe `elem` fs
#else
import Data.Text (Text)

hasFieldAttrMaybe :: [Text] -> Bool
hasFieldAttrMaybe fs = "Maybe" `elem` fs
#endif

-- Persistent 2.11 Deprecated PersistDbSpecific and added PersistLiteral{Escaped} instead
#if MIN_VERSION_persistent(2,11,0)
persistLiteralCompatHelper :: PersistValue -> Maybe ByteString
persistLiteralCompatHelper (PersistLiteral bs) = Just bs
persistLiteralCompatHelper (PersistLiteralEscaped bs) = Just bs
persistLiteralCompatHelper _ = Nothing
{-# INLINE persistLiteralCompatHelper #-}

pattern PersistLiteralCompat :: ByteString -> PersistValue
pattern PersistLiteralCompat bs <- (persistLiteralCompatHelper -> Just bs) where
  PersistLiteralCompat bs = PersistLiteralEscaped bs
#else

pattern PersistLiteralCompat bs = PersistDbSpecific bs

#endif
