-- | Compatibility shim for dealing with changing APIs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module NejlaCommon.Persistence.Compat where


import           Data.ByteString  (ByteString)
import qualified Data.ByteString  as BS
import           Data.Text        (Text)
import           Database.Persist

-- Persistent 2.11 changed the type of FieldDef.fieldAttrs from Text to an ADT
#if MIN_VERSION_persistent(2,11,0)
hasFieldAttrMaybe :: [FieldAttr] -> Bool
hasFieldAttrMaybe fs = FieldAttrMaybe `elem` fs
#else
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
