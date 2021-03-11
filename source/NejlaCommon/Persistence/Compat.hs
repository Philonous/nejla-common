-- | Compatibility shim for dealing with changing APIs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module NejlaCommon.Persistence.Compat where


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
