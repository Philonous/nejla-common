-- | Compatibility shim for dealing with changing APIs
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module NejlaCommon.Persistence.Compat
  ( module NejlaCommon.Persistence.Compat
  , connLogFunc
  )
where

--------------------------------------------------------------------------------
-- LTS 18 ----------------------------------------------------------------------
--------------------------------------------------------------------------------
#if MIN_VERSION_persistent(2,12,0)

import           Data.Text                  (Text)
import Database.Persist.Names as P
    ( FieldNameHS(..), EntityNameHS(unEntityNameHS) )
import Database.Persist.Types as P
    ( getEntityForeignDefs,
      getEntityFields,
      getEntityHaskellName,
      ForeignDef(foreignFields),
      ReferenceDef(ForeignRef),
      FieldDef,
      EntityDef )
-- This has to be .Internal because Database.Persist.SqlBackend exports the
-- wrong function!
import           Database.Persist.SqlBackend.Internal
                 (connLogFunc)

unEntityNameHS :: EntityNameHS -> Text
unEntityNameHS = P.unEntityNameHS

getEntityHaskellName :: EntityDef -> EntityNameHS
getEntityHaskellName ent = P.getEntityHaskellName ent

getEntityFields :: EntityDef -> [FieldDef]
getEntityFields ent = P.getEntityFields ent

unFieldNameHS :: FieldNameHS -> Text
unFieldNameHS = P.unFieldNameHS

getEntityForeignDefs :: EntityDef -> [ForeignDef]
getEntityForeignDefs = P.getEntityForeignDefs
-- #else

foreignFields :: ForeignDef -> [(Text, Text)]
foreignFields frgn =
  [(f,t) | ((FieldNameHS f, _), (FieldNameHS t, _)) <- P.foreignFields frgn ]

unForeignRefs :: ReferenceDef -> Maybe EntityNameHS
unForeignRefs (ForeignRef x) = Just x
unForeignRefs _ = Nothing


--------------------------------------------------------------------------------
-- LTS 17 ----------------------------------------------------------------------
--------------------------------------------------------------------------------
#else

import           Data.Text                  (Text)
import           Database.Persist.Types as P
import           Database.Persist.Sql
                 (connLogFunc)


unEntityNameHS :: HaskellName -> Text
unEntityNameHS = P.unHaskellName

getEntityHaskellName :: EntityDef -> HaskellName
getEntityHaskellName = P.entityHaskell

getEntityFields :: EntityDef -> [FieldDef]
getEntityFields = P.entityFields

unFieldNameHS :: HaskellName -> Text
unFieldNameHS = P.unHaskellName

getEntityForeignDefs :: EntityDef -> [ForeignDef]
getEntityForeignDefs = P.entityForeigns
-- #else

foreignFields :: ForeignDef -> [(Text, Text)]
foreignFields frgn =
  [(f,t) | ((HaskellName f, _), (HaskellName t, _)) <- P.foreignFields frgn ]

unForeignRefs :: ReferenceDef -> Maybe HaskellName
unForeignRefs (ForeignRef x _) = Just x
unForeignRefs _ = Nothing


#endif
