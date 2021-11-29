{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module NejlaCommon.Component where

import           Control.Lens
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           GHC.Exts                       (IsList(fromList))
import           GHC.TypeLits

import           Data.Swagger
import           Servant.Swagger

import           Servant
import           Servant.Server.Internal.Router (Router'(StaticRouter))

data Component (s :: Symbol)

-- | Context determining which components should be activated
newtype EnabledComponents = EnabledComponents (Set Text)

instance ( HasServer api ctx
         , KnownSymbol s
         , HasContextEntry ctx EnabledComponents)
  => HasServer (Component s :> api) ctx where
  type ServerT (Component s :> api) m = ServerT api m
  route _ ctx d =
    let componentName = Text.pack $ symbolVal (Proxy @s)
        EnabledComponents comps = getContextEntry ctx
    in if Set.member componentName comps
       then route (Proxy :: Proxy api) ctx d
       else StaticRouter mempty mempty

  hoistServerWithContext _ ctx hoist m =
    hoistServerWithContext (Proxy :: Proxy api) ctx hoist m

instance (KnownSymbol s, HasSwagger api) => HasSwagger (Component s :> api) where
  toSwagger _ =
    toSwagger (Proxy :: Proxy api)
      & paths . traversed . pathItemOperations . _Just
      . tags
       %~ (<> fromList [componentName])
    where componentName = Text.pack $ symbolVal (Proxy @s)

-- | Traversal over all of the operations of a path
pathItemOperations :: Traversal' PathItem (Maybe Operation)
pathItemOperations inj
  items =
  (\ _pathItemGet
     _pathItemPut
     _pathItemPost
     _pathItemDelete
     _pathItemOptions
     _pathItemHead
     _pathItemPatch ->
      items { _pathItemGet
            , _pathItemPut
            , _pathItemPost
            , _pathItemDelete
            , _pathItemOptions
            , _pathItemHead
            , _pathItemPatch
            }
      ) <$> inj (_pathItemGet     items)
        <*> inj (_pathItemPut     items)
        <*> inj (_pathItemPost    items)
        <*> inj (_pathItemDelete  items)
        <*> inj (_pathItemOptions items)
        <*> inj (_pathItemHead    items)
        <*> inj (_pathItemPatch   items)
