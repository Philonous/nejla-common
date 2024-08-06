{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE UndecidableInstances #-}

module NejlaCommon.OpenApi.Operation
  ( (:!)
  , OperationId
  , HasOperationIds
  , NoDuplicateOperationIds
  , hasOperationIds
  )

where

import           Control.Lens    ( (&), (.~) )

import           Data.Data
import           Data.Kind       ( Constraint, Type )
import qualified Data.OpenApi    as OpenApi
import qualified Data.Text       as Text

import           GHC.TypeLits

import           Servant
import           Servant.OpenApi

data OperationId (oid :: Symbol)

infix 9 :!
type api :! oid = OperationId oid :> api

instance HasServer api ctx => HasServer (OperationId oid :> api) ctx where
  type ServerT (OperationId oid :> api) m = ServerT api m

  route _ = route (Proxy :: Proxy api)

  hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy api)

-- We could be a bit more precise here and instead have individial instances for
-- `Verb` and `NoContentVerb`, which would prevent misuse, but I don't think
-- this is worth the additional effort
instance (KnownSymbol oid, HasOpenApi api)
  => HasOpenApi (OperationId oid :> api) where
  toOpenApi _ =
    let oidStr = Text.pack $ symbolVal (Proxy :: Proxy oid)
    in toOpenApi (Proxy :: Proxy api)
       & OpenApi.allOperations . OpenApi.operationId .~ Just oidStr

instance (HasLink api) => HasLink (OperationId oid :> api) where
  type MkLink (OperationId oid :> api) a = MkLink api a

  toLink f _ = toLink f (Proxy :: Proxy api)

-- Machinery to produce nice type errors for missing OperationIds
-----------------------------------------------------------------

type family MapOperationId oid endpoints where
  MapOperationId oid '[] = '[]
  MapOperationId oid (a ': as) = Just oid ': MapOperationId oid as

-- Find Endpoints that don't have an OperationId
-- We have to keep recursing after we have found an operationId because it
-- might apply to multiple endpoints
type family OperationIds a :: [Maybe Symbol] where
  OperationIds (OperationId oid :> api) = MapOperationId oid (OperationIds api)
  OperationIds (path :> api) = OperationIds api
  OperationIds (a :<|> b) = AppendList (OperationIds a) (OperationIds b)
  OperationIds x = '[ Nothing ]

type family FindDup a as where
  FindDup a '[] = '[]
  FindDup a (Just a ': as) = '[ a ]
  FindDup a (b ': bs) = FindDup a bs

type family FindDups as where
  FindDups '[] = '[]
  FindDups (Just a ': as) = AppendList (FindDup a as) (FindDups as)

type family DuplicateOperationIdsError as :: Constraint where
  DuplicateOperationIdsError '[] = ()
  DuplicateOperationIdsError errs =
    TypeError ('Text "Duplicate OperationIds: " ':$$: 'ShowType errs)

type family NoDuplicateOperationIds a :: Constraint where
  NoDuplicateOperationIds api =
    DuplicateOperationIdsError (FindDups (OperationIds api))

-- Find Endpoints that don't have an OperationId
type family NoOperationId a :: [Type] where
  NoOperationId (OperationId oid :> api) = '[]
  NoOperationId (path :> api) = MapSub path (NoOperationId api)
  NoOperationId (a :<|> b) = AppendList (NoOperationId a) (NoOperationId b)
  NoOperationId x = '[ x ]

type family CheckOperationIds errs :: Constraint where
  CheckOperationIds '[] = ()
  CheckOperationIds errs =
    TypeError ('Text "Some Endpoints don't have an operationId: "
               ':$$: 'ShowType errs)

type family HasOperationIds a :: Constraint where
  HasOperationIds t = CheckOperationIds (NoOperationId t)

-- | Check that every endpoint in the API has operationId set and
-- there are no duplicates
-- Semantically this is `id`, but it adds
hasOperationIds
  :: (HasOperationIds a, NoDuplicateOperationIds a) => proxy a -> proxy a
hasOperationIds x = x

-- Needed to make OperationId work with Servant
type instance IsElem' e (OperationId oid :> api) = IsElem e api
