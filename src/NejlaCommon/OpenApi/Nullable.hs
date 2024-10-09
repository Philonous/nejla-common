{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module NejlaCommon.OpenApi.Nullable where

import           Control.Lens
import           Control.Monad.Reader       as Reader
import           Control.Monad.State        as State

import           Data.Data.Lens
import qualified Data.HashMap.Strict.InsOrd as IMap
import qualified Data.List                  as List
import           Data.OpenApi               as OpenApi
import           Data.String.Interpolate
import           Data.Text                  ( Text )

-- We want to accept `null` as a value for optional fields. Unfortunately by
-- default, only `undefined` or the field being absent are allowed.
--
-- This is complicated by the fact that "nullable" is a property of the schema
-- (i.e. type) itself, not the property (i.e. field of the object).
--
-- To that end, we find all optional fields, if the field schema is inline, we
-- simply set it to `nullable`. In case it's a reference, we look up the
-- referent, and if it isn't `nullable` we add a new `nullable` variant of that
-- schema
--
-- This assumes that the server accepts `null` values as `undefined`, which
-- Aeson by default does
schemaSetOptionalFieldsNullable
  ::
  -- Reader of existing references + State of new references
  Schema -> ReaderT [(Text, Schema)] (State [(Text, Schema)]) Schema
schemaSetOptionalFieldsNullable schema = do
  let req = schema ^. required -- List of required properties
  -- Iterate over all properties
  let props = schema ^. properties
  props' <- traverseIMap props $ \propName schema -> do
    if propName `elem` req
      then
        -- Property is listed as required, don't touch
        pure schema
      else
        -- Property not required, set its schema nullable
        case schema of
            -- Inline schema - just set nullable flag and recurse
            Inline s -> Inline <$> setNullable s
            Ref (Reference refName) -> do
              let refNullable = refName <> "Nullable"
              references <- Reader.ask
              newReferences <- State.get
              -- Check if a nullable schema already exists
              case List.lookup refNullable (newReferences <> references) of
                  -- Nullable schema exists, use
                  Just{} -> pure $ Ref (Reference refNullable)
                  Nothing ->
                    -- Create a Nullable schema and store it in the list of
                    -- reference schemas
                    -- Fetch non-nullable schema
                    case List.lookup refName references of
                        Nothing -> error [i|schemaSetOptionalFieldsNullable: Reference #{refName} not found in openAPI schema|]

                        Just s -> do
                          s' <- setNullable s
                          -- Add nullable schema to reference schemas
                          State.modify ((refNullable, s') :)
                          -- Return reference to new schema
                          pure $ Ref (Reference refNullable)
  pure $ schema & properties .~ props'
  where
    -- Set nullable and ensure that sub-schemata are also fixed
    setNullable s = schemaSetOptionalFieldsNullable s <&> nullable ?~ True

    traverseIMap iMap f = itraverse f iMap

-- | Given an OpenAPI definition, find schema fields that are optional and
-- ensure their type allows `null` as a value
setOptionalFieldsNullable :: OpenApi -> OpenApi
setOptionalFieldsNullable api =
  let references = IMap.toList $ api ^. components . schemas
      (api', additionalReferences) =
        runState (runReaderT (schemaT schemaSetOptionalFieldsNullable $ api)
                             references)
                 []
  in api' & components . schemas
     .~ (IMap.fromList $ references <> additionalReferences)
  where
    -- Traversal over all schemata (using SYB generics)
    schemaT :: Traversal' OpenApi.OpenApi OpenApi.Schema
    schemaT = template
