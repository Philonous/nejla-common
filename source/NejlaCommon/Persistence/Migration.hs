{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

-- | Database schema versioning and Migration
--
--
module NejlaCommon.Persistence.Migration
  ( sql
  , sqlFile
  , migrate
  , M
  , SchemaVersion
  , Migration(..)
  -- * Helper functions
  , schemaEmptyP
  -- ** Re-exports
  , gitHash
  , P.rawExecute
  , P.PersistValue(..)
  , P.Single(..)
  -- * Internal functions
  , setupMetaSchema
  , currentSchemaVersion
  , registerMigration
  ) where

import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.List                    as List
import           Data.Text                    (Text)
import qualified Database.Persist.Sql         as P
import           Development.GitRev
import           NejlaCommon.Persistence.Util (sql, sqlFile)

import           Data.Maybe                   (fromMaybe)
import           System.Exit                  (exitFailure)

type M a = ReaderT P.SqlBackend (LoggingT IO) a
type SchemaVersion = Text

-- | Check if a schema is empty (e.g. hasn't been initualized)
--   Postgres-specfic
schemaEmptyP :: Text -- ^ Name of the schema
             -> M Bool
schemaEmptyP schema = do
  res <- P.rawSql [sql|
    SELECT relname
    FROM pg_class c
    INNER JOIN pg_namespace s
    ON s.oid = c.relnamespace
    WHERE s.nspname=?
    |] [P.PersistText schema] :: M [P.Single Text]
  return $ List.null res

-- | Setup the metadata schema "_meta" and register an empty migration as a
-- starting point
setupMetaSchema :: M ()
setupMetaSchema =
   schemaEmptyP "_meta" >>= \case
    -- DB versioning not initialized
    True -> do
      $logInfo "Schema versioning not found. Initializing now."
      P.rawExecute $(sqlFile "source/NejlaCommon/Persistence/sql/initialize_versioning.sql") []
    -- Schema versioning already installed
    False -> return ()

-- | Query the current schema version
currentSchemaVersion :: M (Maybe SchemaVersion)
currentSchemaVersion = do
  P.rawSql [sql|
               SELECT _meta.schema_version();
               |] [] >>= \case
                  [Nothing] -> return Nothing
                  [Just (P.Single (P.PersistText i))] -> return (Just i)
                  [Just (P.Single P.PersistNull)] -> return Nothing
                  _ -> error "currentSchemaVersion: wrong number of results"


-- | Register a migration. Shouldn't be used manually
registerMigration :: Text -- ^ Program revision (e.g. git revision)
                  -> Maybe SchemaVersion -- ^ Expected schema version before migration
                  -> SchemaVersion -- ^ Schema version after the migration
                  -> Text -- ^ Description of the migration changes
                  -> M ()
registerMigration revision expect to description = do
  _ <- P.rawSql [sql| SELECT _meta.add_migration(?, ?, ?, ?);
                         |] [ maybe P.PersistNull P.PersistText expect
                            , P.PersistText to
                            , P.PersistText description
                            , P.PersistText revision
                            ]
                            :: M [P.Single P.PersistValue]
  return ()

-- | Run a migration
runMigration :: Text -- ^ Program revision
             -> Migration
             -> M ()
runMigration revision Migration{..} = do
  $logInfo $ "Migrating database schema from " <> fromMaybe "<None>" expect <> " to " <> to <> " ("
    <> description <> ")"
  script
  registerMigration revision expect to description

data Migration = Migration { expect :: Maybe SchemaVersion
                           -- ^ Expected schema version before the migration (Nothing if no migrations exist)
                           , to :: SchemaVersion
                           -- ^ Schema version after the migration
                           , description :: Text
                           -- ^ Description of the migration
                           , script :: M ()
                           }

findMigration :: Text -> Maybe SchemaVersion -> [Migration] -> M ()
findMigration _r (Just v) [Migration{..}] | v == to =
  $logInfo $ "Already in schema version " <> v <> "; nothing to do."
                                -- Already in final schema version
findMigration revision v ms@(Migration{..}:mss)
  | v == expect = runMigrations revision v ms
  | otherwise = findMigration revision v mss
findMigration _r v _ = do
  $logError $ "Unknown schema version " <> (fromMaybe "<None>" v)
  liftIO exitFailure

runMigrations :: Text -> Maybe SchemaVersion -> [Migration] -> M ()
runMigrations _ v [] = do
  $logInfo $ "Finished migrations. Final schema: " <> fromMaybe "<None>" v
  return ()
runMigrations revision v (m@Migration{..}:ms) | v == expect = do
  runMigration revision m >> runMigrations revision (Just to) ms
                                              | otherwise = do
  $logError $ "runMigrations: Unknown schema version " <> fromMaybe "<None>" v
  liftIO exitFailure

-- | Finds the current schema version and runs all migrations linearly starting
-- from that version.
--
-- Takes a list of migrations. Each migration should leave the schema in the
-- version the next migration expects (that is, the @to@-field of migration @n@
-- should match the @expect@-field of migration @n+1@)
--
-- The first time this function runs it sets up a metadata schema "_meta" that
-- logs the migrations that have been run in the past. The initial schema
-- version before any migrations are registered is the empty string "", so the
-- first migration should expect this schema.

migrate :: Text -- ^ Program revision (e.g. $(gitHash) from gitrev)
        -> [Migration]
        -> M ()
migrate _ [] = do
  $logError "List of migrations is empty, can't migrate"
  liftIO exitFailure
migrate revision migrations = do
  setupMetaSchema
  sv <- currentSchemaVersion
  findMigration revision sv migrations
