{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Database schema versioning and Migration
module NejlaCommon.Persistence.Migration
  ( sql,
    sqlFile,
    migrate,
    migrateChecked,
    MigrateOptions (..),
    defaultMigrateOptions,
    initializeSchema,
    InitOptions (..),
    checkSchema,
    CheckSchemaOptions (..),
    M,
    SchemaVersion,
    Migration (..),
    MigrateInfo (..),
    runMigration,
    getUnmanagedChanges,
    UnmanagedChangeScope (..),
    UnmanagedSchemaChange (..),

    -- ** Re-exports
    gitHash,
    P.rawExecute,
    P.PersistValue (..),
    P.Single (..),

    -- * Internal functions
    ensureMetaSchema,
    currentSchemaVersion,
    registerMigration,
    checkMigrationConsistency,

    -- * Helper functions
    getMetaSchemaVersion,
    runM,
  )
where

import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State
import Data.Int (Int64)
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock
import Data.Time.Format
import qualified Database.Persist.Sql as P
import Development.GitRev
import NejlaCommon.Persistence.Compat (connLogFunc)
import NejlaCommon.Persistence.Util (sql, sqlFile)
import System.Exit (exitFailure, exitSuccess)

type M = ReaderT P.SqlBackend (LoggingT IO)

runM :: M a -> P.ConnectionPool -> IO a
runM m = P.runSqlPool $ do
  con <- ask
  liftIO $ runLoggingT (runReaderT m con) (connLogFunc con)

-- Used internally for tracking state
type MM = StateT Bool M

type SchemaVersion = Text

-- | Check if a table exists in the _meta schema
--   Postgres-specific
metaTableExistsP ::
  -- | Name of the schema
  Text ->
  M Bool
metaTableExistsP table = do
  res <-
    P.rawSql
      [sql|
    SELECT 1
    FROM pg_class c
    INNER JOIN pg_namespace s
    ON s.oid = c.relnamespace
    WHERE s.nspname = '_meta'
      AND c.relname = ?
      AND c.relkind = 'r'
    |]
      [P.PersistText table] ::
      M [P.Single Int]
  return . not $ List.null res

-- | Retrieve the version of the *meta* schema, that is, the schema of the
-- tables where we store migrations etc. This is *not* the schema version users
-- are interested in.
getMetaSchemaVersion :: M (Maybe Int64)
getMetaSchemaVersion = do
  -- Unconditionally set up meta schema versioning (yes, this is 2nd order
  -- versioning)
  -- This is idempotent, so we run it unconditionally
  metaTableExistsP "_meta_schema_version" >>= \case
    False -> return Nothing
    True -> do
      res <-
        P.rawSql
          [sql|
        SELECT version
        FROM _meta._meta_schema_version
        |]
          []
      case res of
        [P.Single (P.PersistInt64 i)] -> return $ Just i
        _ -> error "getMetaSchemaVersion: impossible"

getMetaSchemaVersionOrInstall :: M Int64
getMetaSchemaVersionOrInstall =
  getMetaSchemaVersion >>= \case
    Nothing -> do
      P.rawExecute
        $(sqlFile "src/NejlaCommon/Persistence/sql/meta_versioning.sql")
        []
      getMetaSchemaVersion >>= \case
        Nothing -> error "getMetaSchemaVersionOrInstall: impossible"
        Just v -> return v
    Just v -> return v

-- | Setup the metadata schema "_meta" and register an empty migration as a
-- starting point
ensureMetaSchema :: M ()
ensureMetaSchema = do
  getMetaSchemaVersionOrInstall >>= \case
    -- Version 1 means we are either in a fresh server or in the old style system
    0 -> do
      $logInfo "Schema versioning not found or outdated, installing"
      P.rawExecute
        $(sqlFile "src/NejlaCommon/Persistence/sql/initialize_versioning.sql")
        []

      P.rawExecute [sql| UPDATE _meta._meta_schema_version SET version = 1 |] []
      return ()
    -- Meta schema version 1 is current
    1 -> return ()
    n -> do
      $logError [i|Meta schema verision #{n} is unknown. Bailing out.|]

-- | Query the current schema version
currentSchemaVersion :: M SchemaVersion
currentSchemaVersion = do
  P.rawSql
    [sql|
               SELECT _meta.schema_version();
               |]
    []
    >>= \case
      [Nothing] -> return ""
      [Just (P.Single (P.PersistText i))] -> return i
      rs -> error $ "currentSchemaVersion: unexpected result" <> show rs

-- Set migration context so that schema changes aren't picked up in
-- unmanaged_schema_changes. Lasts to the end of the transaction
beginManagedMigration :: MM ()
beginManagedMigration = do
  started <- get
  unless started $ do
    lift $ P.rawExecute "SET LOCAL _meta.in_migration = true;" []
    put True

-- | Register a migration. Shouldn't be used manually
registerMigration ::
  -- | Program revision (e.g. git revision)
  Text ->
  -- | Expected schema version before migration
  SchemaVersion ->
  -- | Schema version after the migration
  SchemaVersion ->
  -- | Description of the migration changes
  Text ->
  M ()
registerMigration revision expect to description = do
  _ <-
    P.rawSql
      [sql| SELECT _meta.add_migration(?, ?, ?, ?); |]
      [ P.PersistText expect,
        P.PersistText to,
        P.PersistText description,
        P.PersistText revision
      ] ::
      M [P.Single P.PersistValue]
  return ()

-- | Unconditionally run a single migration. Used in tests
runMigration :: Text -> Migration -> M ()
runMigration revision m = evalStateT (runMigration' revision m) False

runMigration' :: Text -> Migration -> MM ()
runMigration' revision Migration {..} = do
  beginManagedMigration
  $logInfo $
    "Migrating database schema from "
      <> (if Text.null expect then "<initial>" else expect)
      <> " to "
      <> to
      <> " ("
      <> description
      <> ")"
  lift script
  lift $ registerMigration revision expect to description

-- | A database migration. Migrations are unidirectional. Rollback is
-- intentionally unsupported.
data Migration
  = Migration
  { -- | Expected schema version before the migration (Nothing if no migrations
    -- exist)
    expect :: SchemaVersion,
    -- | Schema version after the migration
    to :: SchemaVersion,
    -- | Description of the migration
    description :: Text,
    script :: M ()
  }

-- | Sanity-check a migration list and fixups without touching the database.
--
-- Checks for the main chain:
-- * Adjacent migrations form a chain (@to@ matches next @expect@)
-- * No two migrations share a target version
-- * No migration has an empty target
-- * Every migration has a description
--
-- Checks for fixups:
-- * Each fixup's @expect@ must not appear as any @to@ or @expect@ in the main chain
-- * Each fixup's @to@ must appear as a @to@ in the main chain
-- * No two fixups share the same @expect@
-- * No fixup has an empty target or description
checkMigrationConsistency :: MigrateInfo -> [Text]
checkMigrationConsistency MigrateInfo {miMigrations = []} = ["Empty migration list"]
checkMigrationConsistency MigrateInfo {miMigrations = ms, miFixups = fixups} =
  concat
    [ chainChecks,
      duplicateTargets,
      emptyTargets,
      emptyDescriptions,
      fixupFromNotOrphaned,
      fixupToNotOnChain,
      fixupDuplicateExpects,
      fixupEmptyTargets,
      fixupEmptyDescriptions
    ]
  where
    -- `to` of each migration must equal `expect` of its successor
    chainChecks =
      [ [i|Chain break: migration to #{to a} followed by migration expecting #{expect b}|] ::
          Text
      | (a, b) <- zip ms (drop 1 ms),
        to a /= expect b
      ]
    -- Target versions must be unique (otherwise findMigrations is ambiguous)
    duplicateTargets =
      [ [i|Duplicate target version: #{v}|] :: Text
      | v <-
          map head . filter ((> 1) . length) . List.group . List.sort $
            map to ms
      ]
    -- Empty target would alias the pre-migration sentinel version
    emptyTargets =
      [ [i|Migration with empty target version|] :: Text
      | any (Text.null . to) ms
      ]
    -- Not strictly a consistency issue, but cheap to catch
    emptyDescriptions =
      [ [i|Migration #{expect m} -> #{to m} has empty description|] ::
          Text
      | m <- ms,
        Text.null (description m)
      ]

    -- All versions mentioned in the main chain (both expect and to)
    mainChainVersions = map expect ms ++ map to ms

    -- Fixup `expect` must be orphaned (not appear anywhere in the main chain)
    fixupFromNotOrphaned =
      [ [i|Fixup expect version #{expect f} appears in main migration chain|] ::
          Text
      | f <- fixups,
        expect f `elem` mainChainVersions
      ]
    -- Fixup `to` must converge back onto the main chain
    fixupToNotOnChain =
      [ [i|Fixup target version #{to f} is not a target in the main migration chain|] ::
          Text
      | f <- fixups,
        to f `notElem` map to ms
      ]
    -- No two fixups from the same orphaned version
    fixupDuplicateExpects =
      [ [i|Duplicate fixup expect version: #{v}|] :: Text
      | v <-
          map head . filter ((> 1) . length) . List.group . List.sort $
            map expect fixups
      ]
    fixupEmptyTargets =
      [ [i|Fixup migration with empty target version|] :: Text
      | any (Text.null . to) fixups
      ]
    fixupEmptyDescriptions =
      [ [i|Fixup migration #{expect f} -> #{to f} has empty description|] ::
          Text
      | f <- fixups,
        Text.null (description f)
      ]

-- | Find relevant migrations starting from the current schema version
--
-- * Returns 'Nothing' if the supplied schema version is not found
--   (this is considered an error)
-- * Returns an empty list if the schema version is the current target
--   schema version, that is, it matches the "to" field in the last migration
-- * Otherwise it returns a list of migrations to be run
-- * If the version is orphaned (not on the main chain), checks fixup
--   migrations for a path back onto the chain
findMigrations :: SchemaVersion -> MigrateInfo -> Maybe [Migration]
findMigrations v MigrateInfo {miMigrations = ms, miFixups = fixups} =
  case findOnChain v ms of
    Just result -> Just result
    Nothing ->
      -- Version not on main chain; check fixups
      case List.find (\f -> expect f == v) fixups of
        Just fixup ->
          -- The fixup's `to` is on the main chain, find remaining migrations from there
          case findOnChain (to fixup) ms of
            Just remaining -> Just (fixup : remaining)
            Nothing -> Nothing -- shouldn't happen if consistency checks passed
        Nothing -> Nothing
  where
    findOnChain v' [Migration {..}]
      | v' == to = Just []
    findOnChain v' ms'@(Migration {..} : mss)
      | v' == expect = Just ms'
      | otherwise = findOnChain v' mss
    findOnChain _ _ = Nothing

runMigrations :: Text -> SchemaVersion -> [Migration] -> MM ()
runMigrations _ v [] =
  $logInfo $ "Finished migrations. Final schema: " <> v
runMigrations revision v (m@Migration {..} : ms)
  | v == expect = runMigration' revision m >> runMigrations revision to ms
  | otherwise = do
      $logError $ "runMigrations: Unknown schema version " <> v
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
{-# DEPRECATED migrate "use migrateChecked instead" #-}
migrate ::
  -- | Program revision (e.g. $(gitHash) from gitrev)
  Text ->
  [Migration] ->
  M ()
migrate _ [] = do
  $logError "List of migrations is empty, can't migrate"
  liftIO exitFailure
migrate revision migrations = do
  ensureMetaSchema
  sv <- currentSchemaVersion
  let info =
        MigrateInfo
          { miRevision = revision,
            miMigrations = migrations,
            miFixups = [],
            miPersistentMigration = Nothing,
            miIgnorePersistentMigrations = []
          }
  case findMigrations sv info of
    Nothing -> do
      $logError $ "Unknown schema version " <> sv
      liftIO exitFailure
    Just ms -> evalStateT (runMigrations revision sv ms) False

data MigrateInfo
  = MigrateInfo
  { -- | Provenance (git revision or similar)
    miRevision :: Text,
    -- | Migrations to bring database to latest schema
    miMigrations :: [Migration],
    -- | Fixup migrations from orphaned schema versions back onto the main chain.
    --
    --   When a broken migration has been deployed
    --
    --   1. Fix the migration in the codebase and rename its target version
    --      (e.g. @"5"@ becomes @"5-fixed"@), so there is no ambiguity between
    --      the broken and corrected schemas.
    --   2. Fresh deployments now get the correct schema via the fixed migration.
    --   3. Existing deployments are orphaned — their database is at version
    --      @"5"@ which no longer exists in the main chain.
    --   4. Add a fixup migration from @"5"@ to @"5-fixed"@ that brings the
    --      database from the broken state to the corrected one.
    --
    --   At most one fixup migration will run, and it will be the first migration
    --   to run. After that, normal migrations continue from the fixup's target.
    --
    --   @
    --   Main chain:  "" -- 1 -- 2 -- 3 -- 4 -- 5-fixed -- 6
    --                                              ^
    --   Fixup:                                5 ---+
    --   @
    miFixups :: [Migration],
    -- | Optional Persistent migration for schema drift detection
    miPersistentMigration :: Maybe P.Migration,
    -- | Persistent migrations to ignore (known acceptable drift)
    miIgnorePersistentMigrations :: [Text]
  }

-- | Options determining how migrations should be performed
data MigrateOptions = MigrateOptions
  { -- | Allow migration to proceed despite unmanaged schema changes
    allowUnmanagedChanges :: Bool,
    -- | Allow migration to proceed despite Persistent schema drift
    allowSchemaDrift :: Bool,
    dryRun :: Bool
  }
  deriving (Show, Eq)

defaultMigrateOptions :: MigrateOptions
defaultMigrateOptions =
  MigrateOptions
    { allowUnmanagedChanges = False,
      allowSchemaDrift = False,
      dryRun = False
    }

-- | Like 'migrate', but runs health checks first.
--
-- Checks performed:
--
-- * Migration consistency (always fails on error - these are code bugs)
-- * Unknown schema version (always fails - can't migrate from unknown state)
-- * Unmanaged schema changes (fails unless 'allowUnmanagedChanges' is set)
-- * Schema drift via Persistent (fails unless 'allowSchemaDrift' is set)
--
-- If checks pass (or are bypassed), runs pending migrations.
migrateChecked :: MigrateInfo -> MigrateOptions -> M ()
migrateChecked info options = do
  ensureMetaSchema
  sv <- currentSchemaVersion
  $logInfo [i|Current schema version: #{sv}|]

  -- Check migration consistency (always fatal)
  case checkMigrationConsistency info of
    [] -> return ()
    problems -> do
      $logError "Found problems with migrations:"
      forM_ problems $ \p -> $logError p
      liftIO exitFailure

  -- Check for pending migrations and unknown version
  pendingMigrations <- case findMigrations sv info of
    Nothing -> do
      $logError "Current schema version is unknown. Cannot migrate."
      liftIO exitFailure
    Just ms -> return ms

  -- Check unmanaged changes
  uscs <- getUnmanagedChanges SinceLastMigration
  case uscs of
    [] -> $logInfo "No unmanaged schema changes since last migration"
    (_ : _) -> do
      $logWarn "Unmanaged schema changes since last migration:"
      forM_ uscs $ \usc -> $logWarn $ " - " <> prettyUnmanagedChange usc
      case (allowUnmanagedChanges options, null pendingMigrations) of
        -- We want to migrate but unmanaged changes are blocking us
        (False, False) -> do
          $logError "Use --allow-unmanaged to proceed anyway"
          liftIO exitFailure
        -- No migrations to run, so we aren't "proceeding"
        (_, True) -> return ()
        --- Migrate despite unmanaged changes
        (True, False) -> do
          $logWarn "Proceeding despite unmanaged changes (--allow-unmanaged)"

  -- Run migrations
  case pendingMigrations of
    [] -> $logInfo "No migrations to run, database is up to date"
    ms -> evalStateT (runMigrations (miRevision info) sv ms) False

  -- Check schema drift now that we are in the latest migration
  case miPersistentMigration info of
    Nothing -> return ()
    Just mgs -> do
      migss <- P.getMigration mgs
      case migss List.\\ miIgnorePersistentMigrations info of
        [] -> $logInfo "Schema matches entity definitions"
        drifts -> do
          $logWarn "Persistent reports schema mismatch:"
          forM_ drifts $ \d -> $logWarn d
          unless (allowSchemaDrift options) $ do
            $logError "Use --allow-drift to proceed anyway"
            liftIO exitFailure
          $logWarn "Proceeding despite schema drift (--allow-drift)"
      case miIgnorePersistentMigrations info List.\\ migss of
        [] -> return ()
        is -> do
          $logWarn "You have unmatched ignored persistent migrations, consider removing them:"
          forM_ is $ \i -> $logWarn i

  when (dryRun options) $ liftIO exitSuccess

-- | Options for 'initializeSchema' to control behavior on check failures.
data InitOptions = InitOptions
  { -- | Allow initialization to proceed despite Persistent schema drift
    initAllowSchemaDrift :: Bool,
    -- | Assume that uninitialized database is at this schema version
    initAssumeAt :: Text,
    -- | Abort after successful run
    initDryRun :: Bool
  }
  deriving (Show, Eq)

-- | Initialize schema versioning on an existing database.
--
-- Use this to adopt the migration system for a database that already has
-- a schema but wasn't using migrations. This will:
--
-- 1. Fail if the meta schema is already installed
-- 2. Install the meta schema
-- 3. Register a dummy migration from the empty version to the specified version
-- 4. Run any pending migrations from that version onwards
--
-- @initAssumeAt@ should match one of the @expect@ fields in your
-- migration list, indicating the current state of the database.
initializeSchema ::
  -- | Current schema version to start from
  MigrateInfo ->
  InitOptions ->
  M ()
initializeSchema info options = do
  -- Fail if meta schema already exists
  getMetaSchemaVersion >>= \case
    Just _ -> do
      $logError "Schema versioning is already installed. Use 'migrate' instead."
      liftIO exitFailure
    Nothing -> return ()

  -- Check migration consistency (always fatal)
  case checkMigrationConsistency info of
    [] -> return ()
    problems -> do
      $logError "Found problems with migrations:"
      forM_ problems $ \p -> $logError p
      liftIO exitFailure

  let fromVersion = initAssumeAt options
  -- Verify fromVersion is valid (must be a known version in the migration chain)
  case findMigrations fromVersion info of
    Nothing -> do
      $logError [i|Unknown schema version: #{fromVersion}|]
      $logError "The --assume-schema version must match an 'expect' field in the migrations"
      liftIO exitFailure
    Just _pendingMigrations -> do
      $logInfo [i|Initializing schema versioning from version: #{fromVersion}|]

      -- Install meta schema
      ensureMetaSchema

      -- Register dummy migration to the starting version
      $logInfo [i|Registering initial schema version: #{fromVersion}|]
      registerMigration
        (miRevision info)
        ""
        fromVersion
        "Initial schema (adopted existing database)"
      migrateChecked
        info
        defaultMigrateOptions
          { allowSchemaDrift = initAllowSchemaDrift options
          -- Don't set dry run, we escape ourselves
          }

      when (initDryRun options) $ liftIO exitSuccess

-- | Options determining how migrations should be performed
data CheckSchemaOptions = CheckSchemaOptions
  { -- | Don't fail despite unmanaged schema changes
    checkAllowUnmanagedChanges :: Bool,
    -- | Don't fail despite Persistent schema drift
    checkAllowSchemaDrift :: Bool
  }
  deriving (Show, Eq)

-- | Read-only schema health check. Exits with failure if any issues found.
--   Checks: unmanaged changes, migration consistency, pending migrations,
--   and schema drift (via Persistent).
checkSchema :: MigrateInfo -> CheckSchemaOptions -> M ()
checkSchema info options = do
  getMetaSchemaVersion >>= \case
    Nothing -> do
      $logError "Schema versioning is not installed."
      liftIO exitFailure
    Just {} -> return ()
  sv <- currentSchemaVersion
  $logInfo [i|Current schema version: #{sv}|]
  failures <-
    sequence
      [ checkUnmanagedChanges,
        checkMigrationConsistency',
        checkMigrationsAndDrift sv
      ]
  when (or failures) $ liftIO exitFailure
  where
    checkUnmanagedChanges = do
      uscs <- getUnmanagedChanges SinceLastMigration
      case uscs of
        [] -> do
          $logInfo "No unmanaged schema changes since last migration"
          return False
        (_ : _) -> do
          $logWarn "Unmanaged schema changes since last migration: "
          forM_ uscs $ \usc -> $logWarn $ " - " <> prettyUnmanagedChange usc
          return (not $ checkAllowUnmanagedChanges options)

    checkMigrationConsistency' =
      case checkMigrationConsistency info of
        [] -> return False
        problems -> do
          $logError "Found problems with migrations: "
          forM_ problems $ \p -> $logError p
          return True

    checkMigrationsAndDrift sv =
      case findMigrations sv info of
        Nothing -> do
          $logError "Current schema version is unknown. Migrations won't work"
          return True
        Just [] -> do
          $logInfo "Database is in the latest schema version"
          checkSchemaDrift
        Just ms -> do
          $logWarn "Migrations are pending:"
          forM_ ms $ \migration ->
            $logWarn [i|  - from #{expect migration} to #{to migration} (#{description migration})|]
          return True

    checkSchemaDrift =
      case miPersistentMigration info of
        Nothing -> return False
        Just mgs -> do
          migss <- P.getMigration mgs
          hasMismatch <- case migss List.\\ miIgnorePersistentMigrations info of
            [] -> do
              $logInfo "Schema matches entity definitions"
              return False
            ms -> do
              $logWarn "Persistent reports schema mismatch: "
              forM_ ms $ \m -> $logWarn m
              return (not $ checkAllowSchemaDrift options)
          case miIgnorePersistentMigrations info List.\\ migss of
            [] -> return ()
            is -> do
              $logWarn "You have unmatched ignored persistent migrations, consider removing them:"
              forM_ is $ \i -> $logWarn i
          return hasMismatch

-- Unmanaged schema changes are captured by event triggers on create/update/drop

-- | Unmanaged schema changes detected by event triggers. These represent
--   DDL operations performed outside the migration system (e.g. emergency
--   fixes, manual interventions).
data UnmanagedSchemaChange
  = UnmanagedSchemaChange
  { -- | Migration version when change occurred
    uscSchemaVersion :: Text,
    -- | Timestamp of the change
    uscModifiedAt :: UTCTime,
    -- | Database user who made the change
    uscModifiedBy :: Text,
    -- | DDL command (e.g. "ALTER TABLE", "DROP table")
    uscCommandTag :: Text,
    -- | Object affected (e.g. "public.foo")
    uscObjectIdentity :: Maybe Text,
    -- | Database schema containing the object
    uscDbSchema :: Maybe Text,
    -- | Full SQL query if available
    uscQueryText :: Maybe Text
  }
  deriving (Show, Eq)

-- | Scope for querying unmanaged schema changes.
data UnmanagedChangeScope
  = -- | Changes since the most recent migration. Used to detect drift
    --   before proceeding with the next migration.
    SinceLastMigration
  | -- | All unmanaged changes ever recorded. Useful for auditing.
    AllChanges
  deriving (Show, Eq)

-- | Retrieve unmanaged schema changes. Returns an empty list if no
--   unmanaged changes exist in the given scope.
--
--   'SinceLastMigration' is used by the migration machinery to ensure
--   the schema is in a known state before applying the next migration.
--   'AllChanges' returns the full history, keyed by the migration version
--   each change occurred under.
getUnmanagedChanges :: UnmanagedChangeScope -> M [UnmanagedSchemaChange]
getUnmanagedChanges scope = do
  rows <-
    case scope of
      SinceLastMigration ->
        P.rawSql
          [sql|
            SELECT m.version, u.modified_at, u.modified_by, u.command_tag,
                   u.object_identity, u.schema_name, u.query_text
            FROM _meta.unmanaged_schema_changes u
            INNER JOIN _meta.migrations m ON u.migration_number = m.number
            WHERE u.migration_number =
              (SELECT number FROM _meta.migrations ORDER BY number DESC LIMIT 1)
            ORDER BY u.modified_at
          |]
          []
      AllChanges ->
        P.rawSql
          [sql|
            SELECT m.version, u.modified_at, u.modified_by, u.command_tag,
                   u.object_identity, u.schema_name, u.query_text
            FROM _meta.unmanaged_schema_changes u
            INNER JOIN _meta.migrations m ON u.migration_number = m.number
            ORDER BY u.modified_at
          |]
          []
  return $ map toChange rows
  where
    toChange
      ( P.Single ver,
        P.Single at,
        P.Single by,
        P.Single tag,
        P.Single oid,
        P.Single schema,
        P.Single query
        ) =
        UnmanagedSchemaChange ver at by tag oid schema query

-- | One-line summary with optional query detail on a second line.
--
-- Example output:
--   [2024-03-15 14:22:01] ALTER TABLE public.users (by postgres) @v1.2
--     > ALTER TABLE public.users ADD COLUMN email text;
prettyUnmanagedChange :: UnmanagedSchemaChange -> Text
prettyUnmanagedChange UnmanagedSchemaChange {..} =
  Text.intercalate "\n" $
    catMaybes [Just header, queryLine]
  where
    timestamp =
      Text.pack $
        formatTime defaultTimeLocale "%F %T" uscModifiedAt

    object = case (uscDbSchema, uscObjectIdentity) of
      (Just s, Just o) | s `Text.isInfixOf` o -> o
      (Just s, Just o) -> s <> "." <> o
      (_, Just o) -> o
      (Just s, _) -> s <> ".*"
      _ -> "?"

    header =
      "["
        <> timestamp
        <> "] "
        <> uscCommandTag
        <> " "
        <> object
        <> " (user "
        <> uscModifiedBy
        <> ")"
        <> " @ schema "
        <> (if Text.null uscSchemaVersion then "<init>" else uscSchemaVersion)

    queryLine = (\q -> "  > " <> Text.strip q) <$> uscQueryText
