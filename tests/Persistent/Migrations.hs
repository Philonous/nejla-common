{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Persistent.Migrations where

import Control.Exception (SomeException, try)
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Database.Persist.Postgresql as P
import NejlaCommon.Persistence.Migration
import Persistent.Common (DBSpec)
import Test.Hspec

run :: P.ConnectionPool -> M a -> IO a
run = flip runM

-- Helpers --------------------------------------------------------------------

-- | A migration whose script creates a table in public
mkTableMigration :: SchemaVersion -> SchemaVersion -> Text -> Text -> Migration
mkTableMigration from to desc tableName =
  Migration
    { expect = from,
      to = to,
      description = desc,
      script =
        P.rawExecute
          ("CREATE TABLE IF NOT EXISTS public." <> tableName <> " (id serial PRIMARY KEY)")
          []
    }

-- | A no-op migration (for testing orchestration without side effects)
mkNoopMigration :: SchemaVersion -> SchemaVersion -> Text -> Migration
mkNoopMigration from to desc =
  Migration
    { expect = from,
      to = to,
      description = desc,
      script = return ()
    }

-- | Query a column from _meta.migrations for the latest row
latestMigrationField :: Text -> M [P.Single P.PersistValue]
latestMigrationField field =
  P.rawSql
    ( "SELECT "
        <> field
        <> " FROM _meta.migrations"
        <> " WHERE number > 0 ORDER BY number DESC LIMIT 1"
    )
    []

-- | Check whether a table exists in a given schema
tableExists :: Text -> Text -> M Bool
tableExists schema table = do
  res <-
    P.rawSql
      [sql|
        SELECT 1 FROM pg_class c
        INNER JOIN pg_namespace s ON s.oid = c.relnamespace
        WHERE s.nspname = ? AND c.relname = ? AND c.relkind = 'r'
      |]
      [P.PersistText schema, P.PersistText table] ::
      M [P.Single Int]
  return (not $ null res)

-- | Check whether an event trigger exists by name
eventTriggerExists :: Text -> M Bool
eventTriggerExists name = do
  res <-
    P.rawSql
      [sql| SELECT 1 FROM pg_event_trigger WHERE evtname = ? |]
      [P.PersistText name] ::
      M [P.Single Int]
  return (not $ null res)

-- Tests ----------------------------------------------------------------------

spec :: DBSpec
spec = do
  -- 1. Bootstrap ---------------------------------------------------------------

  describe "ensureMetaSchema" $ do
    it "installs migration infrastructure on a fresh database" $ runM $ do
      ensureMetaSchema
      -- migrations table exists
      hasMigrations <- tableExists "_meta" "migrations"
      liftIO $ hasMigrations `shouldBe` True
      -- no rows yet (no sentinel)
      rows <-
        P.rawSql
          [sql| SELECT number FROM _meta.migrations|]
          [] ::
          M [P.Single Int]
      liftIO $ length rows `shouldBe` 0
      -- meta version updated to 1
      v <- getMetaSchemaVersion
      liftIO $ v `shouldBe` Just 1
      -- event triggers installed
      hasCreate <- eventTriggerExists "log_ddl_changes"
      hasDrop <- eventTriggerExists "log_drop_changes"
      liftIO $ (hasCreate, hasDrop) `shouldBe` (True, True)

    it "is idempotent" $ runM $ do
      ensureMetaSchema
      ensureMetaSchema
      -- still no rows
      rows <-
        P.rawSql
          [sql| SELECT number FROM _meta.migrations|]
          [] ::
          M [P.Single Int]
      liftIO $ length rows `shouldBe` 0
      v <- getMetaSchemaVersion
      liftIO $ v `shouldBe` Just 1

  -- 2. Schema version tracking -------------------------------------------------

  describe "currentSchemaVersion" $ do
    it "returns empty string after initial setup" $ runM $ do
      ensureMetaSchema
      sv <- currentSchemaVersion
      liftIO $ sv `shouldBe` ""

    it "reflects the latest registered migration" $ runM $ do
      ensureMetaSchema
      registerMigration "test" "" "1" "first"
      registerMigration "test" "1" "2" "second"
      sv <- currentSchemaVersion
      liftIO $ sv `shouldBe` "2"

  -- 3. registerMigration / add_migration ---------------------------------------

  describe "registerMigration" $ do
    it "rejects migration from wrong version" $ \pool -> do
      result <-
        try $
          runM
            ( do
                ensureMetaSchema
                registerMigration "test" "99" "100" "should fail"
            )
            pool
      case result of
        Left (_ :: SomeException) -> return ()
        Right _ ->
          expectationFailure
            "Expected exception on version mismatch, but migration succeeded"

    it "stores revision, description, and timestamp" $ runM $ do
      ensureMetaSchema
      registerMigration "abc123" "" "1" "add users table"
      rows <-
        P.rawSql
          [sql|
          SELECT revision, description, applied
          FROM _meta.migrations
          WHERE number > 0
          ORDER BY number DESC LIMIT 1
        |]
          [] ::
          M [(P.Single Text, P.Single Text, P.Single P.PersistValue)]
      case rows of
        [(P.Single rev, P.Single desc, P.Single (P.PersistUTCTime _))] -> liftIO $ do
          rev `shouldBe` "abc123"
          desc `shouldBe` "add users table"
        _ ->
          liftIO $
            expectationFailure $
              "Unexpected query result: " <> show rows

  -- 4. migrate (orchestrator) --------------------------------------------------

  describe "migrate" $ do
    let m1 = mkTableMigration "" "1" "create foo" "mig_foo"
        m2 = mkTableMigration "1" "2" "create bar" "mig_bar"

    it "runs all migrations on a fresh database" $ runM $ do
      migrate "rev" [m1, m2]
      sv <- currentSchemaVersion
      liftIO $ sv `shouldBe` "2"
      hasFoo <- tableExists "public" "mig_foo"
      hasBar <- tableExists "public" "mig_bar"
      liftIO $ (hasFoo, hasBar) `shouldBe` (True, True)

    it "runs only pending migrations" $ runM $ do
      migrate "rev" [m1]
      -- now at "1", run the full list again
      migrate "rev" [m1, m2]
      sv <- currentSchemaVersion
      liftIO $ sv `shouldBe` "2"
      hasBar <- tableExists "public" "mig_bar"
      liftIO $ hasBar `shouldBe` True

    it "is a no-op when fully migrated" $ runM $ do
      migrate "rev" [m1, m2]
      -- run again — should just log "nothing to do"
      migrate "rev" [m1, m2]
      sv <- currentSchemaVersion
      liftIO $ sv `shouldBe` "2"

    it "exits on unknown current version" $ \pool -> do
      result <-
        try $
          runM
            ( do
                ensureMetaSchema
                registerMigration "test" "" "unknown" "put us in a weird state"
                migrate "rev" [mkNoopMigration "" "1" "normal"]
            )
            pool
      case result of
        Left (_ :: SomeException) -> return ()
        Right _ ->
          expectationFailure
            "Expected exit on unknown schema version"

    it "exits on empty migration list" $ \pool -> do
      result <- try $ runM (migrate "rev" []) pool
      case result of
        Left (_ :: SomeException) -> return ()
        Right _ ->
          expectationFailure
            "Expected exit on empty migration list"

    it "exits on gap in migration chain" $ \pool -> do
      let m1' = mkNoopMigration "" "1" "first"
          m3 = mkNoopMigration "2" "3" "third, but second is missing"
      result <- try $ runM (migrate "rev" [m1', m3]) pool
      case result of
        Left (_ :: SomeException) -> return ()
        Right _ ->
          expectationFailure
            "Expected exit on non-contiguous migration chain"

  -- 5. Fixup migrations ---------------------------------------------------------

  describe "fixup migrations" $ do
    it "recovers from an orphaned schema version via fixup" $ \pool -> do
      -- Simulate deploying a broken migration: "" -> "1" -> "2"
      let m1 = mkNoopMigration "" "1" "first"
          m2 = mkNoopMigration "1" "2" "second (broken)"
      run pool $ migrate "rev" [m1, m2]
      sv <- run pool currentSchemaVersion
      liftIO $ sv `shouldBe` "2"

      -- Now fix migration 2 in the codebase: rename target to "2.1",
      -- add migration "2.1" -> "3", and a fixup from "2" -> "2.1"
      let m2fixed = mkNoopMigration "1" "2.1" "second (fixed)"
          m3 = mkNoopMigration "2.1" "3" "third"
          irrelevant = mkNoopMigration "1.bad" "1" "irrelevant"
          fixup = mkNoopMigration "2" "2.1" "fixup broken v2"
          info = mkInfoWithFixups [m1, m2fixed, m3] [irrelevant, fixup]
      run pool $ migrateChecked info defaultMigrateOptions
      sv' <- run pool currentSchemaVersion
      liftIO $ sv' `shouldBe` "3"

  -- 6. Managed vs unmanaged DDL ------------------------------------------------

  describe "unmanaged schema change detection" $ do
    it "DDL inside a migration is not recorded" $ \pool -> do
      let m = mkTableMigration "" "1" "managed create" "managed_tbl"
      run pool $ migrate "rev" [m]
      changes <- run pool $ getUnmanagedChanges SinceLastMigration
      liftIO $ changes `shouldBe` []

    it "DDL outside a migration IS recorded" $ \pool -> do
      run pool $ migrate "rev" [mkNoopMigration "" "1" "bootstrap"]
      run pool $ P.rawExecute "CREATE TABLE public.rogue (id int)" []
      changes <- run pool $ getUnmanagedChanges SinceLastMigration
      length changes `shouldSatisfy` (>= 1)
      let c = head changes
      uscCommandTag c `shouldBe` "CREATE TABLE"
      uscObjectIdentity c `shouldBe` Just "public.rogue"

    it "DROP outside a migration is recorded" $ \pool -> do
      run pool $ migrate "rev" [mkNoopMigration "" "1" "bootstrap"]
      run pool $ P.rawExecute "CREATE TABLE public.drop_me (id int)" []
      run pool $ P.rawExecute "DROP TABLE public.drop_me" []
      changes <- run pool $ getUnmanagedChanges SinceLastMigration
      let dropChanges = filter (Text.isPrefixOf "DROP" . uscCommandTag) changes
      length dropChanges `shouldSatisfy` (>= 1)

    it "SinceLastMigration scopes to current migration only" $ \pool -> do
      let m1 = mkNoopMigration "" "1" "first"
          m2 = mkNoopMigration "1" "2" "second"
      run pool $ migrate "rev" [m1]
      run pool $ P.rawExecute "CREATE TABLE public.before_m2 (id int)" []
      run pool $ migrate "rev" [m1, m2]
      run pool $ P.rawExecute "CREATE TABLE public.after_m2 (id int)" []
      recent <- run pool $ getUnmanagedChanges SinceLastMigration
      allC <- run pool $ getUnmanagedChanges AllChanges
      liftIO $ do
        -- only the post-m2 change
        length recent `shouldBe` 1
        uscObjectIdentity (head recent) `shouldBe` Just "public.after_m2"
        -- both changes in full history
        length allC `shouldBe` 2

    it "ignores DDL in _meta schema" $ \pool -> do
      run pool $ migrate "rev" [mkNoopMigration "" "1" "bootstrap"]
      run pool $ P.rawExecute "CREATE TABLE _meta.should_be_ignored (id int)" []
      changes <- run pool $ getUnmanagedChanges SinceLastMigration
      let metaChanges = filter (\c -> uscDbSchema c == Just "_meta") changes
      liftIO $ metaChanges `shouldBe` []

-- | Build a MigrateInfo with no fixups for testing the main chain
mkInfo :: [Migration] -> MigrateInfo
mkInfo ms = MigrateInfo
  { miRevision = "test"
  , miMigrations = ms
  , miFixups = []
  , miPersistentMigration = Nothing
  , miIgnorePersistentMigrations = []
  }

-- | Build a MigrateInfo with fixups
mkInfoWithFixups :: [Migration] -> [Migration] -> MigrateInfo
mkInfoWithFixups ms fixups = (mkInfo ms) { miFixups = fixups }

consistencySpec :: Spec
consistencySpec = describe "checkMigrationConsistency" $ do
    it "accepts a valid chain" $ do
      let ms = [ mkNoopMigration "" "1" "first"
               , mkNoopMigration "1" "2" "second"
               , mkNoopMigration "2" "3" "third"
               ]
      checkMigrationConsistency (mkInfo ms) `shouldBe` []

    it "rejects an empty list" $ do
      checkMigrationConsistency (mkInfo []) `shouldBe` ["Empty migration list"]

    it "detects a chain break" $ do
      let ms = [ mkNoopMigration "" "1" "first"
               , mkNoopMigration "WRONG" "2" "second"
               ]
      let errs = checkMigrationConsistency (mkInfo ms)
      length errs `shouldBe` 1
      head errs `shouldSatisfy` Text.isInfixOf "Chain break"

    it "detects duplicate target versions" $ do
      let ms = [ mkNoopMigration "" "1" "first"
               , mkNoopMigration "1" "1" "also targets 1"
               ]
      let errs = checkMigrationConsistency (mkInfo ms)
      errs `shouldSatisfy` any (Text.isInfixOf "Duplicate target")

    it "detects empty target version" $ do
      let ms = [ mkNoopMigration "" "" "going nowhere" ]
      let errs = checkMigrationConsistency (mkInfo ms)
      errs `shouldSatisfy` any (Text.isInfixOf "empty target")

    it "detects empty description" $ do
      let ms = [ mkNoopMigration "" "1" "" ]
      let errs = checkMigrationConsistency (mkInfo ms)
      errs `shouldSatisfy` any (Text.isInfixOf "empty description")

    it "reports multiple problems at once" $ do
      let ms = [ mkNoopMigration "" "1" ""          -- empty description
               , mkNoopMigration "WRONG" "1" "dup"  -- chain break + duplicate target
               ]
      let errs = checkMigrationConsistency (mkInfo ms)
      length errs `shouldSatisfy` (>= 3)

    -- Fixup consistency checks
    it "accepts a valid fixup" $ do
      let ms = [ mkNoopMigration "" "1" "first"
               , mkNoopMigration "1" "2-fixed" "second (fixed)"
               ]
          fixups = [ mkNoopMigration "2" "2-fixed" "fixup broken v2" ]
      checkMigrationConsistency (mkInfoWithFixups ms fixups) `shouldBe` []

    it "rejects fixup whose expect appears in main chain" $ do
      let ms = [ mkNoopMigration "" "1" "first"
               , mkNoopMigration "1" "2" "second"
               ]
          fixups = [ mkNoopMigration "1" "2" "fixup from on-chain version" ]
      let errs = checkMigrationConsistency (mkInfoWithFixups ms fixups)
      errs `shouldSatisfy` any (Text.isInfixOf "appears in main migration chain")

    it "rejects fixup whose target is not on main chain" $ do
      let ms = [ mkNoopMigration "" "1" "first"
               , mkNoopMigration "1" "2" "second"
               ]
          fixups = [ mkNoopMigration "bogus" "nowhere" "fixup to nowhere" ]
      let errs = checkMigrationConsistency (mkInfoWithFixups ms fixups)
      errs `shouldSatisfy` any (Text.isInfixOf "not a target in the main migration chain")

    it "rejects duplicate fixup expect versions" $ do
      let ms = [ mkNoopMigration "" "1" "first"
               , mkNoopMigration "1" "2-fixed" "second (fixed)"
               ]
          fixups = [ mkNoopMigration "2" "2-fixed" "fixup 1"
                   , mkNoopMigration "2" "2-fixed" "fixup 2"
                   ]
      let errs = checkMigrationConsistency (mkInfoWithFixups ms fixups)
      errs `shouldSatisfy` any (Text.isInfixOf "Duplicate fixup expect")
