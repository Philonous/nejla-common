{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Command-line interface helpers for database migrations.
module NejlaCommon.Persistence.Migration.CLI
  ( Command (..),
    migrateOptionsParser,
    initOptionsParser,
    commandParser,
    runCommand,
  )
where

import Data.Maybe (fromMaybe)
import NejlaCommon.Persistence.Migration
import Options.Applicative

-- | CLI commands
data Command
  = -- | Check schema health without making changes
    Check CheckSchemaOptions
  | -- | Run pending migrations with options
    Migrate MigrateOptions
  | -- | Initialize schema versioning on existing database
    Init InitOptions
  deriving (Show, Eq)

dryRunParser :: Parser Bool
dryRunParser =
  switch
    ( long "dry-run"
        <> help "Abort after successful run"
    )

migrateOptionsParser :: Parser MigrateOptions
migrateOptionsParser = do
  allowUnmanagedChanges <-
    switch
      ( long "allow-unmanaged"
          <> help "Proceed despite unmanaged schema changes"
      )
  allowSchemaDrift <-
    switch
      ( long "allow-drift"
          <> help "Proceed despite Persistent schema drift"
      )
  dryRun <- dryRunParser
  pure
    MigrateOptions
      { allowUnmanagedChanges,
        allowSchemaDrift,
        dryRun
      }

checkOptionsParser :: Parser CheckSchemaOptions
checkOptionsParser = do
  checkAllowUnmanagedChanges <-
    switch
      ( long "allow-unmanaged"
          <> help "Ignore unmanaged schema changes"
      )
  checkAllowSchemaDrift <-
    switch
      ( long "allow-drift"
          <> help "Ignore Persistent schema drift"
      )
  pure
    CheckSchemaOptions
      { checkAllowUnmanagedChanges,
        checkAllowSchemaDrift
      }

-- | Parser for the @init@ command
initOptionsParser :: Parser InitOptions
initOptionsParser = do
  initAssumeAt <-
    fromMaybe ""
      <$> optional
        ( strOption
            ( long "assume-at"
                <> metavar "VERSION"
                <> help "Assume database is in this schema version"
            )
        )
  initAllowSchemaDrift <-
    switch
      ( long "allow-drift"
          <> help "Proceed despite Persistent schema drift"
      )
  initDryRun <- dryRunParser

  pure
    InitOptions {initAssumeAt, initAllowSchemaDrift, initDryRun}

-- | Parser for all commands
commandParser :: Parser Command
commandParser =
  hsubparser
    ( command
        "check"
        ( info
            (Check <$> checkOptionsParser <**> helper)
            (progDesc "Check schema health without making changes")
        )
        <> command
          "migrate"
          ( info
              (Migrate <$> migrateOptionsParser <**> helper)
              (progDesc "Run pending migrations (with checks)")
          )
        <> command
          "init"
          ( info
              (Init <$> initOptionsParser <**> helper)
              (progDesc "Initialize schema versioning on existing database")
          )
    )

-- | Run a command with the given configuration
runCommand :: MigrateInfo -> Command -> M ()
runCommand info cmd = case cmd of
  Check options -> checkSchema info options
  Migrate options -> migrateChecked info options
  Init options -> initializeSchema info options
