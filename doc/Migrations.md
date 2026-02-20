# Database Migrations

nejla-common provides a migration system for PostgreSQL that tracks schema
versions, runs migrations in order, detects out-of-band schema changes, and
optionally checks for drift against Persistent entity definitions.

## Quick Start

```haskell
import NejlaCommon.Persistence.Migration

migrations :: [Migration]
migrations =
  [ Migration
      { expect = ""       -- the first migration always expects ""
      , to = "1"
      , description = "Create users table"
      , script = rawExecute
          [sql|
            CREATE TABLE users (
              id serial PRIMARY KEY,
              email text NOT NULL UNIQUE,
              name text NOT NULL
            );
          |] []
      }
  , Migration
      { expect = "1"
      , to = "2"
      , description = "Add created_at column to users"
      , script = rawExecute
          [sql|
            ALTER TABLE users
              ADD COLUMN created_at timestamptz NOT NULL DEFAULT now();
          |] []
      }
  ]
```

Migrations form a linear chain: each migration's `to` must match the next
migration's `expect`. The first migration always expects `""` (the empty
string), which represents a fresh database with no migrations applied. The
migration system looks up the current schema version (stored in a special table
in the _meta schema), finds the the matching starting point in the migration
list and then runs all following migrations in order.

## Writing Migrations

A `Migration` has four fields:

- **`expect`** -- the schema version the database must be at before this
  migration runs.
- **`to`** -- the schema version after this migration runs. Can be any text,
  but simple incrementing numbers like `"1"`, `"2"` work well.
- **`description`** -- a short human-readable summary.
- **`script`** -- an `M ()` (IO + database access) block with the code to run to
  migrate the database. You can for example use `rawExecute` with the `sql`
  quasi-quoter for inline SQL, or `sqlFile` to embed an external `.sql`
  file. But you can in principle run any IO code here. It is for example
  possible to load files from the file system into the database as a migration.

### Inline SQL with the `sql` quasi-quoter

The `sql` quasi-quoter strips common leading indentation, so you can write
nicely indented SQL inside Haskell code:

```haskell
script = rawExecute [sql|
    CREATE TABLE foo (
      id serial PRIMARY KEY,
      bar text NOT NULL
    );
  |] []
```

### External SQL files with `sqlFile`

For larger migrations you can put the SQL in a separate file and loaded with
`sqlFile`. The SQL text will be embedded in the compiled executable and doesn't
need to be shipped as an extra file. You may want to use this method even for
smaller migrations for consistency so that every migration lives in its own
file, but that's up to you.

```haskell
script = rawExecute $(sqlFile "migrations/003_add_bar.sql") []
```

### Parameterized queries

`rawExecute` takes a list of `PersistValue` parameters (re-exported from
Persistent). Most migrations don't need parameters, so you can usually pass `[]`.

## Deriving Migrations from Persistent Entities

The migration system does not automatically migrate your database from entity
definition like persistent does. Instead, you provide each migration. However,
you can use Persistent's `mkMigrate` to generate a `Migration` value for *drift
detection*. This lets the system warn you when the actual database schema
doesn't match your entity definitions (e.g. you added a field to a table but
forgot to write a migration).

```haskell
-- In your models module:
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User
  email Text
  name Text
  deriving Eq Show
|]
```

Then pass the generated `migrateAll` via the `miPersistentMigration` field (see
below). The migration system will compare the expected schema against the actual
database after running migrations and report any differences as SQL.

**Tip**: You can use the reported difference as a starting point for your
migration. The workflow would be: update the schema definition, run the
migration on a test database, note the reported difference and copy it into your
migration file.

Persistent's migration generation can be noisy, e.g. it can repeatedly try to
remove "defaults" from your database; alternatively sometimes you are OK with certain
difference and don't want to be warned about them. You can add those lines to
`miIgnorePersistentMigrations` to suppress them in warnings.

## Putting It Together: `MigrateInfo`

Bundle everything into a `MigrateInfo`:

```haskell
import Development.GitRev (gitHash)

migrateInfo :: MigrateInfo
migrateInfo = MigrateInfo
  { miRevision = $(gitHash)            -- or any text identifying the build
  , miMigrations = migrations
  , miFixups = []                      -- see "Fixup Migrations" below
  , miPersistentMigration = Just migrateAll  -- optional drift detection see above
  , miIgnorePersistentMigrations = []  -- known acceptable drift to suppress
  }
```

`miRevision` allows you to specify the "provenance" of your migrations - say
your database reports a migration from `3` to `4` - but which version of this
migration was it? Perhaps the migration in the code base has changed over time
during development. Adding the auto-inserted git revision helps you determine
this.


## Integrating into a Web Server

You can use `withDBPool` from `NejlaCommon.Persistence` to create a connection
pool and run migrations on startup:

```haskell
import NejlaCommon.Persistence (withDBPool, getDBConnectInfo)
import NejlaCommon.Persistence.Migration (runM, migrateChecked, defaultMigrateOptions)

main :: IO ()
main = do
  connInfo <- getDBConnectInfo
  withDBPool connInfo 10 (migrateChecked migrateInfo defaultMigrateOptions) $ \pool -> do
    -- pool is ready, start your server
    runServer pool
```

`withDBPool` will:
1. Create a PostgreSQL connection pool.
2. Run the migration action (with retry logic for transient connection errors or
   when the database is still starting up).
3. Pass the ready pool to your application callback.

On the first run, the migration system automatically creates a `_meta` schema
in the database to track migration history.

## CLI Integration

The `NejlaCommon.Persistence.Migration.CLI` module provides an
`optparse-applicative` parser for three subcommands. This is useful when you
want your application to support migration commands directly:

```haskell
import NejlaCommon.Persistence.Migration.CLI (commandParser, runCommand)
import Options.Applicative

main :: IO ()
main = do
  cmd <- execParser (info (commandParser <**> helper) fullDesc)
  connInfo <- getDBConnectInfo
  withDBPool connInfo 1 (runCommand migrateInfo cmd) $ \_ -> return ()
```

### Commands

**`migrate`** -- Run pending migrations.

```
myapp migrate [--allow-unmanaged] [--allow-drift] [--dry-run]
```

- `--allow-unmanaged` -- Proceed even if someone has made schema changes
  outside the migration system.
- `--allow-drift` -- Proceed even if the schema doesn't match Persistent entity
  definitions.
- `--dry-run` -- Run all checks but don't actually apply migrations.

**`check`** -- Read-only health check. Reports pending migrations, unmanaged
changes, and schema drift. Exits non-zero if problems are found.

```
myapp check [--allow-unmanaged] [--allow-drift]
```

**`init`** -- Adopt the migration system on an existing database that already
has a schema but wasn't using migrations.

```
myapp init --assume-at VERSION [--allow-drift] [--dry-run]
```

- `--assume-at VERSION` -- Tell the system what schema version the existing
  database corresponds to. This must match an `expect` value in your migration
  list. Any migrations after that version will then be applied.

## Fixup Migrations

Sometimes a migration ships with a bug. The fixup mechanism handles this
without requiring manual database intervention on every deployment.

Say migration `"5"` was broken and deployed to production.

1. Fix the migration in the codebase and rename its target -- e.g. change `to =
   "5"` to `to = "5-fixed"`. Renaming avoids confusion whether `5` is the
   broken, deployed schema or the fixed one.
2. Fresh deployments now get the correct schema.
3. Existing production databases are stuck at version `"5"`, which no longer
   exists in the main chain.
4. Write a fixup migration from `"5"` to `"5-fixed"` (or directly to `6`) that
   corrects the broken state.

```
Main chain:  "" -- 1 -- 2 -- 3 -- 4 -- 5-fixed -- 6
                                          ^
Fixup:                                5 --+
```

```haskell
migrateInfo = MigrateInfo
  { ...
  , miFixups =
      [ Migration
          { expect = "5"
          , to = "5-fixed"
          , description = "Fix missing NOT NULL on users.email"
          , script = rawExecute
              [sql| ALTER TABLE users ALTER COLUMN email SET NOT NULL; |] []
          }
      ]
  }
```

At most one fixup runs per migration cycle, and it always runs first. After
the fixup, normal chain migrations continue from the fixup's target version.

## Unmanaged Change Detection

The migration system installs PostgreSQL event triggers that log any DDL
executed outside of a migration (e.g. someone running `ALTER TABLE` in psql).
These are recorded in `_meta.unmanaged_schema_changes`.

By default, `migrate` and `check` will fail if unmanaged changes are detected
since the last migration. Use `--allow-unmanaged` to proceed anyway.

You can also query unmanaged changes programmatically:

```haskell
changes <- getUnmanagedChanges SinceLastMigration  -- or AllChanges
```

## How It Works Under the Hood

The migration system creates a `_meta` schema in your database containing:

- **`_meta.migrations`** -- a log of every migration that has been applied,
  with timestamps and the git revision of the build that applied it.
- **`_meta.schema_version()`** -- a function returning the current schema
  version.
- **`_meta.add_migration()`** -- a function that atomically checks the expected
  version and registers a new migration (raises an exception on mismatch).
- **`_meta.unmanaged_schema_changes`** -- audit log of DDL performed outside
  migrations.
- **Event triggers** that detect out-of-band DDL. During a migration, a session
  variable `_meta.in_migration` is set so the triggers know not to log
  migration-internal DDL.

The meta schema itself is versioned separately (in
`_meta._meta_schema_version`) so the migration infrastructure can be upgraded
independently of your application schema.
