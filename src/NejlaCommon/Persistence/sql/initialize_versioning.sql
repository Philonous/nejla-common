-- Simple schema versioning. Register migrations with
-- SELECT _meta.add_migration(<number>, <description>);
-- Retrieve the current schema version with
-- SELECT _meta.schema_version();
-- Note that the schema version is NULL of no migrations are registered.

-- BEGIN;

-- Create migration table if it doesn't exist yet

CREATE TABLE IF NOT EXISTS _meta.migrations
( "number" serial PRIMARY KEY
, "version" text UNIQUE NOT NULL
, "applied" timestamp NOT NULL
, "description" text NOT NULL
, "revision" text NOT NULL
);

-- If the table did exist, we update it
-- Note that this does nothing to a new table

-- Backfill any NULLs (shouldn't exist, but defensive)
UPDATE _meta.migrations
  SET version = '' WHERE version IS NULL;

UPDATE _meta.migrations
  SET description = '' WHERE description IS NULL;

UPDATE _meta.migrations
  SET revision = '' WHERE revision IS NULL;

-- Now enforce constraints
ALTER TABLE _meta.migrations
  ALTER COLUMN version SET NOT NULL,
  ALTER COLUMN description SET NOT NULL,
  ALTER COLUMN revision SET NOT NULL;

COMMENT ON COLUMN _meta.migrations.applied IS 'Time when migration was applied';
COMMENT ON COLUMN _meta.migrations.description IS 'Short description of the changes';
COMMENT ON COLUMN _meta.migrations.version
  IS 'Version of the schema after this patch was applied';

CREATE OR REPLACE FUNCTION _meta.add_migration( IN migration_from text
                                              , IN migration_to text
                                              , IN description text
                                              , IN revision text
                                              ) RETURNS setof INT4 AS $$
DECLARE
  t_version text;
BEGIN
  LOCK TABLE _meta.migrations IN EXCLUSIVE MODE;

  SELECT COALESCE(_meta.schema_version(), '') INTO t_version;
  IF (t_version IS DISTINCT FROM migration_from) THEN
    RAISE EXCEPTION 'Can''t migrate from %, current version is %'
          , migration_from, t_version ;
  END IF;

  INSERT INTO _meta.migrations ("version", "description", "applied", "revision")
    VALUES (migration_to, description, clock_timestamp(), revision);
  RETURN;
END;
$$ language plpgsql;

CREATE OR REPLACE FUNCTION _meta.schema_version(OUT out_version text)
RETURNS text AS $$
BEGIN
  SELECT "version" INTO out_version FROM _meta.migrations
    ORDER BY number DESC
    LIMIT 1;
END; $$ language plpgsql;

-- Set up schema modification auditing
-- Automatically store information about schema changes that happen outside the migration logic

CREATE TABLE IF NOT EXISTS _meta.unmanaged_schema_changes
( id serial PRIMARY KEY
, migration_number integer NOT NULL
    REFERENCES _meta.migrations(number)
    ON DELETE CASCADE
, modified_at timestamp DEFAULT clock_timestamp()
, modified_by text DEFAULT current_user
, command_tag text NOT NULL
, object_identity text
, schema_name text
, query_text text
);

CREATE OR REPLACE FUNCTION _meta.log_schema_change()
RETURNS event_trigger AS $$
DECLARE
  r RECORD;
  v_number int;
  v_in_migration text;
  v_schema text;
  v_query text;
BEGIN
  BEGIN
    v_in_migration := current_setting('_meta.in_migration', true);
  EXCEPTION
    WHEN undefined_object THEN
      v_in_migration := 'false';
  END;

  IF v_in_migration = 'true' THEN
    RETURN;
  END IF;

  SELECT "number" INTO v_number FROM _meta.migrations
    ORDER BY number DESC
    LIMIT 1;

  IF v_number IS NULL THEN
    RETURN;
  END IF;

  v_query := current_query();

  FOR r IN SELECT * FROM pg_event_trigger_ddl_commands()
  LOOP
    v_schema := r.schema_name;

    IF v_schema IN ('pg_catalog', 'information_schema', '_meta') THEN
      CONTINUE;
    END IF;

    INSERT INTO _meta.unmanaged_schema_changes
      (migration_number, command_tag, object_identity, schema_name, query_text)
    VALUES
      (v_number, r.command_tag, r.object_identity, v_schema, v_query);
  END LOOP;
END;
$$ LANGUAGE plpgsql;

CREATE EVENT TRIGGER log_ddl_changes
  ON ddl_command_end
  EXECUTE FUNCTION _meta.log_schema_change();

CREATE OR REPLACE FUNCTION _meta.log_schema_drop()
RETURNS event_trigger AS $$
DECLARE
  r RECORD;
  v_number int;
  v_in_migration text;
  v_query text;
BEGIN
  BEGIN
    v_in_migration := current_setting('_meta.in_migration', true);
  EXCEPTION
    WHEN undefined_object THEN
      v_in_migration := 'false';
  END;

  IF v_in_migration = 'true' THEN
    RETURN;
  END IF;

  SELECT "number" INTO v_number FROM _meta.migrations
    ORDER BY number DESC
    LIMIT 1;

  IF v_number IS NULL THEN
    RETURN;
  END IF;

  v_query := current_query();

  FOR r IN SELECT * FROM pg_event_trigger_dropped_objects()
  LOOP
    IF r.schema_name IN ('pg_catalog', 'information_schema', 'pg_toast') THEN
      CONTINUE;
    END IF;

    INSERT INTO _meta.unmanaged_schema_changes
      (migration_number, command_tag, object_identity, schema_name, query_text)
    VALUES
      (v_number, 'DROP ' || r.object_type, r.object_identity, r.schema_name, v_query);
  END LOOP;
END;
$$ LANGUAGE plpgsql;

CREATE EVENT TRIGGER log_drop_changes
  ON sql_drop
  EXECUTE FUNCTION _meta.log_schema_drop();
