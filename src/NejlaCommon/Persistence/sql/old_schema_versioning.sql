-- Simple schema versioning. Register migrations with
-- SELECT _meta.add_migration(<number>, <description>);
-- Retrieve the current schema version with
-- SELECT _meta.schema_version();
-- Note that the schema version is NULL of no migrations are registered.

-- BEGIN;

CREATE SCHEMA IF NOT EXISTS _meta;
COMMENT ON SCHEMA _meta IS 'Schema for database metainformation';

CREATE TABLE IF NOT EXISTS _meta.migrations
( "number" serial PRIMARY KEY
, "version" text UNIQUE
, "applied" timestamp
, "description" text
, "revision" text
);

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

  SELECT _meta.schema_version() INTO t_version;
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
