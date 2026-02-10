CREATE SCHEMA IF NOT EXISTS _meta;
COMMENT ON SCHEMA _meta IS 'Schema for database metainformation';

-- Meta versioning, which version of the schema table do we have
CREATE TABLE IF NOT EXISTS _meta._meta_schema_version
    ( version integer NOT NULL
    );

-- Ensure exactly one row
CREATE UNIQUE INDEX ON _meta._meta_schema_version ((true));

-- Set initial value to 0
INSERT INTO _meta._meta_schema_version VALUES (0)
  ON CONFLICT DO NOTHING;
