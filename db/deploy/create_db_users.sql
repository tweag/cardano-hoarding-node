-- Deploy hoard:create_db_users to pg
-- requires: init_schema

BEGIN;

-- Create read-only user for DBRead effects (if not exists for test reusability)
DO $$
BEGIN
  IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = 'hoard_reader') THEN
    CREATE ROLE hoard_reader WITH LOGIN;
  END IF;
END
$$;

-- Create read-write user for DBWrite effects (if not exists for test reusability)
DO $$
BEGIN
  IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = 'hoard_writer') THEN
    CREATE ROLE hoard_writer WITH LOGIN;
  END IF;
END
$$;

-- Grant schema usage to both users
GRANT USAGE ON SCHEMA hoard TO hoard_reader;
GRANT USAGE ON SCHEMA hoard TO hoard_writer;

-- Grant SELECT on all existing tables to reader
GRANT SELECT ON ALL TABLES IN SCHEMA hoard TO hoard_reader;

-- Grant full DML permissions to writer
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA hoard TO hoard_writer;

-- Grant sequence permissions for SERIAL columns
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA hoard TO hoard_reader;
GRANT USAGE, SELECT, UPDATE ON ALL SEQUENCES IN SCHEMA hoard TO hoard_writer;

-- Set default privileges for future tables
-- (These apply to objects created by the postgres user)
ALTER DEFAULT PRIVILEGES IN SCHEMA hoard
  GRANT SELECT ON TABLES TO hoard_reader;

ALTER DEFAULT PRIVILEGES IN SCHEMA hoard
  GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO hoard_writer;

ALTER DEFAULT PRIVILEGES IN SCHEMA hoard
  GRANT USAGE, SELECT ON SEQUENCES TO hoard_reader;

ALTER DEFAULT PRIVILEGES IN SCHEMA hoard
  GRANT USAGE, SELECT, UPDATE ON SEQUENCES TO hoard_writer;

COMMIT;
