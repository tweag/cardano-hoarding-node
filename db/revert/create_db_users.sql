-- Revert hoard:create_db_users from pg

BEGIN;

-- Revoke all privileges
REVOKE ALL PRIVILEGES ON ALL TABLES IN SCHEMA hoard FROM hoard_reader;
REVOKE ALL PRIVILEGES ON ALL TABLES IN SCHEMA hoard FROM hoard_writer;
REVOKE ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA hoard FROM hoard_reader;
REVOKE ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA hoard FROM hoard_writer;
REVOKE USAGE ON SCHEMA hoard FROM hoard_reader;
REVOKE USAGE ON SCHEMA hoard FROM hoard_writer;

-- Reset default privileges
ALTER DEFAULT PRIVILEGES IN SCHEMA hoard
  REVOKE SELECT ON TABLES FROM hoard_reader;

ALTER DEFAULT PRIVILEGES IN SCHEMA hoard
  REVOKE SELECT, INSERT, UPDATE, DELETE ON TABLES FROM hoard_writer;

ALTER DEFAULT PRIVILEGES IN SCHEMA hoard
  REVOKE USAGE, SELECT ON SEQUENCES FROM hoard_reader;

ALTER DEFAULT PRIVILEGES IN SCHEMA hoard
  REVOKE USAGE, SELECT, UPDATE ON SEQUENCES FROM hoard_writer;

-- Reassign any owned objects to postgres before dropping
REASSIGN OWNED BY hoard_reader TO postgres;
REASSIGN OWNED BY hoard_writer TO postgres;

-- Drop privileges (DROP OWNED removes any remaining privileges)
DROP OWNED BY hoard_reader;
DROP OWNED BY hoard_writer;

-- Drop the roles
DROP ROLE hoard_reader;
DROP ROLE hoard_writer;

COMMIT;
