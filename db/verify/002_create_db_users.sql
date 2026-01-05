-- Verify hoard:002_create_db_users on pg

BEGIN;

-- Verify that both roles exist
SELECT 1/COUNT(*) FROM pg_roles WHERE rolname = 'hoard_reader';
SELECT 1/COUNT(*) FROM pg_roles WHERE rolname = 'hoard_writer';

-- Verify reader has SELECT privilege on schema_metadata table
SELECT 1/COUNT(*)
FROM information_schema.table_privileges
WHERE grantee = 'hoard_reader'
  AND table_schema = 'hoard'
  AND table_name = 'schema_metadata'
  AND privilege_type = 'SELECT';

-- Verify writer has full DML privileges on schema_metadata table
SELECT 1/COUNT(*)
FROM information_schema.table_privileges
WHERE grantee = 'hoard_writer'
  AND table_schema = 'hoard'
  AND table_name = 'schema_metadata'
  AND privilege_type IN ('SELECT', 'INSERT', 'UPDATE', 'DELETE')
GROUP BY grantee, table_schema, table_name
HAVING COUNT(*) = 4;

-- Verify schema usage privileges using has_schema_privilege
DO $$
BEGIN
  IF NOT has_schema_privilege('hoard_reader', 'hoard', 'USAGE') THEN
    RAISE EXCEPTION 'hoard_reader does not have USAGE privilege on hoard schema';
  END IF;

  IF NOT has_schema_privilege('hoard_writer', 'hoard', 'USAGE') THEN
    RAISE EXCEPTION 'hoard_writer does not have USAGE privilege on hoard schema';
  END IF;
END $$;

ROLLBACK;
