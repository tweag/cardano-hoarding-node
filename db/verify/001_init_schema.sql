-- Verify hoard:001_init_schema on pg

BEGIN;

-- Verify the hoard schema exists
SELECT pg_catalog.has_schema_privilege('hoard', 'usage');

-- Verify the metadata table exists and has expected columns
SELECT id, version, applied_at, description
FROM hoard.schema_metadata
WHERE FALSE;

ROLLBACK;
