-- Revert hoard:001_init_schema from pg

BEGIN;

-- Drop the metadata table
DROP TABLE IF EXISTS hoard.schema_metadata;

-- Drop the schema (CASCADE will drop any remaining objects)
DROP SCHEMA IF EXISTS hoard CASCADE;

COMMIT;
