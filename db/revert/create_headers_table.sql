-- Revert hoard:create_headers_table from pg

BEGIN;

-- Drop the headers table and its indexes
DROP TABLE IF EXISTS hoard.headers CASCADE;

COMMIT;
