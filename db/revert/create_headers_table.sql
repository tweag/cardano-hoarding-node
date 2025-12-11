-- Revert hoard:create_headers_table from pg

BEGIN;

-- Drop the header_receipts table first (has foreign key to headers)
DROP TABLE IF EXISTS hoard.header_receipts CASCADE;

-- Drop the headers table and its indexes
DROP TABLE IF EXISTS hoard.headers CASCADE;

COMMIT;
