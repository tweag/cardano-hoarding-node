-- Verify hoard:create_headers_table on pg

BEGIN;

-- Verify that the headers table exists and has the correct columns
SELECT
    id,
    received_at,
    received_from_peer_id
FROM hoard.headers
WHERE FALSE;

-- Verify that foreign key constraint exists
SELECT 1/COUNT(*) FROM information_schema.table_constraints
WHERE constraint_schema = 'hoard'
  AND table_name = 'headers'
  AND constraint_type = 'FOREIGN KEY';

-- Verify that indexes exist
SELECT 1/COUNT(*) FROM pg_indexes
WHERE schemaname = 'hoard'
  AND tablename = 'headers'
  AND indexname = 'idx_headers_received_at';

SELECT 1/COUNT(*) FROM pg_indexes
WHERE schemaname = 'hoard'
  AND tablename = 'headers'
  AND indexname = 'idx_headers_received_from_peer_id';

ROLLBACK;
