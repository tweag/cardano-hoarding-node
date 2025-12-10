-- Verify hoard:create_headers_table on pg

BEGIN;

-- Verify that the headers table exists and has the correct columns
SELECT
    header_hash,
    block_hash,
    slot_number,
    block_number,
    first_seen_at
FROM hoard.headers
WHERE FALSE;

-- Verify that the header_receipts table exists and has the correct columns
SELECT
    id,
    header_hash,
    peer_id,
    received_at
FROM hoard.header_receipts
WHERE FALSE;

-- Verify that foreign key constraints exist
SELECT 1/COUNT(*) FROM information_schema.table_constraints
WHERE constraint_schema = 'hoard'
  AND table_name = 'header_receipts'
  AND constraint_type = 'FOREIGN KEY';

-- Verify that unique constraint exists on (header_hash, peer_id)
SELECT 1/COUNT(*) FROM information_schema.table_constraints
WHERE constraint_schema = 'hoard'
  AND table_name = 'header_receipts'
  AND constraint_type = 'UNIQUE';

-- Verify that indexes exist on headers table
SELECT 1/COUNT(*) FROM pg_indexes
WHERE schemaname = 'hoard'
  AND tablename = 'headers'
  AND indexname = 'idx_headers_slot_number';

SELECT 1/COUNT(*) FROM pg_indexes
WHERE schemaname = 'hoard'
  AND tablename = 'headers'
  AND indexname = 'idx_headers_block_number';

SELECT 1/COUNT(*) FROM pg_indexes
WHERE schemaname = 'hoard'
  AND tablename = 'headers'
  AND indexname = 'idx_headers_block_hash';

-- Verify that indexes exist on header_receipts table
SELECT 1/COUNT(*) FROM pg_indexes
WHERE schemaname = 'hoard'
  AND tablename = 'header_receipts'
  AND indexname = 'idx_header_receipts_peer_id';

SELECT 1/COUNT(*) FROM pg_indexes
WHERE schemaname = 'hoard'
  AND tablename = 'header_receipts'
  AND indexname = 'idx_header_receipts_header_hash';

ROLLBACK;
