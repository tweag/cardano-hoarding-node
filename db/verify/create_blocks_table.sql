-- Verify hoard:create_blocks_table on pg

BEGIN;

-- Verify that the blocks table exists and has the correct columns
SELECT
    hash,
    slot_number,
    pool_id,
    block_era,
    block_data,
    validation_status,
    validation_reason,
    is_canonical,
    first_seen
FROM hoard.blocks
WHERE FALSE;

ROLLBACK;
