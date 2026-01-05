-- Revert hoard:004_create_blocks_table from pg

BEGIN;

-- Drop the blocks table and its indexes
DROP TABLE IF EXISTS hoard.blocks CASCADE;

COMMIT;
