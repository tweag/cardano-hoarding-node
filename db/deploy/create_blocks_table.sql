-- Deploy hoard:create_blocks_table to pg

BEGIN;

-- Create the blocks table for BlockFetch mini protocol
CREATE TABLE hoard.blocks (
    hash TEXT PRIMARY KEY,
    slot_number INT NOT NULL,
    pool_id TEXT NOT NULL,
    block_era TEXT NOT NULL,
    block_data BYTEA NOT NULL,
    validation_status TEXT NOT NULL,
    validation_reason TEXT NOT NULL,
    is_canonical BOOLEAN NOT NULL,
    first_seen TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

COMMIT;
