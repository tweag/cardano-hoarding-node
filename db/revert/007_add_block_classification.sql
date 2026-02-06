BEGIN;

-- Drop indexes
DROP INDEX IF EXISTS hoard.idx_blocks_classification_slot;
DROP INDEX IF EXISTS hoard.idx_blocks_unclassified_slot;

-- Restore is_canonical and drop classification columns
ALTER TABLE hoard.blocks
    DROP COLUMN IF EXISTS classified_at,
    DROP COLUMN IF EXISTS classification,
    ADD COLUMN is_canonical BOOLEAN NOT NULL DEFAULT FALSE;

COMMIT;
