BEGIN;

-- Add classification columns for orphan block detection
ALTER TABLE hoard.blocks
    ADD COLUMN classification TEXT,
    ADD COLUMN classified_at TIMESTAMPTZ,
    DROP COLUMN is_canonical;

-- Create composite index for efficient querying
-- Supports both classification-only and classification+slot queries
CREATE INDEX idx_blocks_classification_slot ON hoard.blocks(classification, slot_number);

-- Create partial index for unclassified blocks aging queries
-- More efficient than composite index for NULL classification lookups
CREATE INDEX idx_blocks_unclassified_slot ON hoard.blocks(slot_number)
WHERE classification IS NULL;

COMMIT;
