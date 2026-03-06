-- Revert hoard:012_add_unrequested_block_tag_kind from pg

-- It's not possible to `DROP VALUE` on an enum, so to revert this enum value
-- addition we have to recreate it.

BEGIN;

-- Leave the current enum in place, since it is used by `block_tags`.
ALTER TYPE hoard.block_tag_kind
  RENAME TO _block_tag_kind;

-- Create a new, reverted, version of the enum.
CREATE TYPE hoard.block_tag_kind
  AS ENUM ('CorruptBlockIntegrity');

-- Move the column using the enum to the side for now.
ALTER TABLE hoard.block_tags
  RENAME COLUMN tag TO _tag;

-- Drop rows with (what is now) invalid tags.
DELETE FROM hoard.block_tags
  WHERE _tag <> 'CorruptBlockIntegrity';

-- Create a new column using the new, corrected enum.
ALTER TABLE hoard.block_tags
  ADD tag hoard.block_tag_kind NOT NULL DEFAULT 'CorruptBlockIntegrity';

-- Drop the old enum column.
ALTER TABLE hoard.block_tags
  DROP COLUMN _tag;

-- Set the new enum column to NOT NULL, now that we're done messing with it.
ALTER TABLE hoard.block_tags
  ALTER COLUMN tag SET NOT NULL;

-- Drop the old enum entirely.
DROP TYPE hoard._block_tag_kind;

COMMIT;
