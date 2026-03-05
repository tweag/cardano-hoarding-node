-- Revert hoard:011_add_orphan_block_tag from pg

BEGIN;

ALTER TYPE hoard.block_tag_kind RENAME TO block_tag_kind_old;

CREATE TYPE hoard.block_tag_kind AS ENUM (
  'CorruptBlockIntegrity'
);

ALTER TABLE hoard.block_tags
    ALTER COLUMN tag TYPE hoard.block_tag_kind
        USING tag::text::hoard.block_tag_kind;

DROP TYPE hoard.block_tag_kind_old;

COMMIT;
