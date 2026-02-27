-- Revert hoard:009_create_block_tags_table from pg

BEGIN;

DROP TABLE hoard.block_tags;
DROP TYPE hoard.block_tag_kind;

COMMIT;
