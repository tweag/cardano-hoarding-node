-- Deploy hoard:011_add_orphan_block_tag to pg

BEGIN;

ALTER TYPE hoard.block_tag_kind ADD VALUE 'OrphanBlock';

COMMIT;
