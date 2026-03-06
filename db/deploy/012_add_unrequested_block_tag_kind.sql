-- Deploy hoard:012_add_unrequested_block_tag_kind to pg

BEGIN;

ALTER TYPE hoard.block_tag_kind
  ADD VALUE 'UnrequestedBlock';

COMMIT;
