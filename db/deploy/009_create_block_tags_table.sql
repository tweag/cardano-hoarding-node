-- Deploy hoard:009_create_block_tags_table to pg

BEGIN;

CREATE TYPE hoard.block_tag_kind AS ENUM (
  'CorruptBlockIntegrity'
);

CREATE TABLE hoard.block_tags (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  -- Leave this as _not_ a foreign key to allow tagging blocks we haven't
  -- fetched.
  block_hash TEXT NOT NULL,
  tag hoard.block_tag_kind NOT NULL,
  tagged_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (block_hash, tag)
);

COMMIT;
