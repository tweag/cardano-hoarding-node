-- Deploy hoard:010_create_header_tags_table to pg

BEGIN;

CREATE TYPE hoard.header_tag_kind AS ENUM (
  'CorruptHeaderIntegrity'
);

CREATE TABLE hoard.header_tags (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  hash TEXT NOT NULL REFERENCES hoard.headers(hash) ON DELETE CASCADE,
  tag hoard.header_tag_kind NOT NULL,
  tagged_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (hash, tag)
);

COMMIT;
