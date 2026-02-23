-- Deploy hoard:008_create_peer_notes_table to pg

BEGIN;

CREATE TYPE hoard.note_type AS ENUM ('Adversarial');

CREATE TABLE hoard.peer_notes (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  peer_id UUID NOT NULL REFERENCES hoard.peers(id) ON DELETE CASCADE,
  note_type hoard.note_type NOT NULL,
  note TEXT NOT NULL,
  noted_at TIMESTAMPTZ NOT NULL
);

COMMIT;
