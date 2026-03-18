-- Deploy hoard:012_create_selected_peers_table to pg

BEGIN;

CREATE TABLE hoard.selected_peers (
    peer_id UUID PRIMARY KEY REFERENCES hoard.peers(id) ON DELETE CASCADE,
    note TEXT,
    added_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

COMMIT;
