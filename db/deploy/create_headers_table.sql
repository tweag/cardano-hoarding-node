-- Deploy hoard:create_headers_table to pg

BEGIN;

-- Create the headers table for storing block headers from the chain sync protocol
CREATE TABLE hoard.headers (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    received_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    received_from_peer_id UUID NOT NULL REFERENCES hoard.peers(id) ON DELETE CASCADE
);

-- Add index for looking up headers by received time (for time-based queries)
CREATE INDEX idx_headers_received_at ON hoard.headers(received_at);

-- Add index for looking up headers by source peer
CREATE INDEX idx_headers_received_from_peer_id ON hoard.headers(received_from_peer_id);

COMMIT;
