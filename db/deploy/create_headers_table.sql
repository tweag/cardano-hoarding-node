-- Deploy hoard:create_headers_table to pg
-- requires: create_peers_table

BEGIN;

-- Create the headers table for storing unique block headers
CREATE TABLE hoard.headers (
    header_hash TEXT PRIMARY KEY,
    block_hash TEXT NOT NULL,
    slot_number BIGINT NOT NULL,
    block_number BIGINT NOT NULL,
    first_seen_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Create the header_receipts table for tracking which peer sent which header
CREATE TABLE hoard.header_receipts (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    header_hash TEXT NOT NULL REFERENCES hoard.headers(header_hash) ON DELETE CASCADE,
    peer_id UUID NOT NULL REFERENCES hoard.peers(id) ON DELETE CASCADE,
    received_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    UNIQUE(header_hash, peer_id)  -- Can't receive same header twice from same peer
);

-- Indexes for headers table
CREATE INDEX idx_headers_block_hash ON hoard.headers(block_hash);
CREATE INDEX idx_headers_slot_number ON hoard.headers(slot_number);
CREATE INDEX idx_headers_block_number ON hoard.headers(block_number);
CREATE INDEX idx_headers_first_seen_at ON hoard.headers(first_seen_at);

-- Indexes for header_receipts table
CREATE INDEX idx_header_receipts_peer_id ON hoard.header_receipts(peer_id);
CREATE INDEX idx_header_receipts_received_at ON hoard.header_receipts(received_at);
CREATE INDEX idx_header_receipts_header_hash ON hoard.header_receipts(header_hash);

COMMIT;
