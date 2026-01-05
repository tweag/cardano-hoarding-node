-- Deploy hoard:003_create_peers_table to pg

BEGIN;

-- Create the peers table for P2P network discovery
CREATE TABLE hoard.peers (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    address TEXT NOT NULL,
    port INTEGER NOT NULL,
    first_discovered TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    last_seen TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    last_connected TIMESTAMPTZ,
    last_failure_time TIMESTAMPTZ,
    discovered_via TEXT NOT NULL,
    UNIQUE (address, port)
);

-- Add index for looking up peers by last_seen (for cleanup/maintenance)
CREATE INDEX idx_peers_last_seen ON hoard.peers(last_seen);

-- Add index for looking up peers by address and port combination (for queries)
CREATE INDEX idx_peers_address_port ON hoard.peers(address, port);

COMMIT;
