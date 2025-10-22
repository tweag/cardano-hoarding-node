-- Verify hoard:create_peers_table on pg

BEGIN;

-- Verify that the peers table exists and has the correct columns
SELECT
    peer_id,
    address,
    port,
    first_discovered,
    last_seen,
    discovered_via
FROM hoard.peers
WHERE FALSE;

-- Verify that indexes exist
SELECT 1/COUNT(*) FROM pg_indexes
WHERE schemaname = 'hoard'
  AND tablename = 'peers'
  AND indexname = 'idx_peers_last_seen';

SELECT 1/COUNT(*) FROM pg_indexes
WHERE schemaname = 'hoard'
  AND tablename = 'peers'
  AND indexname = 'idx_peers_address_port';

ROLLBACK;
