-- Revert hoard:003_create_peers_table from pg

BEGIN;

-- Drop the peers table and its indexes
DROP TABLE IF EXISTS hoard.peers CASCADE;

COMMIT;
