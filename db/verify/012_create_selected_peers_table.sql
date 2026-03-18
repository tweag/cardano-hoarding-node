-- Verify hoard:012_create_selected_peers_table on pg

BEGIN;

SELECT
    peer_id,
    note,
    added_at
FROM hoard.selected_peers
WHERE FALSE;

SELECT 1/COUNT(*) FROM information_schema.table_constraints
WHERE constraint_schema = 'hoard'
  AND table_name = 'selected_peers'
  AND constraint_type = 'PRIMARY KEY'
  AND constraint_name = 'selected_peers_pkey';

ROLLBACK;
