-- Verify hoard:008_create_peer_notes_table on pg

BEGIN;

SELECT
    id,
    peer_id,
    note_type,
    note,
    noted_at
FROM hoard.peer_notes
WHERE FALSE;

SELECT 1/COUNT(*) FROM information_schema.table_constraints
WHERE constraint_schema = 'hoard'
  AND table_name = 'peer_notes'
  AND constraint_type = 'FOREIGN KEY'
  AND constraint_name = 'peer_notes_peer_id_fkey';

ROLLBACK;
