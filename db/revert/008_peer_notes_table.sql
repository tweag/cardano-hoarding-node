-- Revert hoard:008_create_peer_notes_table from pg

BEGIN;

DROP TABLE hoard.peer_notes;
DROP TYPE hoard.note_type;

COMMIT;
