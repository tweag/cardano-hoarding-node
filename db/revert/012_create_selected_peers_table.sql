-- Revert hoard:012_create_selected_peers_table from pg

BEGIN;

DROP TABLE hoard.selected_peers;

COMMIT;
