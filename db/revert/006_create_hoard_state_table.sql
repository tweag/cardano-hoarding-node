-- Revert hoard:006_create_hoard_state_table from pg

BEGIN;

DROP TABLE IF EXISTS hoard.hoard_state CASCADE;

COMMIT;
