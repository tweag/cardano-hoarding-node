-- Revert hoard:006_create_hoard_state_table from pg

BEGIN;

DROP TABLE hoard.hoard_state;
DROP TYPE hoard.unit;

COMMIT;
