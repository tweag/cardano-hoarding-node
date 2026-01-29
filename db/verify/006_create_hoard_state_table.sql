-- Verify hoard:006_create_hoard_state_table on pg

BEGIN;

-- Verify that the hoard_state table exists and has the correct columns
SELECT
    unit,
    immutable_tip
FROM hoard.hoard_state
WHERE FALSE;

ROLLBACK;
