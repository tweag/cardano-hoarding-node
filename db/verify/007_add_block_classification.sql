BEGIN;

SELECT hash, slot_number, classification, classified_at
FROM hoard.blocks WHERE FALSE;

ROLLBACK;
