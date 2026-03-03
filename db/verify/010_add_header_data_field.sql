-- Verify hoard:010_add_header_data_field on pg

BEGIN;

SELECT
  hash,
  slot_number,
  block_number,
  first_seen_at,
  header_era,
  header_data
FROM hoard.headers
WHERE FALSE;

ROLLBACK;
