-- Revert hoard:010_add_header_data_field from pg

BEGIN;

ALTER TABLE hoard.headers
    DROP COLUMN IF EXISTS header_data,
    DROP COLUMN IF EXISTS header_era;

COMMIT;
