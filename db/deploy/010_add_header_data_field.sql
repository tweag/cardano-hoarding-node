-- Deploy hoard:010_add_header_data_field to pg

BEGIN;

-- Drop existing rows since they lack header data
DELETE FROM hoard.headers;

ALTER TABLE hoard.headers
    ADD COLUMN header_era TEXT NOT NULL,
    ADD COLUMN header_data BYTEA NOT NULL;

COMMIT;
