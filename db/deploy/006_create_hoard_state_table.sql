-- Deploy hoard:006_create_hoard_state_table to pg

BEGIN;

CREATE TYPE hoard.unit AS ENUM ('');
CREATE TABLE hoard.hoard_state (
    unit unit PRIMARY KEY DEFAULT '',
    immutable_tip JSONB
);


COMMIT;
