-- Deploy hoard:006_create_hoard_state_table to pg

BEGIN;

CREATE TYPE hoard.unit AS ENUM ('Unit');
CREATE TABLE hoard.hoard_state (
    unit hoard.unit PRIMARY KEY DEFAULT 'Unit',
    immutable_tip JSONB
);


COMMIT;
