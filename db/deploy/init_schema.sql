-- Deploy hoard:init_schema to pg

BEGIN;

-- Create a schema for the hoard application
CREATE SCHEMA IF NOT EXISTS hoard;

-- Example: create a metadata table to track schema version
CREATE TABLE hoard.schema_metadata (
    id SERIAL PRIMARY KEY,
    version TEXT NOT NULL,
    applied_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    description TEXT
);

-- Insert initial version
INSERT INTO hoard.schema_metadata (version, description)
VALUES ('0.1.0', 'Initial schema');

COMMIT;
