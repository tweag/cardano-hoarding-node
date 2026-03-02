-- Revert hoard:010_create_header_tags_table from pg

BEGIN;

DROP TABLE hoard.header_tags;
DROP TYPE hoard.header_tag_kind;

COMMIT;
