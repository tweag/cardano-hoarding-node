-- Verify hoard:010_create_header_tags_table on pg

BEGIN;

SELECT
    id,
    hash,
    tag,
    tagged_at
FROM hoard.header_tags
WHERE FALSE;

SELECT 1/COUNT(*) FROM information_schema.table_constraints
WHERE constraint_schema = 'hoard'
  AND table_name = 'header_tags'
  AND constraint_type = 'UNIQUE'
  AND constraint_name = 'header_tags_hash_tag_key';

SELECT 1/COUNT(*)
WHERE ENUM_RANGE(NULL::hoard.header_tag_kind) = '{CorruptHeaderIntegrity}';

ROLLBACK;
