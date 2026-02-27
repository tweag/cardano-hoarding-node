-- Verify hoard:009_create_block_tag_table on pg

BEGIN;

SELECT
    id,
    block_hash,
    tag,
    tagged_at
FROM hoard.block_tags
WHERE FALSE;

SELECT 1/COUNT(*) FROM information_schema.table_constraints
WHERE constraint_schema = 'hoard'
  AND table_name = 'block_tags'
  AND constraint_type = 'UNIQUE'
  AND constraint_name = 'block_tags_block_hash_tag_key';

SELECT 1/COUNT(*)
WHERE ENUM_RANGE(NULL::hoard.block_tag_kind) = '{CorruptBlockIntegrity}';

ROLLBACK;
