-- Verify hoard:012_add_unrequested_block_tag_kind on pg

BEGIN;

SELECT 1/COUNT(*)
WHERE ENUM_RANGE(NULL::hoard.block_tag_kind) = '{CorruptBlockIntegrity,UnrequestedBlock}';

ROLLBACK;
