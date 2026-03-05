-- Verify hoard:011_add_orphan_block_tag on pg

BEGIN;

SELECT 1/COUNT(*)
WHERE ENUM_RANGE(NULL::hoard.block_tag_kind) = '{CorruptBlockIntegrity,OrphanBlock}';

ROLLBACK;
