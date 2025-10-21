# Database Migrations with Sqitch

This directory contains database schema migrations managed by [Sqitch](https://sqitch.org/), a database-agnostic change management system.

## Overview

Sqitch provides a robust way to manage database schema changes with:
- **deploy** scripts: Apply changes to the database
- **revert** scripts: Undo changes (rollback)
- **verify** scripts: Test that changes were applied correctly

## Project Configuration

- **Database Engine**: PostgreSQL
- **Dev Database**: `hoard_dev`
- **Target**: `postgres@/hoard_dev` (via Unix socket)
- **Configuration**: `../sqitch.conf`
- **Plan File**: `sqitch.plan`

## Prerequisites

Start the development PostgreSQL database:

```bash
nix run .#postgres
```

This sets up the local database that sqitch will connect to via the `PGHOST` environment variable.

## Common Workflows

### Checking Database Status

View the current deployment status:

```bash
sqitch status
```

### Creating a New Migration

Create a new migration with deploy, revert, and verify scripts:

```bash
sqitch add <migration_name> -n "Description of the change"
```

Example:
```bash
sqitch add add_blocks_table -n "Add blocks table for storing Cardano blocks"
```

This creates three files:
- `deploy/add_blocks_table.sql` - SQL to apply the change
- `revert/add_blocks_table.sql` - SQL to undo the change
- `verify/add_blocks_table.sql` - SQL to verify the change

### Editing Migration Scripts

After creating a migration, edit the generated SQL files:

1. **deploy/**: Add your CREATE TABLE, ALTER TABLE, etc.
2. **revert/**: Add the corresponding DROP TABLE, DROP COLUMN, etc.
3. **verify/**: Add SELECT statements to verify the schema (runs in a transaction that's rolled back)

### Deploying Migrations

Deploy all pending migrations:

```bash
sqitch deploy
```

Deploy to a specific migration:

```bash
sqitch deploy <migration_name>
```

### Reverting Migrations

Revert the last migration:

```bash
sqitch revert
```

Revert to a specific migration:

```bash
sqitch revert <migration_name>
```

Revert all migrations:

```bash
sqitch revert --to @ROOT
```

### Verifying the Database

Run verification scripts for all deployed migrations:

```bash
sqitch verify
```

### Viewing Migration History

Show the deployment log:

```bash
sqitch log
```

View the migration plan:

```bash
cat sqitch.plan
```

## Migration Best Practices

1. **Always provide revert scripts**: Every migration should be reversible
2. **Test both directions**: Test both deploy and revert before committing
3. **Keep migrations atomic**: Each migration should represent one logical change
4. **Use verify scripts**: Add meaningful verification checks
5. **Use transactions**: Wrap migrations in BEGIN/COMMIT (already in templates)
6. **Describe changes clearly**: Use descriptive migration names and notes

## Directory Structure

```
db/
├── deploy/          # SQL scripts to apply changes
├── revert/          # SQL scripts to undo changes
├── verify/          # SQL scripts to verify changes
└── sqitch.plan      # Migration plan and history
```

## Example Migration Files

### deploy/init_schema.sql
```sql
BEGIN;
CREATE SCHEMA IF NOT EXISTS hoard;
CREATE TABLE hoard.schema_metadata (
    id SERIAL PRIMARY KEY,
    version TEXT NOT NULL,
    applied_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    description TEXT
);
COMMIT;
```

### revert/init_schema.sql
```sql
BEGIN;
DROP TABLE IF EXISTS hoard.schema_metadata;
DROP SCHEMA IF EXISTS hoard CASCADE;
COMMIT;
```

### verify/init_schema.sql
```sql
BEGIN;
SELECT pg_catalog.has_schema_privilege('hoard', 'usage');
SELECT id, version, applied_at, description
FROM hoard.schema_metadata WHERE FALSE;
ROLLBACK;
```

## Targets

The project is configured with a `dev` target. To add additional targets (staging, production), edit `../sqitch.conf`:

```ini
[target "prod"]
    uri = db:pg://user@host/dbname
```

Then deploy to a specific target:

```bash
sqitch deploy prod
```

## Troubleshooting

### Connection Issues

If sqitch can't connect to the database:
1. Ensure the PostgreSQL dev server is running (`nix run .#postgres`)
2. Check that `PGHOST` environment variable is set correctly
3. Verify connection with: `psql -U postgres hoard_dev`

### Migration Conflicts

If you need to modify a migration that's already been deployed:
1. **Never** edit deployed migrations directly
2. Instead, create a new migration to make the additional changes
3. If in development and not yet pushed: revert, edit, redeploy

### Clean Slate

To start fresh (development only):

```bash
sqitch revert --to @ROOT
dropdb hoard_dev
createdb hoard_dev
sqitch deploy
```

## Resources

- [Sqitch Documentation](https://sqitch.org/docs/)
- [Sqitch Tutorial](https://sqitch.org/docs/manual/sqitchtutorial/)
- [PostgreSQL Best Practices](https://wiki.postgresql.org/wiki/Don%27t_Do_This)
