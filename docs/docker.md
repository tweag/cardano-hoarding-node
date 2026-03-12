# Docker

## Building

Images are built per environment using Nix. Each image has the matching
`config/{env}.yaml` and Cardano network config baked in — only secrets need
to be supplied at runtime via environment variables.

```bash
nix build .#docker-dev
nix build .#docker-staging
nix build .#docker-prod
```

Load the resulting image into Docker:

```bash
docker load < result
```

## Running locally

### Prerequisites

- A running Cardano node (see `nix run .#cardano-node-preprod`)
- A running PostgreSQL instance (see `nix run .#postgres`)
- An `.env.local` file with secrets (see below)

### `.env.local`

Secrets and any local overrides are passed via `--env-file`. The required
database credentials follow the standard `HOARD__*` naming convention
(see [configuration.md](configuration.md)):

```bash
HOARD__DATABASE__USERS__READER__USER=hoard_reader
HOARD__DATABASE__USERS__READER__PASSWORD=...
HOARD__DATABASE__USERS__WRITER__USER=hoard_writer
HOARD__DATABASE__USERS__WRITER__PASSWORD=...
```

### Run command

The local PostgreSQL instance uses a Unix socket (no TCP, no password).
Mount both the node socket and the PostgreSQL socket directory into the
container, and use `--network=host` so the container can reach any
TCP services on the host:

```bash
docker run --rm \
  --env-file .env.local \
  --network=host \
  -v $(pwd)/node-db:/app/node-db \
  -v $(pwd)/postgres-data:/app/postgres-data \
  cardano-hoarding-node:dev
```

| Mount | Purpose |
|---|---|
| `node-db` | Cardano node Unix socket (`node-db/preprod/node.socket`) |
| `postgres-data` | PostgreSQL Unix socket directory |

## Deployment (ECS)

In ECS, both the Cardano node socket and the database are reached over the
network, so no volume mounts are needed. Pass secrets as environment
variables in the task definition:

```
HOARD__DATABASE__HOST=<rds-endpoint>
HOARD__DATABASE__USERS__READER__USER=...
HOARD__DATABASE__USERS__READER__PASSWORD=...
HOARD__DATABASE__USERS__WRITER__USER=...
HOARD__DATABASE__USERS__WRITER__PASSWORD=...
```
