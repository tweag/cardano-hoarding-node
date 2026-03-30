# Retrospective: Hoarding Node — Cycle 1

## Original milestones

| # | Milestone | Timescale | Status |
|---|---|---|---|
| 1 | Hoarding Node Design | 2 months | Delivered |
| 2 | Block collector | 4 months | Delivered |
| 3 | Transaction collector | 4 months | Not delivered |
| 4 | Node connection logic | 5 months | Delivered |
| 5 | Hoarding node UI | 6 months | Delivered |
| 6 | Proof against adversarial behaviour | 6 months | Delivered |

## What was delivered

### Hoarding Node Design

Architecture and component design documented. A CIP was drafted and submitted
for community review (PR #1106 in cardano-foundation/CIPs). The CIP process
surfaced useful feedback and the design was refined iteratively alongside the
implementation.

### Block collector

The hoarding node connects to Cardano peers via the Node-to-Node protocol,
runs ChainSync and BlockFetch mini-protocols, and stores all observed blocks
in a PostgreSQL database. Blocks are deduplicated by content hash. Each block
is tagged with metadata: whether it is canonical or orphaned, and whether any
anomalies were detected (integrity violations, header-block mismatches, blocks
outside the requested fetch range).

### Node connection logic

The `PeerManager` component handles the full peer lifecycle:

- **Bootstrap**: initial peer set seeded from a configurable peer snapshot file
  (format compatible with the Cardano ledger's stake pool relay snapshot, but
  any compatible file can be used)
- **Discovery**: new peers discovered at runtime via the Peer Sharing mini-protocol
- **Replenishment**: new collector threads spawned automatically as new peers are found
- **Cooldown**: failed peers excluded from the eligible set for a configurable duration
- **Pinned peers**: operators can fix a specific peer set via the management API

### Hoarding node UI

A Servant-based HTTP API is exposed on a configurable port:

- `GET /metrics` — Prometheus-format metrics for connected peers, blocks/headers
  in DB, block fetch counters, database query latencies, GHC runtime metrics
- `GET /blocks?minSlot=X&maxSlot=Y&tags=T` — query blocks by slot range and
  tags; returns blocks with at least one matching tag
- `GET /blocks/disputes?minSlot=X&maxSlot=Y` — query slot disputes by slot range
- `GET /headers?minSlot=X&maxSlot=Y&tags=T` — query headers by slot range and tags
- `GET /peers/pinned`, `POST /peers/pinned`, `DELETE /peers/pinned` — peer
  management

A full observability stack is included: Prometheus, Grafana, Loki (log
aggregation), Tempo (distributed tracing), and Promtail.

### Proof against adversarial behaviour

The `Sentry` component detects and records adversarial behaviour:

- **Duplicate blocks**: same block delivered multiple times by the same peer
  (per-request deduplication via STM)
- **Out-of-range blocks**: blocks received outside the requested fetch range
- **Header-block mismatches**: block body does not match the advertised header
- **Orphan detection**: blocks classified as canonical or orphaned once below
  the immutable tip, using the external Cardano node or the embedded ChainDB

Detected violations are persisted to the database and queryable via the API.
Eviction heuristics retain interesting blocks (orphaned, violated) while
evicting canonical/valid ones to bound storage.

Additionally, the `Verifier` effect performs KES signature integrity checks
on headers and blocks — detecting structural invalidity without requiring
ledger state.

## What was not delivered

### Transaction collector

Transaction collection was deferred. To observe mempool transactions via the
TxSubmission mini-protocol, peers must be able to run TxSubmission towards the
hoarding node. The Ouroboros network specification (§1.4.1) describes two paths:
inbound connections to a publicly reachable node, or promoted outbound
connections initiated by the hoarding node itself (which would not require public
reachability). A prototype of the promoted-connection path was attempted but did
not succeed in getting any connections promoted, possibly due to preprod peer
behaviour. Until this is resolved, a publicly reachable deployed node remains
the reliable path. Transaction collection is proposed for the next cycle,
conditional on the infrastructure proposal being accepted.

## Beyond scope

### Embedded ChainDB

The most significant addition beyond the original proposal. The hoarding node
depends on a separately running Cardano full node (connected via the
Node-to-Client protocol) to answer two questions: what is the current immutable
tip, and is a given block on the canonical chain?

A prototype of an embedded `ChainDB` (from `ouroboros-consensus`) running
inside the hoard process has been implemented and merged. It is intended to run
in parallel with the external full node initially; once it has been proven in
production the full node dependency can be removed. The embedded ChainDB:

- Provides richer block classification: canonical, orphaned, and — once wired
  into the collection pipeline — invalid at the header envelope, consensus, or
  ledger stage
- Is bootstrapped via Mithril snapshots and syncs the remaining volatile window
  from peers in minutes

The remaining work (OrphanDetection migration, DB schema expansion, API
exposure) is proposed for the next cycle under the validation proposal.

### Header receipts

Every header received from a peer is recorded in a `header_receipts` table with
the peer ID and timestamp. This data is not currently analysed, but it lays the
groundwork for timing investigations: comparing when different peers first
advertised a given header makes it possible to study block propagation patterns
across the network — one of the analytical use cases mentioned in the original
proposal.

### Production-grade internals

Several components were built to a higher standard than the original proposal
implied:

- **Testing**: 19 test files covering unit and integration tests; integration
  tests run against a real PostgreSQL database
- **Configuration**: YAML-based config with environment variable overrides,
  sops-nix secrets management, per-environment Docker images
- **Observability**: full Prometheus + Grafana + Loki + Tempo stack, with
  distributed tracing instrumented throughout the codebase
- **`atelier` library**: reusable effect and component abstractions extracted
  into a standalone library, usable by future Haskell projects in this space

## Adversarial behaviour detected in the wild

The hoarding node has already detected interesting behaviour on the
pre-production network during development and testing, including many orphaned
blocks and a series of equivocating blocks produced by the same peer — multiple
blocks claimed for the same slot by the same pool. This validates the core premise
of the proposal and provides concrete evidence of the node's value.

## Current state

The hoarding node is a functioning tool. It connects to any Cardano network (currently targeting pre-production),
collects and classifies all observed blocks, detects adversarial behaviour, and
exposes results via an HTTP API and Prometheus metrics. It is not
yet deployed to persistent infrastructure — that is the primary goal of the
infrastructure proposal for the next cycle.
