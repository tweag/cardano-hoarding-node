# Distributed Mode

## Related

- [mux.md](mux.md) — the multiplexed protocol channel layer the coordinator connection is built on

## Deliverables

- **Three executables** — `hoard-collector`, `hoard-coordinator`, and `hoard` (single-node, combining both roles). The existing codebase is refactored into a clean collector/coordinator split; `hoard` retains current behaviour for operators not adopting distributed mode.
- **Typed collector-coordinator protocol layer** — a set of mini-protocols built on `typed-protocols`, `network-mux`, and `ouroboros-network-framework` covering peer assignment, data forwarding, and coordinator advisories. Follows the same pattern as the existing Cardano mini-protocol implementations.
- **Single-node mode via in-process bearer** — collector and coordinator run in the same process connected via `Network.Mux.Bearer.Queues`. Behaviour is identical to today; existing deployments are unaffected.
- **Distributed mode via TCP** — collector and coordinator run as separate processes and communicate over TCP. Collector's coordinator address is static configuration for the first version.
- **NixOS modules for both roles** — `nixosModules.hoard-collector` and `nixosModules.hoard-coordinator`, deployable independently, so operators can run collectors in separate regions pointing at a shared coordinator.

## Success criteria

- Two collectors running in separate geographical regions connect to a single coordinator and forward data; blocks and headers from both appear in the same hoarding database.
- Single-node mode (`hoard`) behaves identically to the current deployment — no changes required from existing operators.
- Processing components (`Persistence`, `OrphanDetection`, `Sentry`, `Monitoring`) require no modifications; the collector/coordinator split is transparent to them.
- Protocol layer is covered by integration tests using the in-process bearer, exercising both happy-path and coordinator-disconnect scenarios.

## Motivation

In a user interview, the Cardano Foundation expressed interest in deploying
collectors across multiple geographical regions to minimise latency when
collecting data from peers and to observe data propagation patterns across the
network. This is the primary driver for distributed mode.

## Overview

Distributed mode separates the system into two distinct roles:

- **Collector**: thin data gatherer. Connects to Cardano peers, runs mini-protocols,
  forwards raw events upward. No persistence, no processing logic.
- **Coordinator**: processing hub. Receives events from all collectors, runs Persistence,
  OrphanDetection, Sentry, Monitoring, and manages peer assignment.

Collectors and the coordinator communicate exclusively via typed protocols over a mux
connection. In single-node mode both run in the same process connected via an in-process
queues bearer (`Network.Mux.Bearer.Queues`). In distributed mode collectors run as
separate processes connecting to the coordinator over TCP (`connectTo` /
`withServerNode`). No component code changes between modes — only the connection setup.

```
[ Single-node ]                    [ Distributed ]

  ┌──────────────────────┐           ┌─────────────────────────┐
  │  Coordinator         │           │  Coordinator            │
  │  Persistence         │           │  Persistence            │
  │  OrphanDetection     │           │  OrphanDetection        │
  │  Sentry              │           │  Sentry, Monitoring ... │
  │  Monitoring ...      │           └────────────┬────────────┘
  │  ──────────────────  │                        │ TCP
  │  Collector           │           ┌────────────┴───────┐  ┌────────────┐
  │  (runMuxLocal)       │           │  Collector A       │  │ Collector B│
  └──────────────────────┘           └────────────────────┘  └────────────┘
```

## Effect Stacks

The two roles have distinct effect stacks, reflecting their different responsibilities.

**Collector**:
- `NodeToNode`, `ChainSync`, `BlockFetch`, `KeepAlive`, `PeerSharing` — mini-protocols
- Local `Pub`/`Sub` for intra-collector coordination (e.g. `ChainSync` → `BlockFetch`)
- `PeerAssignment`, `BlockForward`, `HeaderForward`, `PeerDiscovery` — coordinator protocols
- No DB effects, no processing components

**Coordinator**:
- `BlockRepo`, `HeaderRepo`, `PeerRepo` and other DB effects
- Local `Pub`/`Sub` for processing components (`Persistence`, `Sentry`, `OrphanDetection`
  subscribe here, fed by events forwarded from collectors)
- Accept loop running `runMuxConn` per connected collector
- `Monitoring`, `Server`, `ImmutableTip`, `BlockEviction`
- No mini-protocol effects

In distributed mode these become two separate binaries: `hoard-collector` and
`hoard-coordinator`. The existing `Main.hs` currently conflates both; splitting it is a
natural forcing function — anything that requires effects from both stacks indicates the
separation is not yet clean.

## Collector Design Invariant

> **Collectors only publish, never subscribe to events from other collectors.**

Local `Pub`/`Sub` within a collector is intra-collector only — one component reacting to
another within the same peer connection (e.g. a received header triggering a block fetch
request). No collector listens to events produced by another collector. All
cross-collector coordination flows through the coordinator via protocols.

## Protocols

Communication between collector and coordinator is expressed as independent typed
protocols over the mux connection.

| Protocol | Collector → Coordinator | Coordinator → Collector | Purpose |
|---|---|---|---|
| `PeerAssignment` | `WantPeer`, `ReturnPeer` | `AssignPeer`, `NoneAvailable` | Demand-driven peer allocation |
| `PeerDiscovery` | `ReportPeer` | — | Forward peers discovered via PeerSharing |
| `BlockForward` | `ForwardBlock` | — | Forward received blocks for processing |
| `HeaderForward` | `ForwardHeader` | — | Forward received headers for processing |
| `Advisory` | — | `RevokePeer` | Coordinator pushes revocations (e.g. adversarial peers) |

The coordinator receives `BlockForward` and `HeaderForward` events from each collector
and republishes them into its own local `Pub`/`Sub`, where `Persistence`,
`OrphanDetection`, and `Sentry` subscribe as normal.

## Consistency

Collectors do not coordinate at the block level. If two collectors happen to download
the same block from different peers, the duplicate is resolved on write by the
coordinator's `Persistence` component. All writes go through one coordinator, so the
dedup is centralised rather than distributed.

## Graceful Degradation

If the coordinator connection drops, `connectTo` surfaces a `ConnectionError`. The
collector continues running mini-protocols on its existing peer connections but cannot:

- Acquire new peers (no `PeerAssignment`)
- Forward blocks or headers for persistence (no `BlockForward`, `HeaderForward`)
- Receive peer revocations (no `Advisory`)

No data is permanently lost — blocks in flight are simply not persisted until the
connection is restored, depending on how the collector handles the error. Reconnection
logic sits above the `Mux` layer and is out of scope here.

In single-node mode via the queues bearer the connection cannot drop — both sides share
the same process lifetime.

## Open Questions

**Node identity**

The mux connection carries node identity implicitly. A stable node identifier is only
needed if the coordinator persists assignment state across reconnects. A value from
configuration is the simplest option if this is required.

**Static vs. dynamic node membership**

For a first version, a fixed coordinator address in collector configuration is sufficient.
Dynamic membership can be added later if needed.

**Block retention for propagation timing**

The hoarding node currently evicts non-interesting blocks once they move past the immutable tip. For propagation timing to be useful, blocks need to be retained long enough to correlate observations across collectors. A configurable retention period — independent of the immutable tip threshold — is needed. This is a small addition but should be addressed before distributed mode is deployed for timing analysis.

**Buffering on coordinator disconnect**

When a collector loses its coordinator connection, forwarded events are dropped. An
in-process buffer could hold recent events for replay on reconnect, at the cost of
unbounded memory growth during a long partition. Worth revisiting once the basic
architecture is in place.
