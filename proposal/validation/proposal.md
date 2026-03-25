# Embedded Consensus

## Related

- [design.md](design.md) —
  implementation design: how ChainDB maps onto Hoard's Component model, effect
  stack changes, and what happens to ImmutableTip and OrphanDetection

## Overview

The hoarding node currently depends on a separately running Cardano full node,
connected via the Node-to-Client (N2C) protocol over a Unix socket. Two things
are queried from it:

- **`ImmutableTip`** — polled every 30s via `LocalStateQuery`. Used as a
  threshold by `OrphanDetection`: only blocks whose slot is below the immutable
  tip are eligible for canonical/orphaned classification.
- **`IsOnChain`** — per-block query via local `ChainSync` / `FindIntersect`.
  Used to classify each eligible block as canonical or orphaned.

This proposal replaces both with an embedded `ChainDB` running inside the
hoard process, eliminating the external node as an operational dependency
and producing richer block classification as a side effect.

## Background: what the ChainDB is

The `ChainDB` (from `ouroboros-consensus`) is the component responsible for
chain tracking and canonical chain selection. It coordinates three sub-stores:

- **`ImmutableDB`** — append-only storage of blocks older than `k` slots (the
  security parameter; 2160 on mainnet). These can never be rolled back.
- **`VolatileDB`** — the last `k` blocks, possibly across multiple forks.
- **`LedgerDB`** — the last `k` ledger states (UTxO set, stake distribution,
  protocol parameters), one per volatile block. Required for rollback and for
  validating new blocks against consensus rules.

Together these answer the two questions the hoarding node currently asks the
external node:

- The immutable tip is the anchor of the volatile window — readable directly
  from the `ChainDB` API.
- Whether a point is on chain is answered by `ChainDB`'s own follower and chain
  selection state.

## Bootstrapping

The embedded ChainDB database is seeded before first run by running:

```
nix run .#hoard-chaindb-bootstrap-preprod
```

This invokes `mithril-client` to download a recent snapshot into the configured
`chain_db.database_directory` (`data/chaindb` by default). Mithril snapshots
are filesystem artifacts in the native `ouroboros-consensus` format (`ImmutableDB`
chunk files + `LedgerDB` snapshots) — the same format the embedded ChainDB reads.

On first run: the bootstrap command seeds `ImmutableDB` + a recent `LedgerDB`
snapshot. ChainDB opens, syncs the remaining volatile window from peers,
and is current within minutes. On subsequent runs: ChainDB resumes from its
persisted state. The bootstrap command is not needed again unless the database
is wiped.

## Block flow

Peer connections continue to work exactly as they do today. The change is that
every received block is fed to the embedded ChainDB in addition to being stored
in the hoarding database:

```
Peer A ──┐
Peer B ──┤── BlockReceived ──► Hoard
Peer C ──┘                       │
                                 ├── Persistence (store in hoarding DB)
                                 │
                                 └── addBlockAsync noPunishment
                                             │
                                         ChainDB
                                      ┌────┴────┐
                                 ImmutableDB  VolatileDB
                                            LedgerDB
```

`addBlockAsync` takes an `InvalidBlockPunishment` parameter. Passing
`noPunishment` means validation runs fully but no thread is killed and no peer
is disconnected — appropriate here because blocks arrive via our own peer
connections rather than being forwarded from untrusted third parties.

## Classification

Two channels inside the ChainDB interpreter produce pub/sub events:

**Follower** (`newFollower SelectedChain`) — tracks the evolving canonical chain,
running in a dedicated thread inside the interpreter:

```haskell
data ChainUpdate blk a
    = AddBlock a           -- block appended to the selected chain
    | RollBack (Point blk) -- chain reorganised; roll back to this point
```

`AddBlock` publishes `ChainExtended` for every volatile chain extension. When a
block crosses the immutable boundary (tracer event `CopiedBlockToImmutableDB`)
it publishes `BlockSealed` — it can no longer be rolled back. `RollBack`
publishes `BlockRolledBack`.

**`TraceAddBlockEvent` tracer** — runs inline inside the interpreter, publishing
directly into the effect stack without any intermediate queue. Emits
`BlockRejected` for every block that fails validation:

```haskell
AddBlockValidation (InvalidBlock (ExtValidationError blk) (RealPoint blk))
```

`ExtValidationError` distinguishes three stages:

| Variant | Stage | Meaning |
|---|---|---|
| `ExtValidationErrorHeader (HeaderEnvelopeError _)` | 1 | Wrong block/slot number, prev hash mismatch |
| `ExtValidationErrorHeader (HeaderProtocolError _)` | 2 | Invalid VRF, not a valid slot leader |
| `ExtValidationErrorLedger _` | 3 | Invalid transactions, UTxO rules violated |

The classification taxonomy expands from binary (canonical / orphaned) to:

| Classification | Event | ChainDB source |
|---|---|---|
| **Canonical** | `BlockSealed` | Tracer `CopiedBlockToImmutableDB` |
| **Orphaned** | `BlockRolledBack` | Follower `RollBack` |
| **Invalid — header envelope** | `BlockRejected` | Tracer `InvalidBlock`, stage 1 |
| **Invalid — consensus** | `BlockRejected` | Tracer `InvalidBlock`, stage 2 |
| **Invalid — ledger** | `BlockRejected` | Tracer `InvalidBlock`, stage 3 |

Stage 2 and stage 3 invalidity are the most operationally interesting: a block
that is structurally well-formed but claims a slot its leader had no right to
produce, or contains transactions that violate ledger rules, is a signal of
adversarial behaviour.

## What changes

**Removed:**

- `Hoard.Effects.NodeToClient` effect and its interpreter — the Unix socket
  connection to the external node
- `ImmutableTip` polling loop and its 30s refresh interval
- `IsOnChain` per-block query
- The Cardano node as an infrastructure component

**Changed:**

- `OrphanDetection` subscribes to `BlockSealed`, `BlockRolledBack`, and
  `BlockRejected` events instead of querying `NodeToClient`. Classification
  becomes push-based (ChainDB notifies on change) rather than pull-based (poll
  immutable tip, then query per block).

**Added:**

- Embedded ChainDB initialisation in the hoard startup path
- Mithril bootstrap step for hoard's own `ChainDB` directory (`nix run .#hoard-chaindb-bootstrap-preprod`)
- A `ChainDB` effect wrapping `feedBlock` (calls `addBlockAsync`) and
  `getImmutableTip`; the follower and tracer run inside the interpreter and
  publish `ChainExtended`, `BlockSealed`, `BlockRolledBack`, and `BlockRejected`
  directly into the effect stack

## Trade-offs

The embedded ChainDB carries the full ledger validation pipeline. This is the
same pipeline the external Cardano node was running; it moves from a separate
process into the hoard process. The dependencies (`ouroboros-consensus`,
`cardano-ledger-core`) are already present in the project.

Hoard's memory footprint increases by the size of the volatile window
(`k` ledger states ≈ 2160 snapshots on mainnet). The `LedgerDB` manages its own
snapshots and disk footprint independently of the hoarding database.

## Open questions

**ChainDB directory layout**

The embedded ChainDB needs its own database directory, separate from the hoarding
PostgreSQL database. Whether this lives alongside the hoarding DB or in a
dedicated path is a deployment configuration detail.

**Handling blocks the ChainDB ignores**

ChainDB silently drops blocks that are too old (older than the immutable tip) or
already known. Blocks the hoarding node receives that fall into these categories
are still stored in the hoarding DB — the ChainDB just won't classify them. The
hoarding DB classification for these can default to a `TooOld` or `AlreadyKnown`
status rather than `NULL`.

**ChainDB disk growth**

The `ImmutableDB` within the embedded ChainDB will grow without bound if not
pruned. The external Cardano node handles this with its own storage limits. The
embedded instance needs the same configuration applied.
