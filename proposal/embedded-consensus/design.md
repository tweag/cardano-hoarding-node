# Embedded Consensus: Hoard Integration Design

## Related

- [proposal.md](proposal.md) — the proposal this document
  implements; read that first for context and motivation
- [header-validation/design.md](header-validation/design.md) —
  implementation design for header validation (Stage 1b)

## Overview

This document describes how the embedded `ChainDB` integrates with Hoard's
specific abstractions: the `Component` lifecycle model, the `Pub`/`Sub` event
system, and the `effectful` effect stack.

## New events

The ChainDB produces four kinds of output that downstream components care about.
These become first-class events in the pub/sub system:

```haskell
-- The volatile chain was extended with a new block.
data ChainExtended = ChainExtended { point :: CardanoPoint }

-- A block has been confirmed on the canonical chain and is now past the
-- immutable boundary — it can no longer be rolled back.
data BlockSealed = BlockSealed { point :: CardanoPoint }

-- A block was on the canonical chain but has been rolled back by a fork.
-- The block is an orphan.
data BlockRolledBack = BlockRolledBack { point :: CardanoPoint }

-- A block failed ChainDB validation. Stage and error are carried for storage.
data BlockRejected = BlockRejected
    { point :: CardanoPoint
    , validationError :: ExtValidationError CardanoBlock
    }
```

`BlockSealed` fires once per block that crosses the immutable boundary,
replacing both the `ImmutableTipRefreshTriggered` trigger and the
`ImmutableTip.Refreshed` event. Rather than polling for an advancing tip and
then querying per-block, the ChainDB tracer fires directly when a specific block
is copied to the ImmutableDB.

## ChainDB component

The `ChainDB` component wires the embedded consensus store into the Component
lifecycle. The `runChainDB` interpreter handles all ChainDB initialisation,
including opening the database, starting the follower thread, and attaching the
tracer. The component itself is thin:

```
listeners — [blockFeedListener, chainExtendedListener, rollbackListener]
triggers  — [consistencyCheck every 3s]
```

### `blockFeedListener`

Subscribes to `BlockReceived` and calls `feedBlock` for each block. This is the
entry point from the collector pipeline into the ChainDB.

```haskell
blockFeedListener :: (Sub BlockReceived :> es, ChainDB :> es) => BlockReceived -> Eff es ()
blockFeedListener BlockReceived {block} = ChainDB.feedBlock block
```

### `chainExtendedListener` and `rollbackListener`

Log-only listeners that make volatile chain progress and rollbacks visible in
the application log:

```haskell
chainExtendedListener :: (Log :> es) => ChainExtended -> Eff es ()
chainExtendedListener (ChainExtended point) =
    Log.debug $ "ChainDB: chain extended to slot " <> show (pointSlot point)

rollbackListener :: (Log :> es) => BlockRolledBack -> Eff es ()
rollbackListener (BlockRolledBack point) =
    Log.info $ "ChainDB: rollback to " <> show point
```

### `consistencyCheck`

A periodic trigger (every 3 seconds) that compares the immutable tip recorded
in `HoardState` against the one reported by the ChainDB, logging a warning if
they diverge:

```haskell
consistencyCheck :: (ChainDB :> es, Log :> es, State HoardState :> es) => Eff es ()
consistencyCheck = do
    hoardStateTip <- (.immutableTip) <$> get @HoardState
    chainDBTip <- ChainDB.getImmutableTip
    let hoardSlot = case hoardStateTip of
            ChainPoint C.ChainPointAtGenesis -> Origin
            ChainPoint (C.ChainPoint slot _) -> At slot
        chainDBSlot = pointSlot chainDBTip
    if hoardSlot == chainDBSlot
        then Log.debug $ "ChainDB: immutable tip consistent: " <> show hoardSlot
        else Log.warn $ "ChainDB: immutable tip mismatch — HoardState: "
                     <> show hoardSlot <> ", ChainDB: " <> show chainDBSlot
```

## The `runChainDB` interpreter

The interpreter owns the ChainDB handle for its lifetime. On startup it:

1. Reads `Config` (database directory) and `ProtocolInfo` from the effect stack
2. Opens the ChainDB via `openChainDB`, configuring the tracer to publish
   `BlockSealed` and `BlockRejected` directly into the effect stack
3. Forks a follower thread (tracked by `ResourceRegistry`) that subscribes to
   `SelectedChain` and publishes `ChainExtended` on `AddBlock` and
   `BlockRolledBack` on `RollBack`

`feedBlock` calls `addBlock_` with `noPunishment` — no peer is disconnected on
validation failure because blocks arrive via our own peer connections rather than
directly from untrusted peers.

The tracer publishes inline — there is no intermediate queue. Both the tracer
and follower thread use `withEffToIO (ConcUnlift Persistent Unlimited)` to
safely call into the effect stack from IO callbacks and forked threads.

```haskell
mkTracer
    :: (Pub BlockRejected :> es, Pub BlockSealed :> es)
    => Tracer (Eff es) (TraceEvent CardanoBlock)
mkTracer = Tracer $ \case
    TraceCopyToImmutableDBEvent (CopiedBlockToImmutableDB point) ->
        publish (BlockSealed point)
    TraceAddBlockEvent (AddBlockValidation (InvalidBlock err rpoint)) ->
        publish (BlockRejected (realPointToPoint rpoint) err)
    ...
```

The tracer is hoisted to `IO` at the call site with
`Tracer (runInIO . runTracer mkTracer)`.

## Effect: `ChainDB`

A thin effect exposing the two operations hoard needs:

```haskell
data ChainDB :: Effect where
    FeedBlock      :: CardanoBlock -> ChainDB m ()
    GetImmutableTip :: ChainDB m CardanoPoint
```

`runChainDB` interprets these against the open `ChainDB m blk` handle. Both
operations are direct function calls on the in-process handle with no IPC,
no channel, and no reconnection logic.

## Changes to existing components

### `ImmutableTip` — removed

The `ImmutableTip` component exists solely to poll `NodeToClient.immutableTip`
and publish `ImmutableTip.Refreshed` when the tip advances. With the ChainDB
tracer emitting `BlockSealed` directly per settled block, the polling loop
is unnecessary.

`Persistence.persistImmutableTipOnRefresh` is replaced by a listener on
`BlockSealed` that persists the new immutable point. `Monitoring` reads the
immutable tip from `HoardState`, which is now updated by the same listener
instead of by `ImmutableTip`.

### `OrphanDetection` — simplified

The two existing listeners:

| Current | Replacement |
|---|---|
| `blockReceivedClassifier` — defers or immediately queries `isOnChain` | `blockSealedClassifier` — listens to `BlockSealed`, marks as canonical |
| `immutableTipUpdatedAger` — batches unclassified blocks, queries `isOnChain` | `blockRolledBackClassifier` — listens to `BlockRolledBack`, marks as orphaned |

A third listener is added:

- `blockRejectedClassifier` — listens to `BlockRejected`, records the validation
  stage and error alongside the `Invalid` classification.

The per-block `isOnChain` query loop and the `agingBatchSize` configuration
field disappear. Classification becomes fully push-based: the ChainDB interpreter
tells OrphanDetection what happened to each block rather than OrphanDetection
polling for it.

The `NodeToClient :> es` constraint is removed from OrphanDetection entirely.

## Changes to `Main.hs`

**Added in Stage 1:**
- `runConfig @"chain_db" @ChainDB.Config` (database directory path)
- `runChainDB` alongside `runNodeToClient` (not yet replacing it)
- `runPubSub @ChainExtended`
- `runPubSub @BlockSealed`
- `runPubSub @BlockRolledBack`
- `runPubSub @BlockRejected`
- `ChainDB.component` in `runSystem`, listed before `OrphanDetection` and
  `Persistence`

**Removed in later stages (3–4):**
- `runNodeToClient` from the effect stack
- `WithSocket.withNodeSockets` (the `nodeToClient`-labeled socket variant)
- `runConfig @"cardano_node_integration" @ImmutableTip.Config`
- `runPubSub @ImmutableTipRefreshTriggered`
- `runPubSub @ImmutableTip.Refreshed`
- `ImmutableTip.component` from `runSystem`

## Database schema

The block classification enum gains new variants:

```sql
-- Existing
CREATE TYPE block_classification AS ENUM ('canonical', 'orphaned');

-- New
ALTER TYPE block_classification ADD VALUE 'invalid_header_envelope';
ALTER TYPE block_classification ADD VALUE 'invalid_consensus';
ALTER TYPE block_classification ADD VALUE 'invalid_ledger';
```

The `ExtValidationError` detail (the specific error message) is stored in a
nullable `classification_detail` text column added to the blocks table.

## Boot sequence

```
1. runChainDB: open ChainDB from configured directory
   - Configure tracer (publishes BlockSealed, BlockRejected inline)
   - Fork follower thread (publishes ChainExtended, BlockRolledBack)
2. runSystem setup phase:
   Persistence.setup, OrphanDetection.setup, ... (unchanged)
3. runSystem activate phase: all listeners/triggers forked concurrently
   ChainDB.blockFeedListener starts consuming BlockReceived events
   ChainDB.chainExtendedListener and rollbackListener start logging
   ChainDB.consistencyCheck trigger starts firing every 3s
4. Normal operation: peers feed blocks → ChainDB validates →
   events flow to OrphanDetection and Persistence
```

## Implementation stages

### Stage 1 — ChainDB component (proof of concept)

**Goal:** get the embedded `ChainDB` running and fed blocks, with no changes to
existing functionality. `NodeToClient`, `ImmutableTip`, and `OrphanDetection`
are untouched. The new events are published but nothing acts on them yet.

**Adds:**
- `Hoard.Effects.ChainDB` — effect definition (`FeedBlock`, `GetImmutableTip`)
  and `runChainDB` interpreter (open ChainDB, tracer publishing, follower thread)
- `ChainDB.Config` and `runConfig @"chain_db"` entry in `Main.hs`
- `Hoard.ChainDB.Events` — `ChainExtended`, `BlockSealed`, `BlockRolledBack`,
  `BlockRejected`
- `runPubSub` entries for the four new events in `Main.hs`
- `Hoard.ChainDB` component (`blockFeedListener`, `chainExtendedListener`,
  `rollbackListener`, `consistencyCheck`)
- `ChainDB.component` added to `runSystem` before `OrphanDetection`

**Changes nothing.** At the end of this stage: the ChainDB syncs in parallel
with the existing Cardano node. New events fire. Logs confirm blocks are being
validated and chain updates observed.

---

### Stage 1b — Header validation

**Goal:** validate incoming ChainSync headers using the embedded ChainDB's
ledger forecast, detecting invalid consensus proofs before block bodies are
downloaded. Independent of Stages 2–4.

See [header-validation/design.md](header-validation/design.md) for
the full design.

**Adds:**
- `GetHeaderStateHistory` and `GetLedgerForecast` operations on the `ChainDB`
  effect
- `Hoard.Events.InvalidHeaderReceived` event
- `Hoard.HeaderValidation` component (`validateHeaderListener`,
  `rollbackListener`, `peerDisconnectedListener`)
- `evalState @(Map Peer (HeaderStateHistory CardanoBlock))` and
  `runPubSub @InvalidHeaderReceived` in `Main.hs`

**Depends on:** Stage 1. **Changes nothing** in existing classification or
downstream components.

---

### Stage 2 — OrphanDetection via ChainDB events

**Goal:** switch block classification to the push-based model. The DB schema
gains new variants; OrphanDetection gains new listeners and loses the
`NodeToClient` dependency.

**Adds:**
- DB migration: new `block_classification` enum values
  (`invalid_header_envelope`, `invalid_consensus`, `invalid_ledger`) and
  nullable `classification_detail` column
- `blockSealedClassifier`, `blockRolledBackClassifier`,
  `blockRejectedClassifier` listeners in `OrphanDetection`

**Removes:**
- `blockReceivedClassifier` and `immutableTipUpdatedAger` listeners
- `NodeToClient :> es` constraint from `OrphanDetection`
- `agingBatchSize` from `OrphanDetection.Config`

---

### Stage 3 — Remove `ImmutableTip` component

**Goal:** eliminate the `NodeToClient.immutableTip` polling loop. `Persistence`
and `Monitoring` follow `BlockSealed` instead of `ImmutableTip.Refreshed`.

**Adds:**
- `Persistence.persistImmutableTipOnBlockSealed` listener on `BlockSealed`
- `HoardState.immutableTip` updated by the same listener (currently updated by
  `ImmutableTip` component)

**Removes:**
- `Hoard.ImmutableTip` module and component
- `ImmutableTipRefreshTriggered` and `ImmutableTip.Refreshed` events and their
  `runPubSub` entries in `Main.hs`
- `runConfig @"cardano_node_integration" @ImmutableTip.Config`
- `ImmutableTip.component` from `runSystem`

---

### Stage 4 — Remove `NodeToClient` and the external Cardano node

**Goal:** complete the removal. The external Cardano node is no longer required.

**Removes:**
- `Hoard.Effects.NodeToClient` module
- `runNodeToClient` from `Main.hs`
- `Labeled "nodeToClient" WithSocket` from `WithSocket.withNodeSockets` and the
  effect stack; if the `tracer` socket is also unused, `WithSocket` may be
  removed entirely
- `runConfig @"node_sockets"` if no other sockets remain
- Any remaining documentation referencing the Cardano node as a required
  infrastructure component

---

## Open questions

**`blockFeedListener` vs Persistence ordering**

`blockFeedListener` and `Persistence` both subscribe to `BlockReceived`.
Pub/sub delivery order between subscribers is not guaranteed. A block could be
classified as `BlockSealed` (from a previous ChainDB tracer event) before
Persistence has stored it. The classification listener should be idempotent and
handle missing blocks gracefully, or `BlockReceived` delivery must be ordered
(store first, then feed ChainDB).

One option: `blockFeedListener` subscribes to a `BlockStored` event published by
Persistence rather than directly to `BlockReceived`, giving a clear ordering.

**ChainDB disk growth**

The embedded `ChainDB` accumulates `ImmutableDB` data indefinitely. The external
Cardano node applies its own storage limits. The `ChainDB.Config` should expose
equivalent pruning options, or the ImmutableDB can be configured to keep only a
rolling window of blocks (sufficient for the follower's needs).

**`ProtocolInfo` and `NodeConfig` sources**

These are currently read from the Cardano node's genesis and config files via
`cardano-api`. The embedded ChainDB needs the same inputs. The existing
`Reader (ProtocolInfo CardanoBlock)` and `Reader NodeConfig` effects remain in
the stack; only `runNodeToClient` is replaced, not the config loading that feeds
into it.
