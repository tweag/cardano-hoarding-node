# Header Validation: Hoard Integration Design

## Related

- [proposal.md](proposal.md) — the proposal this document
  implements; read that first for context and motivation
- [../design.md](../design.md) —
  ChainDB component and effect design this feature builds on

## Overview

This document describes how header validation integrates with Hoard's
`Component` model, the `ChainDB` effect, and the `effectful` stack.

## Background: `HeaderStateHistory`

Validating a header requires more than a ledger view. The consensus protocol
(Praos) carries chain-dependent state across blocks — evolved VRF and KES keys,
nonce accumulation — so validating a header also requires the consensus state
accumulated by the headers that preceded it on *that peer's* chain. This is
captured by `HeaderState`:

```haskell
data HeaderState blk = HeaderState
    { headerStateTip      :: WithOrigin (AnnTip blk)
    , headerStateChainDep :: ChainDepState (BlockProtocol blk)
    }
```

`headerStateChainDep` for Praos holds the current epoch nonce and the evolved
operational certificate state needed to verify the next block's KES signature.

`HeaderStateHistory` is an anchored sequence of these snapshots — one per
validated header — with `rewind` (for rollbacks) and `trim` (to bound memory
use). The core validation function appends to it:

```haskell
-- From Ouroboros.Consensus.HeaderStateHistory
validateHeader
    :: (BlockSupportsProtocol blk, ValidateEnvelope blk)
    => TopLevelConfig blk
    -> LedgerView (BlockProtocol blk)
    -> Header blk
    -> RelativeTime
    -> HeaderStateHistory blk
    -> Except (HeaderError blk) (HeaderStateHistory blk)
```

`HeaderStateHistory` is **per-peer**: each peer proposes a potentially different
candidate chain. When a peer is first seen, its history is seeded from the local
chain's current header state history (`ChainDB.getHeaderStateHistory`) — the
right anchor because the intersection point is where both chains last agreed.

## New `ChainDB` effect operations

Two operations are added to the existing `ChainDB` effect:

```haskell
data ChainDB :: Effect where
    FeedBlock             :: CardanoBlock -> ChainDB m ()
    GetImmutableTip       :: ChainDB m CardanoPoint
    GetHeaderStateHistory :: ChainDB m (HeaderStateHistory CardanoBlock)
    GetLedgerForecast
        :: CardanoPoint  -- ^ intersection point of local and peer's chain
        -> SlotNo        -- ^ slot to forecast for
        -> ChainDB m (Maybe (LedgerView (BlockProtocol CardanoBlock), RelativeTime))
```

**`GetHeaderStateHistory`** maps to `CDB.getHeaderStateHistory` (an `STM`
action on the open handle). Returns the history for the current local chain.

**`GetLedgerForecast intersection slot`** derives a `LedgerView` forecast
anchored at the *intersection point* of the local and peer's chain. Using the
intersection point rather than the current tip is important: if the peer's chain
diverges before an epoch boundary, the epoch nonce and stake distribution could
differ between the two forks, making a tip-anchored forecast incorrect. Returns
`Nothing` when the slot is beyond the forecast horizon (~3k/f slots ahead of the
intersection, ~36 hours on mainnet), or when the intersection point is no longer
within the last k blocks:

```haskell
GetLedgerForecast intersection slot -> liftIO $ atomically $ do
    mLedger <- CDB.getPastLedger chainDB intersection
    pure $ case mLedger of
        Nothing     -> Nothing  -- intersection point no longer within k blocks
        Just ledger ->
            let forecast = ledgerViewForecastAt (configLedger cfg) (ledgerState ledger)
            in case runExcept (forecastFor forecast slot) of
                Right lv                    -> Just (lv, {- RelativeTime from era summary in lv -})
                Left OutsideForecastRange{} -> Nothing
```

The `RelativeTime` required by `validateHeader` is derived from the era summary
embedded in the `HardForkLedgerView`. To avoid exposing era-summary details to
the component, `GetLedgerForecast` returns a `(LedgerView, RelativeTime)` pair
rather than just the `LedgerView`.

## `HeaderValidation` component

```
state     — Map Peer (CardanoPoint, HeaderStateHistory CardanoBlock)
              -- ^ intersection point and per-peer header state history
listeners — [validateHeaderListener, rollbackListener, peerDisconnectedListener]
triggers  — []
```

```haskell
component
    :: ( ChainDB :> es
       , Log :> es
       , Pub InvalidHeaderReceived :> es
       , Reader (ProtocolInfo CardanoBlock) :> es
       , State (Map Peer (CardanoPoint, HeaderStateHistory CardanoBlock)) :> es
       , Sub ChainSync.HeaderReceived :> es
       , Sub ChainSync.RollBackward :> es
       , Sub PeerManager.PeerDisconnected :> es
       )
    => Component es
```

### `validateHeaderListener`

On `HeaderReceived { peer, header }`:

1. Look up the peer's `(intersection, history)` in state. If absent, seed:
   - `intersection` from `ChainSync.intersectionPoint peer` (the point agreed
     during `FindIntersect`)
   - `history` from `ChainDB.getHeaderStateHistory`
2. Call `ChainDB.getLedgerForecast intersection (blockSlot header)`. If `Nothing`
   (beyond horizon or intersection no longer within k blocks), log at debug level
   and skip.
3. Call `validateHeader cfg ledgerView header slotTime history`.
4. On success: replace the peer's entry in state with the updated history
   (intersection unchanged).
5. On failure: log a warning and publish `InvalidHeaderReceived`.

### `rollbackListener`

On `ChainSync.RollBackward { peer, point }`:

- Call `rewind point` on the peer's `HeaderStateHistory`.
- `rewind` returns `Nothing` only if the rollback point is before the anchor
  (the immutable tip) or not present in the k-block history. Since the Ouroboros
  protocol guarantees rollbacks cannot exceed k blocks, this should never occur
  in practice — if it does, it indicates a serious protocol violation by the
  peer. Log an error and remove the peer's entry.
- Otherwise replace the entry with the rewound history.

### `peerDisconnectedListener`

On `PeerManager.PeerDisconnected { peer }`:

- Remove the peer's entry from state.

Without this cleanup, disconnected peers accumulate in the map indefinitely.

## Changes to `Main.hs`

```haskell
. evalState @(Map Peer (CardanoPoint, HeaderStateHistory CardanoBlock)) mempty
. runPubSub @InvalidHeaderReceived
```

`HeaderValidation.component` is added to `runSystem` alongside
`ChainDB.component`.

## Open questions

**Beyond-horizon headers during initial sync**

While the `ChainDB` is catching up from a Mithril snapshot, its ledger tip lags
the network tip by potentially months. The forecast horizon is ~36 hours ahead
of the *ChainDB's* tip, not the network tip — so all live headers from peers
will be beyond the horizon until the `ChainDB` finishes syncing. This is
expected and unavoidable; the `HeaderValidation` component effectively does
nothing until the `ChainDB` is current. A debug log noting the slot and the
current horizon is sufficient; no action is needed.

In steady state this cannot occur: the horizon is ~36 hours ahead of the local
tip and peers do not produce headers that far in the future.

**Attack mitigation for beyond-horizon and invalid headers**

Silently skipping beyond-horizon headers creates a window where an adversary can
flood the node with such headers at no validation cost. Until the `ChainDB` is
synced, this window is unavoidable, but once in steady state it should not
arise. For invalid headers (stage 2: consensus protocol errors), the component
should apply per-peer bounds: store at most a bounded number of
`InvalidHeaderReceived` events per peer, and avoid fetching blocks for more than
a bounded number of unvalidatable headers per peer. This prevents a malicious
peer from using invalid or unvalidatable headers as a resource exhaustion vector.

**`InvalidHeaderReceived` and Sentry**

A `HeaderProtocolError` (stage 2: invalid VRF / not a valid slot leader) is a
strong adversarial signal. `Sentry` subscribing to `InvalidHeaderReceived` and
emitting `AdversarialBehavior` is a natural follow-on, left for a later stage.
