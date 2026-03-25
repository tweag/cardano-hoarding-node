# Header Validation: Hoard Integration Design

## Related

- [header-validation.md](header-validation.md) — the proposal this document
  implements; read that first for context and motivation
- [embedded-consensus-hoard-design.md](embedded-consensus-hoard-design.md) —
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
        :: SlotNo
        -> ChainDB m (Maybe (LedgerView (BlockProtocol CardanoBlock)))
```

**`GetHeaderStateHistory`** maps to `CDB.getHeaderStateHistory` (an `STM`
action on the open handle). Returns the history for the current local chain.

**`GetLedgerForecast slot`** derives a `LedgerView` forecast from the current
ledger state. Returns `Nothing` when the slot is beyond the forecast horizon
(~3k/f slots ahead of the tip, ~36 hours on mainnet):

```haskell
GetLedgerForecast slot -> liftIO $ atomically $ do
    ledger <- CDB.getCurrentLedger chainDB
    let forecast = ledgerViewForecastAt (configLedger cfg) (ledgerState ledger)
    pure $ case runExcept (forecastFor forecast slot) of
        Right lv                    -> Just lv
        Left OutsideForecastRange{} -> Nothing
```

The `RelativeTime` required by `validateHeader` is derived from the era summary
embedded in the `HardForkLedgerView`. To avoid exposing era-summary details to
the component, `GetLedgerForecast` returns a `(LedgerView, RelativeTime)` pair
rather than just the `LedgerView`.

## `HeaderValidation` component

```
state     — Map Peer (HeaderStateHistory CardanoBlock)
listeners — [validateHeaderListener, rollbackListener, peerDisconnectedListener]
triggers  — []
```

```haskell
component
    :: ( ChainDB :> es
       , Log :> es
       , Pub InvalidHeaderReceived :> es
       , Reader (ProtocolInfo CardanoBlock) :> es
       , State (Map Peer (HeaderStateHistory CardanoBlock)) :> es
       , Sub ChainSync.HeaderReceived :> es
       , Sub ChainSync.RollBackward :> es
       , Sub PeerManager.PeerDisconnected :> es
       )
    => Component es
```

### `validateHeaderListener`

On `HeaderReceived { peer, header }`:

1. Call `ChainDB.getLedgerForecast (blockSlot header)`. If `Nothing` (beyond
   horizon), return — log at debug level and skip.
2. Look up the peer's `HeaderStateHistory` in state. If absent, seed it with
   `ChainDB.getHeaderStateHistory`.
3. Call `validateHeader cfg ledgerView header slotTime history`.
4. On success: replace the peer's entry in state with the updated history.
5. On failure: log a warning and publish `InvalidHeaderReceived`.

### `rollbackListener`

On `ChainSync.RollBackward { peer, point }`:

- Call `rewind point` on the peer's `HeaderStateHistory`.
- If `rewind` returns `Nothing` (the point is before the seeded anchor), remove
  the peer's entry — it will be re-seeded on the next header from that peer.
- Otherwise replace the entry with the rewound history.

### `peerDisconnectedListener`

On `PeerManager.PeerDisconnected { peer }`:

- Remove the peer's entry from state.

Without this cleanup, disconnected peers accumulate in the map indefinitely.

## Changes to `Main.hs`

```haskell
. evalState @(Map Peer (HeaderStateHistory CardanoBlock)) mempty
. runPubSub @InvalidHeaderReceived
```

`HeaderValidation.component` is added to `runSystem` alongside
`ChainDB.component`.

## Open questions

**Beyond-horizon headers during initial sync**

While the ChainDB is catching up from a Mithril snapshot, its ledger tip lags
the network tip and every incoming header will be beyond the forecast horizon.
This is expected — a debug log noting the slot and current horizon is sufficient.
In steady state this cannot occur: the horizon is ~36 hours ahead of the local
tip and peers do not produce headers that far in the future.

**`InvalidHeaderReceived` and Sentry**

A `HeaderProtocolError` (stage 2: invalid VRF / not a valid slot leader) is a
strong adversarial signal. `Sentry` subscribing to `InvalidHeaderReceived` and
emitting `AdversarialBehavior` is a natural follow-on, left for a later stage.
