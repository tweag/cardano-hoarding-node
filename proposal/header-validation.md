# Header Validation

## Related

- [embedded-consensus.md](embedded-consensus.md) — motivation and overview;
  read that first
- [embedded-consensus-hoard-design.md](embedded-consensus-hoard-design.md) —
  ChainDB component and effect design this feature builds on

## Overview

The hoarding node's ChainSync client currently collects headers from peers
without validating them. This document describes a `HeaderValidation` component
that validates each incoming header using the embedded ChainDB's ledger state,
detecting invalid consensus proofs before block bodies are downloaded.

This is Stage 1b, sitting between Stage 1 (ChainDB running) and Stage 2
(OrphanDetection). It depends on the `ChainDB` effect being enabled and its
`LedgerDB` being populated.

## Background: HeaderStateHistory

Validating a header requires more than just a ledger view — it requires the
*previous* validated header's consensus state, because the consensus protocol
(Praos) carries chain-dependent state across blocks (e.g. evolved VRF and KES
keys, nonce accumulation). This state is captured by `HeaderState`:

```haskell
data HeaderState blk = HeaderState
    { headerStateTip      :: WithOrigin (AnnTip blk)
    , headerStateChainDep :: ChainDepState (BlockProtocol blk)
    }
```

`headerStateChainDep` for Praos holds the current epoch nonce and the evolved
operational certificate state needed to verify the next block's KES signature.

`HeaderStateHistory` is an anchored sequence of these snapshots — one per
validated header in a chain:

```haskell
newtype HeaderStateHistory blk = HeaderStateHistory
    { unHeaderStateHistory ::
        AnchoredSeq
            (WithOrigin SlotNo)
            (HeaderStateWithTime blk)
            (HeaderStateWithTime blk)
    }
```

The sequence supports `rewind` (for rollbacks) and `trim` (to bound memory
use). Validation appends to the sequence:

```haskell
-- From Ouroboros.Consensus.HeaderStateHistory
validateHeader
    :: (BlockSupportsProtocol blk, ValidateEnvelope blk)
    => TopLevelConfig blk
    -> LedgerView (BlockProtocol blk)
    -> Header blk
    -> RelativeTime          -- wall-clock time of the header's slot
    -> HeaderStateHistory blk
    -> Except (HeaderError blk) (HeaderStateHistory blk)
```

It ticks the last `HeaderState` to the new slot using the `LedgerView`, then
calls the underlying `HeaderValidation.validateHeader`, and appends the result.

**`HeaderStateHistory` is per-peer.** Each peer proposes a potentially
different candidate chain; the state must track *their* sequence of headers, not
the local chain's. When a new peer is first seen, its history is seeded from the
current local chain's header state history (via `ChainDB.getHeaderStateHistory`)
— the right anchor because the intersection point is where our chains agree.

## New ChainDB effect operations

Two operations are added to the `ChainDB` effect:

```haskell
data ChainDB :: Effect where
    FeedBlock             :: CardanoBlock -> ChainDB m ()
    GetImmutableTip       :: ChainDB m CardanoPoint
    GetHeaderStateHistory :: ChainDB m (HeaderStateHistory CardanoBlock)
    GetLedgerForecast
        :: SlotNo
        -> ChainDB m (Maybe (LedgerView (BlockProtocol CardanoBlock)))
```

**`GetHeaderStateHistory`** maps directly to `CDB.getHeaderStateHistory` (an
`STM` action on the open handle). It returns the history for the current local
chain, used to seed state for a newly seen peer.

**`GetLedgerForecast slot`** derives a ledger view forecast from the current
ledger state:

```haskell
GetLedgerForecast slot -> liftIO $ atomically $ do
    ledger <- CDB.getCurrentLedger chainDB
    let forecast = ledgerViewForecastAt (configLedger cfg) (ledgerState ledger)
    pure $ case runExcept (forecastFor forecast slot) of
        Right lv                    -> Just lv
        Left OutsideForecastRange{} -> Nothing
```

`Nothing` is returned when the slot is beyond the forecast horizon (~3k/f slots
ahead of the current tip). The caller skips validation silently in that case,
matching the full node's behaviour.

## New event

```haskell
-- A header received via ChainSync failed consensus validation.
-- Distinct from BlockRejected: the block body may never arrive.
data InvalidHeaderReceived = InvalidHeaderReceived
    { peer  :: Peer
    , point :: CardanoPoint
    , error :: HeaderError CardanoBlock
    }
```

`HeaderError` distinguishes the same two header-level stages as
`ExtValidationErrorHeader` in `BlockRejected`:

| `HeaderError` variant | Meaning |
|---|---|
| `HeaderEnvelopeError` | Wrong block/slot number, prev-hash mismatch |
| `HeaderProtocolError` | Invalid VRF proof, invalid slot leader claim |

## HeaderValidation component

```
state     — Map Peer (HeaderStateHistory CardanoBlock)
listeners — [validateHeaderListener, rollbackListener, peerDisconnectedListener]
triggers  — []
```

Effect constraints:

```haskell
component
    :: ( ChainDB.ChainDB :> es
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
   horizon), return — the header is too far ahead of our current tip to validate.
2. Look up the peer's `HeaderStateHistory` in state. If absent, seed it with
   `ChainDB.getHeaderStateHistory`.
3. Derive `RelativeTime` for the slot from the era summary embedded in the
   Cardano `LedgerView` (`HardForkLedgerView` carries a `Summary`; use
   `slotToRelativeTime summary slot`).
4. Call `HeaderStateHistory.validateHeader cfg ledgerView header slotTime history`.
5. On success: update the peer's entry in state with the new history.
6. On failure: log a warning and publish `InvalidHeaderReceived`.

### `rollbackListener`

On `ChainSync.RollBackward { peer, point }`:

- Call `rewind point` on the peer's `HeaderStateHistory`.
- Replace the peer's entry in state with the rewound history, or remove it if
  `rewind` returns `Nothing` (the point is before our seeded anchor).

### `peerDisconnectedListener`

On `PeerManager.PeerDisconnected { peer }`:

- Remove the peer's entry from state.

This bounds the size of the map; without cleanup, disconnected peers would
accumulate indefinitely.

## Changes to Main.hs

```haskell
-- New evalState for per-peer header validation state
. evalState @(Map Peer (HeaderStateHistory CardanoBlock)) mempty

-- New pub/sub channel
. runPubSub @HeaderValidation.InvalidHeaderReceived
```

`HeaderValidation.component` is added to `runSystem` alongside
`ChainDB.component`, gated on the same `chainDBCfg.enabled` check.

## Open questions

**`RelativeTime` computation**

`HardForkLedgerView` for Cardano includes the `EraParams` summary needed to
convert a slot number to a `RelativeTime`. The exact extraction is:
`slotToRelativeTime (hardForkSummary ...) slot`. The right place for this
conversion is inside `GetLedgerForecast` in the interpreter, returning a
`(LedgerView, RelativeTime)` pair, so the component does not need to know about
era summaries directly.

**Beyond-horizon headers**

Beyond-horizon headers are expected and normal during initial sync: while the
ChainDB is catching up from a Mithril snapshot, its ledger tip lags the network
tip and every incoming header will be beyond the forecast horizon until the
ChainDB catches up. In steady state this case cannot occur in practice — the
forecast horizon is ~3k/f slots ahead of the local tip (~36 hours on mainnet),
and peers do not produce headers that far in the future.

A debug log line noting the slot and forecast horizon is appropriate for
observability (useful for tracking sync progress) without implying a fault.

**`InvalidHeaderReceived` and Sentry**

A header with `HeaderProtocolError` (stage 2: invalid VRF / not a valid slot
leader) is a strong signal of adversarial behaviour. `Sentry` subscribing to
`InvalidHeaderReceived` and emitting `AdversarialBehavior` is a natural
follow-on, left for a later stage.
