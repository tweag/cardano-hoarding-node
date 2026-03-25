# Header Validation

## Related

- [embedded-consensus.md](embedded-consensus.md) — motivation and overview;
  read that first
- [embedded-consensus-hoard-design.md](embedded-consensus-hoard-design.md) —
  ChainDB component and effect design this feature builds on

## Overview

The hoarding node's ChainSync client currently collects headers from peers
without validating them. Before a block body is downloaded, its header could
already be provably invalid — wrong slot leader, bad VRF proof — and there is
currently no way to detect or record this.

This proposal adds a `HeaderValidation` component that validates each incoming
header against the embedded ChainDB's ledger state. Invalid headers are recorded
and published as `InvalidHeaderReceived` events before the corresponding block
body is ever fetched. It depends on the `ChainDB` effect being available (Stage 1
of the embedded consensus rollout) but is otherwise independent of the
OrphanDetection and ImmutableTip migration stages.

## Why validate headers separately from blocks?

When a block arrives via BlockFetch, the ChainDB validates it fully (Stages 1–3)
and emits `BlockRejected` on failure. Header validation covers a different
moment: headers arrive via ChainSync *before* block bodies are fetched. A
ChainSync peer can advertise a header for a block that doesn't exist or was
never honestly produced, and the hoarding node would currently download the
body before discovering the header was invalid.

Validating at the header stage:

- Detects stage 1 (envelope) and stage 2 (consensus/VRF) invalidity earlier,
  without waiting for a block body
- Associates the invalidity with a specific **peer** — `BlockRejected` knows
  which block was bad, but `InvalidHeaderReceived` knows which peer advertised it
- Avoids unnecessary BlockFetch requests for headers that are already known to
  be invalid

## Per-peer state

Header validation is stateful and **per-peer**. Each peer proposes a potentially
different candidate chain, and validating a header requires knowing the
consensus state accumulated by the headers that preceded it on *that peer's*
chain (e.g. the evolved VRF and KES keys, the nonce). A shared global state
would conflate different chains.

When a new peer is seen for the first time, its header state is seeded from the
local chain at the intersection point — the last block where both chains agree.
As headers arrive, the per-peer state is updated. On rollback it is rewound. On
disconnect it is discarded.

This state is bounded in size (one entry per connected peer) and has no
persistent storage requirement.

## New event

```haskell
data InvalidHeaderReceived = InvalidHeaderReceived
    { peer  :: Peer
    , point :: CardanoPoint
    , error :: HeaderError CardanoBlock
    }
```

`HeaderError` distinguishes the same two stages as the header-level variants of
`BlockRejected`:

| Error | Stage | Meaning |
|---|---|---|
| `HeaderEnvelopeError` | 1 | Wrong block/slot number, broken chain linkage |
| `HeaderProtocolError` | 2 | Invalid VRF proof, not a valid slot leader |

A `HeaderProtocolError` from a peer is a strong adversarial signal — the peer
is advertising a block that no honestly-elected slot leader could have produced.

## Forecast horizon

Header validation requires a *ledger view* — a projection of the current ledger
state containing the stake distribution and protocol parameters needed to verify
the slot leader claim. This view is only defined within a bounded window ahead
of the current tip (~36 hours on mainnet). Headers beyond this window cannot be
validated and are silently skipped, matching the full node's behaviour.

During the initial sync from a Mithril snapshot, the ChainDB's ledger tip lags
the network, so many incoming headers will be beyond the horizon until the
ChainDB catches up. This is expected and not a fault condition.

## Open questions

**Sentry integration**

A `HeaderProtocolError` is a natural trigger for `Sentry` to emit
`AdversarialBehavior`. This is left for a later stage once `InvalidHeaderReceived`
has been observed in practice.

**Relationship to BlockFetch filtering**

If a header is known to be invalid, the block fetch request for it is wasteful.
Whether to suppress BlockFetch requests for headers that have already produced
`InvalidHeaderReceived` events is a follow-on optimisation, not required for
correctness.
