# Transaction Collection

## Motivation

The original hoarding node proposal included transaction collection as a
deliverable. It was deferred on the assumption that observing mempool
transactions via the TxSubmission mini-protocol requires the hoarding node to
accept inbound connections. That assumption was incorrect.

The Ouroboros network specification (§5) describes *duplex* connections: a
single TCP connection can carry mini-protocols in both directions. When the
hoarding node advertises `InitiatorAndResponderDiffusionMode`, remote peers may
promote its outbound connections to duplex, enabling TxSubmission toward us
without a listening socket or public deployment.

A prototype of this approach is implemented (see below). It has been run against
preprod but has not witnessed any promoted connections. The cause is unknown. If
promotion cannot be made to work reliably, the fallback is inbound connections
via a publicly reachable deployed node.

## Promotion path

Advertising duplex mode is necessary but not sufficient for TxSubmission to
run. For a remote peer to push transactions to us, its *outbound governor* must
promote us through the full peer lifecycle:

1. **Cold** — the peer knows of us but has no active outbound mini-protocols.
   Whether we enter the cold pool at all is up to the peer's internal sharing
   between its inbound and outbound governors; it is not guaranteed.
2. **Warm** — the peer runs its established/warm mini-protocols toward us
   (`KeepAlive`, `PeerSharing`). This requires a free slot in the peer's warm
   pool and the peer selecting us over other cold candidates.
3. **Hot** — the peer runs its hot mini-protocols toward us, including
   `TxSubmission`. This again requires a free hot slot and us being selected
   over other warm candidates.

At each step the peer may have saturated pools and simply never select us. In
practice, a relay running in sync mode targets ~5 hot peers; with many
candidates available, promotion is not guaranteed and may take a long time.

## Investigation

The prototype should be run against mainnet with extended observation windows.
The following angles are worth examining if no promotions are observed:

- **Instrumentation** — log whether connected peers are advertising
  `InitiatorAndResponderDiffusionMode` in the handshake. If peers are
  advertising initiator-only mode, they will never promote the connection
  regardless of our advertisement.
- **Promotion chain** — confirm we are entering peers' cold pool and being
  promoted to warm before investigating the hot step. The ouroboros-network
  inbound governor logs should show our connection moving through states.
- **ChainSync server behaviour** — peers judge a connection "interesting" based
  on the frequency of `ChainSync` messages and whether the node has blocks to
  fetch. The current stub drains the channel silently; this may cause peers to
  treat us as uninteresting and rotate us out before reaching hot. A minimal
  `ChainSync` server that serves headers up to the immutable tip — using the
  embedded `ChainDB` — may be required to remain a viable hot candidate.
- **ouroboros-network source** — compare the observed connection lifecycle
  against the inbound governor logic in `ouroboros-network` to confirm the
  promotion path is exercised as expected.

## Caveats

**Connection lifetime and peer churn.** Even once promoted to hot, peers
regularly rotate their active connections. If we are disconnected before
collecting a useful number of transactions, that is normal churn, not a
bug — but it does bound the per-connection collection window.

**Loopback.** Once Hoard runs mini-protocol servers, `PeerSharing` responses
from adversarial peers could advertise Hoard's own address, causing it to
connect to itself. Incoming connections from our own address should be filtered.

## Depends on

- [Infrastructure](../live-network-deployment/README.md) — required only if
  the connection promotion path (below) is ruled out.
