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

## Investigation

The prototype should be run against mainnet with extended observation windows.
The following angles are worth examining if no promotions are observed:

- **Instrumentation** — log whether connected peers are advertising
  `InitiatorAndResponderDiffusionMode` in the handshake. If peers are
  advertising initiator-only mode, they will never promote the connection
  regardless of our advertisement.
- **Peer governor constraints** — a remote peer will only promote a duplex
  connection once it has a free slot in `targetNumberOfActivePeers` (~5 in sync
  mode). If all hot slots are occupied, our connection sits in `inboundDuplexPeers`
  but is never activated. Running against more peers or waiting longer may be
  sufficient.
- **ouroboros-network source** — compare the observed connection lifecycle
  against the inbound governor logic in `ouroboros-network` to confirm the
  promotion path is exercised as expected.

## Depends on

- [Infrastructure](../live-network-deployment/README.md) — required only if
  the connection promotion path (below) is ruled out.
