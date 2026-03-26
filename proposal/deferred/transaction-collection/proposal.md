# Transaction Collection

## Motivation

The original hoarding node proposal included transaction collection as a
deliverable. It was deferred because observing mempool transactions via the
TxSubmission mini-protocol requires the hoarding node to accept inbound
connections — other peers submit transactions to it, not the other way around.
Without a deployed node reachable from the network, there are no inbound peers
and therefore no transactions to collect.

This proposal depends on the infrastructure proposal: once a deployment is in
place and the hoarding node is reachable as a relay, transaction collection
becomes feasible.

## Depends on

- [Infrastructure](../../live-network-deployment/proposal.md) — a live deployment
  accepting inbound connections is a prerequisite.
