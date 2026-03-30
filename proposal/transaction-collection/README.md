# Transaction Collection — Work Package

## Work Package Name

Transaction Collection

---

## High-level Description

The hoarding node connects to multiple Cardano peers simultaneously and records the blocks and headers they advertise. It does not currently observe mempool transactions: the transactions peers hold in their mempools, not yet included in any block, are invisible to the node.

Collecting mempool transactions requires running the TxSubmission2 mini-protocol in server (responder) role — peers push transaction IDs to the server, which pulls the full transactions. Transaction collection was deferred from the first cycle due to complexity and the absence of a live deployment; at the time, the prevailing understanding was that the server role requires accepting inbound connections from publicly reachable peers.

Since then, a further path has been identified. The Ouroboros network specification (§5) describes *duplex* connections: a single TCP connection can carry mini-protocols in both directions simultaneously. When the hoarding node advertises `InitiatorAndResponderDiffusionMode`, remote peers' outbound governors may promote the hoarding node's outbound connections to duplex, enabling them to run TxSubmission toward us on the same connection we opened. No listening socket or publicly reachable deployment is required.

A prototype implementing this approach has been built. It has been run against preprod but has not yet witnessed any promoted connections, so it remains unvalidated end-to-end. The cause is unknown — it may be a bug in the prototype, peer-side configuration of `targetNumberOfActivePeers`, or preprod peers simply not promoting duplex connections to non-relay nodes in practice. Resolving this uncertainty is the first milestone of this work package.

If promotion is confirmed not to work reliably in practice, the fallback is inbound connections via a publicly reachable deployed node, which depends on the infrastructure work package.

**Deliverables:**
- Mempool transactions received from peers recorded in the hoarding database with attribution to the peer that submitted them.
- Transactions queryable via the HTTP API, filterable by peer and time window.
- A confirmed working path to receive transactions — either via connection promotion (no infrastructure dependency) or via inbound connections (infrastructure dependency).

---

## Core Objectives

- **Collect mempool transactions from peers** — observe transactions before they are included in blocks, attributed to the specific peer that advertised them.
- **Confirm or rule out connection promotion** — determine definitively whether outbound connections can be promoted to duplex in practice, resolving the open question left by the prototype.
- **Persist and expose transaction data** — store received transactions in the hoarding database and expose them via the HTTP API with peer attribution.

---

## Expected Value

Mempool transactions are the earliest observable signal of user and protocol activity on the Cardano network. Collecting them fills the gap between what the hoarding node currently records (blocks and headers, after block production) and the full picture of network activity (transactions, before inclusion).

Transaction data has direct value for network analysis: propagation timing (how long before a transaction appears in a block), duplicate submission patterns (same transaction from multiple peers), and anomalous transaction rates attributable to specific peers. It also enables cross-referencing mempool data with block data — observing which transactions from which peers ended up in which blocks.

The connection promotion path, if confirmed working, eliminates the infrastructure prerequisite entirely. The node could collect transactions from its existing outbound connections without any deployment changes.

---

## Metrics for Success

- Mempool transactions received, stored, and queryable via the HTTP API.
- At least one transaction observed from at least one peer — confirming the end-to-end pipeline works.
- Connection promotion outcome documented: either confirmed working (with evidence of promoted connections) or ruled out (with a diagnosis and fallback in place).

---

## Strategic Alignment

**Pillar:** Pillar 1 — Infrastructure & Research Excellence (Focus Area I.2: Threat Detection & Recovery)

**Rationale:** Mempool visibility completes the network observability picture. Blocks record what was accepted; transactions record what was attempted. Anomalous transaction patterns — spam, invalid submissions, coordinated flooding — are detectable at the mempool stage before they affect block production.

**KPI support:** Expands the range of observable network events, supporting both threat detection and research into transaction propagation dynamics.

---

## Classification

- **New initiative or continuation of existing:** Continuation (originally scoped in the first proposal cycle, deferred)
- **Primary nature:** Technical

---

## Milestones

### Milestone 1 — Connection Promotion Investigation

**Deliverables:**
A clear answer to whether outbound connections are promoted to duplex in practice. If they are: the prototype is productionised and this milestone delivers a working transaction collection pipeline. If they are not: a diagnosis of why (peer-side configuration, peer behaviour, prototype bug) and a documented decision on the fallback path (inbound connections via infrastructure deployment).

**Acceptance criteria:**
Either: at least one promoted connection observed and transactions received, confirming the duplex path works. Or: a written diagnosis explaining why promotion does not occur in practice, with a decision to proceed via inbound connections instead.

**Estimated duration:** 2 weeks

---

### Milestone 2 — TxSubmission Pipeline

**Deliverables:**
The `TxReceived` event pipeline is wired into persistence. Received transactions are stored in the hoarding database with peer attribution, timestamp, and transaction content (serialised). Duplicate suppression is applied — the same transaction ID received from multiple peers is stored once with multiple peer attributions.

**Acceptance criteria:**
Transactions received from peers appear in the database. Per-peer attribution is correct. Duplicate transactions from different peers are deduplicated by transaction ID.

**Estimated duration:** 3 weeks

---

### Milestone 3 — API Exposure

**Deliverables:**
Received transactions are queryable via the HTTP API. Endpoints support filtering by peer, time window, and transaction ID.

**Acceptance criteria:**
Transactions are queryable via the API. Results are attributable to specific peers. Integration test confirms end-to-end: transaction received → stored → queryable.

**Estimated duration:** 1 week

---

## Budget

| Cost Category | Description | Quantity | Unit Cost (ADA) | Total (ADA) |
|---|---|---|---|---|
| Resources (Labour) | Software engineer | 2 FTE × 1.5 months | TBD | TBD |

---

## Supporting Material

- [proposal.md](proposal.md) — prototype implementation guide: duplex connection approach, pitfalls, and caveats
- [Infrastructure work package](../live-network-deployment/README.md) — prerequisite if connection promotion is ruled out
