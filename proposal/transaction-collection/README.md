# Transaction Collection — Work Package

## Work Package Name

Transaction Collection

---

## High-level Description

The hoarding node connects to multiple Cardano peers simultaneously and records the blocks and headers they advertise. It does not currently observe mempool transactions: the transactions peers hold in their mempools, not yet included in any block, are invisible to the node.

Collecting mempool transactions requires running the TxSubmission2 mini-protocol in server (responder) role — peers push transaction IDs to the server, which pulls the full transactions. Transaction collection was deferred from the first cycle due to complexity and the absence of a live deployment; at the time, the prevailing understanding was that the server role requires accepting inbound connections from publicly reachable peers.

Since then, a further path has been identified. The Ouroboros network specification (§5) describes *duplex* connections: a single TCP connection can carry mini-protocols in both directions simultaneously. When the hoarding node advertises `InitiatorAndResponderDiffusionMode`, remote peers' outbound governors may promote the hoarding node's outbound connections to duplex, enabling them to run TxSubmission toward us on the same connection we opened. No listening socket or publicly reachable deployment is required.

A prototype implementing this approach has been built. It has been run against preprod but has not yet witnessed any promoted connections, so it remains unvalidated end-to-end. Advertising duplex mode is necessary but not sufficient: for TxSubmission to run, a remote peer's outbound governor must also promote us through its full peer lifecycle — cold → warm → hot — and TxSubmission only runs at the hot stage. With saturated peer pools and many candidates, promotion is not guaranteed and may simply not have occurred during the preprod test window. The current ChainSync stub (which drains the channel silently) may also cause peers to treat us as uninteresting and rotate us out before reaching hot. Resolving these uncertainties is the first milestone of this work package.

If promotion is confirmed not to work reliably in practice, two fallback paths exist: inbound connections via a publicly reachable deployed node (depends on the infrastructure work package), or `LocalTxMonitor` via a node-to-client connection to a trusted full node (requires privileged access from a cooperative operator, but avoids both the promotion uncertainty and the infrastructure dependency).

A note on coverage: with the duplex promotion path, transactions can only arrive from peers whose outbound governor chooses to run TxSubmission toward us. We cannot solicit transactions from peers we connect to — the protocol flows the other way. Coverage is therefore not fully under our control.

**Deliverables:**
- Mempool transactions received from peers recorded in the hoarding database with attribution to the peer that submitted them.
- Transactions queryable via the HTTP API, filterable by peer and time window.
- A confirmed working path to receive transactions — connection promotion, inbound connections, or `LocalTxMonitor` — with the chosen approach documented.

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

**Objective:** Confirm a working transaction collection path — either duplex promotion or a documented fallback — so development can proceed.

**Key Outcomes:**

- *Duplex path confirmed:* ≥1 outbound connection promoted; transactions received from ≥1 peer.
- *Fallback path confirmed:* Root cause of promotion failure documented; fallback chosen (inbound connections or `LocalTxMonitor`).

**Impact:** A clear, actionable answer on connection behaviour; primary uncertainty blocking the work package resolved.

**Estimated duration:** 2 weeks

---

### Milestone 2 — TxSubmission Pipeline

**Objective:** Transactions received via the chosen collection path are durably persisted with full peer attribution and deduplication, regardless of which path (duplex promotion, inbound connections, or `LocalTxMonitor`) was confirmed in Milestone 1.

**Key Outcomes:**

- *Transactions persisted:* Received transactions are stored in the hoarding database with peer attribution, timestamp, and serialised transaction content.
  - KPI (optional): ≥1 transaction stored end-to-end via the chosen collection path.

- *Deduplication correct:* The same transaction ID received from multiple peers is stored once with multiple peer attributions — no duplicate rows per transaction ID.

**Impact:** Transaction data is durably captured with full attribution, enabling downstream analysis of propagation, duplication patterns, and peer behaviour. The collection pipeline is production-quality end-to-end.

**Estimated duration:** 3 weeks

---

### Milestone 3 — API Exposure

**Objective:** Transaction data is accessible via the HTTP API with filtering by peer, time window, and transaction ID.

**Key Outcomes:**

- *Transactions queryable:* API returns transactions filterable by peer, time window, and transaction ID.
- *End-to-end pipeline verified:* Integration test confirms transaction received → stored → queryable.

**Impact:** Researchers and operators can query mempool data programmatically with peer attribution.

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
