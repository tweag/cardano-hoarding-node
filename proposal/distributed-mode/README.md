# Distributed Mode — Work Package

## Work Package Name

Distributed Mode

---

## High-level Description

The hoarding node is an observer on the Cardano network. It connects to multiple Cardano peers simultaneously, collects blocks and block headers as they propagate, and records them in a database. This data enables detection of adversarial behaviour — such as blocks produced by illegitimate slot leaders, conflicting chain forks, or invalid transactions — as well as analysis of how data propagates across the network over time.

In its current form, the hoarding node runs as a single process in a single location. This limits its ability to measure how quickly data reaches different parts of the network — an observer in one region cannot capture propagation timing from other vantage points across the global network.

This work package implements distributed mode: a deployment architecture in which multiple lightweight collectors, running in different geographical regions, forward their observations to a central coordinator for processing and storage. The collector is responsible only for connecting to Cardano peers and forwarding what it sees. The coordinator receives data from all collectors, runs the processing pipeline (anomaly detection, classification, persistence), and exposes it via the HTTP API.

Collector-coordinator communication is built on the same `typed-protocols`, `network-mux`, and `ouroboros-network-framework` stack that Cardano itself uses for peer-to-peer communication. This is not a custom messaging layer — it reuses proven infrastructure from the Cardano node codebase.

The existing single-node deployment mode is fully preserved. Operators who do not need multi-region coverage continue to run a single `hoard` binary with no configuration changes. Distributed mode is an opt-in deployment topology, not a replacement.

**Deliverables:**
- Three executables: `hoard-collector`, `hoard-coordinator`, and `hoard` (single-node, combining both roles).
- A typed protocol layer covering the operations required in distributed mode (for example, peer assignment).
- NixOS modules and OCI images for both roles, deployable independently.
- Integration tests covering both happy-path and coordinator-disconnect scenarios.

---

## Core Objectives

- **Multi-region network observation** — the hoarding node can be deployed with collectors in multiple geographical regions, enabling simultaneous observation of the network from several vantage points.
- **Propagation timing measurement** — with collectors in different regions, it becomes possible to measure and compare how quickly blocks and headers reach each location, producing timing data that a single-node deployment cannot provide.
- **Clean separation of data collection from processing** — the collector/coordinator split makes each role independently deployable and maintainable: collectors gather and forward raw data, the coordinator processes it. No processing logic runs on collectors; no network mini-protocols run on the coordinator.
- **No disruption to existing deployments** — single-node mode is preserved as a first-class deployment topology. Operators who do not need distributed deployment are unaffected.

---

## Expected Value

The primary value is a richer and more accurate picture of how the Cardano network behaves in practice.

A single observer can detect anomalous blocks and record when it saw them, but cannot determine whether a given block reached the rest of the network quickly, slowly, or not at all. With collectors in multiple regions, the hoarding node produces propagation timing data: how long it takes for a block or header to reach each vantage point after first being seen. While the Cardano Foundation's blockperf project collects some propagation data from volunteer operators, no publicly accessible, structured, multi-regional dataset exists. The hoarding node's approach — dedicated observer nodes connecting as full peers — provides a complementary and more controlled source of this data.

The Cardano Foundation has expressed direct interest in this capability, specifically for deploying observers across regions to study propagation patterns. This proposal delivers the infrastructure that makes such deployments possible.

The architectural approach also has value beyond this project. Building the collector-coordinator protocol layer on `typed-protocols` and `ouroboros-network-framework` — the same stack used by the Cardano node itself — means the implementation follows established patterns in the ecosystem, is auditable by anyone familiar with the Cardano networking codebase, and does not introduce a bespoke messaging layer that would need to be maintained independently.

---

## Metrics for Success

- Two collectors running in separate geographical regions connect to a single coordinator
  and forward data; blocks and headers from both appear in the same hoarding database.
- Single-node mode (`hoard`) behaves identically to the current deployment — no changes
  required from existing operators.
- Processing components (`Persistence`, `OrphanDetection`, `Sentry`, `Monitoring`) require
  no modifications; the collector/coordinator split is transparent to them.
- Protocol layer is covered by integration tests using the in-process bearer, exercising
  both happy-path and coordinator-disconnect scenarios.

---

## Strategic Alignment

**Pillar:** Pillar 1 — Infrastructure & Research Excellence (Focus Area I.2: Threat Detection & Recovery)

**Rationale:** Expands network observability to multiple geographical regions, enabling propagation timing measurement a single node cannot provide. Multiple vantage points provide broader and earlier detection of network anomalies.

---

## Classification

- **New initiative or continuation of existing:** Continuation
- **Primary nature:** Technical

---

## Milestones

### Milestone 1 — Collector/Coordinator Split

**Deliverables:**
The codebase is restructured into clean collector and coordinator modules with distinct effect stacks. `Main.hs` is split into separate entry points for `hoard-collector`, `hoard-coordinator`, and `hoard`. Any component that requires effects from both stacks is resolved as part of this milestone.

**Acceptance criteria:**
All three binaries build and the `hoard` single-node binary passes all existing tests with behaviour identical to the pre-split version.

**Estimated duration:** 2 weeks

---

### Milestone 2 — Protocol Layer and Single-node Mode

**Deliverables:**
The typed collector-coordinator protocol layer is implemented over `typed-protocols`, `network-mux`, and `ouroboros-network-framework`. Both roles are connected via the in-process queues bearer (`Network.Mux.Bearer.Queues`).

**Acceptance criteria:**
`hoard` operates end-to-end using the new protocol layer with no regression in behaviour. Integration tests cover the full protocol exchange, including coordinator-disconnect scenarios, and latency/packet-loss scenarios using `Network.Mux.Bearer.AttenuatedChannel`.

**Estimated duration:** 3 weeks

---

### Milestone 3 — Distributed Mode and Deployment

**Deliverables:**
Collector and coordinator deployable as separate networked processes, communicating over TCP, with the collector connecting to a configured remote coordinator address. All three executables packaged with NixOS modules and OCI images. Reference deployment with two collectors connecting to a single remote coordinator. New HTTP API endpoints exposing per-collector propagation timing, derived from existing receipt and peer tables.

**Acceptance criteria:**
Two collectors in separate regions connect to a single coordinator over TCP; data from both appears in the hoarding database. Collector IP is recorded on connection and associated with their receipts. Propagation timing for blocks and headers is queryable via the HTTP API per vantage point. NixOS modules and OCI images are documented and usable by external operators.

**Estimated duration:** 3 weeks

---

## Budget

| Cost Category | Description | Quantity | Unit Cost (ADA) | Total (ADA) |
|---|---|---|---|---|
| Resources (Labour) | Software engineer | 2 FTE × 2 months | TBD | TBD |

---

## Supporting Material

- [proposal.md](proposal.md) — architecture, protocols, effect stacks, open questions
- [mux.md](mux.md) — typed protocol implementation pattern and connection lifecycle
