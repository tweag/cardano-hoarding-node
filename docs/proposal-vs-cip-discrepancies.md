# Proposal vs CIP Discrepancies

This document tracks concerns from `proposal.md` that are either absent or incorrectly represented in `CIP-XXXX.md`.

## Status Legend
- ❌ ABSENT - Not mentioned in CIP
- ⚠️ CONTRADICTORY - CIP contradicts proposal
- ⚠️ UNDERSTATED - CIP mentions but doesn't emphasize enough
- ⚠️ INCOMPLETE - CIP partially addresses but missing key details
- ✅ RESOLVED - Issue has been addressed

---

## 1. Storage Bounds Against Adversarial Behavior ✅ RESOLVED

**Proposal (line 27-28):**
> "Adversarial behavior (such as block equivocation) causes the amount of storage to be trivially unbounded, so appropriate heuristics are to be implemented (such as only storing at most a bounded amount of blocks per election opportunity)."

**CIP Status:** ✅ Added new "Storage Bounds" section in Hoarding Processes (CIP-XXXX.md:178-195) that documents:
- Equivocation protection via `max_blocks_per_slot_leader = 10` default
- Limit enforced per `(slot_number, pool_id)` pair
- Rationale for the chosen bound
- Implementation guidance for handling excess blocks

**Resolution:** CIP now specifies storage bounds to prevent adversarial equivocation attacks.

---

## 2. Default Storage Strategy ✅ RESOLVED

**Proposal (line 26-27):**
> "By default, only headers/blocks/transactions are stored that do not end up on the historical chain, minimizing the storage cost in the optimistic case."

**CIP Status:** ✅ Added design decision in Rationale section (CIP-XXXX.md:719-728) documenting the intentional difference:
- **Choice**: Store-all-then-filter approach (differs from proposal)
- **Rationale**: Simpler collection logic, no validation dependency during collection, better separation of concerns
- **Storage Trade-off**: Temporary storage increase only until background workers classify/evict (proportional to processing lag)
- **Note**: Explicitly acknowledges deviation from original proposal

**Resolution**: CIP now documents why it differs from the proposal and clarifies the temporary nature of additional storage costs.

---

## 3. Opt-out Mechanism for Stake Pools ⚠️ OPEN QUESTION

**Proposal (line 33):**
> "Still, it is possible to allow honest stake pools to opt-out of being connected to by a particular deployment of a hoarding node."

**CIP Status:** ⚠️ Added as open question in "Peer Opt-out Mechanism" section (CIP-XXXX.md:232-238):
- Questions about implementation approach (configuration vs. dynamic mechanism)
- In-protocol signaling vs. out-of-band opt-out
- Ethical and governance considerations
- Standardized registry vs. independent handling

**Status**: Design question requires further discussion with team. No specific implementation proposed yet, but concern is now documented for future resolution.

---

## 4. Best-Effort Recording Limitation ✅ RESOLVED

**Proposal (lines 35-36):**
> "Finally, it is conceptually unavoidable that adversarial behavior can only be recorded on a best-effort basis by the hoarding node, e.g. because adversaries can use heuristics to serve different blocks/transactions to hoarding nodes compared to honest stake pool relays."

**CIP Status:** ✅ Addressed in "Peer Opt-out Mechanism" open question (CIP-XXXX.md:239):
- Added note that any opt-out mechanism could be abused by adversarial participants to avoid monitoring
- Highlights the fundamental tension between respecting operator preferences and comprehensive security monitoring
- Acknowledges that adversaries could use opt-out to hide protocol violations

**Resolution:** The limitation is now documented in context of the opt-out mechanism discussion, where it's most relevant.

---

## 5. Inbound Connection Capability ✅ RESOLVED

**Proposal (line 25):**
> "establishing both outbound (for headers/blocks) and inbound connections (for transactions)"

**CIP Status:** ✅ Added design decision in Rationale section (CIP-XXXX.md:739-746):
- **Choice**: Outbound-only connections (no inbound)
- **Rationale**: Node-to-Node connections are bidirectional for protocol purposes - peers can push transactions/blocks over outbound connections
- Broad peer discovery provides sufficient coverage including malicious/buggy nodes
- Simpler deployment, lower attack surface

**Resolution**: CIP explicitly chooses outbound-only and explains why inbound connections are unnecessary, differing from the original proposal.

---

## 6. Load Equivalence to Normal Relay ✅ RESOLVED

**Proposal (line 33):**
> "We stress that the load of such a hoarding node on honest nodes is equal to that of any node following the chain."

**CIP Status:** ✅ Documented in "Coordinated Peer Selection" design decision (CIP-XXXX.md:702-711):
- Hoarding processes and full node connect to disjoint peer sets
- Avoids double connection slot usage (prevents both systems from connecting to same peer)
- **Achieves 1:1 connection slot usage equivalent to a normal relay node**
- Each peer experiences load from only one connection, just like with a standard relay

**Resolution:** CIP explicitly documents that the coordinated peer selection strategy ensures load equivalence to a normal relay node, fulfilling the proposal's emphasis on network-friendly behavior.

---

## 7. Expensive-to-Validate Block Detection ✅ RESOLVED

**Proposal (lines 29-30):**
> "This also opens the opportunity to automatically detect and flag unusual (e.g. expensive-to-validate) blocks/transactions, enriching the monitoring capabilities"

**CIP Status:** ✅ Added "Expensive-to-Validate Detection" section in Background Workers (CIP-XXXX.md:674-710):
- **Measurement approach**: Background workers time validation queries to full node
- **Configuration**: `expensive_validation_threshold_seconds: 5` parameter to flag slow validations
- **Metadata storage**: Validation timing stored via Repository Service APIs
- **Use cases**: DoS vector detection, SPO behavior analysis, network health monitoring
- **Cost factors**: Documents why blocks/transactions vary in validation cost (Plutus scripts, transaction count, multi-sig, UTxO count, etc.)
- **Limitations**: Acknowledges best-effort nature (caching, network latency, concurrent load)
- **Future enhancement**: Documents option for dedicated lightweight validation service for higher-fidelity measurements

**Resolution:** CIP now specifies how expensive-to-validate blocks/transactions are detected, flagged, and stored for analysis.

---

## 8. Progressive Validation Strategy ✅ RESOLVED

**Proposal (lines 28-30):**
> "A simple initial prototype will not do any validation whatsoever, and hence rely on a separate full node to maintain its selection for diffusion to downstream peers. Further refinements include header and even block validation"

**CIP Status:** ✅ Added design decision in Rationale section (CIP-XXXX.md:748-756):
- **Choice**: Hoarding processes never perform validation (permanent design, not phased approach)
- **Rationale**: Keeps processes lightweight, stateless; validation requires full ledger state; no benefit since processes don't make forwarding decisions
- **Alternative**: Progressive validation approach (as proposal suggests) - rejected as unnecessary complexity
- Updated Implementation Plan (line 787) to clarify validation is always via background workers + full node

**Resolution**: CIP explicitly documents that validation is permanently offloaded to background workers, differing from the proposal's suggestion of progressive validation enhancement.

---

## 9. Metrics for Observability Frameworks ✅ RESOLVED

**Proposal (line 31):**
> "exposed in an easy-to-consume API (e.g. metrics for observability frameworks, and an HTTP API)"

**CIP Status:** ✅ Added "Observability Endpoints" section with `/metrics` endpoint (CIP-XXXX.md:561-598):
- Prometheus exposition format for monitoring and alerting
- **Collection metrics**: blocks/transactions collected by status
- **Peer metrics**: active connections, connection/disconnection totals
- **Violation metrics**: violations detected by type, equivocations by pool
- **Processing metrics**: operation duration histograms, unprocessed block gauge, database operation counts
- **Storage metrics**: database size by table, eviction counts, storage limit hits
- **System health**: process count, last classification timestamp, service availability

**Resolution:** CIP now specifies comprehensive Prometheus-compatible metrics endpoint for integration with observability frameworks like Grafana.

---

## 10. Connection Strategy for Maximum Coverage ✅ RESOLVED

**Proposal milestone (lines 67-68):**
> "Node connection logic: Logic for deciding which nodes to connect to in order to maximise the chances of seeing all entities in a network"

**CIP Status:** ✅ Documented in Behavior section (CIP-XXXX.md:153-159) and added PEERS table (lines 289-297):
- **Strategy**: Iterative expansion via Peer Sharing Protocol
- Store discovered peers in database for persistent tracking
- Periodically spawn new hoarding processes connecting to undiscovered peers
- Continuous network coverage expansion over time
- **PEERS table**: Tracks peer_id, address, discovery time, connection status

**Resolution**: CIP now specifies concrete strategy for maximizing network coverage through iterative peer discovery and process spawning.

---

## 11. Bounded Blocks Per Election Opportunity ✅ RESOLVED

**Proposal (line 28):**
> "such as only storing at most a bounded amount of blocks per election opportunity"

**CIP Status:** ✅ Addressed in Storage Bounds section (CIP-XXXX.md:178-195):
- **Configuration**: `max_blocks_per_slot_pool = 10` (default)
- Limits storage per `(slot_number, pool_id)` pair
- An election opportunity = slot where a pool wins leadership
- Prevents unbounded storage from equivocation attacks
- Implementation guidance included

**Resolution**: CIP now specifies bounded storage per election opportunity (slot/pool combination) to prevent adversarial storage exhaustion.

---

## Summary Statistics

- **Resolved:** 9 items (#1, #2, #4, #5, #6, #8, #9, #10, #11)
- **Open Questions:** 1 item (#3)
- **Still Need Work:** 1 item (#7)

**Total Issues:** 11
**Completion:** 82% (9/11 resolved)
