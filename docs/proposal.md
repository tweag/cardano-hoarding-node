Section 3: Problem Statements and Proposal Benefits
Problem Statement
Blocks and transactions that do not eventually end up on the historical chain are (by design) not permanently stored by the Cardano node. However, for monitoring and investigation purposes, it is often desirable to be able to access such data in an easy manner. Concrete examples include:

Bugs regarding disagreement on block/tx validity between different nodes (example) or crashes due to certain transactions (example). In the future, such bugs might be caused due to subtle differences between different node implementations.
Automatic mempool monitoring for undesirable transactions, such as those causing the June 2024 DoS attack.
Identifying, evidencing, understanding and effectively responding to adversarial behavior, such as induced rollbacks, diffusion of invalid blocks and transactions or block equivocation attacks.
Analytical use cases, such as (apparently non-adversarial) behavior of poorly configured/underresourced nodes (potentially indicating optimization opportunities),  as well as timing of blocks (late/early) and the frequency of slot and height battles.

For this purpose, we propose to design and implement a “hoarding node” that connects to various nodes in the Cardano network and stores all (up to a configurable limit) blocks and transactions, even if they are invalid or orphaned.  
Proposal Benefit
A hoarding node improves the ability to effectively respond to unforeseen situations in the Cardano network, and gives further empirical insights into its complex dynamics.
Does this proposal align to the Product Roadmap and Roadmap Goals?*
Doesn’t directly align
Does your proposal align to any of the categories listed below?*
TBD
Committee Alignment
TSC
Supplementary Endorsement
TBD
Section 4: Proposal Details
Proposal Name
Hoarding Node
Proposal Description
The goal of the proposal is to implement a “hoarding” node dedicated to storing all (up to a configurable limit) observed headers, blocks and transactions. To this end, the hoarding node will act just like a normal relay node in the Cardano P2P network, establishing both outbound (for headers/blocks) and inbound connections (for transactions), just like other normal relay nodes hosted by parties interested in following the chain and allowing inbound connections.

By default, only headers/blocks/transactions are stored that do not end up on the historical chain, minimizing the storage cost in the optimistic case. Adversarial behavior (such as block equivocation) causes the amount of storage to be trivially unbounded, so appropriate heuristics are to be implemented (such as only storing at most a bounded amount of blocks per election opportunity).

A simple initial prototype will not do any validation whatsoever, and hence rely on a separate full node to maintain its selection for diffusion to downstream peers. Further refinements include header and even block validation, recording their validity for easy retrieval. This also opens the opportunity to automatically detect and flag unusual (e.g. expensive-to-validate) blocks/transactions, enriching the monitoring capabilities of a hoarding node.

Any data collected by the hoarding node is exposed in an easy-to-consume API (e.g. metrics for observability frameworks, and an HTTP API for fine-grained further analysis).

We stress that the load of such a hoarding node on honest nodes is equal to that of any node following the chain. Still, it is possible to allow honest stake pools to opt-out of being connected to by a particular deployment of a hoarding node.

Finally, it is conceptually unavoidable that adversarial behavior can only be recorded on a best-effort basis by the hoarding node, e.g. because adversaries can use heuristics to serve different blocks/transactions to hoarding nodes compared to honest stake pool relays. However, all adversarial behavior that is relayed by honest nodes will also be recorded by a hoarding node.
Discussion of prior art and related services
The basic idea of a service as outlined in this proposal has been around for a long time, see e.g. here. A simple version (only for headers/blocks, not transactions) has been prototyped as part of cardano-slurp.

https://pooltool.io/ has overlap in the analytical aspects with the hoarding node. Fundamentally, these tools are complementary, as pooltool relies on opt-in information sent by stake pools, whereas a monitoring node does not.
Dependencies
Discussions in the Network Working Group on the design
Maintenance
Low, needs to be updated for breaking changes in the network protocol (i.e. hard forks).
Key Proposal Deliverables
A design for a hoarding node, describing in particular how the extra load on honest nodes is kept minimal.
An initial implementation using the existing Haskell network stack, together with an easy-to-consume API.
Resourcing & Duration Estimates
1-2 FTE, 6 months (?)
Experience
We have over 7 years of experience working with the core Cardano infrastructure, including leading the consensus and ledger teams, implementing Ouroboros Genesis and design work for Ouroboros Peras. We have been involved in almost all aspects of the core Cardano node.

Outside of Cardano, we have extensive experience with Haskell, Rust, and polyglot projects in general.
Milestones
Milestone
Deliverable
Timescale
Hoarding Node Design
Design for the Hoarding Node, written up as a CIP (perhaps with accompanying CPS)
2 Months
Block collector
Tool which collects to some nodes and stores all blocks received
4 Months
Transaction collector
Tool which collects to some nodes and stores all transactions seen
4 Months
Node connection logic
Logic for deciding which nodes to connect to in order to maximise the chances of seeing all entities in a network
5 Months
Hoarding node UI
Programmatic UI for accessing the hoarding node
6 Months
Proof against adversarial behaviour
Hoarding node hardening to cope with adversarial behaviour, as per the design
6 Months

