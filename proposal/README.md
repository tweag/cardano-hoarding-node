# Proposals

Work packages for the current funding cycle. Each folder contains a `README.md`
with the full work package, and supporting design documents for reference.

## Work Packages

| Proposal | Summary |
|---|---|
| [Live Network Deployment](live-network-deployment/README.md) | Deployment infrastructure, CI/CD pipeline, and six months of live operation against Cardano pre-production |
| [Embedded Consensus Validation of Blocks and Headers](embedded-consensus-validation/README.md) | In-process consensus validation via embedded `ouroboros-consensus`; richer block classification and per-peer header validation |
| [Distributed Mode](distributed-mode/README.md) | Collector/coordinator split enabling multi-region deployment and propagation timing measurement |

## Other Proposals

The following are in earlier stages or blocked on other work packages:

| Proposal | Status |
|---|---|
| [Transaction Collection](deferred/transaction-collection/proposal.md) | Blocked — depends on Live Network Deployment |
| [Block/Header Relaying](deferred/block-header-relaying/proposal.md) | Placeholder — conditional on Embedded Consensus Validation being accepted |
| [Maintenance](deferred/maintenance/proposal.md) | Placeholder |
| [Stretch: Data Export](deferred/stretch/data-export/proposal.md) | Placeholder |
| [Stretch: UI](deferred/stretch/ui/proposal.md) | Placeholder |
