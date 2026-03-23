# Infrastructure

## Deployment modes

The hoarding node can run in two configurations with different infrastructure
requirements:

**Single-node** — coordinator and collector run in the same process on one
machine. One EC2 instance, one RDS database, one deployment target.

**Distributed** — collectors run as separate processes, forwarding events to a
central coordinator over TCP. Collectors are stateless and can run on separate
machines or be scaled horizontally. The coordinator retains its DB and
processing responsibilities.

## Shared components

These are required in both deployment modes.

**PostgreSQL** — the coordinator's database. Stores blocks, headers, peers, and
quota data. In production: RDS PostgreSQL with Multi-AZ. In single-node mode
the coordinator and its DB are co-located; in distributed mode they remain
together on the coordinator host.

**Cardano node** — a full node running on the coordinator host, connected via
the Node-to-Client protocol (Unix socket). The coordinator uses it to query
ledger state that is not available from the node-to-node protocols: the
immutable tip (`LocalStateQuery`) and whether a given chain point is on chain
(local `ChainSync` / `FindIntersect`). Collectors do not require a local
Cardano node.

**Observability stack**

| Component | Role |
|---|---|
| Prometheus | Metrics collection and storage |
| Grafana | Dashboards and alerting |
| Loki | Log aggregation |
| Tempo | Distributed tracing |
| Promtail | Log shipping to Loki |
| node\_exporter | Host-level metrics (CPU, memory, disk, network) |

In single-node mode the full stack runs on the coordinator host. In distributed
mode each collector also runs node\_exporter and Promtail; metrics and logs are
scraped/shipped to a central Prometheus and Loki on the coordinator side.

## Documents

- [infrastructure-base.md](infrastructure-base.md) —
  proposed stack for single-node deployment: Terraform, NixOS, deploy-rs,
  sops-nix, sqitch, GitHub Actions CI/CD
- Distributed mode infrastructure — to be written; will cover collector
  deployment targets, coordinator scaling, and inter-node networking
