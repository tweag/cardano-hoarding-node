# Infrastructure Proposal

## Budget ask

At minimum, **6 months of cloud spend** to run the hoarding node in single-node
mode against pre-production. This covers:

- Initial weeks to get the deployment pipeline working end-to-end (Terraform,
  NixOS, CI/CD, secrets, migrations).
- The remaining time collecting real observability data from the pre-prod
  network — the primary deliverable of this proposal.

If the transaction collection proposal is also accepted, this environment is
where that work would be validated end-to-end for the first time.

The distributed mode and a mainnet deployment are explicitly out of scope for
this proposal.

## Cloud spend estimate

Based on actual preprod data sizes from the prototype (~17GB for the chain
data, ~400MB for the hoarding database). Prices calculated using the
[AWS Pricing Calculator](https://calculator.aws), us-east-1, on-demand.

| Resource | Spec | $/month |
|---|---|---|
| EC2 | `t3.xlarge` (4 vCPU, 16GB) | $121 |
| EBS | 100GB gp3 (chain data + observability) | $8 |
| RDS | `db.t3.medium`, single-AZ | $53 |
| RDS storage | 50GB gp3 | ~$4 |
| Data transfer + misc | — | ~$20 |
| **Total** | | **~$206/month** |

**6-month total: ~$1,236.**

PostgreSQL storage is expected to remain roughly bounded due to regular block
and header eviction.

## Success criteria

- Hoarding node running continuously against pre-production, with observability
  dashboards operational.
- Real orphaned/invalid block data being collected and queryable via the HTTP
  API.
- At least one publicly exposed report of adversarial behaviour detected and
  evidenced using data collected by the hoarding node.
- If the transaction collection proposal is accepted: transactions being
  collected and stored end-to-end in the same environment.

## Deliverables

- A `nixosModules.hoard` export from the flake, with configurable options, so
  that existing NixOS/Cardano operators can deploy their own hoarding node by
  importing it as a flake input. Secrets are injected via environment variables
  — no sops/age setup required on the operator's side (though sops remains an
  option if preferred).
- An OCI image (already buildable via `nix/docker.nix`) documented as a
  secondary deployment path for non-NixOS users.
- Terraform, NixOS configuration, and CI/CD pipeline for the team's own preprod
  deployment, which also serves as the reference implementation.

## Documents

- [overview.md](overview.md) — deployment modes, shared components, observability stack
- [base-stack.md](base-stack.md) — proposed deployment stack: Terraform, NixOS, deploy-rs, sops-nix, sqitch, GitHub Actions
