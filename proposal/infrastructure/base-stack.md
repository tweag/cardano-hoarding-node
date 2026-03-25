# Infrastructure Proposal: Base Stack (Single-node)

## Goals

- Infrastructure defined as code, versioned alongside the application
- Fully automated deployments from CI with no manual steps
- Secrets managed cryptographically — never in plaintext outside the target machine
- Database migrations and rollbacks handled automatically as part of the deploy process
- Minimal operational burden on the infrastructure team after initial setup

## Stack

| Concern | Tool |
|---|---|
| AWS resource provisioning | Terraform (or OpenTofu) |
| Application deployment | deploy-rs |
| NixOS system configuration | Nix flake (`nixosConfigurations`) |
| Secrets | sops-nix (age-encrypted, already in repo) |
| Database migrations | sqitch |
| CI/CD | GitHub Actions |

## AWS resources

All resources would be defined in Terraform within this repository under `terraform/`.

| Resource | Purpose |
|---|---|
| VPC + subnets | Isolated network for the service |
| EC2 instance | Runs the hoarding node (NixOS) |
| RDS PostgreSQL 15+ | Application database (Multi-AZ in prod) |
| Security groups | EC2 → RDS on 5432; SSH on 22 from GitHub Actions |
| SSH key pair | Used for NixOS bootstrap and deploy-rs |
| S3 bucket + DynamoDB table | Terraform remote state backend |

Environments (staging, prod) would be separate Terraform workspaces using
per-environment `.tfvars` files.

### Terraform layout

```
terraform/
  main.tf        # provider, backend
  variables.tf   # region, instance types, etc.
  vpc.tf
  ec2.tf
  rds.tf
  outputs.tf     # EC2 IP, RDS endpoint (consumed by deploy-rs and NixOS config)
  staging.tfvars
  prod.tfvars
```

## Bootstrapping (one-time per environment)

1. Infra team creates the Terraform state backend manually (S3 bucket +
   DynamoDB table) and provides AWS credentials to the dev team.
2. `terraform apply` provisions all AWS resources.
3. `nixos-anywhere` installs NixOS on the EC2 instance from our flake config —
   no manual machine setup required.
4. The age decryption key for sops-nix is uploaded to the machine as part of
   the `nixos-anywhere` bootstrap.

After this, the infrastructure team has no further involvement in deployments.

## Continuous delivery

On every push to `main`, GitHub Actions would:

1. Build the NixOS system closure (binary + config + service definitions),
   cached via Cachix.
2. Run `terraform apply` to reconcile any infrastructure changes.
3. Push the closure to the EC2 instance via deploy-rs.
4. On the machine, in order:
   - sops-nix decrypts secrets at activation time
   - `sqitch deploy` runs pending migrations
   - `hoard.service` restarts with the new binary
   - deploy-rs verifies the service is healthy (30s window)
5. On failure at any step: deploy-rs automatically rolls back to the previous
   NixOS generation; a `sqitch revert` is run to undo any applied migrations.

## Secrets management

Secrets (database credentials, SSH keys for the Cardano node tunnel) are
stored in `secrets/{env}.yaml`, encrypted with SOPS/age and committed to the
repository. sops-nix decrypts them on the target machine at activation time.

They are never stored in:
- CI environment variables
- The Nix store
- AWS Secrets Manager or Parameter Store

The only externally provisioned secret is the age private key placed on each
EC2 instance during the one-time bootstrap.

## Database migrations

sqitch manages the migration lifecycle. Migrations run automatically before
the application starts on each deploy, as a systemd oneshot service. If a
migration fails, the deploy is aborted and rolled back.

Migrations must be backward-compatible (additive) wherever possible. Breaking
schema changes are split across two deploys to allow safe rollback without
schema conflicts.

## NixOS module

The flake exports a `nixosModules.hoard` module. Operators add the hoard flake
as an input to their own NixOS configuration and import the module:

```nix
inputs.hoard.url = "github:...";

imports = [ inputs.hoard.nixosModules.hoard ];

services.hoard = {
  enable = true;
  # options here
};
```

All secrets (database credentials, etc.) are injected via environment variables
at service start — the module does not require sops or any specific secret
management tooling on the operator's side. Operators using sops-nix can wire
the env vars through their existing setup in the usual way.

The team's own deployment (see [Continuous delivery](#continuous-delivery))
uses sops-nix and serves as the reference implementation.

## OCI image

An OCI image is buildable via `nix/docker.nix`. This is the secondary
deployment path for operators not running NixOS. Secrets are passed as
environment variables at container start.

## What we need from the infrastructure team

- An AWS account with sufficient permissions to create the resources listed above
- An IAM role for GitHub Actions (OIDC) with permissions to:
  - Apply Terraform (EC2, RDS, VPC, S3, DynamoDB, IAM)
  - The role ARN, to be set as a GitHub Actions secret
- A decision on: Terraform vs OpenTofu, and preferred AWS region

Everything else — resource definitions, NixOS configuration, deployment
pipeline, secrets — is owned by the development team.
