{
  pkgs,
  inputs,
  system,
}:
let
  # Mithril client from the mithril flake
  mithril-client-cli = inputs.mithril.packages.${system}.mithril-client-cli;

  # Export packages used by our apps so they get cached in CI
  packages = {
    inherit mithril-client-cli;
  };

  # Generic function to create a Mithril bootstrap app for any network
  mkMithrilBootstrap =
    {
      network,
      displayName,
      aggregatorEndpoint,
      genesisVkeyUrl,
      ancillaryVkeyUrl,
      dbPath,
      nodeApp,
    }:
    {
      type = "app";
      program = "${pkgs.writeShellScript "cardano-node-${network}-bootstrap" ''
        set -euo pipefail

        # Configuration for ${network} network
        export CARDANO_NETWORK=${network}
        export AGGREGATOR_ENDPOINT=${aggregatorEndpoint}
        export GENESIS_VERIFICATION_KEY=$(${pkgs.wget}/bin/wget -q -O - ${genesisVkeyUrl})
        export ANCILLARY_VERIFICATION_KEY=$(${pkgs.wget}/bin/wget -q -O - ${ancillaryVkeyUrl})
        export SNAPSHOT_DIGEST=latest

        # Database directory
        DB_PATH="${dbPath}"

        echo "=== Mithril Cardano Node Bootstrap (${displayName}) ==="
        echo ""
        echo "This will download a verified snapshot of the Cardano blockchain"
        echo "Database location: $DB_PATH"
        echo "Network: ${network}"
        echo ""

        # Check if database already exists
        if [ -d "$DB_PATH/immutable" ]; then
          echo "WARNING: Database already exists at $DB_PATH"
          read -p "Do you want to replace it? (yes/no): " response
          if [ "$response" != "yes" ]; then
            echo "Aborted."
            exit 1
          fi
          echo "Removing existing database..."
          rm -rf "$DB_PATH"
        fi

        # Create directory
        mkdir -p "$DB_PATH"

        # List available snapshots
        echo ""
        echo "Fetching available snapshots..."
        ${mithril-client-cli}/bin/mithril-client cardano-db snapshot list | head -5

        echo ""
        echo "Downloading latest snapshot (this may take a few minutes)..."
        cd "$DB_PATH"
        ${mithril-client-cli}/bin/mithril-client cardano-db download --include-ancillary latest

        echo ""
        echo "Moving database files to correct location..."
        mv db/* .
        rmdir db

        echo ""
        echo "✓ Bootstrap complete!"
        echo ""
        echo "You can now start the node with:"
        echo "  nix run .#${nodeApp}"
      ''}";
    };

  # Generic function to create a cardano-node runner app for any network
  mkCardanoNode =
    {
      network,
      displayName,
      configPath,
      topologyPath,
      dbPath,
      socketPath,
      bootstrapApp,
    }:
    {
      type = "app";
      program = "${pkgs.writeShellScript "cardano-node-${network}" ''
        set -euo pipefail

        # Ensure node-db directory exists
        mkdir -p "${dbPath}"

        # Check if database is initialized (look for critical files)
        if [ ! -f "${dbPath}/protocolMagicId" ] && [ ! -d "${dbPath}/immutable" ]; then
          echo "==================================================================="
          echo "No database found at ${dbPath}"
          echo ""
          echo "You have two options:"
          echo "  1. Bootstrap with Mithril (recommended, ~20 minutes)"
          echo "  2. Sync from genesis (slow, can take days)"
          echo ""
          echo "==================================================================="
          echo ""
          read -p "Would you like to bootstrap with Mithril now? (yes/no): " response

          if [ "$response" = "yes" ]; then
            echo ""
            echo "Starting Mithril bootstrap..."
            ${pkgs.nix}/bin/nix run .#${bootstrapApp}
            echo ""
            echo "Bootstrap complete! Starting node..."
            echo ""
          else
            echo ""
            echo "Proceeding with sync from genesis..."
            echo ""
          fi
        fi

        echo "Starting cardano-node with ${displayName} configuration..."
        echo "Config: ${configPath}"
        echo "Database: ${dbPath}"
        echo "Socket: ${socketPath}"
        echo "Tracer socket: /tmp/cardano-tracer.sock"
        echo ""

        # Run cardano-node with config
        exec ${pkgs.nix}/bin/nix run github:IntersectMBO/cardano-node#cardano-node -- run \
          --config ${configPath} \
          --topology ${topologyPath} \
          --database-path ${dbPath} \
          --socket-path ${socketPath} \
          --tracer-socket-path-accept /tmp/cardano-tracer.sock
      ''}";
    };

  # Apps exported by this module
  apps = {
    # Preprod network bootstrap
    cardano-node-preprod-bootstrap = mkMithrilBootstrap {
      network = "preprod";
      displayName = "Preprod";
      aggregatorEndpoint = "https://aggregator.release-preprod.api.mithril.network/aggregator";
      genesisVkeyUrl = "https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey";
      ancillaryVkeyUrl = "https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/ancillary.vkey";
      dbPath = "./node-db/preprod";
      nodeApp = "cardano-node-preprod";
    };

    # Preprod network node runner
    cardano-node-preprod = mkCardanoNode {
      network = "preprod";
      displayName = "Preprod";
      configPath = "./config/preprod/config.json";
      topologyPath = "./config/preprod/topology.json";
      dbPath = "./node-db/preprod";
      socketPath = "./node-db/preprod/node.socket";
      bootstrapApp = "cardano-node-preprod-bootstrap";
    };

    # Future: Mainnet network (example)
    # cardano-node-mainnet-bootstrap = mkMithrilBootstrap {
    #   network = "mainnet";
    #   displayName = "Mainnet";
    #   aggregatorEndpoint = "https://aggregator.release-mainnet.api.mithril.network/aggregator";
    #   genesisVkeyUrl = "https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/genesis.vkey";
    #   ancillaryVkeyUrl = "https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/ancillary.vkey";
    #   dbPath = "./node-db/mainnet";
    #   nodeApp = "cardano-node-mainnet";
    # };
    #
    # cardano-node-mainnet = mkCardanoNode {
    #   network = "mainnet";
    #   displayName = "Mainnet";
    #   configPath = "./config/mainnet/config.json";
    #   topologyPath = "./config/mainnet/topology.json";
    #   dbPath = "./node-db/mainnet";
    #   socketPath = "./node-db/mainnet/node.socket";
    #   bootstrapApp = "cardano-node-mainnet-bootstrap";
    # };

    # Future: Preview network (example)
    # cardano-node-preview-bootstrap = mkMithrilBootstrap {
    #   network = "preview";
    #   displayName = "Preview";
    #   aggregatorEndpoint = "https://aggregator.pre-release-preview.api.mithril.network/aggregator";
    #   genesisVkeyUrl = "https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/genesis.vkey";
    #   ancillaryVkeyUrl = "https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/ancillary.vkey";
    #   dbPath = "./node-db/preview";
    #   nodeApp = "cardano-node-preview";
    # };
    #
    # cardano-node-preview = mkCardanoNode {
    #   network = "preview";
    #   displayName = "Preview";
    #   configPath = "./config/preview/config.json";
    #   topologyPath = "./config/preview/topology.json";
    #   dbPath = "./node-db/preview";
    #   socketPath = "./node-db/preview/node.socket";
    #   bootstrapApp = "cardano-node-preview-bootstrap";
    # };
  };
in
{
  inherit apps packages;
}
