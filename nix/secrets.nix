# Secrets management apps using SOPS
{ pkgs }:
{
  # Edit encrypted secrets for a given environment
  secrets-edit = {
    type = "app";
    program = "${pkgs.writeShellScript "secrets-edit" ''
      if [ -z "$1" ]; then
        echo "Usage: nix run .#secrets-edit <environment>"
        echo "Example: nix run .#secrets-edit dev"
        echo ""
        echo "Available environments: dev, staging, prod, ci"
        exit 1
      fi

      ENV="$1"
      SECRET_FILE="secrets/$ENV.yaml"

      if [ ! -f "$SECRET_FILE" ]; then
        echo "Error: Secret file $SECRET_FILE does not exist"
        exit 1
      fi

      echo "Editing secrets for environment: $ENV"
      ${pkgs.sops}/bin/sops "$SECRET_FILE"
    ''}";
  };

  # Decrypt secrets to stdout for a given environment
  secrets-decrypt = {
    type = "app";
    program = "${pkgs.writeShellScript "secrets-decrypt" ''
      if [ -z "$1" ]; then
        echo "Usage: nix run .#secrets-decrypt <environment>"
        echo "Example: nix run .#secrets-decrypt dev"
        echo ""
        echo "This will decrypt secrets/{env}.yaml to stdout"
        exit 1
      fi

      ENV="$1"
      SECRET_FILE="secrets/$ENV.yaml"

      if [ ! -f "$SECRET_FILE" ]; then
        echo "Error: Secret file $SECRET_FILE does not exist" >&2
        exit 1
      fi

      ${pkgs.sops}/bin/sops -d "$SECRET_FILE"
    ''}";
  };
}
