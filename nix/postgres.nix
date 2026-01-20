{ pkgs }:
{
  # PostgreSQL: local dev database with auto-init
  # Reads configuration from config/dev.yaml to stay in sync with the app
  postgres = {
    type = "app";
    program = "${pkgs.writeShellScript "postgres-app" ''
      set -e

      # Default to dev environment, can be overridden with HOARD_ENV
      ENV=''${HOARD_ENV:-dev}
      CONFIG_FILE="$PWD/config/$ENV.yaml"

      if [ ! -f "$CONFIG_FILE" ]; then
        echo "Error: Config file not found: $CONFIG_FILE"
        echo "Available environments: dev, ci, staging, prod"
        exit 1
      fi

      # Parse config values from YAML
      DB_HOST=''${DB_HOST:-$(${pkgs.yq-go}/bin/yq '.database.host' "$CONFIG_FILE")}
      DB_PORT=''${DB_PORT:-$(${pkgs.yq-go}/bin/yq '.database.port' "$CONFIG_FILE")}
      DB_NAME=''${DB_NAME:-$(${pkgs.yq-go}/bin/yq '.database.database_name' "$CONFIG_FILE")}

      # Resolve relative paths to absolute
      if [[ "$DB_HOST" != /* ]]; then
        DB_HOST="$PWD/$DB_HOST"
      fi

      echo "Loading configuration from: $CONFIG_FILE"
      echo "  Database: $DB_NAME"
      echo "  Port: $DB_PORT"
      echo "  Socket: $DB_HOST"
      echo ""

      PGDATA="$PWD/postgres-data"
      PGHOST="$DB_HOST"

      # Initialize database if it doesn't exist
      if [ ! -d "$PGDATA" ]; then
        echo "Initializing PostgreSQL database in $PGDATA..."
        ${pkgs.postgresql}/bin/initdb -D "$PGDATA" --no-locale --encoding=UTF8

        # Configure for local socket-only access
        cat >> "$PGDATA/postgresql.conf" <<EOF
      unix_socket_directories = '$PGHOST'
      listen_addresses = '''
      port = $DB_PORT
      EOF

        echo "Database initialized!"

        # Start postgres temporarily to create roles and databases
        ${pkgs.postgresql}/bin/pg_ctl -D "$PGDATA" -o "-k $PGHOST" -w start

        # Create postgres role (superuser for dev convenience)
        ${pkgs.postgresql}/bin/createuser -h "$PGHOST" -s postgres || true

        # Create application database
        ${pkgs.postgresql}/bin/createdb -h "$PGHOST" -U postgres "$DB_NAME" || true

        # Stop postgres
        ${pkgs.postgresql}/bin/pg_ctl -D "$PGDATA" stop

        echo "Created postgres role and $DB_NAME database!"
      fi

      echo "Starting PostgreSQL..."
      echo "Connection info:"
      echo "  Socket: $PGHOST/.s.PGSQL.$DB_PORT"
      echo "  Database: $DB_NAME"
      echo "  User: postgres (no password needed)"
      echo ""
      echo "To connect: psql -h $PGHOST -U postgres $DB_NAME"
      echo "Press Ctrl+C to stop"
      echo ""

      # Start postgres
      ${pkgs.postgresql}/bin/postgres -D "$PGDATA" -k "$PGHOST"
    ''}";
  };
}
