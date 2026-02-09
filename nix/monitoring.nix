{ pkgs }:
let
  # Prometheus configuration
  prometheusConfig = pkgs.writeText "prometheus.yml" ''
    global:
      scrape_interval: 15s
      evaluation_interval: 15s

    scrape_configs:
      - job_name: 'hoard'
        static_configs:
          - targets: ['localhost:3000']
            labels:
              service: 'hoard'
              environment: 'dev'

      - job_name: 'node_exporter'
        static_configs:
          - targets: ['localhost:9100']
            labels:
              service: 'system'
              environment: 'dev'

      - job_name: 'prometheus'
        static_configs:
          - targets: ['localhost:9090']
            labels:
              service: 'prometheus'
              environment: 'dev'
  '';

  # Grafana datasource configuration
  grafanaDatasource = pkgs.writeText "datasource.yml" ''
    apiVersion: 1

    datasources:
      - name: Prometheus
        type: prometheus
        access: proxy
        url: http://localhost:9090
        isDefault: true
        editable: false
  '';

  # Grafana dashboard configuration
  grafanaDashboard = pkgs.writeText "hoard-dashboard.json" (
    builtins.toJSON {
      title = "Hoard Monitoring";
      tags = [
        "hoard"
        "cardano"
      ];
      timezone = "browser";
      editable = false;
      schemaVersion = 16;
      version = 0;
      refresh = "5s";
      uid = "hoard-monitoring";

      panels = [
        # Row 1: Chain State
        {
          id = 1;
          title = "Connected Peers";
          type = "stat";
          description = "Number of active peer connections to other Cardano nodes.";
          gridPos = {
            x = 0;
            y = 0;
            w = 8;
            h = 8;
          };
          targets = [
            {
              expr = "hoard_connected_peers";
              refId = "A";
            }
          ];
          options = {
            textMode = "auto";
            colorMode = "value";
            graphMode = "area";
          };
          fieldConfig = {
            defaults = {
              unit = "none";
              thresholds = {
                mode = "absolute";
                steps = [
                  {
                    value = 0;
                    color = "red";
                  }
                  {
                    value = 1;
                    color = "yellow";
                  }
                  {
                    value = 5;
                    color = "green";
                  }
                ];
              };
            };
          };
        }
        {
          id = 2;
          title = "Pending Peers";
          type = "stat";
          description = "Number of peers pending connection. These are peers that have been discovered but are not yet fully connected.";
          gridPos = {
            x = 8;
            y = 0;
            w = 8;
            h = 8;
          };
          targets = [
            {
              expr = "hoard_pending_peers";
              refId = "A";
            }
          ];
          options = {
            textMode = "auto";
            colorMode = "value";
            graphMode = "area";
          };
          fieldConfig = {
            defaults = {
              unit = "none";
              thresholds = {
                mode = "absolute";
                steps = [
                  {
                    value = 0;
                    color = "green";
                  }
                  {
                    value = 10;
                    color = "yellow";
                  }
                  {
                    value = 50;
                    color = "red";
                  }
                ];
              };
            };
          };
        }
        {
          id = 3;
          title = "Blocks in Database";
          type = "stat";
          description = "Total number of blocks stored in the database. This grows as the node synchronizes with the network and stores historical blockchain data.";
          gridPos = {
            x = 16;
            y = 0;
            w = 8;
            h = 8;
          };
          targets = [
            {
              expr = "hoard_blocks_in_db";
              refId = "A";
            }
          ];
          options = {
            textMode = "auto";
            colorMode = "value";
            graphMode = "area";
          };
        }

        # Row 2: Protocol Activity
        {
          id = 4;
          title = "Block Fetch Rate";
          type = "graph";
          description = "Rate at which complete blocks are being downloaded from peers (5-minute average). Higher rates indicate active synchronization with the blockchain.";
          gridPos = {
            x = 0;
            y = 8;
            w = 8;
            h = 8;
          };
          targets = [
            {
              expr = "rate(hoard_blocks_received_total[5m])";
              refId = "A";
              legendFormat = "blocks/sec";
            }
          ];
          yaxes = [
            {
              format = "ops";
              label = "Blocks/sec";
              show = true;
            }
            {
              format = "none";
              show = false;
            }
          ];
        }
        {
          id = 5;
          title = "Header Fetch Rate";
          type = "graph";
          description = "Rate at which block headers are being downloaded from peers (5-minute average). Headers are fetched first before downloading full blocks for validation.";
          gridPos = {
            x = 8;
            y = 8;
            w = 8;
            h = 8;
          };
          targets = [
            {
              expr = "rate(hoard_headers_received_total[5m])";
              refId = "A";
              legendFormat = "headers/sec";
            }
          ];
          yaxes = [
            {
              format = "ops";
              label = "Headers/sec";
              show = true;
            }
            {
              format = "none";
              show = false;
            }
          ];
        }
        {
          id = 6;
          title = "ChainSync Events";
          type = "graph";
          description = "ChainSync protocol events (5-minute average). Rollforwards indicate chain progression, while rollbacks occur when the chain reorganizes due to temporary forks.";
          gridPos = {
            x = 16;
            y = 8;
            w = 8;
            h = 8;
          };
          targets = [
            {
              expr = "rate(hoard_chain_sync_rollforwards_total[5m])";
              refId = "A";
              legendFormat = "rollforwards";
            }
            {
              expr = "rate(hoard_chain_sync_rollbacks_total[5m])";
              refId = "B";
              legendFormat = "rollbacks";
            }
          ];
          yaxes = [
            {
              format = "ops";
              label = "Events/sec";
              show = true;
            }
            {
              format = "none";
              show = false;
            }
          ];
        }

        # Row 3: Peer Management
        {
          id = 15;
          title = "Cull Batch Size";
          type = "graph";
          description = "Distribution of collectors culled per cull event (5-minute window). Shows p50, p90, and p99 quantiles of cull batch sizes.";
          gridPos = {
            x = 0;
            y = 16;
            w = 12;
            h = 8;
          };
          targets = [
            {
              expr = "histogram_quantile(0.5, rate(hoard_peer_manager_cull_batches_bucket[5m]))";
              refId = "A";
              legendFormat = "p50";
            }
            {
              expr = "histogram_quantile(0.9, rate(hoard_peer_manager_cull_batches_bucket[5m]))";
              refId = "B";
              legendFormat = "p90";
            }
            {
              expr = "histogram_quantile(0.99, rate(hoard_peer_manager_cull_batches_bucket[5m]))";
              refId = "C";
              legendFormat = "p99";
            }
          ];
          yaxes = [
            {
              format = "none";
              label = "Collectors culled";
              show = true;
            }
            {
              format = "none";
              show = false;
            }
          ];
        }
        {
          id = 16;
          title = "Collector Replenishment Rate";
          type = "graph";
          description = "Rate at which collectors are being replenished (5-minute average). Shows how frequently the peer manager replaces culled or lost collectors.";
          gridPos = {
            x = 12;
            y = 16;
            w = 12;
            h = 8;
          };
          targets = [
            {
              expr = "rate(hoard_peer_manager_replenished_collector_count[5m])";
              refId = "A";
              legendFormat = "replenishments/sec";
            }
          ];
          yaxes = [
            {
              format = "ops";
              label = "Replenishments/sec";
              show = true;
            }
            {
              format = "none";
              show = false;
            }
          ];
        }

        # Row 4: Database Performance
        {
          id = 7;
          title = "Database Query Rate";
          type = "graph";
          description = "Rate of database queries being executed (5-minute average). Shows the database workload as blocks and metadata are stored and retrieved.";
          gridPos = {
            x = 0;
            y = 24;
            w = 8;
            h = 8;
          };
          targets = [
            {
              expr = "rate(hoard_db_queries_total[5m])";
              refId = "A";
              legendFormat = "queries/sec";
            }
          ];
          yaxes = [
            {
              format = "ops";
              label = "Queries/sec";
              show = true;
            }
            {
              format = "none";
              show = false;
            }
          ];
        }
        {
          id = 8;
          title = "Query Duration (95th percentile)";
          type = "graph";
          description = "95th percentile of database query execution time. This represents the duration that 95% of queries complete within, helping identify performance issues.";
          gridPos = {
            x = 8;
            y = 24;
            w = 8;
            h = 8;
          };
          targets = [
            {
              expr = "histogram_quantile(0.95, rate(hoard_db_query_duration_seconds_bucket[5m]))";
              refId = "A";
              legendFormat = "p95";
            }
          ];
          yaxes = [
            {
              format = "s";
              label = "Duration";
              show = true;
            }
            {
              format = "none";
              show = false;
            }
          ];
        }
        {
          id = 9;
          title = "Database Errors";
          type = "stat";
          description = "Rate of database query errors (5-minute average). Should be zero under normal operation. Non-zero values indicate database connectivity or query issues.";
          gridPos = {
            x = 16;
            y = 24;
            w = 8;
            h = 8;
          };
          targets = [
            {
              expr = "rate(hoard_db_query_errors_total[5m])";
              refId = "A";
            }
          ];
          fieldConfig = {
            defaults = {
              thresholds = {
                mode = "absolute";
                steps = [
                  {
                    value = 0;
                    color = "green";
                  }
                  {
                    value = 0.1;
                    color = "red";
                  }
                ];
              };
            };
          };
        }

        # Row 5: GHC Runtime
        {
          id = 10;
          title = "Heap Size";
          type = "graph";
          description = "Current size of live data in the GHC heap. Shows memory actively used by the application, excluding garbage-collected data.";
          gridPos = {
            x = 0;
            y = 32;
            w = 8;
            h = 8;
          };
          targets = [
            {
              expr = "ghc_gcdetails_live_bytes";
              refId = "A";
              legendFormat = "live heap";
            }
          ];
          yaxes = [
            {
              format = "bytes";
              label = "Bytes";
              show = true;
            }
            {
              format = "none";
              show = false;
            }
          ];
        }
        {
          id = 11;
          title = "GC Time";
          type = "graph";
          description = "Percentage of time spent in garbage collection (5-minute average). Shows GC CPU time and wall clock time. High values indicate memory pressure or inefficient allocation patterns.";
          gridPos = {
            x = 8;
            y = 32;
            w = 8;
            h = 8;
          };
          targets = [
            {
              expr = "rate(ghc_gc_cpu_seconds_total[5m])";
              refId = "A";
              legendFormat = "gc cpu time";
            }
            {
              expr = "rate(ghc_gc_elapsed_seconds_total[5m])";
              refId = "B";
              legendFormat = "gc wall time";
            }
          ];
          yaxes = [
            {
              format = "percentunit";
              label = "Time %";
              show = true;
            }
            {
              format = "none";
              show = false;
            }
          ];
        }

        # Row 6: System Resources
        {
          id = 13;
          title = "CPU Usage";
          type = "graph";
          description = "System-wide CPU usage percentage (5-minute average). Shows overall CPU utilization across all cores. High sustained usage may indicate the need for more CPU resources.";
          gridPos = {
            x = 0;
            y = 40;
            w = 12;
            h = 8;
          };
          targets = [
            {
              expr = ''100 - (avg by (instance) (rate(node_cpu_seconds_total{mode="idle"}[5m])) * 100)'';
              refId = "A";
              legendFormat = "cpu %";
            }
          ];
          yaxes = [
            {
              format = "percent";
              label = "CPU %";
              show = true;
            }
            {
              format = "none";
              show = false;
            }
          ];
        }
        {
          id = 14;
          title = "Memory Usage";
          type = "graph";
          description = "System-wide memory usage percentage. Shows the proportion of total system memory currently in use. Values approaching 100% may lead to swapping and performance degradation.";
          gridPos = {
            x = 12;
            y = 40;
            w = 12;
            h = 8;
          };
          targets = [
            {
              expr = "100 * (1 - node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes)";
              refId = "A";
              legendFormat = "memory %";
            }
          ];
          yaxes = [
            {
              format = "percent";
              label = "Memory %";
              show = true;
            }
            {
              format = "none";
              show = false;
            }
          ];
        }
      ];
    }
  );

  # Grafana provisioning configuration
  grafanaProvisioning = pkgs.writeText "dashboards.yml" ''
    apiVersion: 1

    providers:
      - name: 'Hoard Dashboards'
        orgId: 1
        folder: ""
        type: file
        disableDeletion: false
        updateIntervalSeconds: 10
        allowUiUpdates: true
        options:
          path: /etc/grafana/dashboards
  '';
in
{
  apps = {
    # Prometheus server
    prometheus = {
      type = "app";
      program = "${pkgs.writeShellScript "prometheus-app" ''
        set -e

        PROMETHEUS_DATA="$PWD/prometheus-data"

        mkdir -p "$PROMETHEUS_DATA"

        echo "Starting Prometheus..."
        echo "  Web UI: http://localhost:9090"
        echo "  Storage: $PROMETHEUS_DATA"
        echo "  Config: ${prometheusConfig}"
        echo ""
        echo "Scraping targets:"
        echo "  - Hoard: http://localhost:3000/metrics"
        echo "  - Node Exporter: http://localhost:9100/metrics"
        echo ""
        echo "Press Ctrl+C to stop"
        echo ""

        exec ${pkgs.prometheus}/bin/prometheus \
          --config.file=${prometheusConfig} \
          --storage.tsdb.path="$PROMETHEUS_DATA" \
          --web.listen-address=:9090 \
          --web.console.templates=${pkgs.prometheus}/etc/prometheus/consoles \
          --web.console.libraries=${pkgs.prometheus}/etc/prometheus/console_libraries
      ''}";
    };

    # Grafana server
    grafana = {
      type = "app";
      program = "${pkgs.writeShellScript "grafana-app" ''
                set -e

                GRAFANA_DATA="$PWD/grafana-data"
                GRAFANA_PROVISIONING="$GRAFANA_DATA/provisioning"
                GRAFANA_DASHBOARDS="$GRAFANA_PROVISIONING/dashboards"

                mkdir -p "$GRAFANA_DATA"
                mkdir -p "$GRAFANA_PROVISIONING/datasources"
                mkdir -p "$GRAFANA_PROVISIONING/dashboards"
                mkdir -p "$GRAFANA_DASHBOARDS"

                # Copy datasource and dashboard (use install to handle permissions)
                install -m 644 ${grafanaDatasource} "$GRAFANA_PROVISIONING/datasources/prometheus.yml"
                install -m 644 ${grafanaDashboard} "$GRAFANA_DASHBOARDS/hoard.json"

                # Generate dashboard provisioning config with actual path
                cat > "$GRAFANA_PROVISIONING/dashboards/hoard.yml" <<EOF
        apiVersion: 1

        providers:
          - name: 'Hoard Dashboards'
            orgId: 1
            folder: ""
            type: file
            disableDeletion: false
            updateIntervalSeconds: 10
            allowUiUpdates: true
            options:
              path: $GRAFANA_DASHBOARDS
        EOF

                # Generate grafana.ini with actual paths
                cat > "$GRAFANA_DATA/grafana.ini" <<EOF
                [server]
                http_port = 3001
                http_addr = 127.0.0.1

                [paths]
                data = $GRAFANA_DATA
                logs = $GRAFANA_DATA/log
                plugins = $GRAFANA_DATA/plugins
                provisioning = $GRAFANA_PROVISIONING

                [security]
                admin_user = admin
                admin_password = admin

                [analytics]
                reporting_enabled = false
                check_for_updates = false

                [users]
                allow_sign_up = false
        EOF

                echo "Starting Grafana..."
                echo "  Web UI: http://localhost:3001"
                echo "  Username: admin"
                echo "  Password: admin (you'll be prompted to change it)"
                echo "  Storage: $GRAFANA_DATA"
                echo ""
                echo "Dashboard provisioned: Hoard Monitoring"
                echo "Datasource provisioned: Prometheus (http://localhost:9090)"
                echo ""
                echo "Press Ctrl+C to stop"
                echo ""

                exec ${pkgs.grafana}/bin/grafana server \
                  --homepath ${pkgs.grafana}/share/grafana \
                  --config "$GRAFANA_DATA/grafana.ini"
      ''}";
    };

    # Node exporter for system metrics
    node-exporter = {
      type = "app";
      program = "${pkgs.writeShellScript "node-exporter-app" ''
        set -e

        echo "Starting Node Exporter..."
        echo "  Metrics endpoint: http://localhost:9100/metrics"
        echo ""
        echo "Collecting system metrics:"
        echo "  - CPU usage"
        echo "  - Memory usage"
        echo "  - Disk I/O"
        echo "  - Network I/O"
        echo ""
        echo "Press Ctrl+C to stop"
        echo ""

        exec ${pkgs.prometheus-node-exporter}/bin/node_exporter \
          --web.listen-address=:9100
      ''}";
    };

    # All-in-one monitoring stack
    monitoring = {
      type = "app";
      program = "${pkgs.writeShellScript "monitoring-app" ''
                set -e

                echo "Starting Hoard Monitoring Stack..."
                echo ""
                echo "This will start:"
                echo "  1. Prometheus (metrics storage & querying)"
                echo "  2. Grafana (visualization & dashboards)"
                echo "  3. Node Exporter (system metrics)"
                echo ""
                echo "Make sure Hoard is running on http://localhost:3000"
                echo ""

                # Trap to kill all background jobs on exit
                cleanup() {
                  echo ""
                  echo "Stopping monitoring stack..."
                  kill $(jobs -p) 2>/dev/null || true
                  wait
                  echo "Stopped!"
                }
                trap cleanup EXIT INT TERM

                # Start node exporter
                echo "Starting Node Exporter..."
                ${pkgs.prometheus-node-exporter}/bin/node_exporter \
                  --web.listen-address=:9100 &
                NODE_EXPORTER_PID=$!

                # Wait a bit for node exporter to start
                sleep 2

                # Start Prometheus
                echo "Starting Prometheus..."
                PROMETHEUS_DATA="$PWD/prometheus-data"
                mkdir -p "$PROMETHEUS_DATA"
                ${pkgs.prometheus}/bin/prometheus \
                  --config.file=${prometheusConfig} \
                  --storage.tsdb.path="$PROMETHEUS_DATA" \
                  --web.listen-address=:9090 \
                  --web.console.templates=${pkgs.prometheus}/etc/prometheus/consoles \
                  --web.console.libraries=${pkgs.prometheus}/etc/prometheus/console_libraries &
                PROMETHEUS_PID=$!

                # Wait a bit for Prometheus to start
                sleep 3

                # Start Grafana
                echo "Starting Grafana..."
                GRAFANA_DATA="$PWD/grafana-data"
                GRAFANA_PROVISIONING="$GRAFANA_DATA/provisioning"
                GRAFANA_DASHBOARDS="$GRAFANA_PROVISIONING/dashboards"

                mkdir -p "$GRAFANA_DATA"
                mkdir -p "$GRAFANA_PROVISIONING/datasources"
                mkdir -p "$GRAFANA_PROVISIONING/dashboards"
                mkdir -p "$GRAFANA_DASHBOARDS"

                # Use install to handle permissions properly
                install -m 644 ${grafanaDatasource} "$GRAFANA_PROVISIONING/datasources/prometheus.yml"
                install -m 644 ${grafanaDashboard} "$GRAFANA_DASHBOARDS/hoard.json"

                # Generate dashboard provisioning config with actual path
                cat > "$GRAFANA_PROVISIONING/dashboards/hoard.yml" <<DASHEOF
        apiVersion: 1

        providers:
          - name: 'Hoard Dashboards'
            orgId: 1
            folder: ""
            type: file
            disableDeletion: false
            updateIntervalSeconds: 10
            allowUiUpdates: true
            options:
              path: $GRAFANA_DASHBOARDS
        DASHEOF

                # Generate grafana.ini with actual paths
                cat > "$GRAFANA_DATA/grafana.ini" <<EOF
                [server]
                http_port = 3001
                http_addr = 127.0.0.1

                [paths]
                data = $GRAFANA_DATA
                logs = $GRAFANA_DATA/log
                plugins = $GRAFANA_DATA/plugins
                provisioning = $GRAFANA_PROVISIONING

                [security]
                admin_user = admin
                admin_password = admin

                [analytics]
                reporting_enabled = false
                check_for_updates = false

                [users]
                allow_sign_up = false
        EOF

                ${pkgs.grafana}/bin/grafana server \
                  --homepath ${pkgs.grafana}/share/grafana \
                  --config "$GRAFANA_DATA/grafana.ini" &
                GRAFANA_PID=$!

                echo ""
                echo "✅ Monitoring stack started!"
                echo ""
                echo "Access points:"
                echo "  📊 Grafana:    http://localhost:3001 (admin/admin)"
                echo "  📈 Prometheus: http://localhost:9090"
                echo "  🖥️  Node Exp:   http://localhost:9100/metrics"
                echo ""
                echo "Dashboards:"
                echo "  → Hoard Monitoring (auto-provisioned)"
                echo ""
                echo "Press Ctrl+C to stop all services"
                echo ""

                # Wait for any background job to exit
                wait -n
      ''}";
    };
  };
}
