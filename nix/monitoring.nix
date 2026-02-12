{
  pkgs,
  lib,
  config ? "config/dev.yaml",
}:
let
  inherit (lib) types mkOption;

  # Module options for monitoring configuration
  monitoringOpts = {
    # Grafana options
    grafana = {
      allowSignUp = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to allow user registration";
      };
    };

    # Prometheus options
    prometheus = {
      enableRemoteWrite = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable remote write receiver";
      };
      serviceLabel = mkOption {
        type = types.str;
        default = "service";
        description = "Label name for service identification";
      };
      environmentLabel = mkOption {
        type = types.str;
        default = "environment";
        description = "Label name for environment identification";
      };
    };

    # Application metrics options
    metrics = {
      histogramBuckets = mkOption {
        type = types.listOf types.float;
        default = [
          0.001
          0.01
          0.1
          1.0
          10.0
        ];
        description = "Default histogram buckets for duration metrics (in seconds)";
      };
    };
  };

  # Default configuration with all options
  cfg = {
    grafana = {
      inherit (monitoringOpts.grafana)
        allowSignUp
        ;
    };
    prometheus = {
      inherit (monitoringOpts.prometheus)
        enableRemoteWrite
        serviceLabel
        environmentLabel
        ;
    };
    metrics = {
      inherit (monitoringOpts.metrics) histogramBuckets;
    };
  };

  # Helper to read config values using yq
  yq = "${pkgs.yq-go}/bin/yq";

  # Prometheus configuration script that generates config from YAML
  prometheusConfigScript = pkgs.writeShellScript "prometheus-config" ''
    CONFIG_FILE="''${CONFIG_FILE:-${config}}"
    PROMETHEUS_DATA="''${PROMETHEUS_DATA:-$PWD/prometheus-data}"
    mkdir -p "$PROMETHEUS_DATA"

    SCRAPE_INTERVAL=$(${yq} eval '.monitoring.prometheus.scrape_interval' "$CONFIG_FILE")
    EVAL_INTERVAL=$(${yq} eval '.monitoring.prometheus.evaluation_interval' "$CONFIG_FILE")
    HOARD_TARGET=$(${yq} eval '.monitoring.prometheus.targets.hoard' "$CONFIG_FILE")
    NODE_EXPORTER_TARGET=$(${yq} eval '.monitoring.prometheus.targets.node_exporter' "$CONFIG_FILE")
    PROMETHEUS_PORT=$(${yq} eval '.monitoring.prometheus.port' "$CONFIG_FILE")
    ENVIRONMENT=$(${yq} eval '.monitoring.environment' "$CONFIG_FILE")

    cat > "$PROMETHEUS_DATA/prometheus.yml" <<EOF
    global:
      scrape_interval: $SCRAPE_INTERVAL
      evaluation_interval: $EVAL_INTERVAL

    scrape_configs:
      - job_name: 'hoard'
        static_configs:
          - targets: ['$HOARD_TARGET']
            labels:
              ${cfg.prometheus.serviceLabel.default}: 'hoard'
              ${cfg.prometheus.environmentLabel.default}: '$ENVIRONMENT'

      - job_name: 'node_exporter'
        static_configs:
          - targets: ['$NODE_EXPORTER_TARGET']
            labels:
              ${cfg.prometheus.serviceLabel.default}: 'system'
              ${cfg.prometheus.environmentLabel.default}: '$ENVIRONMENT'

      - job_name: 'prometheus'
        static_configs:
          - targets: ['localhost:$PROMETHEUS_PORT']
            labels:
              ${cfg.prometheus.serviceLabel.default}: 'prometheus'
              ${cfg.prometheus.environmentLabel.default}: '$ENVIRONMENT'
    EOF

    echo "$PROMETHEUS_DATA/prometheus.yml"
  '';

  # Tempo configuration script that generates config with runtime paths
  tempoConfigScript = pkgs.writeShellScript "tempo-config" ''
    CONFIG_FILE="''${CONFIG_FILE:-${config}}"
    TEMPO_DATA="''${TEMPO_DATA:-$PWD/tempo-data}"
    mkdir -p "$TEMPO_DATA/blocks" "$TEMPO_DATA/wal"

    HTTP_PORT=$(${yq} eval '.monitoring.tempo.http_port' "$CONFIG_FILE")
    BLOCK_RETENTION=$(${yq} eval '.monitoring.tempo.block_retention' "$CONFIG_FILE")
    MAX_BLOCK_DURATION=$(${yq} eval '.monitoring.tempo.max_block_duration' "$CONFIG_FILE")
    PROMETHEUS_PORT=$(${yq} eval '.monitoring.prometheus.port' "$CONFIG_FILE")

    cat > "$TEMPO_DATA/tempo.yaml" <<EOF
    server:
      http_listen_port: $HTTP_PORT

    query_frontend:
      search:
        duration_slo: 5s
        throughput_bytes_slo: 1.073741824e+09
      trace_by_id:
        duration_slo: 5s

    distributor:
      receivers:
        otlp:
          protocols:
            http:
            grpc:

    ingester:
      max_block_duration: $MAX_BLOCK_DURATION
      trace_idle_period: 10s
      max_block_bytes: 1000000

    metrics_generator:
      registry:
        external_labels:
          source: tempo
          cluster: local
      storage:
        path: $TEMPO_DATA/generator/wal
        remote_write:
          - url: http://localhost:$PROMETHEUS_PORT/api/v1/write
            send_exemplars: true
      # Traces storage for local_blocks processor
      traces_storage:
        path: $TEMPO_DATA/generator/traces
      # Generate span metrics for "All spans" drilldown in Grafana
      processor:
        service_graphs:
          dimensions:
            - service.name
        span_metrics:
          # Include resource attributes in target_info metric
          enable_target_info: true
          # Custom dimensions from span attributes
          dimensions:
            - span.name
            - span.kind
            - status.code
          # Histogram buckets for duration metrics (in seconds)
          histogram_buckets:
            - 0.001   # 1ms
            - 0.01    # 10ms
            - 0.1     # 100ms
            - 1.0     # 1s
            - 10.0    # 10s
        # Enable TraceQL metrics for span-level queries
        local_blocks:
          max_live_traces: 10000
          max_block_bytes: 100000000
          flush_check_period: 10s

    querier:
      max_concurrent_queries: 20

    compactor:
      compaction:
        block_retention: $BLOCK_RETENTION

    # Enable metrics generator for all tenants
    overrides:
      metrics_generator_processors:
        - service-graphs
        - span-metrics
        - local-blocks

    storage:
      trace:
        backend: local
        local:
          path: $TEMPO_DATA/blocks
        wal:
          path: $TEMPO_DATA/wal
        pool:
          max_workers: 100
          queue_depth: 10000
    EOF

    echo "$TEMPO_DATA/tempo.yaml"
  '';

  # Service startup scripts (reusable)
  startNodeExporter = pkgs.writeShellScript "start-node-exporter" ''
    export CONFIG_FILE="''${CONFIG_FILE:-${config}}"
    PORT=$(${yq} eval '.monitoring.node_exporter.port' "$CONFIG_FILE")

    echo "Starting Node Exporter on port $PORT..."
    exec ${pkgs.prometheus-node-exporter}/bin/node_exporter \
      --web.listen-address=:$PORT
  '';

  startTempo = pkgs.writeShellScript "start-tempo" ''
    export CONFIG_FILE="''${CONFIG_FILE:-${config}}"
    export TEMPO_DATA="$PWD/tempo-data"
    TEMPO_CONFIG=$(${tempoConfigScript})

    echo "Starting Tempo..."
    exec ${pkgs.tempo}/bin/tempo -config.file="$TEMPO_CONFIG"
  '';

  startPrometheus = pkgs.writeShellScript "start-prometheus" ''
    export CONFIG_FILE="''${CONFIG_FILE:-${config}}"
    export PROMETHEUS_DATA="$PWD/prometheus-data"
    PROMETHEUS_CONFIG=$(${prometheusConfigScript})
    PORT=$(${yq} eval '.monitoring.prometheus.port' "$CONFIG_FILE")

    echo "Starting Prometheus on port $PORT..."
    exec ${pkgs.prometheus}/bin/prometheus \
      --config.file="$PROMETHEUS_CONFIG" \
      --storage.tsdb.path="$PROMETHEUS_DATA" \
      --web.listen-address=:$PORT \
      ${lib.optionalString cfg.prometheus.enableRemoteWrite.default "--web.enable-remote-write-receiver"} \
      --web.console.templates=${pkgs.prometheus}/etc/prometheus/consoles \
      --web.console.libraries=${pkgs.prometheus}/etc/prometheus/console_libraries
  '';

  startGrafana = pkgs.writeShellScript "start-grafana" ''
        export CONFIG_FILE="''${CONFIG_FILE:-${config}}"
        GRAFANA_DATA="$PWD/grafana-data"
        GRAFANA_PROVISIONING="$GRAFANA_DATA/provisioning"
        GRAFANA_DASHBOARDS="$GRAFANA_PROVISIONING/dashboards"

        PORT=$(${yq} eval '.monitoring.grafana.port' "$CONFIG_FILE")
        HOST=$(${yq} eval '.monitoring.grafana.host' "$CONFIG_FILE")
        ADMIN_USER=$(${yq} eval '.monitoring.grafana.admin_user' "$CONFIG_FILE")
        ADMIN_PASSWORD=$(${yq} eval '.monitoring.grafana.admin_password' "$CONFIG_FILE")

        mkdir -p "$GRAFANA_DATA"
        mkdir -p "$GRAFANA_PROVISIONING/datasources"
        mkdir -p "$GRAFANA_PROVISIONING/dashboards"
        mkdir -p "$GRAFANA_PROVISIONING/plugins"
        mkdir -p "$GRAFANA_PROVISIONING/alerting"
        mkdir -p "$GRAFANA_DASHBOARDS"

        # Clean up old datasource files
        rm -f "$GRAFANA_PROVISIONING/datasources"/*.yml

        # Generate datasources configuration
        ${grafanaDatasourcesScript} "$GRAFANA_PROVISIONING/datasources/datasources.yml"
        install -m 644 ${performanceDashboard} "$GRAFANA_DASHBOARDS/performance.json"
        install -m 644 ${errorDashboard} "$GRAFANA_DASHBOARDS/errors.json"
        install -m 644 ${domainDashboard} "$GRAFANA_DASHBOARDS/domain.json"

        # Generate dashboard provisioning config
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

        # Generate grafana.ini
        cat > "$GRAFANA_DATA/grafana.ini" <<EOF
    [server]
    http_port = $PORT
    http_addr = $HOST

    [paths]
    data = $GRAFANA_DATA
    logs = $GRAFANA_DATA/log
    plugins = $GRAFANA_DATA/plugins
    provisioning = $GRAFANA_PROVISIONING

    [security]
    admin_user = $ADMIN_USER
    admin_password = $ADMIN_PASSWORD

    [analytics]
    reporting_enabled = false
    check_for_updates = false

    [users]
    allow_sign_up = ${if cfg.grafana.allowSignUp.default then "true" else "false"}
    EOF

        echo "Starting Grafana on $HOST:$PORT..."
        exec ${pkgs.grafana}/bin/grafana server \
          --homepath ${pkgs.grafana}/share/grafana \
          --config "$GRAFANA_DATA/grafana.ini"
  '';

  # Grafana datasource configuration script
  grafanaDatasourcesScript = pkgs.writeShellScript "grafana-datasources" ''
    CONFIG_FILE="''${CONFIG_FILE:-${config}}"
    OUTPUT_FILE="$1"

    PROMETHEUS_PORT=$(${yq} eval '.monitoring.prometheus.port' "$CONFIG_FILE")
    TEMPO_PORT=$(${yq} eval '.monitoring.tempo.http_port' "$CONFIG_FILE")

    cat > "$OUTPUT_FILE" <<EOF
    apiVersion: 1

    datasources:
      - name: Prometheus
        type: prometheus
        access: proxy
        url: http://localhost:$PROMETHEUS_PORT
        isDefault: true
        editable: false
        jsonData:
          exemplarTraceIdDestinations:
            - name: trace_id
              datasourceUid: tempo

      - name: Tempo
        type: tempo
        access: proxy
        url: http://localhost:$TEMPO_PORT
        uid: tempo
        editable: false
        jsonData:
          nodeGraph:
            enabled: true
    EOF
  '';

  # Performance metrics dashboard
  performanceDashboard = pkgs.writeText "hoard-performance.json" (
    builtins.toJSON {
      title = "Hoard Performance";
      tags = [
        "hoard"
        "performance"
      ];
      timezone = "browser";
      editable = false;
      schemaVersion = 16;
      version = 0;
      refresh = "5s";
      uid = "hoard-performance";

      panels = [
        # Memory Usage Row
        {
          id = 100;
          title = "Memory Usage";
          type = "row";
          gridPos = {
            x = 0;
            y = 0;
            w = 24;
            h = 1;
          };
          collapsed = false;
        }
        {
          id = 1;
          title = "Heap Memory";
          type = "graph";
          description = "Current and peak live heap size. Shows memory actively used by the application.";
          gridPos = {
            x = 0;
            y = 1;
            w = 12;
            h = 8;
          };
          targets = [
            {
              expr = "ghc_gcdetails_live_bytes";
              refId = "A";
              legendFormat = "current live heap";
            }
            {
              expr = "ghc_max_live_bytes";
              refId = "B";
              legendFormat = "peak live heap";
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
          id = 2;
          title = "Total Memory In Use";
          type = "graph";
          description = "Total memory in use by the RTS including heap, stacks, and metadata.";
          gridPos = {
            x = 12;
            y = 1;
            w = 12;
            h = 8;
          };
          targets = [
            {
              expr = "ghc_gcdetails_mem_in_use_bytes";
              refId = "A";
              legendFormat = "current mem in use";
            }
            {
              expr = "ghc_max_mem_in_use_bytes";
              refId = "B";
              legendFormat = "peak mem in use";
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
          id = 3;
          title = "Large Objects";
          type = "graph";
          description = "Memory used by large objects (objects too large for regular heap blocks).";
          gridPos = {
            x = 0;
            y = 9;
            w = 12;
            h = 8;
          };
          targets = [
            {
              expr = "ghc_gcdetails_large_objects_bytes";
              refId = "A";
              legendFormat = "current large objects";
            }
            {
              expr = "ghc_max_large_objects_bytes";
              refId = "B";
              legendFormat = "peak large objects";
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
          id = 4;
          title = "Memory Slop";
          type = "graph";
          description = "Wasted memory due to fragmentation and padding.";
          gridPos = {
            x = 12;
            y = 9;
            w = 12;
            h = 8;
          };
          targets = [
            {
              expr = "ghc_gcdetails_slop_bytes";
              refId = "A";
              legendFormat = "current slop";
            }
            {
              expr = "ghc_max_slop_bytes";
              refId = "B";
              legendFormat = "peak slop";
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

        # Garbage Collection Row
        {
          id = 101;
          title = "Garbage Collection";
          type = "row";
          gridPos = {
            x = 0;
            y = 17;
            w = 24;
            h = 1;
          };
          collapsed = false;
        }
        {
          id = 5;
          title = "GC Time";
          type = "graph";
          description = "Percentage of time spent in garbage collection. Shows GC CPU time and wall clock time.";
          gridPos = {
            x = 0;
            y = 18;
            w = 12;
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
        {
          id = 6;
          title = "Total GCs";
          type = "stat";
          description = "Total number of garbage collections since start.";
          gridPos = {
            x = 12;
            y = 18;
            w = 6;
            h = 8;
          };
          targets = [
            {
              expr = "ghc_gcs_total";
              refId = "A";
            }
          ];
          options = {
            textMode = "value_and_name";
            colorMode = "none";
          };
          fieldConfig = {
            defaults = {
              unit = "short";
            };
          };
        }
        {
          id = 18;
          title = "Major GCs";
          type = "stat";
          description = "Total number of major garbage collections since start.";
          gridPos = {
            x = 18;
            y = 18;
            w = 6;
            h = 8;
          };
          targets = [
            {
              expr = "ghc_major_gcs_total";
              refId = "A";
            }
          ];
          options = {
            textMode = "value_and_name";
            colorMode = "none";
          };
          fieldConfig = {
            defaults = {
              unit = "short";
            };
          };
        }
        {
          id = 7;
          title = "GC Rate";
          type = "graph";
          description = "Rate of garbage collections per second.";
          gridPos = {
            x = 0;
            y = 26;
            w = 12;
            h = 8;
          };
          targets = [
            {
              expr = "rate(ghc_gcs_total[5m])";
              refId = "A";
              legendFormat = "gc rate";
            }
            {
              expr = "rate(ghc_major_gcs_total[5m])";
              refId = "B";
              legendFormat = "major gc rate";
            }
          ];
          yaxes = [
            {
              format = "ops";
              label = "GCs/sec";
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
          title = "GC Details";
          type = "stat";
          description = "Current GC generation and thread count.";
          gridPos = {
            x = 12;
            y = 26;
            w = 6;
            h = 8;
          };
          targets = [
            {
              expr = "ghc_gcdetails_gen";
              refId = "A";
            }
          ];
          options = {
            textMode = "value_and_name";
            colorMode = "none";
          };
        }
        {
          id = 9;
          title = "GC Threads";
          type = "stat";
          description = "Number of threads used for garbage collection.";
          gridPos = {
            x = 18;
            y = 26;
            w = 6;
            h = 8;
          };
          targets = [
            {
              expr = "ghc_gcdetails_threads";
              refId = "A";
            }
          ];
          options = {
            textMode = "value_and_name";
            colorMode = "none";
          };
        }

        # Allocation & Copying Row
        {
          id = 102;
          title = "Allocation & Copying";
          type = "row";
          gridPos = {
            x = 0;
            y = 34;
            w = 24;
            h = 1;
          };
          collapsed = false;
        }
        {
          id = 10;
          title = "Allocation Rate";
          type = "graph";
          description = "Rate of memory allocation in bytes per second.";
          gridPos = {
            x = 0;
            y = 35;
            w = 12;
            h = 8;
          };
          targets = [
            {
              expr = "rate(ghc_allocated_bytes_total[5m])";
              refId = "A";
              legendFormat = "allocation rate";
            }
          ];
          yaxes = [
            {
              format = "Bps";
              label = "Bytes/sec";
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
          title = "Total Allocated";
          type = "stat";
          description = "Total bytes allocated since start.";
          gridPos = {
            x = 12;
            y = 35;
            w = 12;
            h = 8;
          };
          targets = [
            {
              expr = "ghc_allocated_bytes_total";
              refId = "A";
            }
          ];
          options = {
            textMode = "value_and_name";
            colorMode = "none";
            graphMode = "none";
          };
          fieldConfig = {
            defaults = {
              unit = "bytes";
            };
          };
        }
        {
          id = 12;
          title = "GC Copying Rate";
          type = "graph";
          description = "Rate at which GC copies live data between generations.";
          gridPos = {
            x = 0;
            y = 43;
            w = 24;
            h = 8;
          };
          targets = [
            {
              expr = "rate(ghc_copied_bytes_total[5m])";
              refId = "A";
              legendFormat = "copy rate";
            }
          ];
          yaxes = [
            {
              format = "Bps";
              label = "Bytes/sec";
              show = true;
            }
            {
              format = "none";
              show = false;
            }
          ];
        }

        # Application Time Row
        {
          id = 103;
          title = "Application (Mutator) Time";
          type = "row";
          gridPos = {
            x = 0;
            y = 51;
            w = 24;
            h = 1;
          };
          collapsed = false;
        }
        {
          id = 14;
          title = "Mutator Time";
          type = "graph";
          description = "Percentage of time spent in application code (not GC).";
          gridPos = {
            x = 0;
            y = 52;
            w = 12;
            h = 8;
          };
          targets = [
            {
              expr = "rate(ghc_mutator_cpu_seconds_total[5m])";
              refId = "A";
              legendFormat = "mutator cpu time";
            }
            {
              expr = "rate(ghc_mutator_elapsed_seconds_total[5m])";
              refId = "B";
              legendFormat = "mutator wall time";
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
        {
          id = 15;
          title = "Mutator vs GC Time";
          type = "graph";
          description = "Comparison of time spent in application code vs garbage collection.";
          gridPos = {
            x = 12;
            y = 52;
            w = 12;
            h = 8;
          };
          targets = [
            {
              expr = "rate(ghc_mutator_cpu_seconds_total[5m])";
              refId = "A";
              legendFormat = "mutator cpu";
            }
            {
              expr = "rate(ghc_gc_cpu_seconds_total[5m])";
              refId = "B";
              legendFormat = "gc cpu";
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

        # Database Performance Row
        {
          id = 104;
          title = "Database Performance";
          type = "row";
          gridPos = {
            x = 0;
            y = 60;
            w = 24;
            h = 1;
          };
          collapsed = false;
        }
        {
          id = 20;
          title = "Database Query Rate";
          type = "graph";
          description = "Rate of database queries being executed (5-minute average). Shows the database workload as blocks and metadata are stored and retrieved.";
          gridPos = {
            x = 0;
            y = 61;
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
          id = 21;
          title = "Query Duration (95th percentile)";
          type = "graph";
          description = "95th percentile of database query execution time. This represents the duration that 95% of queries complete within, helping identify performance issues.";
          gridPos = {
            x = 8;
            y = 61;
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
          id = 22;
          title = "Query Duration (p50, p90, p99)";
          type = "graph";
          description = "Query duration percentiles over time. p50 is median, p90 is 90th percentile, p99 is 99th percentile.";
          gridPos = {
            x = 16;
            y = 61;
            w = 8;
            h = 8;
          };
          targets = [
            {
              expr = "histogram_quantile(0.5, rate(hoard_db_query_duration_seconds_bucket[5m]))";
              refId = "A";
              legendFormat = "p50";
            }
            {
              expr = "histogram_quantile(0.9, rate(hoard_db_query_duration_seconds_bucket[5m]))";
              refId = "B";
              legendFormat = "p90";
            }
            {
              expr = "histogram_quantile(0.99, rate(hoard_db_query_duration_seconds_bucket[5m]))";
              refId = "C";
              legendFormat = "p99";
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

        # Total Runtime Row
        {
          id = 105;
          title = "Total Runtime";
          type = "row";
          gridPos = {
            x = 0;
            y = 69;
            w = 24;
            h = 1;
          };
          collapsed = false;
        }
        {
          id = 16;
          title = "Uptime";
          type = "stat";
          description = "Total wall clock time since application start.";
          gridPos = {
            x = 0;
            y = 70;
            w = 8;
            h = 8;
          };
          targets = [
            {
              expr = "ghc_elapsed_seconds_total";
              refId = "A";
            }
          ];
          options = {
            textMode = "value_and_name";
            colorMode = "none";
            graphMode = "none";
          };
          fieldConfig = {
            defaults = {
              unit = "s";
            };
          };
        }
        {
          id = 17;
          title = "Total CPU Time";
          type = "stat";
          description = "Total CPU time consumed by the application since start.";
          gridPos = {
            x = 8;
            y = 70;
            w = 8;
            h = 8;
          };
          targets = [
            {
              expr = "ghc_cpu_seconds_total";
              refId = "A";
            }
          ];
          options = {
            textMode = "value_and_name";
            colorMode = "none";
            graphMode = "none";
          };
          fieldConfig = {
            defaults = {
              unit = "s";
            };
          };
        }
        {
          id = 19;
          title = "CPU Efficiency";
          type = "gauge";
          description = "Ratio of CPU time to wall time (higher is better, max 1.0 for single-threaded).";
          gridPos = {
            x = 16;
            y = 70;
            w = 8;
            h = 8;
          };
          targets = [
            {
              expr = "ghc_cpu_seconds_total / ghc_elapsed_seconds_total";
              refId = "A";
            }
          ];
          options = {
            showThresholdLabels = false;
            showThresholdMarkers = true;
          };
          fieldConfig = {
            defaults = {
              unit = "percentunit";
              min = 0;
              max = 1;
              thresholds = {
                mode = "absolute";
                steps = [
                  {
                    value = 0;
                    color = "red";
                  }
                  {
                    value = 0.5;
                    color = "yellow";
                  }
                  {
                    value = 0.8;
                    color = "green";
                  }
                ];
              };
            };
          };
        }
      ];
    }
  );

  # Error dashboard
  errorDashboard = pkgs.writeText "hoard-errors.json" (
    builtins.toJSON {
      title = "Hoard Errors";
      tags = [
        "hoard"
        "errors"
      ];
      timezone = "browser";
      editable = false;
      schemaVersion = 16;
      version = 0;
      refresh = "5s";
      uid = "hoard-errors";

      panels = [
        {
          id = 9;
          title = "Database Errors";
          type = "stat";
          description = "Rate of database query errors (5-minute average). Should be zero under normal operation. Non-zero values indicate database connectivity or query issues.";
          gridPos = {
            x = 0;
            y = 0;
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
      ];
    }
  );

  # Domain metrics dashboard
  domainDashboard = pkgs.writeText "hoard-domain.json" (
    builtins.toJSON {
      title = "Hoard Domain Metrics";
      tags = [
        "hoard"
        "domain"
      ];
      timezone = "browser";
      editable = false;
      schemaVersion = 16;
      version = 0;
      refresh = "5s";
      uid = "hoard-domain";

      panels = [
        # Chain State Row
        {
          id = 100;
          title = "Chain State";
          type = "row";
          gridPos = {
            x = 0;
            y = 0;
            w = 24;
            h = 1;
          };
          collapsed = false;
        }
        {
          id = 1;
          title = "Connected Peers";
          type = "stat";
          description = "Number of active peer connections to other Cardano nodes.";
          gridPos = {
            x = 0;
            y = 1;
            w = 6;
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
          title = "Blocks in Database";
          type = "stat";
          description = "Total number of blocks stored in the database. This grows as the node synchronizes with the network and stores historical blockchain data.";
          gridPos = {
            x = 6;
            y = 1;
            w = 6;
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
        {
          id = 5;
          title = "Unclassified Blocks";
          type = "stat";
          description = "Number of blocks in the database that have not yet been classified as canonical or orphaned. Blocks are classified after the immutable tip advances past them.";
          gridPos = {
            x = 12;
            y = 1;
            w = 6;
            h = 8;
          };
          targets = [
            {
              expr = "hoard_unclassified_blocks";
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
                    value = 100;
                    color = "yellow";
                  }
                  {
                    value = 1000;
                    color = "orange";
                  }
                ];
              };
            };
          };
        }
        {
          id = 6;
          title = "Blocks Being Classified";
          type = "stat";
          description = "Number of blocks currently being classified as canonical or orphaned. This represents active classification work in progress.";
          gridPos = {
            x = 18;
            y = 1;
            w = 6;
            h = 8;
          };
          targets = [
            {
              expr = "hoard_blocks_being_classified";
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
                    value = 50;
                    color = "yellow";
                  }
                  {
                    value = 200;
                    color = "orange";
                  }
                ];
              };
            };
          };
        }

        # IsOnChain Query Performance Row
        {
          id = 110;
          title = "IsOnChain Query Performance";
          type = "row";
          gridPos = {
            x = 0;
            y = 9;
            w = 24;
            h = 1;
          };
          collapsed = false;
        }
        {
          id = 3;
          title = "IsOnChain Query Duration";
          type = "graph";
          description = "Distribution of IsOnChain query execution times. Shows p50 (median), p90, and p99 percentiles.";
          gridPos = {
            x = 0;
            y = 10;
            w = 12;
            h = 8;
          };
          targets = [
            {
              expr = "histogram_quantile(0.5, rate(hoard_is_on_chain_duration_seconds_bucket[5m]))";
              refId = "A";
              legendFormat = "p50";
            }
            {
              expr = "histogram_quantile(0.9, rate(hoard_is_on_chain_duration_seconds_bucket[5m]))";
              refId = "B";
              legendFormat = "p90";
            }
            {
              expr = "histogram_quantile(0.99, rate(hoard_is_on_chain_duration_seconds_bucket[5m]))";
              refId = "C";
              legendFormat = "p99";
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
          id = 4;
          title = "IsOnChain Query Rate";
          type = "graph";
          description = "Rate of IsOnChain queries being executed (5-minute average). Indicates orphan classification activity.";
          gridPos = {
            x = 12;
            y = 10;
            w = 12;
            h = 8;
          };
          targets = [
            {
              expr = "rate(hoard_is_on_chain_duration_seconds_count[5m])";
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
      ];
    }
  );

  # Export histogram buckets configuration for use in application config
  metricsConfig = pkgs.writeText "metrics-config.yaml" ''
    metrics:
      histogram_buckets: ${builtins.toJSON cfg.metrics.histogramBuckets.default}
  '';
in
{
  # Export the configuration for external use
  inherit cfg metricsConfig;

  apps = {
    # Prometheus server
    prometheus = {
      type = "app";
      program = "${pkgs.writeShellScript "prometheus-app" ''
        set -e
        export CONFIG_FILE="''${CONFIG_FILE:-${config}}"
        PORT=$(${yq} eval '.monitoring.prometheus.port' "$CONFIG_FILE")
        HOARD_TARGET=$(${yq} eval '.monitoring.prometheus.targets.hoard' "$CONFIG_FILE")
        NODE_EXPORTER_TARGET=$(${yq} eval '.monitoring.prometheus.targets.node_exporter' "$CONFIG_FILE")

        echo "Starting Prometheus..."
        echo "  Web UI: http://localhost:$PORT"
        echo "  Scraping: http://$HOARD_TARGET/metrics, http://$NODE_EXPORTER_TARGET/metrics"
        echo ""
        echo "Press Ctrl+C to stop"
        echo ""

        exec ${startPrometheus}
      ''}";
    };

    # Grafana server
    grafana = {
      type = "app";
      program = "${pkgs.writeShellScript "grafana-app" ''
        set -e
        export CONFIG_FILE="''${CONFIG_FILE:-${config}}"
        PORT=$(${yq} eval '.monitoring.grafana.port' "$CONFIG_FILE")
        HOST=$(${yq} eval '.monitoring.grafana.host' "$CONFIG_FILE")
        ADMIN_USER=$(${yq} eval '.monitoring.grafana.admin_user' "$CONFIG_FILE")

        echo "Starting Grafana..."
        echo "  Web UI: http://$HOST:$PORT"
        echo "  Username: $ADMIN_USER"
        echo "  Dashboards: Performance, Errors, Domain"
        echo ""
        echo "Press Ctrl+C to stop"
        echo ""

        exec ${startGrafana}
      ''}";
    };

    # Node exporter for system metrics
    node-exporter = {
      type = "app";
      program = "${pkgs.writeShellScript "node-exporter-app" ''
        set -e
        export CONFIG_FILE="''${CONFIG_FILE:-${config}}"
        PORT=$(${yq} eval '.monitoring.node_exporter.port' "$CONFIG_FILE")

        echo "Starting Node Exporter..."
        echo "  Metrics endpoint: http://localhost:$PORT/metrics"
        echo "  Collecting: CPU, Memory, Disk I/O, Network I/O"
        echo ""
        echo "Press Ctrl+C to stop"
        echo ""

        exec ${startNodeExporter}
      ''}";
    };

    # Tempo for distributed tracing
    tempo = {
      type = "app";
      program = "${pkgs.writeShellScript "tempo-app" ''
        set -e
        export CONFIG_FILE="''${CONFIG_FILE:-${config}}"
        HTTP_PORT=$(${yq} eval '.monitoring.tempo.http_port' "$CONFIG_FILE")
        OTLP_HTTP_PORT=$(${yq} eval '.monitoring.tempo.otlp_http_port' "$CONFIG_FILE")

        echo "Starting Tempo..."
        echo "  HTTP: http://localhost:$HTTP_PORT"
        echo "  OTLP HTTP: localhost:$OTLP_HTTP_PORT"
        echo ""
        echo "Press Ctrl+C to stop"
        echo ""

        exec ${startTempo}
      ''}";
    };

    # All-in-one monitoring stack
    monitoring = {
      type = "app";
      program = "${pkgs.writeShellScript "monitoring-app" ''
        set -e

        export CONFIG_FILE="''${CONFIG_FILE:-${config}}"

        # Read config values
        PROMETHEUS_PORT=$(${yq} eval '.monitoring.prometheus.port' "$CONFIG_FILE")
        GRAFANA_PORT=$(${yq} eval '.monitoring.grafana.port' "$CONFIG_FILE")
        GRAFANA_HOST=$(${yq} eval '.monitoring.grafana.host' "$CONFIG_FILE")
        GRAFANA_ADMIN_USER=$(${yq} eval '.monitoring.grafana.admin_user' "$CONFIG_FILE")
        GRAFANA_ADMIN_PASSWORD=$(${yq} eval '.monitoring.grafana.admin_password' "$CONFIG_FILE")
        TEMPO_PORT=$(${yq} eval '.monitoring.tempo.http_port' "$CONFIG_FILE")
        NODE_EXPORTER_PORT=$(${yq} eval '.monitoring.node_exporter.port' "$CONFIG_FILE")
        HOARD_TARGET=$(${yq} eval '.monitoring.prometheus.targets.hoard' "$CONFIG_FILE")

        echo "Starting Hoard Monitoring Stack..."
        echo ""
        echo "This will start:"
        echo "  1. Prometheus (metrics storage & querying)"
        echo "  2. Grafana (visualization & dashboards)"
        echo "  3. Node Exporter (system metrics)"
        echo "  4. Tempo (distributed tracing)"
        echo ""
        echo "Make sure Hoard is running on http://$HOARD_TARGET"
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

        # Start all services in background
        ${startNodeExporter} &
        sleep 2

        ${startTempo} &
        sleep 2

        ${startPrometheus} &
        sleep 3

        ${startGrafana} &

        echo ""
        echo "✅ Monitoring stack started!"
        echo ""
        echo "Access points:"
        echo "  📊 Grafana:    http://$GRAFANA_HOST:$GRAFANA_PORT ($GRAFANA_ADMIN_USER/$GRAFANA_ADMIN_PASSWORD)"
        echo "  📈 Prometheus: http://localhost:$PROMETHEUS_PORT"
        echo "  🔍 Tempo:      http://localhost:$TEMPO_PORT"
        echo "  🖥️  Node Exp:   http://localhost:$NODE_EXPORTER_PORT/metrics"
        echo ""
        echo "Dashboards:"
        echo "  → Hoard Performance"
        echo "  → Hoard Errors"
        echo "  → Hoard Domain Metrics"
        echo ""
        echo "Press Ctrl+C to stop all services"
        echo ""

        # Wait for any background job to exit
        wait -n
      ''}";
    };
  };
}
