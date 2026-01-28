# Observability & Monitoring

This document describes the observability infrastructure for Hoard, including metrics collection, visualization, and the complete monitoring stack.

## Overview

Hoard's observability system is built on:

- **Prometheus** - Time-series metrics storage and querying
- **Grafana** - Visualization and dashboards
- **prometheus-client** - Haskell library for metrics collection
- **prometheus-metrics-ghc** - GHC runtime metrics
- **node_exporter** - System-level metrics

### Access Points

- **Hoard Metrics Endpoint:** http://localhost:3000/metrics
- **Prometheus UI:** http://localhost:9090
- **Grafana:** http://localhost:3001
  - Username: `admin`
  - Password: `admin` (change on first login)
- **Node Exporter:** http://localhost:9100/metrics

## Architecture

### Metrics Effect

Hoard uses an Effectful-based `Metrics` effect for type-safe, composable metrics collection:

```haskell
-- Example usage
myFunction :: (Metrics :> es) => Eff es ()
myFunction = do
    gaugeSet "hoard_connected_peers" 5.0
    counterInc "hoard_blocks_received_total"
```

### GHC Runtime Metrics

Automatically collected when Hoard starts (GHC metrics are registered automatically by the Metrics effect):

**Note:** Hoard is configured with `-with-rtsopts=-T` in `package.yaml` to enable GHC runtime statistics collection.

Key metrics include:
- `ghc_gcdetails_live_bytes` - Current heap size in bytes
- `ghc_gc_cpu_seconds_total` - Cumulative CPU time spent in GC
- `ghc_gc_elapsed_seconds_total` - Cumulative wall clock time spent in GC
- `ghc_gcs_total` - Total number of garbage collections
- `ghc_allocated_bytes_total` - Total bytes allocated

### System Metrics

Collected by `node_exporter`:

- `node_cpu_seconds_total` - CPU usage by mode (user, system, idle, etc.)
- `node_memory_*` - Memory statistics (available, total, etc.)
- `node_disk_*` - Disk I/O statistics
- `node_network_*` - Network interface statistics

## HTTP Endpoint

**Endpoint:** `GET /metrics`
- **Port:** 3000 (same as main Hoard API)
- **Format:** Prometheus text format (OpenMetrics compatible)
- **Implementation:** `src/Hoard/API.hs`

Example:
```bash
curl http://localhost:3000/metrics
```

## Instrumentation Points

### Monitoring Module
**File:** `src/Hoard/Monitoring.hs`

Polls every N seconds (configured via `monitoring.pollingIntervalSeconds`):
- Updates `hoard_connected_peers` gauge
- Updates `hoard_blocks_in_db` gauge

### Protocol Listeners

**BlockFetch:** `src/Hoard/BlockFetch/Listeners.hs`
- Increments `hoard_blocks_received_total` on block receipt
- Increments `hoard_block_fetch_failures_total` on failures

**ChainSync:** `src/Hoard/ChainSync/Listeners.hs`
- Increments `hoard_headers_received_total` on header receipt
- Increments `hoard_chain_sync_rollbacks_total` on rollbacks
- Increments `hoard_chain_sync_rollforwards_total` on rollforwards

### Database Operations

**DBRead Effect:** `src/Hoard/Effects/DBRead.hs`

All database queries are automatically instrumented:
- Increments `hoard_db_queries_total` before execution
- Observes duration in `hoard_db_query_duration_seconds` histogram
- Increments `hoard_db_query_errors_total` on failures

## Monitoring Stack

### Starting the Complete Stack

```bash
# Terminal 1: Start PostgreSQL database
nix run .#postgres

# Terminal 2: Start Cardano node
nix run .#cardano-node-preprod

# Terminal 3: Start Hoard (GHC metrics automatically enabled via -with-rtsopts=-T)
cabal run hoard-exe

# Terminal 4: Start monitoring stack (Prometheus + Grafana + node_exporter)
nix run .#monitoring
```

### Individual Services

Start services separately if needed:

```bash
# Start only Prometheus
nix run .#prometheus

# Start only Grafana
nix run .#grafana

# Start only node_exporter
nix run .#node-exporter
```

## Extending Metrics

### Adding a New Metric

1. **Define metric name** in `src/Hoard.Effects.Metrics.Definitions.hs`:
   ```haskell
   metricNewThing :: Text
   metricNewThing = "hoard_new_thing_total"
   ```

2. **Add helper function** (optional):
   ```haskell
   recordNewThing :: (Metrics :> es) => Eff es ()
   recordNewThing = counterInc metricNewThing
   ```

3. **Instrument code:**
   ```haskell
   myFunction :: (Metrics :> es) => Eff es ()
   myFunction = do
       -- ... your code ...
       recordNewThing
   ```

4. **Verify metric appears:**
   ```bash
   curl http://localhost:3000/metrics | grep hoard_new_thing
   ```

5. **Add to Grafana dashboard** (optional):
   - Edit dashboard definition in nix/monitoring.nix
   - Add new panel with query: `rate(hoard_new_thing_total[5m])`
