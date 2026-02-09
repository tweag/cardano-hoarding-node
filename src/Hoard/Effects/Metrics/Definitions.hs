-- | Application-specific metric definitions.
--
-- Defines metric names and provides helper functions for common metric operations.
module Hoard.Effects.Metrics.Definitions
    ( -- * Metric Names

      -- ** Gauges
      metricConnectedPeers
    , metricPendingPeers
    , metricBlocksInDB

      -- ** Counters
    , metricBlocksReceived
    , metricHeadersReceived
    , metricBlockFetchFailures
    , metricChainSyncRollbacks
    , metricChainSyncRollforwards
    , metricDBQueries
    , metricDBQueryErrors

      -- ** Histograms
    , metricBlockFetchDuration
    , metricDBQueryDuration
    , metricPeerManagerCullBatches
    , metricPeerManagerReplenishedCollector

      -- * Helper Functions
    , recordBlockReceived
    , recordHeaderReceived
    , recordBlockFetchFailure
    , recordChainSyncRollback
    , recordChainSyncRollforward
    , recordDBQuery
    , recordDBQueryError
    ) where

import Effectful (Eff, (:>))

import Hoard.Effects.Metrics (Metrics, counterInc)


-- | Gauges - Point-in-time values
metricConnectedPeers :: Text
metricConnectedPeers = "hoard_connected_peers"


metricPendingPeers :: Text
metricPendingPeers = "hoard_pending_peers"


metricBlocksInDB :: Text
metricBlocksInDB = "hoard_blocks_in_db"


-- | Counters - Monotonically increasing values
metricBlocksReceived :: Text
metricBlocksReceived = "hoard_blocks_received_total"


metricHeadersReceived :: Text
metricHeadersReceived = "hoard_headers_received_total"


metricBlockFetchFailures :: Text
metricBlockFetchFailures = "hoard_block_fetch_failures_total"


metricChainSyncRollbacks :: Text
metricChainSyncRollbacks = "hoard_chain_sync_rollbacks_total"


metricChainSyncRollforwards :: Text
metricChainSyncRollforwards = "hoard_chain_sync_rollforwards_total"


metricDBQueries :: Text
metricDBQueries = "hoard_db_queries_total"


metricDBQueryErrors :: Text
metricDBQueryErrors = "hoard_db_query_errors_total"


-- | Histograms - Distributions with buckets
metricBlockFetchDuration :: Text
metricBlockFetchDuration = "hoard_block_fetch_duration_seconds"


metricDBQueryDuration :: Text
metricDBQueryDuration = "hoard_db_query_duration_seconds"


metricPeerManagerCullBatches :: Text
metricPeerManagerCullBatches = "hoard_peer_manager_cull_batches"


metricPeerManagerReplenishedCollector :: Text
metricPeerManagerReplenishedCollector = "hoard_peer_manager_replenished_collector"


-- | Record a block received event
recordBlockReceived :: (Metrics :> es) => Eff es ()
recordBlockReceived = counterInc metricBlocksReceived


-- | Record a header received event
recordHeaderReceived :: (Metrics :> es) => Eff es ()
recordHeaderReceived = counterInc metricHeadersReceived


-- | Record a block fetch failure
recordBlockFetchFailure :: (Metrics :> es) => Eff es ()
recordBlockFetchFailure = counterInc metricBlockFetchFailures


-- | Record a chain sync rollback
recordChainSyncRollback :: (Metrics :> es) => Eff es ()
recordChainSyncRollback = counterInc metricChainSyncRollbacks


-- | Record a chain sync rollforward
recordChainSyncRollforward :: (Metrics :> es) => Eff es ()
recordChainSyncRollforward = counterInc metricChainSyncRollforwards


-- | Record a database query
recordDBQuery :: (Metrics :> es) => Eff es ()
recordDBQuery = counterInc metricDBQueries


-- | Record a database query error
recordDBQueryError :: (Metrics :> es) => Eff es ()
recordDBQueryError = counterInc metricDBQueryErrors
