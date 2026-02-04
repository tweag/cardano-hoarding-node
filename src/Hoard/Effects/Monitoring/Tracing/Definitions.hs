-- | Application-specific tracing span definitions.
--
-- Defines span names and provides convenience wrappers for common tracing patterns.
module Hoard.Effects.Monitoring.Tracing.Definitions
    ( -- * Span Names

      -- ** API Spans
      spanAPIMetrics
    , spanAPIBlocks
    , spanAPIHeaders

      -- ** Protocol Spans
    , spanChainSyncRollForward
    , spanChainSyncRollBackward
    , spanBlockFetchRequest
    , spanBlockFetchReceive

      -- ** Database Spans
    , spanDBQueryBlocks
    , spanDBInsertBlock
    , spanDBQueryHeaders
    , spanDBInsertHeader

      -- ** System Spans
    , spanServerStartup
    , spanServerShutdown
    , spanMonitoringPoll

      -- * Convenience Functions
    , withAPISpan
    , withQuerySpan
    ) where

import Effectful (Eff, (:>))

import Hoard.Effects.Monitoring.Tracing (Tracing, addAttribute, withSpan)


-- | API-level span names
spanAPIMetrics :: Text
spanAPIMetrics = "api.metrics"


spanAPIBlocks :: Text
spanAPIBlocks = "api.blocks"


spanAPIHeaders :: Text
spanAPIHeaders = "api.headers"


-- | Protocol span names
spanChainSyncRollForward :: Text
spanChainSyncRollForward = "protocol.chainsync.rollforward"


spanChainSyncRollBackward :: Text
spanChainSyncRollBackward = "protocol.chainsync.rollbackward"


spanBlockFetchRequest :: Text
spanBlockFetchRequest = "protocol.blockfetch.request"


spanBlockFetchReceive :: Text
spanBlockFetchReceive = "protocol.blockfetch.receive"


-- | Database span names
spanDBQueryBlocks :: Text
spanDBQueryBlocks = "db.query.blocks"


spanDBInsertBlock :: Text
spanDBInsertBlock = "db.insert.block"


spanDBQueryHeaders :: Text
spanDBQueryHeaders = "db.query.headers"


spanDBInsertHeader :: Text
spanDBInsertHeader = "db.insert.header"


-- | System-level span names
spanServerStartup :: Text
spanServerStartup = "system.server.startup"


spanServerShutdown :: Text
spanServerShutdown = "system.server.shutdown"


spanMonitoringPoll :: Text
spanMonitoringPoll = "system.monitoring.poll"


-- | Convenience wrapper for API spans with automatic endpoint attribute
withAPISpan
    :: (Tracing :> es)
    => Text
    -- ^ Span name
    -> Text
    -- ^ Endpoint path (e.g., "/blocks")
    -> Eff es a
    -> Eff es a
withAPISpan spanName endpoint action = do
    withSpan spanName $ do
        addAttribute "http.endpoint" endpoint
        action


-- | Convenience wrapper for database query spans with automatic query type attribute
withQuerySpan
    :: (Tracing :> es)
    => Text
    -- ^ Span name
    -> Text
    -- ^ Query type (e.g., "SELECT", "INSERT", "UPDATE")
    -> Eff es a
    -> Eff es a
withQuerySpan spanName queryType action = do
    withSpan spanName $ do
        addAttribute "db.operation" queryType
        action
