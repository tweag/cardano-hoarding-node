module Hoard.OrphanDetection.Listeners
    ( blockReceivedClassifier
    , immutableTipUpdatedAger
    ) where

import Cardano.Api (BlockHeader, ChainPoint (..), Hash, asType)
import Cardano.Api.Serialise.Raw (deserialiseFromRawBytes)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.State.Static.Shared (State, gets, modify)
import Ouroboros.Consensus.Block (SlotNo (..), blockSlot, getHeader, toRawHash)
import Ouroboros.Network.Block (blockHash)
import Prelude hiding (State, gets, modify)

import Data.Set qualified as Set

import Hoard.BlockFetch.Events (BlockReceived (..))
import Hoard.Data.Block (Block (..))
import Hoard.Data.BlockHash (blockHashFromHeader)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Clock (Clock, currentTime)
import Hoard.Effects.Monitoring.Tracing (SpanStatus (..), Tracing, addAttribute, addEvent, setStatus, withSpan)
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Listeners.ImmutableTipRefreshTriggeredListener (ImmutableTipRefreshed)
import Hoard.OrphanDetection.Data (BlockClassification (..))
import Hoard.Types.Cardano (CardanoBlock)
import Hoard.Types.HoardState (HoardState (..))

import Hoard.Effects.BlockRepo qualified as BlockRepo
import Hoard.Effects.NodeToClient qualified as NodeToClient
import Hoard.Types.Cardano qualified as Hoard


-- | Classify a block by querying isOnChain and updating the database
-- Also removes the block from the blocksBeingClassified set in HoardState
classifyBlockByChainStatus
    :: ( BlockRepo :> es
       , Clock :> es
       , Error Text :> es
       , NodeToClient :> es
       , State HoardState :> es
       , Tracing :> es
       )
    => CardanoBlock
    -> Eff es ()
classifyBlockByChainStatus blockData = withSpan "classify_block_by_chain_status" $ do
    let header = getHeader blockData
        hash = blockHashFromHeader header

    addAttribute "block.hash" (show hash)
    addAttribute "block.slot" (show $ blockSlot blockData)

    apiHash <- deserialiseBlockHeaderHash blockData
    let blockChainPoint = Hoard.ChainPoint (ChainPoint (blockSlot blockData) apiHash)

    NodeToClient.isOnChain blockChainPoint >>= \case
        Nothing -> do
            addEvent "is_on_chain_query_failed" [("block.hash", show hash)]
            setStatus $ Error "Failed to query isOnChain"
            -- Remove from being classified set even on failure to prevent permanent blocking
            modify $ \s -> s {blocksBeingClassified = Set.delete hash s.blocksBeingClassified}
        Just isOnChain -> do
            timestamp <- currentTime
            let classification = if isOnChain then Canonical else Orphaned
            addAttribute "block.classification" (show classification)
            addEvent "block_classified" [("block.hash", show hash), ("classification", show classification)]
            BlockRepo.classifyBlock hash classification timestamp
            setStatus Ok
            -- Remove from being classified set after successful classification
            modify $ \s -> s {blocksBeingClassified = Set.delete hash s.blocksBeingClassified}


-- | Listener that classifies blocks as they arrive
--
-- Classification algorithm:
-- 1. If block.slot >= immutableTip.slot → defer classification (keep NULL)
-- 2. If block.slot < immutableTip.slot:
--    - Query isOnChain for the block
--    - If True → classify as Canonical
--    - If False → classify as Orphaned
--
-- Note: There's a benign race between reading immutableTip and classifying.
-- This is self-correcting: blocks classified early are still correct, and
-- blocks deferred incorrectly will be caught by the aging listener.
blockReceivedClassifier
    :: ( BlockRepo :> es
       , Clock :> es
       , Error Text :> es
       , NodeToClient :> es
       , State HoardState :> es
       , Tracing :> es
       )
    => BlockReceived
    -> Eff es ()
blockReceivedClassifier event = do
    let blockSlotNumber = fromIntegral $ unSlotNo $ blockSlot event.block :: Int64

    immutableSlot <- gets (chainPointToSlot . (.immutableTip))

    addAttribute "block.slot" (show blockSlotNumber)
    addAttribute "immutable.slot" (show immutableSlot)

    -- Check if block is after immutable tip
    if blockSlotNumber >= immutableSlot then do
        -- Defer classification for blocks after immutable tip
        addEvent "classification_deferred" [("block.slot", show blockSlotNumber), ("reason", "after immutable tip")]
    else do
        -- Classify blocks before immutable tip immediately
        addEvent "classifying_block" [("block.slot", show blockSlotNumber)]
        classifyBlockByChainStatus event.block


-- | Listener that ages unclassified blocks when immutable tip advances
--
-- Aging algorithm:
-- 1. Get up to 1 unclassified blocks (classification IS NULL) with slot < new immutableTip slot
--    Excludes blocks currently being classified to prevent duplicate queries
-- 2. Add blocks to blocksBeingClassified set in HoardState
-- 3. For each block, send isOnChain query
-- 4. If True → classify as Canonical
-- 5. If False → classify as Orphaned
-- 6. Remove from blocksBeingClassified set (done in classifyBlockByChainStatus)
--
-- This creates a backlog when the node is unavailable - blocks remain unclassified
-- and will be processed when the service recovers.
immutableTipUpdatedAger
    :: ( BlockRepo :> es
       , Clock :> es
       , Error Text :> es
       , NodeToClient :> es
       , State HoardState :> es
       , Tracing :> es
       )
    => ImmutableTipRefreshed
    -> Eff es ()
immutableTipUpdatedAger _event = do
    newSlot <- gets (chainPointToSlot . (.immutableTip))
    beingClassified <- gets (.blocksBeingClassified)

    addAttribute "immutable.slot" (show newSlot)
    addAttribute "being_classified.count" (show $ Set.size beingClassified)

    -- Get up to 1 unclassified blocks that are not currently being classified
    unclassifiedBlocks <- BlockRepo.getUnclassifiedBlocksBeforeSlot newSlot 1 beingClassified

    let blocksToAge = length unclassifiedBlocks
    addAttribute "unclassified.count" (show blocksToAge)
    addEvent "aging_blocks" [("count", show blocksToAge), ("before_slot", show newSlot)]

    -- Add blocks to the being classified set
    let blockHashes = Set.fromList $ fmap (.hash) unclassifiedBlocks
    modify $ \s -> s {blocksBeingClassified = Set.union s.blocksBeingClassified blockHashes}

    -- Age each block by sending isOnChain query
    forM_ unclassifiedBlocks $ \block -> classifyBlockByChainStatus block.blockData


-- | Convert a ChainPoint to a slot number
-- Genesis is treated as slot 0
chainPointToSlot :: Hoard.ChainPoint -> Int64
chainPointToSlot (Hoard.ChainPoint ChainPointAtGenesis) = 0
chainPointToSlot (Hoard.ChainPoint (ChainPoint slot _)) = fromIntegral $ unSlotNo slot


-- | Convert an Ouroboros block header hash to a Cardano.Api hash
-- This is needed for querying the node with ChainPoint
-- Throws an error if deserialization fails
deserialiseBlockHeaderHash :: (Error Text :> es) => CardanoBlock -> Eff es (Hash BlockHeader)
deserialiseBlockHeaderHash blockData = do
    let header = getHeader blockData
        headerHashBytes = toRawHash (Proxy @CardanoBlock) (blockHash header)
    case deserialiseFromRawBytes (asType @(Hash BlockHeader)) headerHashBytes of
        Left _ -> throwError "Failed to deserialize block hash"
        Right apiHash -> pure apiHash
