module Hoard.Sentry
    ( component
    , DuplicateBlocks (..)
    , AdversarialBehavior (..)
    , AdversarialSeverity (..)
    , ReceivedBlockOutsideRequestedRange (..)
    , runDuplicateBlocksReader

      -- * Guards
    , duplicateBlockGuard
    , DuplicateBlocksKey (..)
    , receivedBlockIsOutsideRequestedRangeGuard

      -- * Config
    , Config (..)
    , AdversarialThresholds (..)
    , receivedHeaderElectionProofGuard
    ) where

import Cardano.Api (PastHorizonException)
import Effectful.Error.Static (runErrorNoCallStack)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)

import Hoard.Effects.NodeToClient
    ( ElectionValidationError
    , HeaderAtGenesis
    , IntersectNotFound
    , NoByronSupport
    , NodeConnectionError
    , NodeToClient
    , PointPastEpochHorizon
    , validateVrfSignature
    )
import Hoard.Effects.Publishing (Pub, Sub, publish)
import Hoard.Effects.Quota (Quota)
import Hoard.Events.BlockFetch (BlockReceived (..))
import Hoard.Events.ChainSync (HeaderReceived (HeaderReceived))
import Hoard.Types.Cardano (CardanoBlock)
import Hoard.Types.QuietSnake (QuietSnake (..))
import Prelude hiding (Map)

import Hoard.Effects.Publishing qualified as Sub
import Hoard.Effects.Quota qualified as Quota


component
    :: ( Pub AdversarialBehavior :> es
       , Pub ReceivedBlockOutsideRequestedRange :> es
       , Quota DuplicateBlocksKey :> es
       , Reader Config :> es
       , Sub BlockReceived :> es
       , Tracing :> es
       )
    => Component es
component =
    defaultComponent
        { name = "Sentry"
        , listeners =
            pure
                [ Sub.listen_ duplicateBlockGuard
                , Sub.listen_ receivedBlockIsOutsideRequestedRangeGuard
                ]
        }


newtype DuplicateBlocks = DuplicateBlocks
    { duplicateBlocks :: Map (ID Peer, UUID, BlockHash) Word
    }


runDuplicateBlocksReader :: (Concurrent :> es) => Eff (Reader DuplicateBlocks : es) a -> Eff es a
runDuplicateBlocksReader eff = do
    m <- atomically Map.new
    runReader (DuplicateBlocks m) eff


duplicateBlockGuard
    :: ( Pub AdversarialBehavior :> es
       , Quota DuplicateBlocksKey :> es
       , Reader Config :> es
       , Tracing :> es
       )
    => BlockReceived -> Eff es ()
duplicateBlockGuard event = withSpan "sentry.duplicate_block_guard" do
    addAttribute "peer.address" event.peer.address
    addAttribute "request.id" $ show @Text event.requestId
    let blockHash = mkBlockHash event.block
    let key =
            DuplicateBlocksKey
                { peerId = event.peer.id
                , requestId = event.requestId
                , hash = blockHash
                }
    cfg <- asks @Config (.duplicateBlocks)
    Quota.withQuotaCheck key (classify cfg) $ \case
        Nothing -> addAttribute @Text "result" "none"
        Just Critical -> do
            addAttribute @Text "result" "warning"
            publish
                $ AdversarialBehavior
                    { peer = event.peer
                    , description = "exceeded duplicate block critical threshold"
                    , severity = Critical
                    }
        Just Minor -> do
            addAttribute @Text "result" "critical"
            publish
                $ AdversarialBehavior
                    { peer = event.peer
                    , description = "exceeded duplicate block warning threshold"
                    , severity = Minor
                    }
  where
    classify cfg c
        | c > fromIntegral cfg.criticalThreshold = Just Critical
        | c > fromIntegral cfg.warningThreshold = Just Minor
        | otherwise = Nothing


data DuplicateBlocksKey = DuplicateBlocksKey
    { peerId :: ID Peer
    , requestId :: UUID
    , hash :: BlockHash
    }
    deriving (Eq, Generic, Hashable, Ord)


receivedBlockIsOutsideRequestedRangeGuard
    :: ( Pub AdversarialBehavior :> es
       , Pub ReceivedBlockOutsideRequestedRange :> es
       , Tracing :> es
       )
    => BlockReceived -> Eff es ()
receivedBlockIsOutsideRequestedRangeGuard event =
    when (blockNo < startNo || blockNo > endNo) $ withSpan "sentry.unrequested_block_guard" do
        publish
            AdversarialBehavior
                { peer = event.peer
                , severity = Minor
                , description = "returned block outside of requested range"
                }
        publish
            ReceivedBlockOutsideRequestedRange
                { peer = event.peer
                , block = event.block
                }
  where
    BlockFetch.ChainRange start end = event.range
    SlotNo blockNo = Block.blockSlot event.block
    startNo = pointToNo start
    endNo = pointToNo end
    pointToNo = \case
        Block.GenesisPoint -> 0
        Block.BlockPoint (SlotNo n) _ -> n


receivedHeaderElectionProofGuard
    :: ( NodeToClient :> es
       , Pub AdversarialBehavior :> es
       , Tracing :> es
       )
    => HeaderReceived -> Eff es ()
receivedHeaderElectionProofGuard (HeaderReceived peer header _tip) =
    withSpan "sentry.received_header_election_proof_guard" do
        addAttribute "peer.address" peer.address
        -- Run all infrastructure/protocol Error effects locally, treating them as
        -- indeterminate (we cannot blame the peer for node connectivity issues,
        -- epoch horizon gaps, genesis edge-cases, or Byron headers).
        result <-
            runErrorNoCallStack @PointPastEpochHorizon
                . runErrorNoCallStack @PastHorizonException
                . runErrorNoCallStack @NodeConnectionError
                . runErrorNoCallStack @NoByronSupport
                . runErrorNoCallStack @IntersectNotFound
                . runErrorNoCallStack @HeaderAtGenesis
                . runErrorNoCallStack @AcquireFailure
                $ validateVrfSignature header
        case flattenIndeterminate result of
            Nothing ->
                addAttribute @Text "result" "indeterminate"
            Just (Left err) -> do
                addAttribute @Text "result" "invalid_election_proof"
                addAttribute "error" $ show @Text err
                publish
                    AdversarialBehavior
                        { peer = peer
                        , severity = Critical
                        , description = "invalid VRF election proof in block header"
                        }
            Just (Right ()) ->
                addAttribute @Text "result" "valid"
  where
    -- Collapse any Left at any nesting level (infrastructure error) into Nothing.
    flattenIndeterminate
        :: Either
            PointPastEpochHorizon
            ( Either
                PastHorizonException
                ( Either
                    NodeConnectionError
                    ( Either
                        NoByronSupport
                        ( Either
                            IntersectNotFound
                            ( Either
                                HeaderAtGenesis
                                ( Either
                                    AcquireFailure
                                    (Either ElectionValidationError ())
                                )
                            )
                        )
                    )
                )
            )
        -> Maybe (Either ElectionValidationError ())
    flattenIndeterminate = \case
        Left _ -> Nothing
        Right (Left _) -> Nothing
        Right (Right (Left _)) -> Nothing
        Right (Right (Right (Left _))) -> Nothing
        Right (Right (Right (Right (Left _)))) -> Nothing
        Right (Right (Right (Right (Right (Left _))))) -> Nothing
        Right (Right (Right (Right (Right (Right (Left _)))))) -> Nothing
        Right (Right (Right (Right (Right (Right (Right r)))))) -> Just r


data AdversarialSeverity
    = Minor
    | Critical
    deriving (Bounded, Enum, Eq, Show)


data AdversarialBehavior = AdversarialBehavior
    { peer :: Peer
    , severity :: AdversarialSeverity
    , description :: Text
    }


data ReceivedBlockOutsideRequestedRange = ReceivedBlockOutsideRequestedRange
    { peer :: Peer
    , block :: CardanoBlock
    }


data Config = Config
    { duplicateBlocks :: AdversarialThresholds
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { duplicateBlocks =
                AdversarialThresholds
                    { warningThreshold = 1
                    , criticalThreshold = 20
                    }
            }


data AdversarialThresholds = AdversarialThresholds
    { warningThreshold :: Word
    -- ^ Threshold before the peer is considered to exhibit adversarial behavior.
    , criticalThreshold :: Word
    -- ^ Threshold before the peer is considered adequately adversarial to warrant major action.
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake AdversarialThresholds
