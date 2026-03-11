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

import Data.Aeson (FromJSON)
import Data.Default (Default (..))
import Data.UUID (UUID)
import Effectful.Concurrent.STM (Concurrent, atomically)
import Effectful.Reader.Static (Reader, asks, runReader)
import Ouroboros.Consensus.Block (SlotNo (..))
import StmContainers.Map (Map)

import Ouroboros.Consensus.Block.Abstract qualified as Block
import Ouroboros.Network.Protocol.BlockFetch.Type qualified as BlockFetch
import StmContainers.Map qualified as Map

import Hoard.Component (Component (..), defaultComponent)
import Hoard.Data.BlockHash (BlockHash, mkBlockHash)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Monitoring.Tracing
    ( Tracing
    , addAttribute
    , withSpan
    )
import Hoard.Effects.NodeToClient (NodeToClient, validateVrfSignature_)
import Hoard.Effects.Publishing (Pub, Sub, publish)
import Hoard.Effects.Quota (Quota)
import Hoard.Events.BlockFetch (BlockReceived (..))
import Hoard.Events.ChainSync (HeaderReceived (HeaderReceived))
import Hoard.Types.Cardano (CardanoBlock)
import Hoard.Types.QuietSnake (QuietSnake (..))
import Prelude hiding (Map)

import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Publishing qualified as Sub
import Hoard.Effects.Quota qualified as Quota


component
    :: ( Log.Log :> es
       , NodeToClient :> es
       , Pub AdversarialBehavior :> es
       , Pub ReceivedBlockOutsideRequestedRange :> es
       , Quota DuplicateBlocksKey :> es
       , Reader Config :> es
       , Sub BlockReceived :> es
       , Sub HeaderReceived :> es
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
                , Sub.listen_ receivedHeaderElectionProofGuard
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


receivedHeaderElectionProofGuard :: (Log.Log :> es, NodeToClient :> es) => HeaderReceived -> Eff es ()
receivedHeaderElectionProofGuard (HeaderReceived _peer header _tip) =
    Log.withNamespace "sentry.header_election_proof_guard"
        $ (=<<) Log.err
        $ fmap show
        $ validateVrfSignature_
        $ header


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
