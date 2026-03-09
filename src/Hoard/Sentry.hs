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
    ) where

import Data.Aeson (FromJSON)
import Data.Default (Default (..))
import Data.UUID (UUID)
import Effectful.Concurrent.STM (Concurrent, atomically)
import Effectful.Reader.Static (Reader, asks, runReader)
import Ouroboros.Consensus.Block (SlotNo (..), getHeader)
import StmContainers.Map (Map)

import Ouroboros.Consensus.Block.Abstract qualified as Block
import Ouroboros.Network.Protocol.BlockFetch.Type qualified as BlockFetch
import StmContainers.Map qualified as Map

import Hoard.Component (Component (..), defaultComponent)
import Hoard.Data.BlockHash (BlockHash, blockHashFromHeader)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Monitoring.Tracing
    ( Tracing
    , addAttribute
    , withSpan
    )
import Hoard.Effects.Publishing (Pub, Sub, publish)
import Hoard.Effects.Quota (Quota)
import Hoard.Events.BlockFetch (BlockReceived (..))
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
    let blockHash = blockHashFromHeader $ getHeader event.block
    let key =
            DuplicateBlocksKey
                { peerId = event.peer.id
                , requestId = event.requestId
                , hash = blockHash
                }
    c <- fromIntegral <$> Quota.addHit key
    cfg <- asks @Config (.duplicateBlocks)
    if
        | c > cfg.criticalThreshold -> do
            addAttribute @Text "result" "warning"
            publish
                $ AdversarialBehavior
                    { peer = event.peer
                    , description = "exceeded duplicate block critical threshold"
                    , severity = Critical
                    }
        | c > cfg.warningThreshold -> do
            addAttribute @Text "result" "critical"
            publish
                $ AdversarialBehavior
                    { peer = event.peer
                    , description = "exceeded duplicate block warning threshold"
                    , severity = Minor
                    }
        | otherwise -> do
            addAttribute @Text "result" "none"
            pure ()


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
