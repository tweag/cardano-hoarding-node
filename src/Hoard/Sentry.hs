module Hoard.Sentry
    ( component
    , DuplicateBlocks (..)
    , AdversarialBehavior (..)
    , AdversarialSeverity (..)
    , runDuplicateBlocksReader

      -- * Config
    , Config (..)
    , DuplicateBlocksConfig (..)
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
import Hoard.Events.BlockFetch (BlockReceived (..))
import Hoard.Types.QuietSnake (QuietSnake (..))
import Prelude hiding (Map)

import Hoard.Effects.Publishing qualified as Sub


component
    :: ( Concurrent :> es
       , Pub AdversarialBehavior :> es
       , Reader Config :> es
       , Reader DuplicateBlocks :> es
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
                , Sub.listen_ unrequestedBlockGuard
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
    :: ( Concurrent :> es
       , Pub AdversarialBehavior :> es
       , Reader Config :> es
       , Reader DuplicateBlocks :> es
       , Tracing :> es
       )
    => BlockReceived -> Eff es ()
duplicateBlockGuard event = withSpan "sentry.duplicate_block_guard" do
    addAttribute "peer.address" event.peer.address
    addAttribute "request.id" $ show @Text event.requestId
    let blockHash = blockHashFromHeader $ getHeader event.block
    m <- asks @DuplicateBlocks (.duplicateBlocks)
    let key = (event.peer.id, event.requestId, blockHash)
    c <- atomically do
        count <- fromMaybe 0 <$> Map.lookup key m
        let newCount = count + 1
        Map.insert newCount key m
        pure newCount
    duplicateConfig <- asks @Config (.duplicateBlocks)
    if
        | c > duplicateConfig.criticalThreshold -> do
            addAttribute @Text "result" "warning"
            publish
                $ AdversarialBehavior
                    { peer = event.peer
                    , description = "exceeded duplicate block critical threshold"
                    , severity = Critical
                    }
        | c > duplicateConfig.warningThreshold -> do
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


unrequestedBlockGuard
    :: ( Pub AdversarialBehavior :> es
       , Tracing :> es
       )
    => BlockReceived -> Eff es ()
unrequestedBlockGuard event =
    when (blockNo < startNo || blockNo > endNo) $ withSpan "sentry.unrequested_block_guard" do
        publish
            AdversarialBehavior
                { peer = event.peer
                , severity = Minor
                , description = "returned block outside of requested range"
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


data Config = Config
    { duplicateBlocks :: DuplicateBlocksConfig
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { duplicateBlocks = def
            }


data DuplicateBlocksConfig = DuplicateBlocksConfig
    { warningThreshold :: Word
    -- ^ Threshold before the peer is considered to exhibit adversarial behavior.
    , criticalThreshold :: Word
    -- ^ Threshold before the peer is considered adequately adversarial to warrant major action.
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake DuplicateBlocksConfig


instance Default DuplicateBlocksConfig where
    def =
        DuplicateBlocksConfig
            { warningThreshold = 1
            , criticalThreshold = 20
            }
