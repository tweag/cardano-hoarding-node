module Hoard.Sentry
    ( component
    , DuplicateBlockKey (..)
    , DuplicateBlocks (..)
    , AdversarialBehavior (..)
    , AdversarialSeverity (..)
    , runDuplicateBlocksState

      -- * Config
    , Config (..)
    , DuplicateBlocksConfig (..)
    ) where

import Data.Aeson (FromJSON)
import Data.Default (Default (..))
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Effectful (Eff, (:>))
import Effectful.Concurrent.STM (Concurrent, atomically)
import Effectful.Reader.Static (Reader, asks)
import Effectful.State.Static.Shared (State, evalState, gets)
import Ouroboros.Consensus.Block (getHeader)
import StmContainers.Map (Map)
import Prelude hiding (Map, Reader, State, asks, atomically, evalState, gets)

import StmContainers.Map qualified as Map

import Hoard.Component (Component (..), defaultComponent)
import Hoard.Data.BlockHash (BlockHash, blockHashFromHeader)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Monitoring.Tracing (Tracing, addAttribute, withSpan)
import Hoard.Effects.Publishing (Pub, Sub, publish)
import Hoard.Events.BlockFetch (BlockReceived (..))
import Hoard.Types.QuietSnake (QuietSnake (..))

import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.Publishing qualified as Sub


component
    :: ( Clock :> es
       , Concurrent :> es
       , Pub AdversarialBehavior :> es
       , Reader Config :> es
       , State DuplicateBlocks :> es
       , Sub BlockReceived :> es
       , Tracing :> es
       )
    => Component es
component =
    defaultComponent
        { name = "Sentry"
        , listeners =
            pure
                [ Sub.listen duplicateBlockGuard
                ]
        }


newtype DuplicateBlocks = DuplicateBlocks
    { duplicateBlocks :: Map (ID Peer, UUID, BlockHash) Word
    }


runDuplicateBlocksState :: (Concurrent :> es) => Eff (State DuplicateBlocks : es) a -> Eff es a
runDuplicateBlocksState eff = do
    m <- atomically Map.new
    evalState (DuplicateBlocks m) eff


duplicateBlockGuard
    :: ( Clock :> es
       , Concurrent :> es
       , Pub AdversarialBehavior :> es
       , Reader Config :> es
       , State DuplicateBlocks :> es
       , Tracing :> es
       )
    => BlockReceived -> Eff es ()
duplicateBlockGuard event = withSpan "sentry.duplicate_block_guard" do
    addAttribute "peer.id" $ show event.peer.id
    addAttribute "peer.address" $ show event.peer.address
    addAttribute "request.id" $ show event.requestId
    let blockHash = blockHashFromHeader $ getHeader event.block
    m <- gets (.duplicateBlocks)
    let key = (event.peer.id, event.requestId, blockHash)
    c <- atomically do
        count <- fromMaybe 0 <$> Map.lookup key m
        let newCount = count + 1
        Map.insert newCount key m
        pure newCount
    duplicateConfig <- asks (.duplicateBlocks)
    timestamp <- Clock.currentTime
    if
        | c > duplicateConfig.criticalThreshold -> do
            addAttribute "result" "warning"
            publish
                $ AdversarialBehavior
                    { timestamp
                    , peer = event.peer
                    , description = "exceeded duplicate block critical threshold"
                    , severity = Critical
                    }
        | c > duplicateConfig.warningThreshold -> do
            addAttribute "result" "critical"
            publish
                $ AdversarialBehavior
                    { timestamp
                    , peer = event.peer
                    , description = "exceeded duplicate block warning threshold"
                    , severity = Minor
                    }
        | otherwise -> do
            addAttribute "result" "none"
            pure ()


data DuplicateBlockKey = DuplicateBlockKey
    { peerId :: ID Peer
    , hash :: BlockHash
    , requestId :: UUID
    }
    deriving (Hashable)
    deriving stock (Eq, Generic, Ord, Show)


data AdversarialSeverity
    = Minor
    | Critical
    deriving (Bounded, Enum, Eq, Show)


data AdversarialBehavior = AdversarialBehavior
    { timestamp :: UTCTime
    , peer :: Peer
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
