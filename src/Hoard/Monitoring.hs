module Hoard.Monitoring
    ( component
    , listener
    , Poll (..)
    , Config (..)
    ) where

import Data.Aeson (FromJSON (..))
import Data.Default (Default (..))
import Data.List (partition)
import Effectful.Reader.Static (Reader, asks)
import Effectful.State.Static.Shared (State, gets)
import Rel8 (isNull)

import Cardano.Api qualified as C
import Data.Set qualified as S
import Data.Text qualified as T

import Atelier.Component (Component (..), defaultComponent)
import Atelier.Effects.Delay (Delay)
import Atelier.Effects.Log (Log)
import Atelier.Effects.Monitoring.Metrics (Metrics, gaugeSet)
import Atelier.Effects.Monitoring.Tracing (Tracing, withSpan)
import Atelier.Effects.Publishing (Pub, Sub, publish)
import Atelier.Time (Second)
import Atelier.Types.QuietSnake (QuietSnake (..))
import Hoard.DB.Schema (countRows, countRowsWhere)
import Hoard.Effects.DB (DBRead)
import Hoard.Effects.Monitoring.Metrics.Definitions (metricBlocksBeingClassified, metricBlocksInDB, metricConnectedPeers, metricUnclassifiedBlocks)
import Hoard.PeerManager.Peers (Connection (..), ConnectionState (..), Peers (..))
import Hoard.Types.Cardano (ChainPoint (ChainPoint))
import Hoard.Types.HoardState (HoardState (..))

import Atelier.Effects.Delay qualified as Delay
import Atelier.Effects.Log qualified as Log
import Atelier.Effects.Publishing qualified as Sub
import Hoard.DB.Schemas.Blocks qualified as BlocksSchema


component
    :: ( DBRead :> es
       , Delay :> es
       , Log :> es
       , Metrics :> es
       , Pub Poll :> es
       , Reader Config :> es
       , State HoardState :> es
       , State Peers :> es
       , Sub Poll :> es
       , Tracing :> es
       )
    => Component es
component =
    defaultComponent
        { name = "Monitoring"
        , listeners = pure [Sub.listen_ listener]
        , triggers = do
            pollingInterval <- asks (.pollingIntervalSeconds)
            pure [Delay.every pollingInterval $ publish Poll]
        }


listener
    :: ( DBRead :> es
       , Log :> es
       , Metrics :> es
       , State HoardState :> es
       , State Peers :> es
       , Tracing :> es
       )
    => Poll
    -> Eff es ()
listener Poll = withSpan "monitoring" do
    peers <- gets @Peers $ toList . (.peers)
    let (connectedPeers, pendingPeers) = bimap length length $ partition ((== Connected) . (.state)) peers
    tip <- gets @HoardState (.immutableTip)
    blockCount <- countRows BlocksSchema.schema
    unclassifiedCount <- countRowsWhere BlocksSchema.schema (\row -> isNull row.classification)
    beingClassifiedCount <- gets @HoardState (S.size . (.blocksBeingClassified))

    -- Update metrics
    gaugeSet metricConnectedPeers (fromIntegral connectedPeers)
    gaugeSet metricBlocksInDB (fromIntegral blockCount)
    gaugeSet metricUnclassifiedBlocks (fromIntegral unclassifiedCount)
    gaugeSet metricBlocksBeingClassified (fromIntegral beingClassifiedCount)

    let tipSlot = case coerce tip of
            C.ChainPointAtGenesis -> "genesis"
            C.ChainPoint slot _ -> show slot
    Log.withNamespace "Monitoring"
        $ Log.info
        $ T.intercalate
            " | "
            [ "Current peer connections: " <> show connectedPeers
            , "Pending peer connections: " <> show pendingPeers
            , "Immutable tip slot: " <> tipSlot
            , "Blocks in DB: " <> show blockCount
            , "Unclassified blocks: " <> show unclassifiedCount
            , "Blocks being classified: " <> show beingClassifiedCount
            ]


data Poll = Poll
    deriving stock (Show)


data Config = Config
    { pollingIntervalSeconds :: Second
    -- ^ Interval between peer status polling
    }
    deriving stock (Generic)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { pollingIntervalSeconds = 5
            }
