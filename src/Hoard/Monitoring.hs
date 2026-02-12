module Hoard.Monitoring
    ( run
    , runListeners
    , listener
    , runTriggers
    , Poll (..)
    , runConfig
    ) where

import Cardano.Api qualified as C
import Data.Aeson (FromJSON (..))
import Data.Default (Default (..))
import Data.List (partition)
import Data.Set qualified as S
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Reader.Static (Reader, ask, asks, runReader)
import Effectful.State.Static.Shared (State, gets)
import Rel8 (isNull)
import Prelude hiding (Reader, State, ask, asks, gets, runReader)

import Hoard.DB.Schema (countRows, countRowsWhere)
import Hoard.DB.Schemas.Blocks qualified as BlocksSchema
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.ConfigPath (ConfigPath, loadYaml)
import Hoard.Effects.DBRead (DBRead)
import Hoard.Effects.Log (Log, withNamespace)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Monitoring.Metrics (Metrics, gaugeSet)
import Hoard.Effects.Monitoring.Metrics.Definitions (metricBlocksBeingClassified, metricBlocksInDB, metricConnectedPeers, metricUnclassifiedBlocks)
import Hoard.Effects.Publishing (Pub, Sub, publish)
import Hoard.Effects.Publishing qualified as Sub
import Hoard.PeerManager.Peers (Connection (..), ConnectionState (..), Peers (..))
import Hoard.Triggers (every)
import Hoard.Types.Cardano (ChainPoint (ChainPoint))
import Hoard.Types.HoardState (HoardState (..))
import Hoard.Types.QuietSnake (QuietSnake (..))


run
    :: ( Concurrent :> es
       , Pub Poll :> es
       , Reader Config :> es
       , State HoardState :> es
       , State Peers :> es
       , Conc :> es
       , Sub Poll :> es
       , Log :> es
       , DBRead :> es
       , Metrics :> es
       )
    => Eff es ()
run = do
    runListeners
    runTriggers


runListeners
    :: ( Conc :> es
       , Sub Poll :> es
       , Log :> es
       , State HoardState :> es
       , State Peers :> es
       , DBRead :> es
       , Metrics :> es
       )
    => Eff es ()
runListeners = do
    Conc.fork_ $ Sub.listen listener


listener
    :: ( DBRead :> es
       , Log :> es
       , Metrics :> es
       , State HoardState :> es
       , State Peers :> es
       )
    => Poll
    -> Eff es ()
listener Poll = do
    withNamespace "Monitoring" $ do
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
        Log.info $ "Currently connected to " <> show connectedPeers <> " peers | " <> show pendingPeers <> " peer connections pending | Immutable tip slot: " <> tipSlot <> " | Blocks in DB: " <> show blockCount <> " | Unclassified: " <> show unclassifiedCount <> " | Being classified: " <> show beingClassifiedCount


runTriggers :: (Conc :> es, Concurrent :> es, Pub Poll :> es, Reader Config :> es) => Eff es ()
runTriggers = do
    pollingInterval <- asks $ (.pollingIntervalSeconds)
    every pollingInterval $ publish Poll


data Poll = Poll
    deriving stock (Show, Typeable)


data Config = Config
    { pollingIntervalSeconds :: Int
    -- ^ Interval between peer status polling
    }
    deriving stock (Generic)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { pollingIntervalSeconds = 5
            }


data ConfigFile = ConfigFile
    { monitoring :: Config
    }
    deriving stock (Generic)
    deriving (FromJSON) via QuietSnake ConfigFile


runConfig :: (IOE :> es, Reader ConfigPath :> es) => Eff (Reader Config : es) a -> Eff es a
runConfig eff = do
    configPath <- ask
    configFile <- loadYaml @ConfigFile configPath
    runReader configFile.monitoring eff
