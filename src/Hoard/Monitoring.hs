module Hoard.Monitoring
    ( run
    , runListeners
    , listener
    , runTriggers
    , Poll (..)
    , Config (..)
    , ConfigFile (..)
    , runConfig
    ) where

import Cardano.Api qualified as C
import Data.List (partition)
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Reader.Static (Reader, asks, runReader)
import Effectful.State.Static.Shared (State, gets)
import Prelude hiding (Reader, State, asks, gets, runReader)

import Data.Aeson (FromJSON (..))
import Data.Default (Default (..))
import Hoard.DB.Schema (countRows)
import Hoard.DB.Schemas.Blocks qualified as BlocksSchema
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.DBRead (DBRead)
import Hoard.Effects.Environment (ConfigPath (..), loadYaml)
import Hoard.Effects.Log (Log, withNamespace)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Metrics (Metrics, gaugeSet)
import Hoard.Effects.Metrics.Definitions (metricBlocksInDB, metricConnectedPeers, metricPendingPeers)
import Hoard.Effects.Publishing (Pub, Sub, publish)
import Hoard.Effects.Publishing qualified as Sub
import Hoard.PeerManager.Peers (Connection (..), ConnectionState (..), Peers (..))
import Hoard.Triggers (every)
import Hoard.Types.Cardano (ChainPoint (ChainPoint))
import Hoard.Types.HoardState (HoardState (..))
import Hoard.Types.QuietSnake (QuietSnake (..))


run
    :: ( Concurrent :> es
       , Pub :> es
       , Reader Config :> es
       , State HoardState :> es
       , State Peers :> es
       , Conc :> es
       , Sub :> es
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
       , Sub :> es
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

        -- Update metrics
        gaugeSet metricConnectedPeers (fromIntegral connectedPeers)
        gaugeSet metricPendingPeers (fromIntegral pendingPeers)
        gaugeSet metricBlocksInDB (fromIntegral blockCount)

        let tipSlot = case coerce tip of
                C.ChainPointAtGenesis -> "genesis"
                C.ChainPoint slot _ -> show slot
        Log.info $ "Currently connected to " <> show connectedPeers <> " peers | " <> show pendingPeers <> " peer connections pending | Immutable tip slot: " <> tipSlot <> " | Blocks in DB: " <> show blockCount


runTriggers :: (Conc :> es, Concurrent :> es, Pub :> es, Reader Config :> es) => Eff es ()
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
    configPath <- asks (.unConfigPath)
    configFile <- loadYaml @ConfigFile configPath
    runReader configFile.monitoring eff
