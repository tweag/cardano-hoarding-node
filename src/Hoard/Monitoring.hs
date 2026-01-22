module Hoard.Monitoring
    ( run
    , runListeners
    , listener
    , runTriggers
    , Poll (..)
    ) where

import Cardano.Api (ChainPoint (..))
import Data.Set qualified as S
import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Reader.Static (Reader, asks)
import Effectful.State.Static.Shared (State, gets)
import Prelude hiding (Reader, State, asks, gets)

import Hoard.Collectors.State (ConnectedPeers (..))
import Hoard.DB.Schema (countRows)
import Hoard.DB.Schemas.Blocks qualified as BlocksSchema
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.DBRead (DBRead)
import Hoard.Effects.Log (Log, withNamespace)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Metrics (Metrics, gaugeSet)
import Hoard.Effects.Metrics.Definitions (metricBlocksInDB, metricConnectedPeers)
import Hoard.Effects.Publishing (Pub, Sub, publish)
import Hoard.Effects.Publishing qualified as Sub
import Hoard.Triggers (every)
import Hoard.Types.Environment (Config (..), MonitoringConfig (..))
import Hoard.Types.HoardState (HoardState (..))


run
    :: ( Conc :> es
       , Concurrent :> es
       , DBRead :> es
       , Log :> es
       , Metrics :> es
       , Pub :> es
       , Reader Config :> es
       , State ConnectedPeers :> es
       , State HoardState :> es
       , Sub :> es
       )
    => Eff es ()
run = do
    runListeners
    runTriggers


runListeners
    :: ( Conc :> es
       , DBRead :> es
       , Log :> es
       , Metrics :> es
       , State ConnectedPeers :> es
       , State HoardState :> es
       , Sub :> es
       )
    => Eff es ()
runListeners = do
    Conc.fork_ $ Sub.listen listener


listener
    :: ( DBRead :> es
       , Log :> es
       , Metrics :> es
       , State ConnectedPeers :> es
       , State HoardState :> es
       )
    => Poll
    -> Eff es ()
listener Poll = do
    withNamespace "Monitoring" $ do
        numPeers <- gets @ConnectedPeers (S.size . (.connectedPeers))
        tip <- gets @HoardState (.immutableTip)
        blockCount <- countRows BlocksSchema.schema

        -- Update metrics
        gaugeSet metricConnectedPeers (fromIntegral numPeers)
        gaugeSet metricBlocksInDB (fromIntegral blockCount)

        let tipSlot = case tip of
                ChainPointAtGenesis -> "genesis"
                ChainPoint slot _ -> show slot
        Log.info $ "Currently connected to " <> show numPeers <> " peers | Immutable tip slot: " <> tipSlot <> " | Blocks in DB: " <> show blockCount


runTriggers :: (Conc :> es, Concurrent :> es, Pub :> es, Reader Config :> es) => Eff es ()
runTriggers = do
    pollingInterval <- asks $ (.monitoring.pollingIntervalSeconds)
    every pollingInterval $ publish Poll


data Poll = Poll
    deriving stock (Show, Typeable)
