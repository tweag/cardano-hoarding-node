module Hoard.Monitoring
    ( run
    , runListeners
    , listener
    , runTriggers
    , Poll (..)
    ) where

import Cardano.Api qualified as C
import Data.Set qualified as S
import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Reader.Static (Reader, asks)
import Effectful.State.Static.Shared (State, gets)
import Prelude hiding (Reader, State, asks, gets)

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
import Hoard.Types.Cardano (ChainPoint (ChainPoint))
import Hoard.Types.Environment (Config (..), MonitoringConfig (..))
import Hoard.Types.HoardState (HoardState (..))


run
    :: ( Concurrent :> es
       , Pub :> es
       , Reader Config :> es
       , State HoardState :> es
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
       , DBRead :> es
       , Metrics :> es
       )
    => Eff es ()
runListeners = do
    Conc.fork_ $ Sub.listen listener


listener :: (Log :> es, State HoardState :> es, DBRead :> es, Metrics :> es) => Poll -> Eff es ()
listener Poll = do
    withNamespace "Monitoring" $ do
        numPeers <- gets (S.size . (.connectedPeers))
        tip <- gets (.immutableTip)
        blockCount <- countRows BlocksSchema.schema

        -- Update metrics
        gaugeSet metricConnectedPeers (fromIntegral numPeers)
        gaugeSet metricBlocksInDB (fromIntegral blockCount)

        let tipSlot = case coerce tip of
                C.ChainPointAtGenesis -> "genesis"
                C.ChainPoint slot _ -> show slot
        Log.info $ "Currently connected to " <> show numPeers <> " peers | Immutable tip slot: " <> tipSlot <> " | Blocks in DB: " <> show blockCount


runTriggers :: (Conc :> es, Concurrent :> es, Pub :> es, Reader Config :> es) => Eff es ()
runTriggers = do
    pollingInterval <- asks $ (.monitoring.pollingIntervalSeconds)
    every pollingInterval $ publish Poll


data Poll = Poll
    deriving stock (Show, Typeable)
