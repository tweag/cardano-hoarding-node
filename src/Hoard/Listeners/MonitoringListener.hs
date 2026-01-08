module Hoard.Listeners.MonitoringListener (monitoringListener) where

import Effectful (Eff, (:>))
import Effectful.State.Static.Shared (State, gets)
import Prelude hiding (State, gets)

import Data.Set qualified as S
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Events.MonitoringRequest (MonitoringRequest (..))
import Hoard.Types.HoardState (HoardState (..))


monitoringListener :: (Log :> es, State HoardState :> es) => MonitoringRequest -> Eff es ()
monitoringListener MonitoringRequest = do
    numPeers <- gets (S.size . (.connectedPeers))
    Log.info $ "Currently connected to " <> show numPeers <> " peers"
