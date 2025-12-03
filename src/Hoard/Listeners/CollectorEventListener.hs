module Hoard.Listeners.CollectorEventListener (collectorEventListener) where

import Effectful (Eff, (:>))

import Hoard.Data.Peer (PeerAddress (..))
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Events.Collector (CollectorEvent (..))


-- | Listener that logs collector events
collectorEventListener :: (Log :> es) => CollectorEvent -> Eff es ()
collectorEventListener = \case
    CollectorStarted addr ->
        Log.info $ "Collector: started for " <> show addr.host
    ConnectingToPeer addr ->
        Log.info $ "Collector: connecting to peer " <> show addr.host
    ConnectedToPeer addr ->
        Log.info $ "Collector: connected to peer " <> show addr.host
    ConnectionFailed addr reason ->
        Log.info $ "Collector: failed to connect to peer " <> show addr.host <> ": " <> reason
    ChainSyncReceived addr ->
        Log.info $ "Collector: chain sync received from " <> show addr.host
    BlockFetchReceived addr ->
        Log.info $ "Collector: block fetch received from " <> show addr.host
