module Hoard.Listeners (runListeners) where

import Effectful (Eff)
import Hoard.Effects (AppEff)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Sub (listen)
import Hoard.Listeners.BlockFetchEventListener (blockFetchEventListener)
import Hoard.Listeners.ChainSyncEventListener (chainSyncEventListener)
import Hoard.Listeners.CollectorEventListener (collectorEventListener)
import Hoard.Listeners.DiscoveredNodesListener (dispatchDiscoveredNodes)
import Hoard.Listeners.HeaderReceivedListener (headerReceivedListener)
import Hoard.Listeners.ImmutableTipRefreshTriggeredListener (immutableTipRefreshTriggeredListener)
import Hoard.Listeners.NetworkEventListener (networkEventListener)
import Hoard.Listeners.PeerSharingEventListener (peerSharingEventListener)
import Hoard.Listeners.PeersReceivedListener (peersReceivedListener)


runListeners :: (AppEff es) => Eff es ()
runListeners = do
    _ <- Conc.fork $ listen headerReceivedListener
    _ <- Conc.fork $ listen peersReceivedListener
    _ <- Conc.fork $ listen dispatchDiscoveredNodes
    _ <- Conc.fork $ listen networkEventListener
    _ <- Conc.fork $ listen peerSharingEventListener
    _ <- Conc.fork $ listen chainSyncEventListener
    _ <- Conc.fork $ listen blockFetchEventListener
    _ <- Conc.fork $ listen collectorEventListener
    _ <- Conc.fork $ listen immutableTipRefreshTriggeredListener
    pure ()
