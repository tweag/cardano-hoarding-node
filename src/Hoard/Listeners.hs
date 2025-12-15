module Hoard.Listeners (runListeners) where

import Effectful (Eff, (:>))
import Effectful.Concurrent (threadDelay)
import Effectful.State.Static.Shared (State, modify)
import Hoard.Collector (dispatchDiscoveredNodes)
import Hoard.Data.ProtocolInfo (ProtocolConfigPath)
import Hoard.Effects (AppEff)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Input (Input)
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.NodeToClient qualified as N
import Hoard.Effects.Sub (listen)
import Hoard.Listeners.ChainSyncEventListener (chainSyncEventListener)
import Hoard.Listeners.CollectorEventListener (collectorEventListener)
import Hoard.Listeners.HeaderReceivedListener (headerReceivedListener)
import Hoard.Listeners.NetworkEventListener (networkEventListener)
import Hoard.Listeners.PeerSharingEventListener (peerSharingEventListener)
import Hoard.Listeners.PeersReceivedListener (peersReceivedListener)
import Hoard.Types.HoardState (HoardState (immutableTip))
import Ouroboros.Network.IOManager (IOManager)
import Prelude hiding (State, modify)


runListeners
    :: ( AppEff es
       , Input IOManager :> es
       , Input ProtocolConfigPath :> es
       )
    => Eff es ()
runListeners = do
    refreshImmutableTip
    _ <- Conc.fork $ listen headerReceivedListener
    _ <- Conc.fork $ listen peersReceivedListener
    _ <- Conc.fork $ listen dispatchDiscoveredNodes
    _ <- Conc.fork $ listen networkEventListener
    _ <- Conc.fork $ listen peerSharingEventListener
    _ <- Conc.fork $ listen chainSyncEventListener
    _ <- Conc.fork $ listen collectorEventListener
    Conc.fork_ $ forever $ do
        threadDelay (20 * 1000000)
        refreshImmutableTip


refreshImmutableTip :: (NodeToClient :> es, State HoardState :> es) => Eff es ()
refreshImmutableTip =
    do
        tip <- N.immutableTip
        modify (\hoardState -> hoardState {immutableTip = tip})
