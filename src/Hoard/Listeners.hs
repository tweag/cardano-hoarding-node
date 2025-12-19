module Hoard.Listeners (runListeners) where

import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.State.Static.Shared (State)
import Prelude hiding (State)

import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Chan (Chan)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.HeaderRepo (HeaderRepo)
import Hoard.Effects.Log (Log)
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.NodeToNode (NodeToNode)
import Hoard.Effects.PeerRepo (PeerRepo)
import Hoard.Effects.Pub (Pub)
import Hoard.Effects.Sub (Sub, listen)
import Hoard.Listeners.BlockFetchEventListener (blockFetchEventListener)
import Hoard.Listeners.ChainSyncEventListener (chainSyncEventListener)
import Hoard.Listeners.CollectorEventListener (collectorEventListener)
import Hoard.Listeners.DiscoveredNodesListener (dispatchDiscoveredNodes)
import Hoard.Listeners.HeaderReceivedListener (headerReceivedListener)
import Hoard.Listeners.ImmutableTipRefreshTriggeredListener (immutableTipRefreshTriggeredListener)
import Hoard.Listeners.NetworkEventListener (networkEventListener)
import Hoard.Listeners.PeerSharingEventListener (peerSharingEventListener)
import Hoard.Listeners.PeersReceivedListener (peersReceivedListener)
import Hoard.Types.HoardState (HoardState)


runListeners
    :: ( BlockRepo :> es
       , Chan :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , HeaderRepo :> es
       , Log :> es
       , NodeToClient :> es
       , NodeToNode :> es
       , PeerRepo :> es
       , Pub :> es
       , State HoardState :> es
       , Sub :> es
       )
    => Eff es ()
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
