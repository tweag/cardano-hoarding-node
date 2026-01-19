module Hoard.Listeners (runListeners) where

import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Shared (State)
import Effectful.Timeout (Timeout)
import Prelude hiding (Reader, State)

import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Chan (Chan)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log (Log)
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.NodeToNode (NodeToNode)
import Hoard.Effects.PeerRepo (PeerRepo)
import Hoard.Effects.Pub (Pub)
import Hoard.Effects.Sub (Sub, listen)
import Hoard.Listeners.BlockFetchEventListener
    ( blockBatchCompletedListener
    , blockFetchFailedListener
    , blockFetchStartedListener
    , blockReceivedListener
    )
import Hoard.Listeners.CollectorEventListener (collectorEventListener)
import Hoard.Listeners.DiscoveredNodesListener (dispatchDiscoveredNodes)
import Hoard.Listeners.HeaderReceivedListener (headerReceivedListener)
import Hoard.Listeners.ImmutableTipRefreshTriggeredListener (immutableTipRefreshTriggeredListener)
import Hoard.Listeners.NetworkEventListener (protocolErrorListener)
import Hoard.Listeners.PeerSharingEventListener
    ( peerSharingFailedListener
    , peerSharingStartedListener
    , peersReceivedLogListener
    )
import Hoard.Listeners.PeersReceivedListener (peersReceivedListener)
import Hoard.Types.Environment (Config)
import Hoard.Types.HoardState (BlocksBeingFetched, HoardState)


runListeners
    :: ( BlockRepo :> es
       , Chan :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , Log :> es
       , NodeToClient :> es
       , NodeToNode :> es
       , PeerRepo :> es
       , Pub :> es
       , Reader Config :> es
       , State BlocksBeingFetched :> es
       , State HoardState :> es
       , Sub :> es
       , Timeout :> es
       )
    => Eff es ()
runListeners = do
    _ <- Conc.fork $ listen headerReceivedListener
    _ <- Conc.fork $ listen peersReceivedListener
    _ <- Conc.fork $ listen dispatchDiscoveredNodes
    _ <- Conc.fork $ listen protocolErrorListener
    _ <- Conc.fork $ listen peerSharingStartedListener
    _ <- Conc.fork $ listen peersReceivedLogListener
    _ <- Conc.fork $ listen peerSharingFailedListener
    _ <- Conc.fork $ listen blockFetchStartedListener
    _ <- Conc.fork $ listen blockReceivedListener
    _ <- Conc.fork $ listen blockFetchFailedListener
    _ <- Conc.fork $ listen blockBatchCompletedListener
    _ <- Conc.fork $ listen collectorEventListener
    _ <- Conc.fork $ listen immutableTipRefreshTriggeredListener
    pure ()
