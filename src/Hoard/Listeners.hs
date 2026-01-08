module Hoard.Listeners (runListeners) where

import Effectful (Eff, (:>))
import Effectful.State.Static.Shared (State)
import Prelude hiding (Reader, State)

import Hoard.Collectors.State (BlocksBeingFetched)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log (Log)
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.PeerRepo (PeerRepo)
import Hoard.Effects.Sub (Sub, listen)
import Hoard.Listeners.BlockFetchEventListener (blockBatchCompletedListener, blockFetchFailedListener, blockFetchStartedListener, blockReceivedListener)
import Hoard.Listeners.HeaderReceivedListener (headerReceivedListener)
import Hoard.Listeners.ImmutableTipRefreshTriggeredListener (immutableTipRefreshTriggeredListener)
import Hoard.Listeners.NetworkEventListener (protocolErrorListener)
import Hoard.Listeners.PeerSharingEventListener
    ( peerSharingFailedListener
    , peerSharingStartedListener
    , peersReceivedLogListener
    )
import Hoard.Listeners.PeersReceivedListener (peersReceivedListener)
import Hoard.Types.HoardState (HoardState)


runListeners
    :: ( BlockRepo :> es
       , Conc :> es
       , Log :> es
       , NodeToClient :> es
       , PeerRepo :> es
       , State BlocksBeingFetched :> es
       , State HoardState :> es
       , Sub :> es
       )
    => Eff es ()
runListeners = do
    _ <- Conc.fork $ listen headerReceivedListener
    _ <- Conc.fork $ listen peersReceivedListener
    _ <- Conc.fork $ listen protocolErrorListener
    _ <- Conc.fork $ listen peerSharingStartedListener
    _ <- Conc.fork $ listen peersReceivedLogListener
    _ <- Conc.fork $ listen peerSharingFailedListener
    _ <- Conc.fork $ listen blockFetchStartedListener
    _ <- Conc.fork $ listen blockReceivedListener
    _ <- Conc.fork $ listen blockFetchFailedListener
    _ <- Conc.fork $ listen blockBatchCompletedListener
    _ <- Conc.fork $ listen immutableTipRefreshTriggeredListener
    pure ()
