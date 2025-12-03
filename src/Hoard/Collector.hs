module Hoard.Collector (dispatchDiscoveredNodes, runCollector) where

import Control.Concurrent (threadDelay)
import Data.Set qualified as S
import Effectful (Eff, IOE, (:>))

import Effectful.State.Static.Shared (State, gets, modify)
import Hoard.Data.Peer (PeerAddress)
import Hoard.Effects.Conc (Conc, fork_)
import Hoard.Effects.Network (Network, connectToPeer)
import Hoard.Effects.Pub (Pub, publish)
import Hoard.Events.Collector (CollectorEvent (..))
import Hoard.Network.Events (PeerSharingEvent (..), PeersReceivedData (..))
import Hoard.Types.HoardState (HoardState, connectedPeers)
import Prelude hiding (State, gets, modify)


dispatchDiscoveredNodes
    :: ( Conc :> es
       , IOE :> es
       , Network :> es
       , Pub :> es
       , State HoardState :> es
       )
    => PeerSharingEvent
    -> Eff es ()
dispatchDiscoveredNodes = \case
    (PeersReceived (PeersReceivedData {peerAddresses})) -> do
        currPeers <- gets connectedPeers
        let peersToConnect = S.difference peerAddresses currPeers
        forM_ peersToConnect $ \addr -> do
            fork_ $ runCollector addr
            modify $ \r -> r {connectedPeers = S.insert addr r.connectedPeers}
    _ -> pure ()


runCollector
    :: (IOE :> es, Network :> es, Pub :> es)
    => PeerAddress
    -> Eff es Void
runCollector peer = do
    publish $ CollectorStarted peer
    publish $ ConnectingToPeer peer

    _conn <- connectToPeer peer
    publish $ ConnectedToPeer peer

    -- Connection is now running autonomously!
    -- Protocols publish events as they receive data
    -- For now, just keep the collector alive
    forever $ liftIO $ threadDelay 1000000
