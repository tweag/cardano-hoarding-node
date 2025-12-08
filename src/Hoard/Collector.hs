module Hoard.Collector (dispatchDiscoveredNodes, runCollector) where

import Control.Concurrent (threadDelay)
import Data.Set qualified as S
import Effectful (Eff, IOE, (:>))

import Effectful.Exception (bracket)
import Effectful.State.Static.Shared (State, gets, modify, state)
import Hoard.Control.Exception (withExceptionLogging)
import Hoard.Data.Peer (PeerAddress)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Network (Network, connectToPeer)
import Hoard.Effects.Pub (Pub, publish)
import Hoard.Events.Collector (CollectorEvent (..))
import Hoard.Network.Events (PeerSharingEvent (..), PeersReceivedData (..))
import Hoard.Types.HoardState (HoardState, connectedPeers)
import Prelude hiding (State, gets, modify, state)


dispatchDiscoveredNodes
    :: ( Conc :> es
       , IOE :> es
       , Log :> es
       , Network :> es
       , Pub :> es
       , State HoardState :> es
       )
    => PeerSharingEvent
    -> Eff es ()
dispatchDiscoveredNodes = \case
    (PeersReceived (PeersReceivedData {peerAddresses})) -> do
        Log.info "Dispatch: Received peers"
        currPeers <- gets connectedPeers
        let peersToConnect = S.difference peerAddresses currPeers
        Log.info $ "Dispatch: " <> show (S.size peersToConnect) <> " new peers to connect to"
        forM_ peersToConnect $ \peerAddr -> do
            _ <-
                Conc.forkTry @SomeException
                    . withExceptionLogging ("collector " <> show peerAddr)
                    $ bracket
                        ( state \r ->
                            (peerAddr, r {connectedPeers = S.insert peerAddr r.connectedPeers})
                        )
                        ( \addr ->
                            modify \r -> r {connectedPeers = S.delete addr r.connectedPeers}
                        )
                        runCollector
            pure ()
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
