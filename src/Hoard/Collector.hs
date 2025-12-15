module Hoard.Collector (dispatchDiscoveredNodes) where

import Control.Concurrent (threadDelay)
import Data.Set qualified as S
import Effectful (Eff, IOE, (:>))

import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.Exception (bracket)
import Effectful.State.Static.Shared (State, gets, modify, state)
import Hoard.Control.Exception (withExceptionLogging)
import Hoard.Data.Peer (Peer (..))
import Hoard.Data.ProtocolInfo (ProtocolConfigPath)
import Hoard.Effects.Chan (Chan)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Input (Input)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToNode (connectToPeer)
import Hoard.Effects.PeerRepo (PeerRepo, upsertPeers)
import Hoard.Effects.Pub (Pub, publish)
import Hoard.Effects.Sub (Sub)
import Hoard.Events.Collector (CollectorEvent (..))
import Hoard.Network.Events (PeerSharingEvent (..), PeersReceivedData (..))
import Hoard.Types.HoardState (HoardState, connectedPeers)
import Ouroboros.Network.IOManager (IOManager)
import Prelude hiding (State, gets, modify, state)


dispatchDiscoveredNodes
    :: ( Chan :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , Error Text :> es
       , IOE :> es
       , Input IOManager :> es
       , Input ProtocolConfigPath :> es
       , Log :> es
       , PeerRepo :> es
       , Pub :> es
       , State HoardState :> es
       , Sub :> es
       )
    => PeerSharingEvent
    -> Eff es ()
dispatchDiscoveredNodes = \case
    (PeersReceived (PeersReceivedData {peer = sourcePeer, peerAddresses})) -> do
        Log.info "Dispatch: Received peers"

        -- First, upsert all discovered peer addresses to create Peer records
        timestamp <- Clock.currentTime
        upsertedPeers <- upsertPeers peerAddresses sourcePeer.address timestamp

        currPeers <- gets connectedPeers
        let peersToConnect = S.difference upsertedPeers currPeers
        Log.info $ "Dispatch: " <> show (S.size peersToConnect) <> " new peers to connect to"

        forM_ peersToConnect $ \peer -> do
            _ <-
                Conc.forkTry @SomeException
                    . withExceptionLogging ("collector " <> show peer.address)
                    $ bracket
                        ( state \r ->
                            (peer, r {connectedPeers = S.insert peer r.connectedPeers})
                        )
                        ( \p ->
                            modify \r -> r {connectedPeers = S.delete p r.connectedPeers}
                        )
                        runCollector
            pure ()
    _ -> pure ()
  where
    runCollector peer = do
        publish $ CollectorStarted peer.address
        publish $ ConnectingToPeer peer.address

        _conn <- connectToPeer peer
        publish $ ConnectedToPeer peer.address

        -- Connection is now running autonomously!
        -- Protocols publish events as they receive data
        -- For now, just keep the collector alive
        forever $ liftIO $ threadDelay 1000000
