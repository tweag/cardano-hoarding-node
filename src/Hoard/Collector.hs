module Hoard.Collector (dispatchDiscoveredNodes, runCollector) where

import Control.Concurrent (threadDelay)
import Data.Set qualified as S
import Effectful (Eff, IOE, (:>))
import Effectful.Exception (bracket)
import Effectful.State.Static.Shared (State, gets, modify, state)
import Prelude hiding (State, gets, modify, state)

import Hoard.Control.Exception (withExceptionLogging)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Chan (Chan)
import Hoard.Effects.Chan qualified as Chan
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Input (Input, input, runInputChan)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToNode (Config (..), NodeToNode, connectToPeer)
import Hoard.Effects.NodeToNode qualified as NodeToNode
import Hoard.Effects.Output (Output, output, runOutputChan)
import Hoard.Effects.PeerRepo (PeerRepo, upsertPeers)
import Hoard.Effects.Pub (Pub, publish)
import Hoard.Events.Collector (CollectorEvent (..))
import Hoard.Network.Events
    ( BlockFetchRequest (..)
    , BlockReceivedData (..)
    , HeaderReceivedData (..)
    , PeerSharingEvent (..)
    , PeersReceivedData (..)
    )
import Hoard.Types.HoardState (HoardState, connectedPeers)


dispatchDiscoveredNodes
    :: ( Chan :> es
       , Conc :> es
       , IOE :> es
       , Log :> es
       , NodeToNode :> es
       , PeerRepo :> es
       , Pub :> es
       , State HoardState :> es
       , Clock :> es
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


runCollector
    :: ( Conc :> es
       , Chan :> es
       , IOE :> es
       , NodeToNode :> es
       , Pub :> es
       )
    => Peer
    -> Eff es Void
runCollector peer =
    withBridge @BlockFetchRequest
        . withBridge @HeaderReceivedData
        . withBridge @BlockReceivedData
        $ do
            publish $ CollectorStarted peer.address
            publish $ ConnectingToPeer peer.address

            Conc.fork_ pickBlockFetchRequest
            _conn <-
                connectToPeer $
                    NodeToNode.Config
                        { peer
                        , emitFetchedHeader = output
                        , emitFetchedBlock = output
                        , awaitBlockFetchRequest = input
                        }
            publish $ ConnectedToPeer peer.address

            -- Connection is now running autonomously!
            -- Protocols publish events as they receive data
            -- For now, just keep the collector alive
            forever $ liftIO $ threadDelay 1000000


-- | Re-emit `HeaderReceived` events as `BlockFetchRequests`.
pickBlockFetchRequest
    :: ( Input HeaderReceivedData :> es
       , Output BlockFetchRequest :> es
       )
    => Eff es Void
pickBlockFetchRequest = forever do
    dat <- input
    output $
        BlockFetchRequest
            { timestamp = dat.timestamp
            , point = dat.point
            , peer = dat.peer.address
            }


-- | Bridge effects through bidirectional channels.
--
-- Creates a channel pair and interprets Input/Output effects over them,
-- enabling communication between threads.
--
-- This function:
-- 1. Creates a bidirectional channel pair using 'Chan.newChan'
-- 2. Interprets 'Input' and 'Output' effects over these channels
-- 3. Provides these effects to the wrapped action
-- 4. The action can fork threads that communicate via these effects
withBridge :: forall b es a. (Chan :> es) => Eff (Input b : Output b : es) a -> Eff es a
withBridge action = do
    (inChan, outChan) <- Chan.newChan
    runOutputChan inChan . runInputChan outChan $ action
