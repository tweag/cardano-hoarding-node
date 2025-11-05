module Hoard.Collector (dispatchDiscoveredNodes, runCollector) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Void (Void)
import Effectful (Eff, IOE, liftIO, (:>))

import Hoard.Data.Peer (Peer)
import Hoard.Effects.Conc (Conc, fork_)
import Hoard.Effects.Network (Network, connectToPeer)
import Hoard.Effects.Pub (Pub, publish)
import Hoard.Effects.Sub (Sub, listen)
import Hoard.Events.Collector (CollectorEvent (..))
import Hoard.Events.Node (NodeDiscovered (..))


dispatchDiscoveredNodes
    :: (Conc :> es, IOE :> es, Network :> es, Pub :> es, Sub :> es)
    => Eff es Void
dispatchDiscoveredNodes = listen $ \(NodeDiscovered peer) ->
    fork_ $ runCollector peer


runCollector
    :: (IOE :> es, Network :> es, Pub :> es)
    => Peer
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
