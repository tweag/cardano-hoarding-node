module Hoard.Collector (dispatchDiscoveredNodes, runCollector) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Void (Void)
import Effectful (Eff, IOE, liftIO, (:>))

import Hoard.Effects.Conc (Conc, fork_)
import Hoard.Effects.Pub (Pub, publish)
import Hoard.Effects.Sub (Sub, listen)
import Hoard.Events.Collector (CollectorEvent (..))
import Hoard.Events.Node (NodeDiscovered (..))
import Hoard.Types.Collector (Peer)


dispatchDiscoveredNodes :: (Conc :> es, IOE :> es, Pub :> es, Sub :> es) => Eff es Void
dispatchDiscoveredNodes = listen $ \(NodeDiscovered peer) ->
    fork_ $ runCollector peer


runCollector :: (IOE :> es, Pub :> es) => Peer -> Eff es a
runCollector peer = do
    publish $ CollectorStarted peer
    publish $ ConnectingToPeer peer
    conn <- connectToPeer peer
    publish $ ConnectedToPeer peer
    forever $ do
        processChainSync peer conn
        processBlockFetch peer conn
        liftIO $ threadDelay 10000


connectToPeer :: Peer -> Eff es Connection
connectToPeer _ =
    -- TODO: Implement connectToPeer
    pure Connection


processChainSync :: Peer -> Connection -> Eff es ()
processChainSync _ _ = do
    -- TODO: Implement processChainSync
    pure ()


processBlockFetch :: Peer -> Connection -> Eff es ()
processBlockFetch _ _ = do
    -- TODO: Implement processBlockFetch
    pure ()


data Connection = Connection
