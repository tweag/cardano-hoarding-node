module Hoard.Collectors (startCollectors) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, forever)
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.State.Static.Shared (State, modify)

import Data.Map.Strict qualified as Map

import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Pub (Pub, publish)
import Hoard.Events.Collector (CollectorEvent (..))
import Hoard.Types.Collector (CollectorHandle (..), CollectorId (..), Peer)
import Hoard.Types.HoardState (HoardState (..))

import Hoard.Effects.Conc qualified as Conc


startCollectors
    :: ( Conc :> es
       , IOE :> es
       , Pub :> es
       , State HoardState :> es
       )
    => Int
    -> [Peer]
    -> Eff es ()
startCollectors maxPeersPerCollector peers = do
    let peerGroups = groupPeers maxPeersPerCollector peers
    forM_ peerGroups $ \handle ->
        Conc.fork_
            . collectorLoop
            $ handle
    Conc.awaitAll


collectorLoop
    :: ( IOE :> es
       , Pub :> es
       , State HoardState :> es
       )
    => CollectorHandle
    -> Eff es a
collectorLoop handle = do
    modify $ \s -> s {collectors = Map.insert handle.cid handle s.collectors}
    publish $ CollectorStarted handle.cid handle.peers
    connections <- forM handle.peers $ \peer -> do
        publish $ ConnectingToPeer handle.cid peer
        conn <- connectToPeer peer
        publish $ ConnectedToPeer handle.cid peer
        pure (peer, conn)
    forever $ do
        forM_ connections $ \(peer, conn) -> do
            processChainSync handle.cid peer conn
            processBlockFetch handle.cid peer conn
        liftIO $ threadDelay 10000


groupPeers :: Int -> [Peer] -> [CollectorHandle]
groupPeers maxPeersPerCollector =
    fmap (uncurry CollectorHandle) . zip (CollectorId <$> [1 ..]) . chunksOf maxPeersPerCollector
  where
    chunksOf _ [] = []
    chunksOf n xs =
        let (chunk, rest) = splitAt n xs
        in  chunk : chunksOf n rest


connectToPeer :: Peer -> Eff es Connection
connectToPeer _ =
    -- TODO: Implement connectToPeer
    pure Connection


processChainSync :: CollectorId -> Peer -> Connection -> Eff es ()
processChainSync _ _ _ = do
    -- TODO: Implement processChainSync
    pure ()


processBlockFetch :: CollectorId -> Peer -> Connection -> Eff es ()
processBlockFetch _ _ _ = do
    -- TODO: Implement processBlockFetch
    pure ()


data Connection = Connection
