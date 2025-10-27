module Hoard.Collector (runCollector) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Effectful (Eff, IOE, liftIO, (:>))

import Hoard.Effects.Pub (Pub, publish)
import Hoard.Events.Collector (CollectorEvent (..))
import Hoard.Types.Collector (CollectorHandle (..), CollectorId (..), Peer)


runCollector :: (IOE :> es, Pub :> es) => CollectorHandle -> Eff es a
runCollector CollectorHandle {cid, peer} = do
    publish $ CollectorStarted cid peer
    publish $ ConnectingToPeer cid peer
    conn <- connectToPeer peer
    publish $ ConnectedToPeer cid peer
    forever $ do
        processChainSync cid peer conn
        processBlockFetch cid peer conn
        liftIO $ threadDelay 10000


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
