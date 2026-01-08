module Hoard.Collectors
    ( -- * Main
      run
    , runListeners
    , runCollectors
    ) where

import Data.Set qualified as S
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Shared (State)
import Effectful.Timeout (Timeout)
import Prelude hiding (Reader, State, asks, gets, modify, state)

import Hoard.Bootstrap (bootstrapPeers)
import Hoard.Collectors.Listeners qualified as Listeners
import Hoard.Collectors.State (BlocksBeingFetched)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Chan (Chan)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToNode (NodeToNode)
import Hoard.Effects.PeerRepo (PeerRepo, getAllPeers)
import Hoard.Effects.Pub (Pub)
import Hoard.Effects.Sub (Sub)
import Hoard.Effects.Sub qualified as Sub
import Hoard.Types.Environment (Config (..))
import Hoard.Types.HoardState (HoardState (..))


run
    :: ( BlockRepo :> es
       , Chan :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , IOE :> es
       , Log :> es
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
run = do
    runCollectors
    runListeners


runListeners
    :: ( BlockRepo :> es
       , Chan :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , Log :> es
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
    Conc.fork_ $ Sub.listen Listeners.dispatchDiscoveredNodes
    Conc.fork_ $ Sub.listen Listeners.collectorEvent


-- | Start collectors for all known peers, or bootstrap if none exist.
--
-- This queries the database for known peers and:
-- - If peers exist, starts collectors for all of them
-- - If no peers exist, bootstraps from the hardcoded preview-node peer
runCollectors
    :: ( BlockRepo :> es
       , Chan :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , IOE :> es
       , Log :> es
       , NodeToNode :> es
       , Reader Config :> es
       , PeerRepo :> es
       , Pub :> es
       , State BlocksBeingFetched :> es
       , State HoardState :> es
       , Timeout :> es
       )
    => Eff es ()
runCollectors = do
    -- Check if there are any known peers in the database
    knownPeers <- getAllPeers

    if S.null knownPeers
        then do
            -- No known peers, bootstrap from peer snapshot
            Log.debug "No known peers found, bootstrapping from peer snapshot"
            bootstrappedPeers <- bootstrapPeers
            let peerCount = S.size bootstrappedPeers
            Log.debug $ "Bootstrapped " <> show peerCount <> " peers from peer snapshot"
            for_ (S.toList bootstrappedPeers) Listeners.bracketCollector
        else do
            -- Start collectors for all known peers
            let peerCount = S.size knownPeers
            Log.debug $ "Found " <> show peerCount <> " known peers, starting collectors"
            for_ (S.toList knownPeers) Listeners.bracketCollector
