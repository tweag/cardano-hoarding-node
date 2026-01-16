module Hoard.Listeners.DiscoveredNodesListener (dispatchDiscoveredNodes) where

import Data.Set qualified as S
import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Shared (State)
import Effectful.Timeout (Timeout)
import Prelude hiding (Reader, State, gets, modify)

import Hoard.Collector (bracketCollector)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Chan (Chan)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToNode (NodeToNode)
import Hoard.Effects.PeerRepo (PeerRepo, upsertPeers)
import Hoard.Effects.Pub (Pub)
import Hoard.Network.Events (PeersReceived (..))
import Hoard.Types.Environment (Config)
import Hoard.Types.HoardState (BlocksBeingFetched, HoardState (..))


-- | Dispatch discovered peer nodes for connection.
--
-- When peers are discovered via peer sharing:
-- - Upserts all discovered peer addresses to the database
-- - Filters out peers already connected
-- - Filters out peers in cooldown period (recent failures)
-- - Starts collectors for all eligible peers
dispatchDiscoveredNodes
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
       , Timeout :> es
       )
    => PeersReceived
    -> Eff es ()
dispatchDiscoveredNodes event = do
    Log.info "Dispatch: Received peers"

    -- First, upsert all discovered peer addresses to create Peer records
    timestamp <- Clock.currentTime
    upsertedPeers <- upsertPeers event.peerAddresses event.peer.address timestamp

    Log.info $ "Dispatch: " <> show (S.size upsertedPeers) <> " new peers to connect to"

    for_ upsertedPeers bracketCollector
