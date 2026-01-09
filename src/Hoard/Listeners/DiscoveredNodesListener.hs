module Hoard.Listeners.DiscoveredNodesListener (dispatchDiscoveredNodes) where

import Data.Set qualified as S
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Shared (State)
import Prelude hiding (Reader, State, gets, modify)

import Effectful.Concurrent (Concurrent)
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
import Hoard.Network.Events (PeerSharingEvent (..), PeersReceivedData (..))
import Hoard.Types.Environment (Config)
import Hoard.Types.HoardState (HoardState (..))


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
       , Conc :> es
       , Log :> es
       , NodeToNode :> es
       , PeerRepo :> es
       , Concurrent :> es
       , Pub :> es
       , Reader Config :> es
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

        Log.info $ "Dispatch: " <> show (S.size upsertedPeers) <> " new peers to connect to"

        for_ upsertedPeers bracketCollector
    _ -> pure ()
