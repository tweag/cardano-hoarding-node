module Hoard.DB.Schemas.Peers
    ( Row (..)
    , schema
    , peerFromRow
    , rowFromPeer
    )
where

import Data.Time (UTCTime)
import Rel8
    ( Column
    , Name
    , Rel8able
    , Result
    , TableSchema
    )

import Hoard.DB.Schema (mkSchema)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Types.NodeIP (NodeIP)


data Row f = Row
    { id :: Column f (ID Peer)
    , address :: Column f NodeIP
    , port :: Column f Int32
    , firstDiscovered :: Column f UTCTime
    , lastSeen :: Column f UTCTime
    , lastConnected :: Column f (Maybe UTCTime)
    , discoveredVia :: Column f Text
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)


deriving instance Eq (Row Result)


deriving instance Show (Row Result)


-- | Table schema for peers
schema :: TableSchema (Row Name)
schema = mkSchema "peers"


-- | Convert a database row to a Peer domain type
peerFromRow :: Row Result -> Peer
peerFromRow row =
    Peer
        { id = row.id
        , address = PeerAddress row.address (fromIntegral row.port)
        , firstDiscovered = row.firstDiscovered
        , lastSeen = row.lastSeen
        , lastConnected = row.lastConnected
        , discoveredVia = row.discoveredVia
        }


-- | Convert a Peer domain type to a database row
rowFromPeer :: Peer -> Row Result
rowFromPeer peer =
    Row
        { id = peer.id
        , address = peer.address.host
        , port = fromIntegral peer.address.port
        , firstDiscovered = peer.firstDiscovered
        , lastSeen = peer.lastSeen
        , lastConnected = peer.lastConnected
        , discoveredVia = peer.discoveredVia
        }
