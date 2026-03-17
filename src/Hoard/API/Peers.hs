module Hoard.API.Peers
    ( PeersRoutes (..)
    , PeersAPI
    , PinPeerRequest (..)
    , peersHandler
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Servant
    ( DeleteNoContent
    , Get
    , JSON
    , NamedRoutes
    , NoContent (..)
    , Post
    , ReqBody
    , type (:-)
    , type (:>)
    )
import Servant.Server.Generic (AsServerT)

import Hoard.API.Util ((::>))
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.PeerRepo (PeerRepo)
import Hoard.Types.QuietSnake (QuietSnake (..))
import Prelude hiding ((:>))

import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.PeerRepo qualified as PeerRepo


-- | Request body for pinning or unpinning a peer.
--
-- We don't take an (ID Peer) for ergonomics: operators can submit any address
-- and the peer is upserted on the fly.
data PinPeerRequest = PinPeerRequest
    { peer :: PeerAddress
    , note :: Maybe Text
    }
    deriving (ToJSON)
    deriving stock (Generic)
    deriving (FromJSON) via QuietSnake PinPeerRequest


data PeersRoutes mode = PeersRoutes
    { getPinned
        :: mode :- "pinned" :> Get '[JSON] [Peer]
    , addPinned
        :: mode :- "pinned" :> ReqBody '[JSON] [PinPeerRequest] :> Post '[JSON] [Peer]
    , removePinned
        :: mode :- "pinned" :> ReqBody '[JSON] [PinPeerRequest] :> DeleteNoContent
    }
    deriving (Generic)


type PeersAPI mode = mode :- "peers" :> NamedRoutes PeersRoutes


peersHandler
    :: (Clock ::> es, PeerRepo ::> es)
    => PeersRoutes (AsServerT (Eff es))
peersHandler =
    PeersRoutes
        { getPinned = PeerRepo.getPinnedPeers
        , addPinned = \reqs -> do
            now <- Clock.currentTime
            PeerRepo.pinPeers now [(req.peer, req.note) | req <- reqs]
        , removePinned = \reqs -> do
            PeerRepo.unpinPeer [req.peer | req <- reqs]
            pure NoContent
        }
