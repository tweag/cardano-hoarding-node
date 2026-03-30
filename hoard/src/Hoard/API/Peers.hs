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

import Atelier.Effects.Clock (Clock)
import Atelier.Types.QuietSnake (QuietSnake (..))
import Hoard.API.Util ((::>))
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Effects.PeerRepo (PeerRepo)
import Prelude hiding ((:>))

import Atelier.Effects.Clock qualified as Clock
import Hoard.Effects.PeerRepo qualified as PeerRepo


-- | Request body for pinning or unpinning a peer.
--
-- We don't take an (ID Peer) for ergonomics: operators can submit any address
-- and the peer is upserted on the fly.
data PinPeerRequest = PinPeerRequest
    { peer :: PeerAddress
    , note :: Maybe Text
    }
    deriving stock (Generic)
    -- TODO: Consider adding ToJSON instance to QuietSnake and use that here.
    deriving anyclass (ToJSON)
    deriving (FromJSON) via QuietSnake PinPeerRequest


data PeersRoutes mode = PeersRoutes
    { getPinned
        :: mode :- Get '[JSON] [Peer]
    , addPinned
        :: mode :- ReqBody '[JSON] [PinPeerRequest] :> Post '[JSON] [Peer]
    , removePinned
        :: mode :- ReqBody '[JSON] [PeerAddress] :> DeleteNoContent
    }
    deriving stock (Generic)


type PeersAPI mode = mode :- "peers" :> "pinned" :> NamedRoutes PeersRoutes


peersHandler
    :: (Clock ::> es, PeerRepo ::> es)
    => PeersRoutes (AsServerT (Eff es))
peersHandler =
    PeersRoutes
        { getPinned = PeerRepo.getPinnedPeers
        , addPinned = \reqs -> do
            now <- Clock.currentTime
            PeerRepo.pinPeers now [(req.peer, req.note) | req <- reqs]
        , removePinned = \addrs -> do
            PeerRepo.unpinPeers addrs
            pure NoContent
        }
