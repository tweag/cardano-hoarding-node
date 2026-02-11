module Hoard.PeerSharing (PeerSharing (..)) where

import Effectful ((:>))
import Hoard.Component (Component (..), Listener)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Monitoring.Tracing (Tracing)
import Hoard.Effects.PeerRepo (PeerRepo)
import Hoard.Effects.Publishing (Sub)
import Hoard.Effects.Publishing qualified as Sub
import Hoard.PeerSharing.Events (PeerSharingFailed, PeerSharingStarted, PeersReceived)
import Hoard.PeerSharing.Listeners qualified as Listeners


data PeerSharing = PeerSharing


instance Component PeerSharing es where
    type
        Effects PeerSharing es =
            ( Conc :> es
            , Sub PeerSharingStarted :> es
            , Sub PeersReceived :> es
            , Sub PeerSharingFailed :> es
            , PeerRepo :> es
            , Tracing :> es
            )


    listeners :: (Effects PeerSharing es) => [Listener es]
    listeners =
        [ Sub.listen Listeners.peerSharingStarted
        , Sub.listen Listeners.peersReceived
        , Sub.listen Listeners.peerSharingFailed
        ]
