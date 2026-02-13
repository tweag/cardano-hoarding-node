module Hoard.PeerSharing (component) where

import Effectful ((:>))

import Hoard.Component (Component (..), defaultComponent)
import Hoard.Effects.Monitoring.Tracing (Tracing)
import Hoard.Effects.PeerRepo (PeerRepo)
import Hoard.Effects.Publishing (Sub)
import Hoard.PeerSharing.Events (PeerSharingFailed, PeerSharingStarted, PeersReceived)

import Hoard.Effects.Publishing qualified as Sub
import Hoard.PeerSharing.Listeners qualified as Listeners


component
    :: ( PeerRepo :> es
       , Sub PeerSharingFailed :> es
       , Sub PeerSharingStarted :> es
       , Sub PeersReceived :> es
       , Tracing :> es
       )
    => Component es
component =
    defaultComponent
        { name = "PeerSharing"
        , listeners =
            pure
                [ Sub.listen Listeners.peerSharingStarted
                , Sub.listen Listeners.peersReceived
                , Sub.listen Listeners.peerSharingFailed
                ]
        }
