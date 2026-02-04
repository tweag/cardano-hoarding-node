module Hoard.PeerSharing (run, runListeners) where

import Effectful (Eff, (:>))
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Monitoring.Tracing (Tracing, withSpan)
import Hoard.Effects.PeerRepo (PeerRepo)
import Hoard.Effects.Publishing (Sub)
import Hoard.Effects.Publishing qualified as Sub
import Hoard.PeerSharing.Listeners qualified as Listeners


run :: (Conc :> es, Sub :> es, PeerRepo :> es, Tracing :> es) => Eff es ()
run = withSpan "peer_sharing" $ do
    runListeners


runListeners :: (Conc :> es, Sub :> es, PeerRepo :> es, Tracing :> es) => Eff es ()
runListeners = withSpan "listeners" $ do
    Conc.fork_ $ Sub.listen Listeners.peerSharingStarted
    Conc.fork_ $ Sub.listen Listeners.peersReceived
    Conc.fork_ $ Sub.listen Listeners.peerSharingFailed
