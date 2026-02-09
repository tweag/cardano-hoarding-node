module Hoard.PeerSharing (run, runListeners) where

import Effectful (Eff, (:>))
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log (Log)
import Hoard.Effects.Publishing (Sub)
import Hoard.Effects.Publishing qualified as Sub
import Hoard.PeerSharing.Listeners qualified as Listeners


run :: (Conc :> es, Sub :> es, Log :> es) => Eff es ()
run = runListeners


runListeners :: (Conc :> es, Sub :> es, Log :> es) => Eff es ()
runListeners = do
    Conc.fork_ $ Sub.listen Listeners.peerSharingStarted
    Conc.fork_ $ Sub.listen Listeners.peerSharingFailed
