module Hoard.Triggers (runTriggers) where

import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.State.Static.Shared (State, modify)
import Hoard.Effects (AppEff)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.NodeToClient qualified as N
import Hoard.Types.HoardState (HoardState (immutableTip))
import Prelude hiding (State, modify)


runTriggers :: (AppEff es) => Eff es ()
runTriggers = do
    nowAndEvery 20 refreshImmutableTip


refreshImmutableTip :: (NodeToClient :> es, State HoardState :> es) => Eff es ()
refreshImmutableTip =
    do
        tip <- N.immutableTip
        modify (\hoardState -> hoardState {immutableTip = tip})


nowAndEvery :: (Concurrent :> es, Conc :> es) => Int -> Eff es () -> Eff es ()
nowAndEvery delay action = do
    action
    Conc.fork_ $ forever $ do
        threadDelay (delay * 1000000)
        action
