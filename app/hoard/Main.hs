module Main (main) where

import Effectful (Eff)
import Hoard.Collector (runCollectors)
import Hoard.Effects (runEffectStack)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Listeners (runListeners)
import Hoard.Server (runServer)
import Hoard.Triggers (runTriggers)


main :: IO ()
main = runEffectStack mainEffectful


mainEffectful :: (_) => Eff es ()
mainEffectful =
    do
        runServer
        runListeners
        runCollectors
        runTriggers
        Conc.awaitAll
