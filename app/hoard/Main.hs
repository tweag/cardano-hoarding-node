module Main (main) where

import Hoard.Collector (runCollectors)
import Hoard.Effects (runEffectStack)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Listeners (runListeners)
import Hoard.Server (runServer)


main :: IO ()
main = runEffectStack do
    runServer
    runListeners
    runCollectors
    Conc.awaitAll
