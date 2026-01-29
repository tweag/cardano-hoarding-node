module Hoard.BlockFetch (run, runListeners) where

import Effectful (Eff, (:>))

import Hoard.BlockFetch.Listeners qualified as Listeners
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log (Log, withNamespace)
import Hoard.Effects.Metrics (Metrics)
import Hoard.Effects.Publishing (Sub)
import Hoard.Effects.Publishing qualified as Sub


run
    :: ( BlockRepo :> es
       , Conc :> es
       , Log :> es
       , Metrics :> es
       , Sub :> es
       )
    => Eff es ()
run = withNamespace "BlockFetch" $ runListeners


runListeners
    :: ( BlockRepo :> es
       , Conc :> es
       , Log :> es
       , Metrics :> es
       , Sub :> es
       )
    => Eff es ()
runListeners = do
    _ <- Conc.fork_ $ Sub.listen Listeners.blockFetchStarted
    _ <- Conc.fork_ $ Sub.listen Listeners.blockReceived
    _ <- Conc.fork_ $ Sub.listen Listeners.blockFetchFailed
    _ <- Conc.fork_ $ Sub.listen Listeners.blockBatchCompleted
    pure ()
