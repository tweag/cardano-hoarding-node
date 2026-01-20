module Hoard.BlockFetch (run, runListeners) where

import Effectful (Eff, (:>))
import Prelude hiding (Reader, State)

import Effectful.State.Static.Shared (State)
import Hoard.BlockFetch.Listeners qualified as Listeners
import Hoard.Collectors.State (BlocksBeingFetched)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log (Log)
import Hoard.Effects.Sub (Sub)
import Hoard.Effects.Sub qualified as Sub


run
    :: ( BlockRepo :> es
       , Conc :> es
       , Log :> es
       , State BlocksBeingFetched :> es
       , Sub :> es
       )
    => Eff es ()
run = runListeners


runListeners
    :: ( BlockRepo :> es
       , Conc :> es
       , Log :> es
       , State BlocksBeingFetched :> es
       , Sub :> es
       )
    => Eff es ()
runListeners = do
    _ <- Conc.fork_ $ Sub.listen Listeners.blockFetchStarted
    _ <- Conc.fork_ $ Sub.listen Listeners.blockReceived
    _ <- Conc.fork_ $ Sub.listen Listeners.blockFetchFailed
    _ <- Conc.fork_ $ Sub.listen Listeners.blockBatchCompleted
    pure ()
