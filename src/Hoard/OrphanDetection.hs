module Hoard.OrphanDetection (run, runListeners) where

import Effectful (Eff, (:>))
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.State.Static.Shared (State)
import Prelude hiding (State)

import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log (Log, withNamespace)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.Publishing (Sub)
import Hoard.Effects.Publishing qualified as Sub
import Hoard.OrphanDetection.Listeners qualified as Listeners
import Hoard.Types.HoardState (HoardState)


-- | Run the orphan detection system
run
    :: ( BlockRepo :> es
       , Conc :> es
       , Clock :> es
       , Log :> es
       , NodeToClient :> es
       , State HoardState :> es
       , Sub :> es
       )
    => Eff es ()
run = withNamespace "OrphanDetection" runListeners


-- | Start orphan detection listeners
runListeners
    :: ( BlockRepo :> es
       , Conc :> es
       , Clock :> es
       , Log :> es
       , NodeToClient :> es
       , State HoardState :> es
       , Sub :> es
       )
    => Eff es ()
runListeners = do
    _ <- Conc.fork_ $ Sub.listen $ withErrorHandling Listeners.blockReceivedClassifier
    _ <- Conc.fork_ $ Sub.listen $ withErrorHandling Listeners.immutableTipUpdatedAger
    pure ()
  where
    withErrorHandling listener event =
        runErrorNoCallStack (listener event) >>= \case
            Left err -> Log.err err
            Right () -> pure ()
