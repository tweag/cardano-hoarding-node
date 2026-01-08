module Hoard.ChainSync
    ( run
    , runListeners
    ) where

import Cardano.Api.LedgerState ()
import Effectful (Eff, (:>))

import Hoard.ChainSync.Listeners qualified as Listeners
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.HeaderRepo (HeaderRepo)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Sub (Sub)
import Hoard.Effects.Sub qualified as Sub


run :: (Log :> es, Conc :> es, HeaderRepo :> es, Sub :> es) => Eff es ()
run = runListeners


runListeners :: (Log :> es, Conc :> es, HeaderRepo :> es, Sub :> es) => Eff es ()
runListeners = do
    _ <- Conc.fork_ $ Sub.listen Listeners.chainSyncHeaderReceived
    _ <- Conc.fork_ $ Sub.listen Listeners.chainSyncStarted
    _ <- Conc.fork_ $ Sub.listen Listeners.chainSyncRollBackward
    _ <- Conc.fork_ $ Sub.listen Listeners.chainSyncRollForward
    _ <- Conc.fork_ $ Sub.listen Listeners.chainSyncIntersectionFound
    pure ()
