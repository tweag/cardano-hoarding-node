module Hoard.ChainSync
    ( run
    , runListeners
    ) where

import Cardano.Api.LedgerState ()
import Effectful (Eff, (:>))

import Hoard.ChainSync.Events (ChainSyncIntersectionFound, ChainSyncStarted, HeaderReceived, RollBackward, RollForward)
import Hoard.ChainSync.Listeners qualified as Listeners
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.HeaderRepo (HeaderRepo)
import Hoard.Effects.Monitoring.Metrics (Metrics)
import Hoard.Effects.Monitoring.Tracing (Tracing, withSpan)
import Hoard.Effects.Publishing (Sub)
import Hoard.Effects.Publishing qualified as Sub


run :: (Tracing :> es, Conc :> es, HeaderRepo :> es, Metrics :> es, Sub HeaderReceived :> es, Sub ChainSyncStarted :> es, Sub RollBackward :> es, Sub RollForward :> es, Sub ChainSyncIntersectionFound :> es) => Eff es ()
run = withSpan "chain_sync" $ do
    runListeners


runListeners :: (Tracing :> es, Conc :> es, HeaderRepo :> es, Metrics :> es, Sub HeaderReceived :> es, Sub ChainSyncStarted :> es, Sub RollBackward :> es, Sub RollForward :> es, Sub ChainSyncIntersectionFound :> es) => Eff es ()
runListeners = withSpan "listeners" $ do
    _ <- Conc.fork_ $ Sub.listen Listeners.chainSyncHeaderReceived
    _ <- Conc.fork_ $ Sub.listen Listeners.chainSyncStarted
    _ <- Conc.fork_ $ Sub.listen Listeners.chainSyncRollBackward
    _ <- Conc.fork_ $ Sub.listen Listeners.chainSyncRollForward
    _ <- Conc.fork_ $ Sub.listen Listeners.chainSyncIntersectionFound
    pure ()
