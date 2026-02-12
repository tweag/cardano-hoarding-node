module Hoard.ChainSync (ChainSync (..)) where

import Cardano.Api.LedgerState ()
import Effectful (Eff, (:>))

import Hoard.ChainSync.Events (ChainSyncIntersectionFound, ChainSyncStarted, HeaderReceived, RollBackward, RollForward)
import Hoard.ChainSync.Listeners qualified as Listeners
import Hoard.Component (Component (..), Listener)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.HeaderRepo (HeaderRepo)
import Hoard.Effects.Monitoring.Metrics (Metrics)
import Hoard.Effects.Monitoring.Tracing (Tracing)
import Hoard.Effects.Publishing (Sub)
import Hoard.Effects.Publishing qualified as Sub


data ChainSync = ChainSync


instance Component ChainSync es where
    type
        Effects ChainSync es =
            ( Tracing :> es
            , Conc :> es
            , HeaderRepo :> es
            , Metrics :> es
            , Sub HeaderReceived :> es
            , Sub ChainSyncStarted :> es
            , Sub RollBackward :> es
            , Sub RollForward :> es
            , Sub ChainSyncIntersectionFound :> es
            )


    listeners :: (Effects ChainSync es) => Eff es [Listener es]
    listeners =
        pure
            [ Sub.listen Listeners.chainSyncHeaderReceived
            , Sub.listen Listeners.chainSyncStarted
            , Sub.listen Listeners.chainSyncRollBackward
            , Sub.listen Listeners.chainSyncRollForward
            , Sub.listen Listeners.chainSyncIntersectionFound
            ]
