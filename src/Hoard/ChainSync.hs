module Hoard.ChainSync (component) where

import Cardano.Api.LedgerState ()
import Effectful ((:>))

import Hoard.ChainSync.Events (ChainSyncIntersectionFound, ChainSyncStarted, HeaderReceived, RollBackward, RollForward)
import Hoard.Component (Component (..), defaultComponent)
import Hoard.Effects.HeaderRepo (HeaderRepo)
import Hoard.Effects.Monitoring.Metrics (Metrics)
import Hoard.Effects.Monitoring.Tracing (Tracing)
import Hoard.Effects.Publishing (Sub)

import Hoard.ChainSync.Listeners qualified as Listeners
import Hoard.Effects.Publishing qualified as Sub


component
    :: ( HeaderRepo :> es
       , Metrics :> es
       , Sub ChainSyncIntersectionFound :> es
       , Sub ChainSyncStarted :> es
       , Sub HeaderReceived :> es
       , Sub RollBackward :> es
       , Sub RollForward :> es
       , Tracing :> es
       )
    => Component es
component =
    defaultComponent
        { name = "ChainSync"
        , listeners =
            pure
                [ Sub.listen Listeners.chainSyncHeaderReceived
                , Sub.listen Listeners.chainSyncStarted
                , Sub.listen Listeners.chainSyncRollBackward
                , Sub.listen Listeners.chainSyncRollForward
                , Sub.listen Listeners.chainSyncIntersectionFound
                ]
        }
