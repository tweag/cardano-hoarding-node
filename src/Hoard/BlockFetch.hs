module Hoard.BlockFetch (component) where

import Effectful ((:>))

import Hoard.BlockFetch.Events (BlockBatchCompleted, BlockFetchFailed, BlockFetchStarted, BlockReceived)
import Hoard.Component (Component (..), defaultComponent)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Monitoring.Metrics (Metrics)
import Hoard.Effects.Monitoring.Tracing (Tracing)
import Hoard.Effects.Publishing (Sub)
import Hoard.Effects.Verifier (Verifier)

import Hoard.BlockFetch.Listeners qualified as Listeners
import Hoard.Effects.Publishing qualified as Sub


component
    :: ( BlockRepo :> es
       , Metrics :> es
       , Sub BlockBatchCompleted :> es
       , Sub BlockFetchFailed :> es
       , Sub BlockFetchStarted :> es
       , Sub BlockReceived :> es
       , Tracing :> es
       , Verifier :> es
       )
    => Component es
component =
    defaultComponent
        { name = "BlockFetch"
        , listeners =
            pure
                [ Sub.listen Listeners.blockFetchStarted
                , Sub.listen Listeners.blockReceived
                , Sub.listen Listeners.blockFetchFailed
                , Sub.listen Listeners.blockBatchCompleted
                ]
        }
