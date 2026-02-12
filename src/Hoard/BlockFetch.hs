module Hoard.BlockFetch (BlockFetch (..)) where

import Effectful (Eff, (:>))

import Hoard.BlockFetch.Events (BlockBatchCompleted, BlockFetchFailed, BlockFetchStarted, BlockReceived)
import Hoard.Component (Component (..), Listener)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Monitoring.Metrics (Metrics)
import Hoard.Effects.Monitoring.Tracing (Tracing)
import Hoard.Effects.Publishing (Sub)

import Hoard.BlockFetch.Listeners qualified as Listeners
import Hoard.Effects.Publishing qualified as Sub


data BlockFetch = BlockFetch


instance Component BlockFetch es where
    type
        Effects BlockFetch es =
            ( BlockRepo :> es
            , Conc :> es
            , Tracing :> es
            , Metrics :> es
            , Sub BlockFetchStarted :> es
            , Sub BlockReceived :> es
            , Sub BlockFetchFailed :> es
            , Sub BlockBatchCompleted :> es
            )


    listeners :: (Effects BlockFetch es) => Eff es [Listener es]
    listeners =
        pure
            [ Sub.listen Listeners.blockFetchStarted
            , Sub.listen Listeners.blockReceived
            , Sub.listen Listeners.blockFetchFailed
            , Sub.listen Listeners.blockBatchCompleted
            ]
