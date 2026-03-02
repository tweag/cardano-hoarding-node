module Hoard.BlockEviction (component, Config (..)) where

import Effectful.Concurrent (Concurrent)
import Effectful.Reader.Static (Reader, ask)

import Hoard.BlockEviction.Config (Config (..))
import Hoard.Component (Component (..), defaultComponent)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Monitoring.Tracing (Tracing, addAttribute, withSpan)
import Hoard.Triggers (every)

import Hoard.Effects.BlockRepo qualified as BlockRepo


component
    :: ( BlockRepo :> es
       , Concurrent :> es
       , Reader Config :> es
       , Tracing :> es
       )
    => Component es
component =
    defaultComponent
        { name = "BlockEviction"
        , triggers = do
            cfg <- ask
            let interval = cfg.evictionIntervalSeconds
            pure
                [ every interval $ withSpan "block_eviction.evict" do
                    count <- BlockRepo.evictBlocks
                    addAttribute "evicted.count" count
                ]
        }
