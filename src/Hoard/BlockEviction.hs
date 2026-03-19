module Hoard.BlockEviction (component, Config (..)) where

import Effectful.Reader.Static (Reader, ask)

import Atelier.Component (Component (..), defaultComponent)
import Atelier.Effects.Delay (Delay)
import Atelier.Effects.Monitoring.Tracing (Tracing, addAttribute, withSpan)
import Hoard.BlockEviction.Config (Config (..))
import Hoard.Effects.BlockRepo (BlockRepo)

import Atelier.Effects.Delay qualified as Delay
import Hoard.Effects.BlockRepo qualified as BlockRepo


component
    :: ( BlockRepo :> es
       , Delay :> es
       , Reader Config :> es
       , Tracing :> es
       )
    => Component es
component =
    defaultComponent
        { name = "BlockEviction"
        , triggers = do
            cfg <- ask
            let interval = Delay.seconds cfg.evictionIntervalSeconds
            pure
                [ Delay.every interval $ withSpan "block_eviction.evict" do
                    count <- BlockRepo.evictBlocks
                    addAttribute "evicted.count" count
                ]
        }
