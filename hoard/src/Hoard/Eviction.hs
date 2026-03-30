module Hoard.Eviction (component, Config (..)) where

import Effectful.Reader.Static (Reader, ask)

import Atelier.Component (Component (..), defaultComponent)
import Atelier.Effects.Delay (Delay)
import Atelier.Effects.Monitoring.Tracing (Tracing, addAttribute, withSpan)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.HeaderRepo (HeaderRepo)
import Hoard.Eviction.Config (Config (..))

import Atelier.Effects.Delay qualified as Delay
import Hoard.Effects.BlockRepo qualified as BlockRepo
import Hoard.Effects.HeaderRepo qualified as HeaderRepo


component
    :: ( BlockRepo :> es
       , Delay :> es
       , HeaderRepo :> es
       , Reader Config :> es
       , Tracing :> es
       )
    => Component es
component =
    defaultComponent
        { name = "Eviction"
        , triggers = do
            cfg <- ask
            let interval = cfg.evictionIntervalSeconds
            pure
                [ Delay.every interval $ withSpan "eviction.evict" do
                    blockCount <- BlockRepo.evictBlocks
                    addAttribute "evicted.blocks" blockCount
                    headerCount <- HeaderRepo.evictHeaders
                    addAttribute "evicted.headers" headerCount
                ]
        }
