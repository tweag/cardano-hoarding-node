module Hoard.Effects.Log.Labeled
    ( LabeledLogger (..)
    , LogFn
    , labeled
    ) where

import Effectful (Eff, (:>))
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log


labeled :: (HasCallStack, Log :> es) => Text -> LabeledLogger (Eff es)
labeled =
    withFrozenCallStack $ \label ->
        let withLabel f s = f (label <> s)
        in  LabeledLogger
                { debug = withLabel Log.debug
                , info = withLabel Log.info
                , warn = withLabel Log.warn
                , err = withLabel Log.err
                , appendLabel = \l -> labeled $ label <> l
                }


data LabeledLogger m = LabeledLogger
    { debug :: LogFn m
    , info :: LogFn m
    , warn :: LogFn m
    , err :: LogFn m
    , appendLabel :: Text -> LabeledLogger m
    }


type LogFn m = Text -> m ()
