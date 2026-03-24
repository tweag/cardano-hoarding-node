module Hoard.ChainDB (component) where

import Effectful.State.Static.Shared (State, get)
import Ouroboros.Network.Block (pointSlot)
import Ouroboros.Network.Point (WithOrigin (..))

import Cardano.Api qualified as C

import Atelier.Component (Component (..), defaultComponent)
import Atelier.Effects.Delay (Delay)
import Atelier.Effects.Log (Log)
import Atelier.Effects.Publishing (Sub)
import Atelier.Time (Second)
import Hoard.ChainDB.Events (BlockRolledBack (..), ChainExtended (..))
import Hoard.Events.BlockFetch (BlockReceived (..))
import Hoard.Types.Cardano (ChainPoint (..))
import Hoard.Types.HoardState (HoardState (..))

import Atelier.Effects.Delay qualified as Delay
import Atelier.Effects.Log qualified as Log
import Atelier.Effects.Publishing qualified as Sub
import Hoard.Effects.ChainDB qualified as ChainDB


component
    :: ( ChainDB.ChainDB :> es
       , Delay :> es
       , Log :> es
       , State HoardState :> es
       , Sub BlockReceived :> es
       , Sub BlockRolledBack :> es
       , Sub ChainExtended :> es
       )
    => Component es
component =
    defaultComponent
        { name = "ChainDB"
        , triggers =
            pure
                [ Delay.every (3 :: Second) consistencyCheck
                ]
        , listeners =
            pure
                [ Sub.listen_ blockFeedListener
                , Sub.listen_ chainExtendedListener
                , Sub.listen_ rollbackListener
                ]
        }


-- | Subscribe to BlockReceived and feed each block into the embedded ChainDB.
blockFeedListener :: (ChainDB.ChainDB :> es) => BlockReceived -> Eff es ()
blockFeedListener BlockReceived {block} = ChainDB.feedBlock block


-- | Log volatile chain extensions published by the ChainDB interpreter.
chainExtendedListener :: (Log :> es) => ChainExtended -> Eff es ()
chainExtendedListener (ChainExtended point) =
    Log.debug $ "ChainDB: chain extended to slot " <> show (pointSlot point)


-- | Log rollbacks published by the ChainDB interpreter.
rollbackListener :: (Log :> es) => BlockRolledBack -> Eff es ()
rollbackListener (BlockRolledBack point) =
    Log.info $ "ChainDB: rollback to " <> show point


consistencyCheck
    :: ( ChainDB.ChainDB :> es
       , Log :> es
       , State HoardState :> es
       )
    => Eff es ()
consistencyCheck = do
    hoardStateTip <- (.immutableTip) <$> get @HoardState
    chainDBTip <- ChainDB.getImmutableTip
    let hoardSlot = case hoardStateTip of
            ChainPoint C.ChainPointAtGenesis -> Origin
            ChainPoint (C.ChainPoint slot _) -> At slot
        chainDBSlot = pointSlot chainDBTip
    if hoardSlot == chainDBSlot then
        Log.debug $ "ChainDB: immutable tip consistent: " <> show hoardSlot
    else
        Log.warn
            $ "ChainDB: immutable tip mismatch — HoardState: "
                <> show hoardSlot
                <> ", ChainDB: "
                <> show chainDBSlot
