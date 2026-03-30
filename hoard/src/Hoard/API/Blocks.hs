module Hoard.API.Blocks
    ( Routes (..)
    , API
    , handler
    )
where

import Servant (Get, JSON, NamedRoutes, QueryParam, QueryParams, Summary, (:-), type (:>))
import Servant.Server.Generic (AsServerT)

import Hoard.API.Data.Block (Block (..))
import Hoard.API.Data.BlockViolation (SlotDispute)
import Hoard.API.Util ((::>))
import Hoard.Data.BlockTag (BlockTag)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Types.SlotRange (SlotRange (..))
import Prelude hiding ((:>))

import Hoard.API.Data.Block qualified as Block
import Hoard.Effects.BlockRepo qualified as BlockRepo


data Routes mode = Routes
    { getBlocks
        :: mode
            :- Summary "Get blocks by slot range and tags. Returns blocks with at least one matching tag."
                :> QueryParam "minSlot" Int64
                :> QueryParam "maxSlot" Int64
                :> QueryParams "tags" BlockTag
                :> Get '[JSON] [Block]
    , getSlotDisputes
        :: mode
            :- "disputes"
                :> QueryParam "minSlot" Int64
                :> QueryParam "maxSlot" Int64
                :> Get '[JSON] [SlotDispute]
    }
    deriving stock (Generic)


type API = NamedRoutes Routes


handler :: (BlockRepo ::> es) => Routes (AsServerT (Eff es))
handler =
    Routes
        { getBlocks = \from to tags ->
            fmap Block.fromBlock <$> BlockRepo.getBlocks (SlotRange from to) tags
        , getSlotDisputes = BlockRepo.getSlotDisputesInRange
        }
