module Hoard.API.Violations
    ( ViolationsAPI
    , violationsHandler
    ) where

import Servant (Get, JSON, QueryParam, type (:-), type (:>))

import Hoard.API.Data.BlockViolation (SlotDispute)
import Hoard.Effects ((::>))
import Hoard.Effects.BlockRepo (BlockRepo)
import Prelude hiding ((:>))

import Hoard.Effects.BlockRepo qualified as BlockRepo


-- | Violations API type
type ViolationsAPI mode =
    mode
        :- "violations"
            :> QueryParam "minSlot" Int64
            :> QueryParam "maxSlot" Int64
            :> Get '[JSON] [SlotDispute]


-- | Handler for violations endpoint
violationsHandler :: (BlockRepo ::> es) => Maybe Int64 -> Maybe Int64 -> Eff es [SlotDispute]
violationsHandler = BlockRepo.getViolations
