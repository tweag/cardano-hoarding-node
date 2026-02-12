module Hoard.API.Violations
    ( ViolationsAPI
    , violationsHandler
    ) where

import Effectful (Eff)
import Servant (Get, JSON, QueryParam, type (:-), type (:>))

import Hoard.Data.BlockViolation (BlockViolation)
import Hoard.Effects ((::>))
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.OrphanDetection.Data (BlockClassification)

import Hoard.Effects.BlockRepo qualified as BlockRepo


-- | Violations API type
type ViolationsAPI mode =
    mode
        :- "violations"
            :> QueryParam "classification" BlockClassification
            :> QueryParam "minSlot" Int64
            :> QueryParam "maxSlot" Int64
            :> Get '[JSON] [BlockViolation]


-- | Handler for violations endpoint
violationsHandler :: (BlockRepo ::> es) => Maybe BlockClassification -> Maybe Int64 -> Maybe Int64 -> Eff es [BlockViolation]
violationsHandler = BlockRepo.getViolations
