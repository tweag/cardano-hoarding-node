module Hoard.OrphanDetection.Data
    ( BlockClassification (..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Rel8 (DBEq, DBType, ReadShow (..))
import Servant (FromHttpApiData (..), ToHttpApiData (..))

import Hoard.Types.JsonReadShow (JsonReadShow (..))


-- | Classification state for blocks
data BlockClassification
    = -- | Block is on the canonical chain
      Canonical
    | -- | Block is permanently orphaned (not on chain, past immutable tip)
      Orphaned
    deriving (Eq, Show, Read)
    deriving (DBType, DBEq) via (ReadShow BlockClassification)
    deriving (FromJSON, ToJSON) via (JsonReadShow BlockClassification)


-- | HTTP API instances for Servant
instance FromHttpApiData BlockClassification where
    parseUrlPiece "Canonical" = Right Canonical
    parseUrlPiece "Orphaned" = Right Orphaned
    parseUrlPiece t = Left $ "Invalid BlockClassification: " <> t


instance ToHttpApiData BlockClassification where
    toUrlPiece Canonical = "Canonical"
    toUrlPiece Orphaned = "Orphaned"
