module Hoard.OrphanDetection.Data
    ( BlockClassification (..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Rel8 (DBEq, DBType, ReadShow (..))
import Servant (FromHttpApiData (..), ToHttpApiData (..))

import Hoard.Effects.Monitoring.Tracing (ToAttribute, ToAttributeShow (..))
import Hoard.Types.JsonReadShow (JsonReadShow (..))


-- | Classification state for blocks
data BlockClassification
    = -- | Block is on the canonical chain
      Canonical
    | -- | Block is permanently orphaned (not on chain, before immutable tip)
      Orphaned
    deriving (Eq, Read, Show)
    deriving (FromJSON, ToJSON) via (JsonReadShow BlockClassification)
    deriving (DBEq, DBType) via (ReadShow BlockClassification)
    deriving (ToAttribute) via (ToAttributeShow BlockClassification)


-- | HTTP API instances for Servant
instance FromHttpApiData BlockClassification where
    parseUrlPiece "Canonical" = Right Canonical
    parseUrlPiece "Orphaned" = Right Orphaned
    parseUrlPiece t = Left $ "Invalid BlockClassification: " <> t


instance ToHttpApiData BlockClassification where
    toUrlPiece Canonical = "Canonical"
    toUrlPiece Orphaned = "Orphaned"
