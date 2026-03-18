module Hoard.Data.ID
    ( ID (..)
    )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import Rel8 (DBEq, DBOrd, DBType)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

import Hoard.Effects.Monitoring.Tracing (ToAttribute, ToAttributeShow (..))


-- | Phantom type around a UUID, to distinguish between different types of identifiers.
-- E.g. @ID Peer@ vs @ID Block@.
newtype ID a = ID UUID
    deriving stock (Generic)
    deriving newtype (DBEq, DBOrd, DBType, Eq, FromHttpApiData, FromJSON, Hashable, Ord, Show, ToHttpApiData, ToJSON)
    deriving (ToAttribute) via (ToAttributeShow (ID a))
