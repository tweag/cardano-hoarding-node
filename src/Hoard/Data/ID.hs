module Hoard.Data.ID
  ( ID (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Rel8 (DBEq, DBOrd, DBType)

-- | Phantom type around a UUID, to distinguish between different types of identifiers.
-- E.g. @ID Peer@ vs @ID Block@.
newtype ID a = ID UUID
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, DBType, DBEq, DBOrd)
  deriving stock (Generic)
