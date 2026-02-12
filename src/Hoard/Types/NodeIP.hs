module Hoard.Types.NodeIP
    ( NodeIP (..)
    ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.IP (IP)
import Rel8 (DBEq, DBType (..), ReadShow (..))

import Hoard.Types.JsonReadShow (JsonReadShow (..))


newtype NodeIP = NodeIP {getNodeIP :: IP}
    deriving stock (Eq, Generic, Ord)
    deriving (Read, Show) via (IP)
    deriving (FromJSON, ToJSON) via JsonReadShow NodeIP
    deriving (DBEq, DBType) via ReadShow NodeIP
