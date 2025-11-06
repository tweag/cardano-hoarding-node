module Hoard.Types.NodeIP
    ( NodeIP (..)
    ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.IP (IP)
import Hoard.Types.JsonReadShow (JsonReadShow (..))
import Rel8 (DBEq, DBType (..), ReadShow (..))


newtype NodeIP = NodeIP {getNodeIP :: IP}
    deriving stock (Eq, Ord, Generic)
    deriving (Show, Read) via (IP)
    deriving (DBType, DBEq) via ReadShow NodeIP
    deriving (FromJSON, ToJSON) via JsonReadShow NodeIP
