module Hoard.Events.HeaderReceived
    ( HeaderReceived (..)
    , Header (..)
    )
where

import Data.Aeson (FromJSON, ToJSON)


-- | Event emitted when a header is received
data HeaderReceived = HeaderReceived
    { header :: Header
    }
    deriving (Eq, Show, Typeable)


data Header = Header
    { info :: Text
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)
