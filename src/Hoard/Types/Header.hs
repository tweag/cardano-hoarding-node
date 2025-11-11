module Hoard.Types.Header
    ( Header (..)
    )
where

import Data.Aeson (FromJSON, ToJSON)


-- | Simple header type for testing events
data Header = Header
    { info :: Text
    }
    deriving (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)
