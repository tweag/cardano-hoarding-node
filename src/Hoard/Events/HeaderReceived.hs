module Hoard.Events.HeaderReceived
    ( HeaderReceived (..)
    )
where

import Data.Typeable (Typeable)

import Hoard.Types.Header (Header)


-- | Event emitted when a header is received
data HeaderReceived = HeaderReceived
    { header :: Header
    }
    deriving (Eq, Show, Typeable)
