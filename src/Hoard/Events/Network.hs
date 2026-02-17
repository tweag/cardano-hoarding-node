module Hoard.Events.Network (ProtocolError (..)) where

import Data.Time (UTCTime)

import Hoard.Data.Peer (Peer)


--------------------------------------------------------------------------------
-- Network Lifecycle Events
--------------------------------------------------------------------------------

data ProtocolError = ProtocolError
    { peer :: Peer
    , timestamp :: UTCTime
    , errorMessage :: Text
    }
    deriving (Show, Typeable)
