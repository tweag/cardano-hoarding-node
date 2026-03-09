module Hoard.Events.KeepAlive
    ( Ping (..)
    ) where

import Hoard.Data.Peer (Peer)


newtype Ping = Ping
    { peer :: Peer
    }
    deriving (Show, Typeable)
