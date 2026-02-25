module Hoard.Events.KeepAlive
    ( Ping (..)
    ) where

import Data.Time (UTCTime)

import Hoard.Data.Peer (Peer)


data Ping = Ping
    { timestamp :: UTCTime
    , peer :: Peer
    }
    deriving (Show, Typeable)
