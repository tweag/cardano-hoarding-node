module Hoard.Events.KeepAlive
    ( KeepAlivePing (..)
    ) where

import Data.Time (UTCTime)

import Hoard.Data.Peer (Peer)


data KeepAlivePing = KeepAlivePing
    { timestamp :: UTCTime
    , peer :: Peer
    }
    deriving (Show, Typeable)
