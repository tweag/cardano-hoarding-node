module Hoard.Events.Collector
    ( CollectorEvent (..)
    ) where

import Data.Typeable (Typeable)

import Hoard.Types.Collector (Peer)


data CollectorEvent
    = CollectorStarted Peer
    | ConnectingToPeer Peer
    | ConnectedToPeer Peer
    | ConnectionFailed Peer String
    | ChainSyncReceived Peer
    | BlockFetchReceived Peer
    deriving (Show, Typeable)
