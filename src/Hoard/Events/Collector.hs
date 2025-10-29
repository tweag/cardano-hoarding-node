module Hoard.Events.Collector
    ( CollectorEvent (..)
    ) where

import Data.Text (Text)
import Data.Typeable (Typeable)

import Hoard.Data.Peer (Peer)


data CollectorEvent
    = CollectorStarted Peer
    | ConnectingToPeer Peer
    | ConnectedToPeer Peer
    | ConnectionFailed Peer Text
    | ChainSyncReceived Peer
    | BlockFetchReceived Peer
    deriving (Show, Typeable)
