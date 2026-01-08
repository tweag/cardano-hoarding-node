module Hoard.Collectors.Events (CollectorEvent (..)) where

import Hoard.Data.Peer (PeerAddress)


data CollectorEvent
    = CollectorStarted PeerAddress
    | ConnectingToPeer PeerAddress
    | ConnectedToPeer PeerAddress
    | ConnectionFailed PeerAddress Text
    | ChainSyncReceived PeerAddress
    | BlockFetchReceived PeerAddress
    deriving (Show, Typeable)
