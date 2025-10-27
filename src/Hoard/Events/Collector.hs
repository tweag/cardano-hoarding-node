module Hoard.Events.Collector
    ( CollectorEvent (..)
    ) where

import Data.Typeable (Typeable)

import Hoard.Types.Collector (CollectorId, Peer)


data CollectorEvent
    = CollectorStarted CollectorId Peer
    | ConnectingToPeer CollectorId Peer
    | ConnectedToPeer CollectorId Peer
    | ConnectionFailed CollectorId Peer String
    | ChainSyncReceived CollectorId Peer
    | BlockFetchReceived CollectorId Peer
    deriving (Show, Typeable)
