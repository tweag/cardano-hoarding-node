module Hoard.Events.Node (NodeDiscovered (..)) where

import Hoard.Data.Peer (PeerAddress)


newtype NodeDiscovered = NodeDiscovered PeerAddress
    deriving (Show, Typeable)
