module Hoard.Events.Node (NodeDiscovered (..)) where

import Hoard.Data.Peer (Peer)


newtype NodeDiscovered = NodeDiscovered Peer
    deriving (Show, Typeable)
