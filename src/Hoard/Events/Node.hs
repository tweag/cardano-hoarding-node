module Hoard.Events.Node (NodeDiscovered (..)) where

import Data.Typeable (Typeable)

import Hoard.Data.Peer (Peer)


newtype NodeDiscovered = NodeDiscovered Peer
    deriving (Show, Typeable)
