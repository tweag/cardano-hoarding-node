module Hoard.Types.HoardState (HoardState (..)) where

import Data.Default (Default (..))

import Data.Set qualified as S

import Hoard.Data.Peer (PeerAddress)


-- | Application state
data HoardState = HoardState
    { connectedPeers :: Set PeerAddress
    }
    deriving (Eq, Show)


instance Default HoardState where
    def =
        HoardState
            { connectedPeers = S.empty
            }
