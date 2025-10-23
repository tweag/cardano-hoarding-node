module Hoard.Types.Collector
    ( CollectorId (..)
    , CollectorHandle (..)
    , Peer
    ) where


newtype CollectorId = CollectorId Int
    deriving (Eq, Ord, Show)


data CollectorHandle = CollectorHandle
    { cid :: CollectorId
    , peers :: [Peer]
    }
    deriving (Eq, Show)


type Peer = String
