module Hoard.Types.Collector
    ( CollectorId (..)
    , CollectorHandle (..)
    , Peer
    ) where


newtype CollectorId = CollectorId Int
    deriving (Eq, Ord, Show)


data CollectorHandle = CollectorHandle
    { cid :: CollectorId
    , peer :: Peer
    }
    deriving (Eq, Show)


type Peer = String
