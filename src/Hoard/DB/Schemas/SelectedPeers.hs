module Hoard.DB.Schemas.SelectedPeers
    ( Row (..)
    , schema
    )
where

import Data.Time (UTCTime)
import Rel8 (Column, Name, Rel8able, Result, TableSchema)

import Hoard.DB.Schema (mkSchema)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer)


data Row f = Row
    { peerId :: Column f (ID Peer)
    , note :: Column f (Maybe Text)
    , addedAt :: Column f UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)


deriving instance Show (Row Result)


schema :: TableSchema (Row Name)
schema = mkSchema "selected_peers"
