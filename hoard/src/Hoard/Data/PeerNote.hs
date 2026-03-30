module Hoard.Data.PeerNote
    ( PeerNote (..)
    , NoteType (..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Rel8 (DBEq, DBType (typeInformation), ReadShow (..), TypeInformation (typeName), TypeName (name))

import Rel8 qualified

import Atelier.Types.JsonReadShow (JsonReadShow (..))
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer)


-- | A given note about a peer.
data PeerNote = PeerNote
    { id :: ID PeerNote
    , peerId :: ID Peer
    , noteType :: NoteType
    , note :: Text
    , notedAt :: UTCTime
    }


-- | Potential forms of notes about peers.
data NoteType
    = Adversarial
    deriving stock (Eq, Generic, Ord, Read, Show)
    deriving (FromJSON, ToJSON) via JsonReadShow NoteType


instance DBType NoteType where
    typeInformation =
        coerce
            (typeInformation @(ReadShow NoteType))
                { typeName = "note_type" & (overName . overSchema) (const $ Just "hoard")
                }
      where
        overName f a = a {name = f a.name}
        overSchema f a = a {Rel8.schema = f a.schema}


instance DBEq NoteType
