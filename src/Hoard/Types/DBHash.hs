module Hoard.Types.DBHash
    ( DBHash (..)
    , HashKind (..)
    ) where

import Cardano.Crypto.Hash.Class ()
import Rel8 (DBEq, DBType)


newtype DBHash hashKind = DBHash {getDBHash :: ByteString}
    deriving (Eq, Ord, Show)
    deriving (DBType, DBEq) via ByteString


data HashKind where
    HashForHeader :: HashKind
    HashForBlock :: HashKind
