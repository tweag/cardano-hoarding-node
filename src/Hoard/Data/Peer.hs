module Hoard.Data.Peer
    ( Peer (..)
    , resolvePeerAddress
    )
where

import Control.Exception (SomeException (..), try)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Effectful (MonadIO (..))
import GHC.Generics (Generic)
import Network.Socket (AddrInfo (..), SockAddr, SocketType (..), defaultHints, getAddrInfo)

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as T

import Hoard.Data.ID (ID)


-- | Represents a peer in the P2P network
data Peer = Peer
    { id :: ID Peer
    , address :: Text
    , port :: Int
    , firstDiscovered :: UTCTime
    , lastSeen :: UTCTime
    , discoveredVia :: Text
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)


-- | Resolve peer address to socket address.
resolvePeerAddress :: (MonadIO m) => Peer -> m (Maybe SockAddr)
resolvePeerAddress peer = do
    liftIO
        $ either (const Nothing) Just
            <$> try @SomeException resolved
  where
    hints = defaultHints {addrSocketType = Stream}
    resolved =
        addrAddress . NonEmpty.head
            <$> getAddrInfo
                (Just hints)
                (Just $ T.unpack $ address peer)
                (Just $ show $ port peer)
