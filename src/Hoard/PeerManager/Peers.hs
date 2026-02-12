module Hoard.PeerManager.Peers
    ( Peers (..)
    , Connection (..)
    , ConnectionState (..)
    , mkConnection
    , awaitTermination
    , signalTermination
    ) where

import Data.Default (Default (..))
import Data.Time (UTCTime)
import Effectful (Eff, (:>))
import Effectful.Concurrent.MVar (Concurrent, newEmptyMVar, takeMVar, tryPutMVar)
import Prelude hiding (newEmptyMVar, takeMVar, tryPutMVar)

import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer)
import Hoard.Effects.Clock (Clock)

import Hoard.Effects.Clock qualified as Clock


newtype Peers = Peers
    { peers :: Map (ID Peer) Connection
    }


instance Default Peers where
    def = Peers mempty


data Connection = Connection
    { terminator :: MVar ()
    -- ^ MVar used to signal to the collector when to terminate.
    , connectedAt :: UTCTime
    -- ^ Time of first connection attempt.
    , state :: ConnectionState
    -- ^ Current connection state for the collector.
    }


data ConnectionState
    = Connecting
    | Connected
    deriving (Eq)


-- | Creates a baseline `Connection` for a peer that just started attempting to
-- connect to a peer.
mkConnection :: (Clock :> es, Concurrent :> es) => Eff es Connection
mkConnection = do
    terminator <- newEmptyMVar
    connectedAt <- Clock.currentTime
    pure
        Connection
            { terminator
            , connectedAt
            , state = Connecting
            }


-- | Wait for another thread to signal that the collector should terminate.
awaitTermination :: (Concurrent :> es) => Connection -> Eff es ()
awaitTermination = takeMVar . (.terminator)


-- | Signals the corresponding collector to terminate. This function does not
-- block the calling thread even if another thread has signalled the
-- termination. Signalling more than once is effectively a noop.
signalTermination :: (Concurrent :> es) => Connection -> Eff es ()
signalTermination = void . (`tryPutMVar` ()) . (.terminator)
