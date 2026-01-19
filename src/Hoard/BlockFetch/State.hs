module Hoard.BlockFetch.State
    ( Status (..)
    , BytesInFlight (..)
    , mkStatus
    , waitUntilReadyToFetch
    , registerRequest
    , unregisterRequest
    ) where

import Effectful (Eff, (:>))
import Effectful.Concurrent.MVar (Concurrent, newMVar, readMVar, tryPutMVar, tryTakeMVar)
import Effectful.State.Static.Shared (State, gets, stateM)
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common (estimateHfcBlockSize)
import Prelude hiding (State, gets, newMVar, readMVar, tryPutMVar, tryTakeMVar)

import Hoard.BlockFetch.Config (Config (..))
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Types.Cardano (CardanoHeader)


data Status = Status
    { stateVar :: MVar ()
    -- ^ Signals whether this collector is ready to issue block fetch requests
    -- to its connected peer.
    , bytesInFlight :: BytesInFlight
    }


mkStatus :: (Concurrent :> es) => Eff es Status
mkStatus = do
    stateVar <- newMVar ()
    pure
        Status
            { stateVar
            , bytesInFlight = 0
            }


newtype BytesInFlight = BytesInFlight Int
    deriving (Eq, Ord, Show)
    deriving (Num) via Int


waitUntilReadyToFetch :: (Concurrent :> es, State Status :> es) => Eff es ()
waitUntilReadyToFetch = do
    _ <- readMVar =<< gets (.stateVar)
    pure ()


-- | Registers a new batch of blocks being fetched with BlockFetch. Calculates
-- the estimated total size of the fetched blocks and adds it to the total of
-- bytes in flight. If the new total of bytes in flight exceed the high
-- watermark, the block fetch status is set to "Busy", awaiting the bytes in
-- flight count to drop below the low watermark.
registerRequest :: (Log :> es, Concurrent :> es, State Status :> es) => Config -> [CardanoHeader] -> Eff es ()
registerRequest config headers = do
    stateM \s ->
        let
            newBytes = fromIntegral . sum $ estimateHfcBlockSize <$> toList headers
            bytesInFlight = s.bytesInFlight + newBytes
        in
            do
                when (coerce bytesInFlight > config.highWatermark) $ do
                    x <- tryTakeMVar s.stateVar
                    when (isJust x) $
                        Log.debug "Entering busy state"
                pure ((), s {bytesInFlight})


-- | Removes the passed batch of blocks being fetched from the total count of
-- bytes in flight. Calculates the estimated total size of the fetched blocks
-- and removes it from the total of bytes in flight. If the new total of bytes
-- in flight is lower than the low watermark, the block fetch status is set to
-- "Ready", and block fetching may continue if it was previously "Busy".
unregisterRequest :: (Log :> es, Concurrent :> es, State Status :> es) => Config -> [CardanoHeader] -> Eff es ()
unregisterRequest config headers = do
    stateM \s ->
        let
            newBytes = fromIntegral . sum $ estimateHfcBlockSize <$> toList headers
            bytesInFlight = s.bytesInFlight - newBytes
        in
            do
                when (coerce bytesInFlight < config.lowWatermark) $ do
                    wasBusy <- tryPutMVar s.stateVar ()
                    when wasBusy $
                        Log.debug "Entering ready state"
                pure ((), s {bytesInFlight})
