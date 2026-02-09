module Hoard.PeerSharing.Listeners
    ( peerSharingStarted
    , peerSharingFailed
    ) where

import Effectful (Eff, (:>))

import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.PeerSharing.Events (PeerSharingFailed (..), PeerSharingStarted (..))


-- | Listener that logs PeerSharing started events
peerSharingStarted :: (Log :> es) => PeerSharingStarted -> Eff es ()
peerSharingStarted event = do
    Log.info $ "🔍 PeerSharing protocol started at " <> show event.timestamp


-- | Listener that logs PeerSharing failed events
peerSharingFailed :: (Log :> es) => PeerSharingFailed -> Eff es ()
peerSharingFailed event = do
    Log.warn $ "❌ PeerSharing failed: " <> event.errorMessage
