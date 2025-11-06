module Hoard.Effects.Network.Versions (unliftVersions) where

import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Instances ()
import Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolCb (..), OuroborosApplication (..), OuroborosApplicationWithMinimalCtx, RunMiniProtocol (..))
import Ouroboros.Network.NodeToNode (Versions (..))
import Ouroboros.Network.Protocol.Handshake.Version (Version (..))

import Data.Map.Strict qualified as M
import Ouroboros.Network.Channel (Channel (..))


-- | A more appropriate name for this might be a "natural transformation" from
-- `Eff es` to `IO`. The `unlift` function should be supplied by `withRunInIO`
-- or something along those lines to ensure the `Eff` environment is retained.
unliftVersions
    :: (IOE :> es)
    => (forall x. Eff es x -> IO x)
    -> Versions v vd (OuroborosApplicationWithMinimalCtx mode addr b (Eff es) a res)
    -> Versions v vd (OuroborosApplicationWithMinimalCtx mode addr b IO a res)
unliftVersions unlift (Versions versionMap) = Versions $ M.map (unliftVersion unlift) versionMap


unliftVersion
    :: (IOE :> es)
    => (forall x. Eff es x -> IO x)
    -> Version vData (OuroborosApplicationWithMinimalCtx mode addr c (Eff es) a b)
    -> Version vData (OuroborosApplicationWithMinimalCtx mode addr c IO a b)
unliftVersion unlift v =
    Version
        { versionData = v.versionData
        , versionApplication = \x ->
            let
                OuroborosApplication protocols = v.versionApplication x
            in
                OuroborosApplication $ unliftMiniProtocol unlift <$> protocols
        }


unliftMiniProtocol :: (IOE :> es) => (forall x. Eff es x -> IO x) -> MiniProtocol mode iCtx rCtx bytes (Eff es) a b -> MiniProtocol mode iCtx rCtx bytes IO a b
unliftMiniProtocol unlift prot =
    MiniProtocol
        { miniProtocolNum = prot.miniProtocolNum
        , miniProtocolStart = prot.miniProtocolStart
        , miniProtocolLimits = prot.miniProtocolLimits
        , miniProtocolRun = unliftRunMiniProtocol unlift prot.miniProtocolRun
        }


unliftRunMiniProtocol
    :: (IOE :> es)
    => (forall x. Eff es x -> IO x)
    -> RunMiniProtocol mode iCtx rCtx bytes (Eff es) a b
    -> RunMiniProtocol mode iCtx rCtx bytes IO a b
unliftRunMiniProtocol unlift = \case
    InitiatorProtocolOnly cb -> InitiatorProtocolOnly $ unliftMiniProtocolCb unlift cb
    ResponderProtocolOnly cb -> ResponderProtocolOnly $ unliftMiniProtocolCb unlift cb
    InitiatorAndResponderProtocol iCb rCb -> InitiatorAndResponderProtocol (unliftMiniProtocolCb unlift iCb) (unliftMiniProtocolCb unlift rCb)


unliftMiniProtocolCb
    :: (IOE :> es)
    => (forall x. Eff es x -> IO x)
    -> MiniProtocolCb ctx bytes (Eff es) a
    -> MiniProtocolCb ctx bytes IO a
unliftMiniProtocolCb unlift (MiniProtocolCb f) = MiniProtocolCb $ \ctx channel -> unlift $ f ctx (liftChannel channel)


liftChannel :: (IOE :> es) => Channel IO a -> Channel (Eff es) a
liftChannel (Channel send recv) = Channel (liftIO . send) (liftIO recv)
