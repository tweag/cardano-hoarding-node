module Hoard.Effects.Network.Codecs (hoistCodecs) where

import Network.TypedProtocol.Codec (hoistCodec)
import Ouroboros.Consensus.Network.NodeToNode (Codecs (..))
import Prelude hiding (ByteString)


hoistCodecs
    :: (Functor n)
    => (forall a. m a -> n a)
    -> Codecs blk addr e m bCS bSCS bBF bSBF bTX bKA bPS
    -> Codecs blk addr e n bCS bSCS bBF bSBF bTX bKA bPS
hoistCodecs trans codecs =
    Codecs
        { cChainSyncCodec = hoistCodec trans codecs.cChainSyncCodec
        , cChainSyncCodecSerialised = hoistCodec trans codecs.cChainSyncCodecSerialised
        , cBlockFetchCodec = hoistCodec trans codecs.cBlockFetchCodec
        , cBlockFetchCodecSerialised = hoistCodec trans codecs.cBlockFetchCodecSerialised
        , cTxSubmission2Codec = hoistCodec trans codecs.cTxSubmission2Codec
        , cKeepAliveCodec = hoistCodec trans codecs.cKeepAliveCodec
        , cPeerSharingCodec = hoistCodec trans codecs.cPeerSharingCodec
        }
