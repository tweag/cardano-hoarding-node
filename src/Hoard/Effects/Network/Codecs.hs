module Hoard.Effects.Network.Codecs (defaultCodecs) where

import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding (Encoding)
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read (DeserialiseFailure)
import Data.ByteString.Lazy (ByteString)
import Data.Proxy (Proxy (..))
import Ouroboros.Consensus.Block (decodeRawHash, encodeRawHash)
import Ouroboros.Consensus.Network.NodeToNode (Codecs (..))
import Ouroboros.Consensus.Node.NetworkProtocolVersion (BlockNodeToNodeVersion, NodeToNodeVersion)
import Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints)
import Ouroboros.Consensus.Node.Serialisation (SerialiseNodeToNode, decodeNodeToNode, encodeNodeToNode)
import Ouroboros.Consensus.Shelley.Ledger.Config (CodecConfig)
import Ouroboros.Consensus.Util.IOLike (MonadST)
import Ouroboros.Network.Block (decodePoint, decodeTip, encodePoint, encodeTip)
import Ouroboros.Network.Protocol.BlockFetch.Codec (codecBlockFetch)
import Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSync)
import Ouroboros.Network.Protocol.KeepAlive.Codec (codecKeepAlive_v2)
import Ouroboros.Network.Protocol.PeerSharing.Codec (codecPeerSharing)
import Ouroboros.Network.Protocol.TxSubmission2.Codec (codecTxSubmission2)


-- | Protocol codecs for the node-to-node protocols
--
-- This is a direct copy of the original
-- `Ouroboros.Consensus.Network.NodeToNode.defaultCodecs` function which
-- originally includes the superfluous constraint `IOLike`, which we cannot
-- implement for non-IO monads.
defaultCodecs
    :: forall m blk addr
     . ( SerialiseNodeToNodeConstraints blk
       , MonadST m
       )
    => CodecConfig blk
    -> BlockNodeToNodeVersion blk
    -> (NodeToNodeVersion -> addr -> CBOR.Encoding)
    -> (NodeToNodeVersion -> forall s. CBOR.Decoder s addr)
    -> NodeToNodeVersion
    -> Codecs
        blk
        addr
        DeserialiseFailure
        m
        ByteString
        ByteString
        ByteString
        ByteString
        ByteString
        ByteString
        ByteString
defaultCodecs ccfg version encAddr decAddr nodeToNodeVersion =
    Codecs
        { cChainSyncCodec =
            codecChainSync
                enc
                dec
                (encodePoint (encodeRawHash p))
                (decodePoint (decodeRawHash p))
                (encodeTip (encodeRawHash p))
                (decodeTip (decodeRawHash p))
        , cChainSyncCodecSerialised =
            codecChainSync
                enc
                dec
                (encodePoint (encodeRawHash p))
                (decodePoint (decodeRawHash p))
                (encodeTip (encodeRawHash p))
                (decodeTip (decodeRawHash p))
        , cBlockFetchCodec =
            codecBlockFetch
                enc
                dec
                (encodePoint (encodeRawHash p))
                (decodePoint (decodeRawHash p))
        , cBlockFetchCodecSerialised =
            codecBlockFetch
                enc
                dec
                (encodePoint (encodeRawHash p))
                (decodePoint (decodeRawHash p))
        , cTxSubmission2Codec =
            codecTxSubmission2
                enc
                dec
                enc
                dec
        , cKeepAliveCodec = codecKeepAlive_v2
        , cPeerSharingCodec = codecPeerSharing (encAddr nodeToNodeVersion) (decAddr nodeToNodeVersion)
        }
  where
    p :: Proxy blk
    p = Proxy

    enc :: (SerialiseNodeToNode blk a) => a -> Encoding
    enc = encodeNodeToNode ccfg version

    dec :: (SerialiseNodeToNode blk a) => forall s. Decoder s a
    dec = decodeNodeToNode ccfg version
