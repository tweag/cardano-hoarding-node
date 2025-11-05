module Hoard.Effects.Network.MiniProtocols (mkApplication, mkApplication') where

import Control.Tracer (nullTracer)
import Data.Time (getCurrentTime)
import Data.Typeable (Typeable)
import Data.Void (Void)
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Prim (Prim)
import Network.Mux (Mode (..), StartOnDemandOrEagerly (..))
import Network.Socket (SockAddr (..))
import Ouroboros.Network.Driver.Simple (runPeer)
import Ouroboros.Network.Mux
    ( MiniProtocol (..)
    , MiniProtocolCb (..)
    , MiniProtocolLimits (..)
    , OuroborosApplication (..)
    , OuroborosApplicationWithMinimalCtx
    , OuroborosBundle
    , RunMiniProtocol (..)
    )
import Ouroboros.Network.NodeToNode
    ( MiniProtocolParameters (..)
    , NodeToNodeProtocols (..)
    , NodeToNodeVersion (..)
    , NodeToNodeVersionData (..)
    , blockFetchMiniProtocolNum
    , chainSyncMiniProtocolNum
    , defaultMiniProtocolParameters
    , keepAliveMiniProtocolNum
    , nodeToNodeProtocols
    , peerSharingMiniProtocolNum
    , txSubmissionMiniProtocolNum
    )
import Ouroboros.Network.PeerSelection.PeerSharing.Codec (decodeRemoteAddress, encodeRemoteAddress)
import Ouroboros.Network.Protocol.PeerSharing.Client (PeerSharingClient (..), peerSharingClientPeer)
import Ouroboros.Network.Protocol.PeerSharing.Codec (codecPeerSharing)
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount (..))

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Debug.Trace qualified

import Effectful.Instances ()
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Pub (Pub, publish)
import Hoard.Network.Events
    ( PeerSharingEvent (..)
    , PeerSharingStartedData (..)
    , PeersReceivedData (..)
    )


--------------------------------------------------------------------------------
-- Mini-Protocol Application
--------------------------------------------------------------------------------

mkApplication
    :: (IOE :> es, Prim :> es, Pub :> es)
    => Peer
    -> NodeToNodeVersion
    -> NodeToNodeVersionData
    -> OuroborosBundle InitiatorMode initiatorCtx responderCtx LBS.ByteString (Eff es) () Void
mkApplication peer =
    nodeToNodeProtocols
        defaultMiniProtocolParameters
        ( NodeToNodeProtocols
            { chainSyncProtocol = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx _channel -> do
                -- Stub: For Ticket #1, just return immediately to allow connection to complete
                -- Real implementation in Ticket #3 will do actual protocol handshake
                liftIO $ putStrLn "[DEBUG] ChainSync protocol stub started"
                pure ((), Nothing)
            , blockFetchProtocol = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx _channel -> do
                -- Stub: For Ticket #1, just return immediately
                liftIO $ putStrLn "[DEBUG] BlockFetch protocol stub started"
                pure ((), Nothing)
            , txSubmissionProtocol = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx _channel -> do
                -- Stub: For Ticket #1, just return immediately
                liftIO $ putStrLn "[DEBUG] TxSubmission protocol stub started"
                pure ((), Nothing)
            , keepAliveProtocol = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx _channel -> do
                -- Stub: For Ticket #1, just return immediately
                liftIO $ putStrLn "[DEBUG] KeepAlive protocol stub started"
                pure ((), Nothing)
            , peerSharingProtocol = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx channel -> do
                -- Publish started event
                timestamp <- liftIO getCurrentTime
                publish $ PeerSharingStarted PeerSharingStartedData {peer, timestamp}
                liftIO $ putStrLn "[DEBUG] PeerSharing: Published PeerSharingStarted event"

                -- Create and run the PeerSharing client
                let client = peerSharingClientImpl peer
                let clientPeer = peerSharingClientPeer client
                    codec = codecPeerSharing (encodeRemoteAddress undefined) (decodeRemoteAddress undefined)

                liftIO $ putStrLn "[DEBUG] PeerSharing: About to run peer protocol..."
                -- Run the protocol
                _ <- runPeer nullTracer codec channel clientPeer
                liftIO $ putStrLn "[DEBUG] PeerSharing: Protocol completed"
                pure ((), Nothing)
            }
        )


--------------------------------------------------------------------------------
-- PeerSharing Protocol Implementation
--------------------------------------------------------------------------------

-- | Create a PeerSharing client that requests peer addresses.
--
-- This client:
-- 1. Requests up to 100 peer addresses from the remote peer
-- 2. Publishes a PeersReceived event with the results
-- 3. Terminates after one request
peerSharingClientImpl
    :: (IOE :> es, Pub :> es)
    => Peer
    -> PeerSharingClient SockAddr (Eff es) ()
peerSharingClientImpl peer =
    Debug.Trace.trace "[DEBUG] PeerSharing: Creating SendMsgShareRequest..."
        $ SendMsgShareRequest (PeerSharingAmount 100)
        $ \peerAddrs -> do
            liftIO $ putStrLn "[DEBUG] PeerSharing: *** CALLBACK EXECUTED - GOT RESPONSE ***"
            liftIO $ putStrLn $ "[DEBUG] PeerSharing: Received response with " <> show (length peerAddrs) <> " peers"
            timestamp <- liftIO getCurrentTime
            let peerAddrTexts = map (T.pack . show) peerAddrs
                peerCount = length peerAddrs
            publish
                $ PeersReceived
                    PeersReceivedData
                        { peer = peer
                        , peerAddresses = peerAddrTexts
                        , peerCount = peerCount
                        , timestamp = timestamp
                        }
            liftIO $ putStrLn "[DEBUG] PeerSharing: Published PeersReceived event"
            pure $ SendMsgDone (pure ())


-- | Create the Ouroboros application with all mini-protocols.
--
-- This bundles together ChainSync, BlockFetch, and KeepAlive protocols into
-- an application that runs over the multiplexed connection.
--
-- For Ticket #1, these are minimal stubs that just keep the connection alive.
-- Proper protocol implementations will be added in later tickets.
mkApplication'
    :: Peer
    -> (forall event. (Typeable event) => event -> IO ())
    -- ^ Publish event callback
    -> OuroborosApplicationWithMinimalCtx 'InitiatorMode SockAddr LBS.ByteString IO () Void
mkApplication' _peer _publishEvent =
    OuroborosApplication
        [ -- ChainSync mini-protocol (stub)
          MiniProtocol
            { miniProtocolNum = chainSyncMiniProtocolNum
            , miniProtocolLimits = chainSyncLimits
            , miniProtocolStart = StartOnDemand
            , miniProtocolRun = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx _channel -> do
                -- Stub: For Ticket #1, just return immediately to allow connection to complete
                -- Real implementation in Ticket #3 will do actual protocol handshake
                putStrLn "[DEBUG] ChainSync protocol stub started"
                pure ((), Nothing)
            }
        , -- BlockFetch mini-protocol (stub)
          MiniProtocol
            { miniProtocolNum = blockFetchMiniProtocolNum
            , miniProtocolLimits = blockFetchLimits
            , miniProtocolStart = StartOnDemand
            , miniProtocolRun = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx _channel -> do
                -- Stub: For Ticket #1, just return immediately
                putStrLn "[DEBUG] BlockFetch protocol stub started"
                pure ((), Nothing)
            }
        , -- KeepAlive mini-protocol (stub)
          MiniProtocol
            { miniProtocolNum = keepAliveMiniProtocolNum
            , miniProtocolLimits = keepAliveLimits
            , miniProtocolStart = StartOnDemand
            , miniProtocolRun = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx _channel -> do
                -- Stub: For Ticket #1, just return immediately
                putStrLn "[DEBUG] KeepAlive protocol stub started"
                pure ((), Nothing)
            }
        , -- PeerSharing mini-protocol (stub)
          MiniProtocol
            { miniProtocolNum = peerSharingMiniProtocolNum
            , miniProtocolLimits = peerSharingLimits
            , miniProtocolStart = StartOnDemand
            , miniProtocolRun = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx _channel -> do
                -- Stub: For Ticket #1, just return immediately
                putStrLn "[DEBUG] PeerSharing protocol stub started"
                pure ((), Nothing)
            }
        , -- TxSubmission mini-protocol (stub, not needed for hoarding)
          MiniProtocol
            { miniProtocolNum = txSubmissionMiniProtocolNum
            , miniProtocolLimits = txSubmissionLimits
            , miniProtocolStart = StartOnDemand
            , miniProtocolRun = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx _channel -> do
                -- Stub: For Ticket #1, just return immediately
                putStrLn "[DEBUG] TxSubmission protocol stub started"
                pure ((), Nothing)
            }
        ]
  where
    -- Get protocol parameters
    params = defaultMiniProtocolParameters

    -- Protocol limits from parameters
    -- Using chainSyncPipeliningHighMark as a reasonable default for all protocols
    chainSyncLimits =
        MiniProtocolLimits
            { maximumIngressQueue = fromIntegral $ chainSyncPipeliningHighMark params
            }
    blockFetchLimits =
        MiniProtocolLimits
            { maximumIngressQueue = fromIntegral $ blockFetchPipeliningMax params
            }
    keepAliveLimits =
        MiniProtocolLimits
            { maximumIngressQueue = 1000 -- Reasonable default for keep alive
            }
    peerSharingLimits =
        MiniProtocolLimits
            { maximumIngressQueue = 1000 -- Reasonable default for peer sharing
            }
    txSubmissionLimits =
        MiniProtocolLimits
            { maximumIngressQueue = fromIntegral $ txSubmissionMaxUnacked params
            }
