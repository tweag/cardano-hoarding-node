# Transaction Collection

## Motivation

The original proposal stated that transaction collection requires the hoarding node to accept
inbound connections — other peers submit transactions to it, not the other way around. This
turns out to be incorrect.

The ouroboros-network connection manager supports *duplex* connections: a single TCP connection
can carry mini-protocols in both directions simultaneously. When a node advertises
`InitiatorAndResponderDiffusionMode`, the remote peer's Inbound Protocol Governor will start
*responder* mini-protocols on the same connection the hoarding node opened as an outbound
connection. This is designed explicitly for nodes behind firewalls, documented in
§5 of the [Shelley Networking Protocol spec](https://ouroboros-network.cardano.intersectmbo.org/pdfs/network-spec).

TxSubmission2 role assignments in NtN connections make this directly applicable:

| Connection role | TxSubmission role | Meaning |
|---|---|---|
| Initiator (us, outbound) | Client | Submits our txs to the peer |
| **Responder (us, on a duplex connection)** | **Server** | **Receives txs from the peer** |

Because the hoarding node already establishes outbound connections for ChainSync and
BlockFetch, switching those connections to duplex mode is sufficient. The Inbound Protocol
Governor will then start our TxSubmission server on those connections, and peers that choose
to use the duplex path will push their mempool transactions to us — no listening socket or
infrastructure deployment required.

## Implementation

### Step 1 — Switch outbound connections to duplex mode (`NodeToNode.hs`)

Change `diffusionMode` in `versionData` and update `mkApplication`'s return type:

```haskell
-- In versionData:
diffusionMode = InitiatorAndResponderDiffusionMode   -- was: InitiatorOnlyDiffusionMode

-- Return type of mkApplication:
OuroborosApplicationWithMinimalCtx 'InitiatorResponderMode SockAddr LBS.ByteString IO () ()
-- was: 'InitiatorMode ... () Void
```

`connectTo` from `Ouroboros.Network.NodeToNode` is hardcoded to `'InitiatorMode` and cannot
accept the new application type. We must call `connectToNode` from `Ouroboros.Network.Socket`
directly, constructing `ConnectToArgs` manually with the same values `connectTo` used
internally:

```haskell
let connectArgs =
        ConnectToArgs
            { ctaHandshakeCodec      = nodeToNodeHandshakeCodec
            , ctaHandshakeTimeLimits = timeLimitsHandshake
            , ctaVersionDataCodec    = cborTermVersionDataCodec nodeToNodeCodecCBORTerm
            , ctaConnectTracers      = tracers
            , ctaHandshakeCallbacks  = HandshakeCallbacks acceptableVersion queryVersion
            }
connectToNode snocket makeSocketBearer connectArgs configureOutboundSocket versions Nothing addr
```

`configureOutboundSocket` replicates what the original `connectTo` did internally and must not
be omitted — the socket configuration callback in `connectToNode` is not optional:

```haskell
configureOutboundSocket :: Socket -> IO ()
configureOutboundSocket sock = do
    Socket.setSocketOption sock Socket.NoDelay 1
    Socket.setSockOpt sock Socket.Linger
        StructLinger{sl_onoff = 1, sl_linger = 0}
```

`TCP_NODELAY` is required for low-latency protocol framing; without it Nagle's algorithm
buffers small mux frames. `SO_LINGER (on, 0)` issues an immediate RST on close, preventing
TIME_WAIT exhaustion under high peer churn.

Switching to `'InitiatorResponderMode` also means all mini-protocols in the application must
use `InitiatorAndResponderProtocol` — the `RunMiniProtocol` GADT enforces this.
`InitiatorProtocolOnly` only type-checks for `'InitiatorMode` applications. The existing
BlockFetch, ChainSync, KeepAlive, and PeerSharing mini-protocols each need a stub responder
added. **The stub must drain the channel rather than return immediately** (see pitfalls):

```haskell
InitiatorAndResponderProtocol
    (mkMiniProtocolCbFromPeer $ \_ -> ... )    -- existing initiator, unchanged
    (MiniProtocolCb $ \_ channel ->            -- stub responder
        let drain = recv channel >>= maybe (pure ()) (\_ -> drain)
        in drain >> pure ((), Nothing))
```

`recv` is imported from `Network.Mux`.

`CardanoMiniProtocol` in `Hoard.Types.Cardano` is updated accordingly:

```haskell
type CardanoMiniProtocol =
    MiniProtocol 'InitiatorResponderMode   -- was: 'InitiatorMode
        (MinimalInitiatorContext SockAddr)
        (ResponderContext SockAddr)
        LBS.ByteString IO () ()            -- second () was: Void
```

### Step 2 — Define a `TxReceived` event

`src/Hoard/Events/TxSubmission.hs`:

```haskell
data TxReceived = TxReceived
    { peer :: Peer
    , tx   :: GenTx CardanoBlock   -- from Ouroboros.Consensus.Ledger.SupportsMempool
    }
```

(Not `CardanoGenTx` — that alias does not exist; the actual type is `GenTx CardanoBlock`.)

### Step 3 — Implement the TxSubmission server mini-protocol

`src/Hoard/Effects/NodeToNode/TxSubmission.hs`. Uses `InitiatorAndResponderProtocol` with a
stub initiator side (we are only a receiver), and `mkMiniProtocolCbFromPeerPipelined` for the
responder:

```haskell
miniProtocol unlift codecs peer =
    MiniProtocol
        { miniProtocolNum    = MiniProtocolNum 4  -- txSubmissionMiniProtocolNum not in project deps
        , miniProtocolLimits = MiniProtocolLimits maxBound
        , miniProtocolStart  = StartOnDemand
        , miniProtocolRun    =
            InitiatorAndResponderProtocol
                (MiniProtocolCb $ \_ channel ->
                    let drain = recv channel >>= maybe (pure ()) (\_ -> drain)
                    in drain >> pure ((), Nothing))
                (mkMiniProtocolCbFromPeerPipelined $ \_ ->
                    ( nullTracer
                    , cTxSubmission2Codec codecs
                    , txSubmissionServerPeerPipelined (receiveLoop unlift peer)
                    ))
        }
```

`receiveLoop` implements `TxSubmissionServerPipelined` using the blocking (non-pipelined)
variant. `NumTxIdsToAck` and `NumTxIdsToReq` are newtypes (not bare `Word16`) imported from
`Ouroboros.Network.Protocol.TxSubmission2.Type`. `SendMsgRequestTxsPipelined` takes `[txid]`
(not `Map txid SizeInBytes` as in a newer version of the library). `Z` is a data constructor
of `N`, imported via `Network.TypedProtocol.Core (N (..))`.

```haskell
receiveLoop unlift peer = TxSubmissionServerPipelined $ unlift do
    Log.debug $ "tx_submission: server started for peer=" <> show peer.address
    pure $ idle (NumTxIdsToAck 0)
  where
    idle ackN =
        SendMsgRequestTxIdsBlocking ackN (NumTxIdsToReq 10)
            (unlift $ Log.debug $ "tx_submission: session closed by peer=" <> show peer.address)
            (\txids -> unlift do
                Log.debug $ "tx_submission: " <> show (length txids) <> " txids advertised by peer=" <> show peer.address
                pure $ fetch (NumTxIdsToAck (fromIntegral (length txids))) (map fst (toList txids)))

    fetch ackN txids
        | null txids = idle ackN
        | otherwise =
            SendMsgRequestTxsPipelined txids $ pure $
                CollectPipelined Nothing $ \case
                    CollectTxs _ txs -> unlift do
                        Log.debug $ "tx_submission: " <> show (length txs) <> " txs received from peer=" <> show peer.address
                        for_ txs $ \tx -> do
                            Log.debug $ "tx_submission: new tx from peer=" <> show peer.address
                            publish TxReceived{peer, tx}
                        pure $ idle ackN
                    CollectTxIds _ _ -> pure $ idle ackN
```

### Step 4 — Wire into `mkApplication` and `Main.hs`

Add to the mini-protocol list in `mkApplication`:

```haskell
, NodeToNode.TxSubmission.miniProtocol unlift codecs peer
```

Add `Pub TxReceived` to both `runNodeToNode` and `mkApplication` effect constraints.

Add `runPubSub @TxSubmission.TxReceived` to the effect stack in `Main.hs`.

Don't forget to run `hpack` after adding new source files, since the project uses
`source-dirs: src` without an explicit module list.

## Implementation Pitfalls

### `simpleMuxCallback` ignores `miniProtocolStart`

`connectToNode` internally uses `simpleMuxCallback`, which schedules every mini-protocol with
`Mx.StartEagerly` regardless of the `miniProtocolStart` field in the `MiniProtocol` record.
This has an important consequence: a stub of the form `\_ _ -> pure ((), Nothing)` returns
immediately, completing its STM action. `simpleMuxCallback` uses `waitOnAny` — it terminates
the mux as soon as *any* scheduled operation completes. A single immediately-returning stub
therefore kills the entire connection within microseconds of the mux starting.

The `StartOnDemand` declaration on TxSubmission's initiator stub has no practical effect under
`simpleMuxCallback`. All stubs, regardless of their declared start mode, must block until the
channel closes. The drain loop achieves this:

```haskell
MiniProtocolCb $ \_ channel ->
    let drain = recv channel >>= maybe (pure ()) (\_ -> drain)
    in drain >> pure ((), Nothing)
```

This applies to every stub in the application — BlockFetch responder, ChainSync responder,
KeepAlive responder, PeerSharing responder, and TxSubmission initiator.

The symptom of a missing drain is 0 established connections with all peers permanently in
the pending state, cycling through rapid connect-fail-retry loops too fast for any protocol
to complete.

### `connectTo` socket configuration must be replicated

`connectTo` from `cardano-diffusion` sets `TCP_NODELAY` and `SO_LINGER` on the socket before
connecting. These are absent from `connectToNode`'s socket configuration callback if left as
`(\_ -> pure ())`. Always pass `configureOutboundSocket` explicitly.

## Caveats

- **Peer limits:** the remote peer's outbound governor controls which peers it promotes to
  hot (active) state. A saturated peer that has reached its `targetNumberOfActivePeers` limit
  (default ~5 in sync mode) will not run TxSubmission toward Hoard until another hot peer is
  demoted. Hoard's connection appears as an inbound duplex peer in the remote's
  `inboundDuplexPeers` set; the outbound governor decides if and when to promote it. Duplex
  connections count once toward the peer limit, not twice.
- Peers are not obligated to use duplex mode even if we advertise it. Transaction coverage
  depends on how many connected peers choose to promote the connection. In practice,
  full-node relays that run the connection manager will promote duplex connections
  automatically via their outbound governor.
- This collects transactions visible to our upstream peers' mempools, not the full network
  mempool. Coverage improves with more peer connections.

## Dependencies

None. The infrastructure deployment is no longer a prerequisite.
