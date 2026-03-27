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
connectToNode snocket makeSocketBearer connectArgs (\_ -> pure ()) versions Nothing addr
```

Switching to `'InitiatorResponderMode` also means all mini-protocols in the application must
use `InitiatorAndResponderProtocol` — the `RunMiniProtocol` GADT enforces this.
`InitiatorProtocolOnly` only type-checks for `'InitiatorMode` applications. The existing
BlockFetch, ChainSync, KeepAlive, and PeerSharing mini-protocols each need a stub responder
added:

```haskell
InitiatorAndResponderProtocol
    (mkMiniProtocolCbFromPeer $ \_ -> ... )  -- existing initiator, unchanged
    (MiniProtocolCb $ \_ _ -> pure ((), Nothing))  -- stub: (result, leftover bytes)
```

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
                (MiniProtocolCb $ \_ _ -> pure ((), Nothing))
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

## Caveats

- Peers are not obligated to use duplex mode even if we advertise it. Transaction coverage
  depends on how many connected peers choose to promote the connection. In practice,
  full-node relays that run the connection manager will promote duplex connections
  automatically via their outbound governor.
- This collects transactions visible to our upstream peers' mempools, not the full network
  mempool. Coverage improves with more peer connections.

## Dependencies

None. The infrastructure deployment is no longer a prerequisite.
