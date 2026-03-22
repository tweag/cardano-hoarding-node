# Multiplexed Protocol Channels (Mux)

## Related

- [distributed-mode.md](distributed-mode.md) — the distributed deployment this enables
- [keyed-pubsub.md](keyed-pubsub.md) — a parallel idea that applies the same routing concept locally

## Overview

Coordinator-collector communication uses the same `network-mux` /
`ouroboros-network-framework` stack that already handles Cardano mini-protocols
(ChainSync, BlockFetch, KeepAlive, PeerSharing). No new multiplexing abstractions
are needed — the pattern established in `NodeToNode/BlockFetch.hs` is the template.

`Atelier.Effects.Mux` (the custom mux effect written for the earlier design) is
superseded by this approach and can be removed.

## The Pattern

Each protocol follows the same three-layer structure:

**1. Protocol definition**

Messages are a GADT indexed by from/to states, declared inside a `Protocol` instance.
States encode agency — who is allowed to send next — at the type level:

```haskell
data PeerAssignmentProtocol  -- phantom type, used as the protocol kind

data StIdle  -- client's turn: may request a peer, return one, or finish
data StBusy  -- server's turn: must reply with an assignment or none
data StDone  -- terminal

instance Protocol PeerAssignmentProtocol where
    data Message PeerAssignmentProtocol from to where
        MsgWantPeer      :: Message PeerAssignmentProtocol StIdle StBusy
        MsgReturnPeer    :: Peer -> Message PeerAssignmentProtocol StIdle StIdle
        MsgAssignPeer    :: Peer -> Message PeerAssignmentProtocol StBusy StIdle
        MsgNoneAvailable :: Message PeerAssignmentProtocol StBusy StIdle
        MsgDone          :: Message PeerAssignmentProtocol StIdle StDone

    type StateAgency StIdle = ClientAgency
    type StateAgency StBusy = ServerAgency
    type StateAgency StDone = NobodyAgency

    -- StateToken instances omitted for brevity

peerAssignmentCodec :: Codec PeerAssignmentProtocol CBOR.DeserialiseFailure IO LBS.ByteString
```

**2. Client/server implementation**

The state machine is written using `Yield` (send), `Await` (receive), `Effect`
(embed a monadic action), and `Done` (terminate). `unlift` is the bridge that
lets effectful operations run inside the IO-based peer:

```haskell
-- Collector side: requests peers from the coordinator.
collectorClient
    :: ( Pub PeerAssigned :> es )
    => (forall x. Eff es x -> IO x)
    -> Client PeerAssignmentProtocol NonPipelined StIdle IO ()
collectorClient unlift =
    Yield MsgWantPeer $
    Await \case
        MsgAssignPeer peer ->
            Effect $ unlift $ do
                publish (PeerAssigned peer)
                pure $ collectorClient unlift
        MsgNoneAvailable ->
            Effect $ unlift $ do
                threadDelay retryDelayMicros
                pure $ collectorClient unlift
```

**3. The `miniProtocol` function**

Each protocol module exposes a single function returning a `MiniProtocol` value.
The client/server implementation is passed directly to `mkMiniProtocolCbFromPeer`:

```haskell
miniProtocol
    :: ( Pub PeerAssigned :> es )
    => PeerAssignmentConfig
    -> (forall x. Eff es x -> IO x)
    -> CollectorMiniProtocol
miniProtocol conf unlift =
    MiniProtocol
        { miniProtocolNum    = peerAssignmentMiniProtocolNum
        , miniProtocolLimits = MiniProtocolLimits conf.maximumIngressQueue
        , miniProtocolStart  = StartEagerly
        , miniProtocolRun    = InitiatorProtocolOnly $ mkMiniProtocolCbFromPeer $ \_ ->
            (nullTracer, peerAssignmentCodec, collectorClient unlift)
        }
```

## Composing Protocols

A `mkCollectorApplication` function (analogous to `mkApplication` in
`Hoard.Effects.NodeToNode`) assembles all protocols for a collector-coordinator
connection:

```haskell
mkCollectorApplication
    :: ( Pub PeerAssigned :> es
       , Sub PeerRequested :> es
       , Pub BlockForwarded :> es
       , ...
       , IOE :> es
       )
    => CollectorProtocolsConfig
    -> Eff es (OuroborosApplicationWithMinimalCtx 'InitiatorMode SockAddr LBS.ByteString IO () Void)
mkCollectorApplication conf =
    withEffToIO (ConcUnlift Persistent Unlimited) \unlift ->
        pure
            $ OuroborosApplication
            $ [ PeerAssignment.miniProtocol conf.peerAssignment unlift
              , BlockForward.miniProtocol   conf.blockForward   unlift
              , HeaderForward.miniProtocol  conf.headerForward  unlift
              , Advisory.miniProtocol       conf.advisory       unlift
              ]
```

The coordinator side mirrors this with swapped initiator/responder roles and its
own effect constraints.

## Interpreters

### Single-node mode

Both coordinator and collector run in the same process. `Network.Mux.Bearer.Queues`
provides an in-process STM bearer — no networking involved:

```haskell
(collectorBearer, coordinatorBearer) <- newQueueMuxBearer
Conc.fork_ $ runCoordinator coordinatorBearer
runCollector collectorBearer
```

This is the production interpreter for single-node mode and the natural choice for
integration tests.

### Distributed mode (collector)

`connectTo` from `ouroboros-network-framework` handles connection setup, the
Handshake mini-protocol (capability negotiation), and mux startup:

```haskell
connectTo
    (localSnocket iomgr)
    mempty
    (collectorVersions cfg)
    coordinatorAddr
```

Version data (the Handshake payload) is where supported protocol sets and any
node identity information are exchanged before application protocols start.

### Distributed mode (coordinator)

The coordinator's accept loop runs `withServerNode`, which calls a
`ConnectionHandler` per accepted connection — the same pattern used by the
Cardano node's diffusion layer.

## Protocol Numbers

Protocol numbers for collector-coordinator protocols are assigned in a dedicated
module, separate from the Cardano node-to-node protocol numbers already in use.
The Handshake mini-protocol negotiates the agreed set on each connection; numbers
only need to be consistent between builds, not globally coordinated.

## Testing

`Network.Mux.Bearer.Queues` makes unit testing straightforward: spin up both sides
in the same process, run protocols against each other with no network I/O. For
scenarios that require latency or packet loss simulation,
`Network.Mux.Bearer.AttenuatedChannel` provides a configurable channel with
configurable delay and loss.

## Connection Lifecycle

If the TCP connection drops, `connectTo` surfaces a connection error. The collector
continues running Cardano mini-protocols on its existing peer connections but cannot
acquire new peers or forward blocks until reconnected. Reconnection logic sits above
this layer and is out of scope here.

In single-node mode via the queues bearer, the connection cannot drop — both sides
share the same process lifetime.
