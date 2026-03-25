# Keyed Pub/Sub

## Motivation

The current `Pub`/`Sub` effects broadcast every event to every subscriber for a given event
type. Subscribers that only care about events for a specific peer (or request, or any other
discriminator) receive everything and filter manually:

```haskell
listen_ \(BlockReceived peer block) ->
    when (peer == myPeer) $ processBlock block
```

This pattern scatters routing logic across listener bodies and delivers events to subscribers
that will discard them. With many per-peer listeners in flight this is harmless in practice,
but it obscures intent and makes the routing implicit.

## Proposal

Introduce a routing key as a type parameter on `Pub` and `Sub`:

```haskell
data Pub key event :: Effect where
    Publish :: key -> event -> Pub key event m ()

data Sub key event :: Effect where
    ListenOn :: key -> (UTCTime -> event -> m ()) -> Sub key event m Void
```

Routing moves from listener bodies into the interpreter, which maintains a
`HashMap key (Chan event)` and delivers events only to subscribers with a matching key.

The listener above becomes:

```haskell
listenOn myPeer \block -> processBlock block
```

For global broadcast events the key is `()`, which is equivalent to the current behaviour:

```haskell
-- These are the same concept:
publish () event      -- keyed, key = ()
listenOn () handler   -- receives all
```

## Routing Key Examples

Keyed pub/sub is most useful for intra-collector event routing, where multiple peer
connections run in the same process and events need to reach only the right one.

| Event type | Key type | Meaning |
|---|---|---|
| `BlockFetch.BlockReceived` | `Peer` | Route to the component managing that peer |
| `BlockFetch.Request` | `Peer` | Route fetch requests to the right peer connection |
| `ChainSync.HeaderReceived` | `Peer` | Route headers to the right block fetch client |

## Local Interpreter

```haskell
runPubSubKeyed
    :: forall key event es a
     . ( Hashable key
       , Chan :> es
       , Clock :> es
       , Tracing :> es
       )
    => Eff (Pub key event : Sub key event : es) a
    -> Eff es a
runPubSubKeyed action = do
    registry <- newTVarIO HashMap.empty

    let handlePub eff = interpretWith_ eff \case
            Publish key event -> do
                timestamp <- Clock.currentTime
                spanCtx   <- Tracing.getSpanContext
                atomically do
                    m <- readTVar registry
                    for_ (HashMap.lookup key m) \chan ->
                        Chan.writeChan chan TracedEvent{event, timestamp, spanCtx}

        handleSub eff = interpretWith eff \env -> \case
            ListenOn key listener -> localSeqUnlift env \unlift -> do
                chan <- Chan.newChan
                atomically $ modifyTVar registry (HashMap.insert key chan)
                forever do
                    TracedEvent{event, timestamp, spanCtx} <- Chan.readChan chan
                    Tracing.withLinkPropagation spanCtx $
                        unlift $ listener timestamp event

    handleSub . handlePub $ action
```

## Relation to Distributed Mode

Keyed pub/sub is a local concern — it improves intra-collector event routing within a
single process. Cross-collector coordination in distributed mode is handled by explicit
typed protocols over the `Mux` layer (see [mux.md](mux.md)), not by keyed pub/sub.

The two ideas share the same motivation (routing events to the right place) but operate
at different levels: keyed pub/sub routes within a process; the mux layer routes between
processes via distinct protocol channels.

## Migration

The `key` type parameter is a breaking change to `Pub` and `Sub`. The migration path:

1. Add `Pub key event` and `Sub key event` alongside the existing `Pub event` / `Sub event`
2. Migrate event types one at a time, starting with the noisiest per-peer listeners
3. Remove the old effects once all sites are migrated

For event types that remain global broadcasts, `key ~ ()` means the call sites change
minimally (`publish event` → `publish () event`). Type aliases and helper functions make
this read naturally:

```haskell
type Broadcast event = Pub () event
type Listener  event = Sub () event

broadcast :: (Broadcast event :> es) => event -> Eff es ()
broadcast = publish ()

listenAll :: (Listener event :> es) => (UTCTime -> event -> Eff es ()) -> Eff es Void
listenAll = listenOn ()
```

`runPubSub @CollectorRequested` in the application wiring would become
`runPubSub @() @CollectorRequested`, but with the aliases the constraint signatures stay
readable: `Broadcast CollectorRequested :> es` rather than `Pub () CollectorRequested :> es`.
