# Channels as event bus

We use `unagi-chan` channels as a means to communicate between collector
threads and worker threads. We do this using the `Sub` and `Pub` effects to
subscribe and publish new events, respectively. Each subscriber is handed a
separate `OutChan` that is duplicated from a given `InChan`.

## OutChan does not leak

When creating a new `InChan`, we also get out an `OutChan`. This `OutChan` must
either be used or dropped: if we leave it in memory, but never read from it,
the underlying stream backing the `InChan` and any `OutChan`s created from it
will accumulate messages and grow in size.

This can be mitigated with a bounded channel, if deemed necessary, but that
introduces potential blocking on the writing side.

Our options are as follows:

1. Prevent keeping the `OutChan` around, instead using `dupChan` to create new
   `OutChan`s from the `InChan` for each listener as we go. By not keeping a
   reference to `OutChan`, it will be garbage collected, and thus won't pose a
   problem.
2. `OutChan` for the first listener, and subsequently `dupChan` for all
   other listeners. This is more tedious to track, but doable.

For now we will use option 1.

## Arbitrary events

Since we want to emit any arbitrary event, the channels pass
`Data.Dynamic.Dynamic` values around. As such, any `Typeable` data can be sent
as a message.

`Pub` handles converting the published message to `Dynamic`.

`Sub` handles converting the messages to the subscriber's desired type, only
passing messages that can be converted into the desired type through.
