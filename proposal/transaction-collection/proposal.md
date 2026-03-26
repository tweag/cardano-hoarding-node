# Transaction collection

## Related

- [proposal/embedded-consensus.md](../proposal/embedded-consensus.md) — the
  embedded ChainDB that classifies blocks and replaces the external Cardano node
- [proposal/embedded-consensus-hoard-design.md](../proposal/embedded-consensus-hoard-design.md) —
  implementation detail: how ChainDB maps onto Hoard's Component model

## Overview

To collect transactions from the Cardano network, Hoard will use the
`TxSubmission` mini-protocol described in section 3.9 of [the Ouroboros network
design technical report][network-spec].

Transactions are transmitted between nodes through the `TxSubmission`
mini-protocol. Hoard collects headers and blocks by making **outbound**
connections to peers and running as the initiator side of the Node-to-Node
mini-protocols. While information in the `ChainSync` and `BlockFetch`
mini-protocols flows from the server to the client, the `TxSubmission` protocol
flips this around, and instead the client publishes transactions to the server.

The initiator of a connection always runs `TxSubmission` as the _client_, but
it should be noted that the `TxSubmission` client behaves in a manner that
would normally be described as the "server" in this client-server relationship,
since information flows from the client to the server. The client pushes
transactions to the server, instead of the server serving transactions for the
client to request. We will keep consistent with [the
specification][network-spec]'s usage of "client" and "server" in this proposal.

To _receive_ transactions, Hoard needs to have a `TxSubmission` _server_,
instead of merely a _client_.

This proposal describes the requirements specific for the `TxSubmission` server
specific to Hoard.

## Background: NodeToNode servers vs clients

A node can either be in `InitiatorOnly` mode, where it only has mini-protocol
clients, `ResponderOnly` mode, where it only has mini-protocol servers, or in
`InitiatorAndResponder` mode, where it has both clients and servers. While in
`InitiatorOnly` mode, the node can implement as few clients
as it likes. `InitiatorAndResponder` mode, however, requires the node to
implement _all_ mini-protocol servers, in addition to the clients it wants.[^1]

This poses an additional challenge for us as Hoard then has to implement servers
for `KeepAlive`, `PeerSharing`, `BlockFetch` and `ChainSync`, in addition to
the `TxSubmission` server. Further thoughts and concerns regarding these
additional servers are expounded upon in the [Other mini-protocol
server](#other-mini-protocol-servers) section.

## Considerations

Certain considerations needs to be addressed due to how fundamentally different
`InitiatorAndResponder` mode is compared to `InitiatorOnly` mode.

### Connection lifetime

When connecting as a client through `InitiatorOnly` mode, it is uncommon for
nodes to choose to disconnect from the client, since resource-contentious
operations are governed by the server itself. `cardano-node` clients, however,
utilize multiple heuristics to promote or demote a given connection, ultimately
dropping the connection if the client deems it adversarial or uninteresting.
Clients also regularly rotate servers as part of normal "peer churn". A node
that has been rotated out due to peer churn is imposed a timeout period before
the client will attempt to connect to it again.

These lifecycle considerations will have to be addressed to ensure that Hoard
can gather an adequate number of transactions from a given peer before Hoard
will inevitably be disconnected due to peer churn.

To summarize succinctly: if Hoard is disconnected before the peer churn
interval, Hoard has exhibited adequately adversarial or sub-optimal behaviour
as a server.

### Other mini-protocol servers

There are 2 key aspects to the other mini-protocol servers to consider:

1. To allow other nodes to connect to Hoard's mini-protocol servers, Hoard has
   to implement _all_ mini-protocol servers. Clients will not connect to
   servers that do not implement all the mini-protocols they require, so to
   ensure as many clients will connect as possible Hoard has to implement all
   mini-protocol servers.
2. To ensure the connection remains open for as long as possible, Hoard needs
   to implement the other mini-protocol servers in a manner that satisfies
   `cardano-node`'s expectations for "interesting" and "non-adversarial" nodes.

#### ChainSync mini-protocol server

Of particular note among these other mini-protocol servers is the `ChainSync`
server. A normal `cardano-node` node is mainly motivated to extend its chain to
as much as possible. Hoard is instead motivated to collect as much information
from the connected nodes as possible. Due to this difference in motivation,
Hoard's implementation of the `ChainSync` server only has two priorities:

1. Ensure the client does not drop Hoard due to adverse or "uninteresting"
   behaviour.
2. Ensure the client sends as much information to Hoard as possible.

Hoard has little control over `2`, but for `1` Hoard only needs to implement
the minimum necessary behaviour to prevent the connected peer from
disconnecting.

It needs to reply with `MsgRollForward` and `MsgRollBackward` for
`MsgRequestNext` requests. `MsgRollForward` contains a `header` and a `tip`,
while `MsgRollBackward` contains a `point` and a `tip`. Replying with these
messages requires Hoard to know of a sensible `header`, `point` and/or `tip` to
reply with.

`ChainSync` is a stateful protocol that starts off at the genesis point of the
chain, and _only_ advances the _read-pointer_ one block at a time for each
`MsgRequestNext` message that is received, in the case of `MsgRollForward`.
`MsgFindIntersect` notably does _not_ move the read-pointer. Once the server
has reached the tip of its chain for a given client, it will return
`MsgAwaitReply` instead of a `MsgRollForward` or `MsgRollBackward`, signalling
that the client has to wait further for the server's chain to change.[^2]

It is then possible to implement Hoard's `ChainSync` server in such a way that
it will return all the headers for blocks between genesis and the immutable
tip, and then keep returning new immutable tips as the immutable tip
progresses. It is possible that `cardano-node` nodes might disconnect from a
server that serves an adequately old header as its last header before a
`MsgAwaitReply`, or if the replied header after `MsgAwaitReply` is an old
header itself.

### "Interesting" nodes

Usage of the term "interesting" in this text is meant to signify whether a
`cardano-node` node finds a given connection to be of value, and worth keeping,
instead of rotating it out of the pool of active connections in favour of
another potential connection that might be more "interesting". A node is
considered "interesting" based on the frequency with which it transmits
`ChainSync` messages and whether it has blocks to fetch through `BlockFetch`.

### Loopback

Once Hoard has a server implementation of the mini-protocols, it is possible
for Hoard's mini-protocol clients to connect to its own mini-protocol servers.
This could happen by not filtering peers discovered through `PeerSharing`, and
having an adversarial peer reply with either the loopback address or the public
IP of Hoard itself.

Measures should be taken to prevent this from happening, since it is not
helpful for Hoard to connect to itself.

## References

[^1]: [Network specification][network-spec]

[^2]: 3.7.6 "Implementation of the Chain Producer", page 24, [Network specification][network-spec]

[network-spec]: https://ouroboros-network.cardano.intersectmbo.org/pdfs/network-spec/network-spec.pdf
[network-design]: https://ouroboros-network.cardano.intersectmbo.org/pdfs/network-design/network-design.pdf
