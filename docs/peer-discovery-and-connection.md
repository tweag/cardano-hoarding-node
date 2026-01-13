# Peer Discovery and Connection

This document describes how the Cardano Hoarding Node discovers and connects to peers in the Cardano network.

> **Note:** This documentation reflects the codebase as implemented on 2026-01-08. As the code evolves, some implementation details may change.

## Overview

The peer discovery system uses a multi-stage approach:

1. **Bootstrap**: Start with a hardcoded set of well-known peers from the configuration
2. **PeerSharing Protocol**: Request peer addresses from connected peers
3. **Recursive Discovery**: Connect to newly discovered peers and request more peers from them
4. **Failure Handling**: Track connection failures and implement retry cooldown periods

## Architecture Components

### Key Effects

| Effect       | Purpose                                                                   |
| ------------ | ------------------------------------------------------------------------- |
| `PeerRepo`   | Database operations for peer persistence (upsert, query, update failures) |
| `NodeToNode` | Network connection management and protocol execution                      |

### Core Modules

| Module                                    | Responsibility                                                          |
| ----------------------------------------- | ----------------------------------------------------------------------- |
| `Hoard.Bootstrap`                         | Initial peer bootstrapping from configuration                           |
| `Hoard.Collector`                         | Per-peer connection lifecycle management                                |
| `Hoard.Effects.NodeToNode`                | Network connection and protocol implementations (including PeerSharing) |
| `Hoard.Listeners.DiscoveredNodesListener` | Spawn collectors for newly discovered peers                             |
| `Hoard.Effects.PeerRepo`                  | Peer database operations                                                |

## Bootstrap Process

**File:** `src/Hoard/Bootstrap.hs`

On first startup (when no peers exist in the database), the node bootstraps from the configuration:

```haskell
bootstrapPeers :: (PeerRepo :> es, Clock :> es, IOE :> es, Reader Config :> es)
               => Eff es (Set Peer)
```

### Bootstrap Steps

1. **Extract relay addresses** from `bigLedgerPools` in the peer snapshot configuration
2. **Resolve domains/IPs** to concrete `PeerAddress` values:
   - For domain-based relays: resolve DNS to IP addresses
   - For IP-based relays: parse IP directly or resolve as hostname if parsing fails
3. **Persist to database** via `upsertPeers` with the first address as the bootstrap source
4. **Return peer set** for initial collector spawning

**Configuration Source:** The bootstrap configuration is provided through the `peerSnapshot` field in the application config, which contains `bigLedgerPools` - a list of well-known stake pool relays.

## Connection Lifecycle

### Collector Spawning

**File:** `src/Hoard/Collector.hs`

The `runCollectors` function is called at application startup:

```haskell
runCollectors :: (Collector :> es, Log :> es, PeerRepo :> es) => Eff es ()
```

**Logic:**

```
IF database has known peers THEN
    Spawn collector for each known peer
ELSE
    Bootstrap peers from configuration
    Spawn collector for each bootstrapped peer
END
```

### Bracket Pattern for Resource Management

**File:** `src/Hoard/Collector.hs:99-144`

Each peer connection is managed through `bracketCollector`:

```haskell
bracketCollector :: Peer -> Eff es ()
```

**Phases:**

1. **Acquire** (`src/Hoard/Collector.hs:102-115`):
   
   - Check if peer is already connected (tracked in `HoardState.connectedPeers`)
   - Check if peer is eligible for connection based on cooldown period
   - If eligible and not connected: add peer ID to `connectedPeers` state
   - If ineligible or already connected: skip

2. **Use** (`src/Hoard/Collector.hs:116-117`):
   
   - Execute `runCollector` to spawn Cardano mini-protocols
   - This runs until connection fails or is terminated

3. **Release** (`src/Hoard/Collector.hs:118-134`):
   
   - On exception: check if it's a collector exception
   - If it is: call `updatePeerFailure` to record failure timestamp
   - Remove peer ID from `connectedPeers` state
   - Publish `CollectorStopped` event

### Peer Eligibility and Retry Logic

**File:** `src/Hoard/Collector.hs`

```haskell
isPeerEligible :: UTCTime -> Peer -> Bool
isPeerEligible currentTime peer =
    case peer.lastFailureTime of
        Nothing -> True  -- Never failed, always eligible
        Just failureTime ->
            let timeSinceFailure = diffUTCTime currentTime failureTime
            in timeSinceFailure >= peerFailureCooldown
```

**Behavior:**

- Peers that have never failed are always eligible
- After a failure, peers must wait **5 minutes** (configurable) before being eligible for reconnection
- This prevents rapid retry loops and respects failing peers

## PeerSharing Protocol

**File:** `src/Hoard/Effects/NodeToNode.hs:481-505`

**Purpose:** Discover new peer addresses from connected peers

**Implementation:** Periodic request client

**Flow:**

1. Publish `PeerSharingStarted` event
2. Send `MsgShareRequest` asking for up to 100 peer addresses
3. Receive peer addresses from the remote peer
4. Filter valid addresses using `sockAddrToPeerAddress`
5. Publish `PeersReceived` event with discovered peer addresses
6. Wait **1 hour** (`oneHour = 3_600_000_000` microseconds)
7. Loop back to step 2

**Events:**

- `PeerSharingStarted`: Protocol initiated
- `PeersReceived`: Peer addresses received from remote peer

**Key Detail:** The protocol runs indefinitely in a loop, requesting new peers every hour. This ensures continuous peer discovery throughout the node's lifetime.

## Peer Discovery Event Flow

### Event: PeersReceived

**Published by:** PeerSharing protocol (`src/Hoard/Effects/NodeToNode.hs:493-499`)

**Event Data:**

```haskell
data PeersReceivedData = PeersReceivedData
    { peer :: Peer              -- The peer we requested from (source)
    , peerAddresses :: Set PeerAddress  -- Discovered peer addresses
    , timestamp :: UTCTime
    }
```

**Handled by:** `dispatchDiscoveredNodes` listener (`src/Hoard/Listeners/DiscoveredNodesListener.hs:25-57`)

### Discovery Flow

```
┌─────────────────────────────────────────────────────────────┐
│ PeerSharing Protocol (running in collector)                 │
│                                                             │
│ 1. Request 100 peers from remote peer                       │
│ 2. Receive SockAddr list                                    │
│ 3. Convert to PeerAddress                                   │
│ 4. Publish PeersReceived event                              │
└────────────────────┬────────────────────────────────────────┘
                     │
                     │ Event via Pub/Sub channel
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│ DiscoveredNodesListener.dispatchDiscoveredNodes             │
│                                                             │
│ 1. Receive PeersReceived event                              │
│ 2. Upsert peer addresses to database                        │
│    - New peers are inserted                                 │
│    - Existing peers are updated                             │
│    - Returns set of newly created peers                     │
│ 3. For each NEW peer: spawn bracketCollector                │
└────────────────────┬────────────────────────────────────────┘
                     │
                     │ For each new peer
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│ bracketCollector                                            │
│                                                             │
│ 1. Check eligibility (not in cooldown)                      │
│ 2. Add to connectedPeers state                              │
│ 3. Run collector → spawn all protocols                      │
│ 4. PeerSharing in new collector will discover MORE peers    │
│ 5. Recursive growth of peer network                         │
└─────────────────────────────────────────────────────────────┘
```

### Peer Persistence

**File:** `src/Hoard/Effects/PeerRepo.hs:97-117`

```haskell
upsertPeers :: Set PeerAddress -> PeerAddress -> UTCTime -> Eff es (Set Peer)
```

**Logic:**

For each peer address in the set:
  - Check if a peer with this address already exists in the database
  - If exists: updates the `lastSeen` timestamp
  - If does not exist: insert new peer record with:
    - `discoveredVia`: method of discovery
    - `lastFailureTime`: NULL (no failures yet)

## Failure Handling

### Recording Failures

**File:** `src/Hoard/Effects/PeerRepo.hs:155-170`

```haskell
updatePeerFailure :: Peer -> UTCTime -> Eff es ()
```

**Trigger:** Called from `bracketCollector` exit handler when a non-graceful exception occurs (`src/Hoard/Collector.hs:125-129`)

**Graceful Shutdown:** The code checks for specific exception types that might indicate intentional shutdown or a failure not pertaining to the collector process (e.g., `UserInterrupt`, `ThreadKilled`) and does NOT record these as failures.

### Cooldown Period

**Duration:** 5 minutes (configurable)

**Enforcement:** Before attempting connection in `bracketCollector`, `isPeerEligible` checks if sufficient time has passed since `lastFailureTime`.

**Benefits:**

- Prevents rapid reconnection attempts to failing peers
- Reduces network churn and resource usage
- Allows transient issues to resolve before retry
- Respects peers that may be experiencing problems

### Connection State Management

**State Location:** `HoardState.connectedPeers :: Set PeerId`

**Purpose:** Track which peers currently have active connections to prevent duplicate connection attempts

**Lifecycle:**

- **Add**: When `bracketCollector` successfully starts (`src/Hoard/Collector.hs:108-112`)
- **Remove**: In `bracketCollector` exit handler, regardless of success/failure (`src/Hoard/Collector.hs:135-142`)

**Thread Safety:** Uses `Effectful.State.Static.Shared.State` which provides thread-safe state management across concurrent collectors.

## Database Schema

### Peers Table

**File:** `src/Hoard/DB/Schemas/Peers.hs`

```haskell
data Row f = Row
    { id :: Column f (ID Peer)
    , address :: Column f NodeIP
    , port :: Column f Int32
    , firstDiscovered :: Column f UTCTime
    , lastSeen :: Column f UTCTime
    , lastConnected :: Column f (Maybe UTCTime)
    , lastFailureTime :: Column f (Maybe UTCTime)
    , discoveredVia :: Column f Text
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)
```

**Key Fields:**

- `id`: Unique peer identifier (UUID)
- `host`, `port`: Peer's network address
- `lastConnected`: Last time a successful connection to this peer happened
- `discoveredVia`,: Method of discovery (bootstrapping, peer sharing)
- `lastFailureTime`: Most recent connection failure timestamp (NULL = never failed or last attempt succeeded)

### Important Operations

**Upsert Peers:** Insert new peers, update `lastSeen` on existing ones by address
**Update Failure:** Set `lastFailureTime` to current timestamp
**Get All Peers:** Retrieve all known peers for collector spawning

## Performance Considerations

### Connection Limits

**Current Implementation:** No hard limit on concurrent connections

**State Tracking:** `HoardState.connectedPeers` tracks active peer IDs but doesn't enforce a maximum

### Cooldown Impact

**Benefit:** Prevents retry storms from failing peers
**Trade-off:** May slow peer network growth if many initial peers are unreachable
**Configuration:** Configurable (defaults to 5 minutes)

### PeerSharing Request Rate

**Current:** Requests 100 peers every 1 hour per connection
**Rationale:**

- Avoids overwhelming peers with requests
- Provides continuous peer discovery

**Growth Model:** With N connected peers, we discover up to 100 * N new addresses per hour (deduplicated by database)

### Database Deduplication

**Efficiency:** `upsertPeers` uses address-based uniqueness to prevent duplicate peer records

**Benefit:** Multiple peers may share the same address; database ensures we only store and connect once per unique address

## Summary

The peer discovery and connection system provides:

- **Automated Bootstrap**: Start from known relays (from the `peer-snapshot.json` configuration file)
- **Recursive Discovery**: Exponential peer network growth through PeerSharing protocol requesting 100 peers/hour from each connected peer
- **Failure Resilience**: Peers are only retried after cooldown period expires to prevent retry storms, automatic recovery after transient issues
- **Event-Driven Architecture**: Decoupled components communicate through typed pub/sub events
- **Database Persistence**: Peer network survives node restarts, builds over time

This architecture enables the Cardano Hoarding Node to autonomously discover and maintain connections to a large, diverse set of Cardano network peers for robust blockchain data collection.
