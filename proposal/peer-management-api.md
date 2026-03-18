# Peer Management API

## Motivation

The hoarding node currently discovers and connects to peers through two mechanisms:

1. **Bootstrap snapshot** — on first start, seed peers from a ledger pool relay snapshot file
2. **Peer sharing** — while running, discover additional peers via the Cardano `PeerSharing` mini-protocol

Both mechanisms are fully automatic. There is no way to:

- Restrict the node to a fixed, operator-chosen set of peers (e.g., for private relay networks or trusted topologies)
- Inspect or modify the peer set at runtime without a restart

This proposal introduces a **Peer Management API** and extends `PeerManager.Config` with two orthogonal knobs that replace the old three-state `PeerMode`. A block list (useful in automatic mode) is left for a future proposal.

---

## Current State

- `PeerManager` maintains a map of active collectors (one per connected peer)
- Peer records are stored in the `peers` table and queried via `PeerRepo`
- `PeerRepo.GetEligiblePeers` filters out peers that failed recently (within `peerFailureCooldownSeconds`)
- No notion of an operator-pinned peer set exists today

---

## Data Model

### New table: `selected_peers`

| Column     | Type                         | Notes                        |
|------------|------------------------------|------------------------------|
| `peer_id`  | `UUID` (PK, FK → `peers.id`) | The pinned peer              |
| `note`     | `Maybe Text`                 | Optional operator annotation |
| `added_at` | `UTCTime`                    | When this entry was created  |

**Adding an unknown peer:**
When an operator submits an address not yet in `peers`, the peer is upserted first (with `discoveredVia = "pinned"`) before the pin entry is created. This keeps `selected_peers` a pure join table with no duplicated address data.

### Extended `PeerRepo` effect

Pinned peer operations are added to the existing `PeerRepo` effect to keep all peer-related persistence in one place.

New operations:

```haskell
-- Pinning management
GetPinnedPeers :: PeerRepo m [Peer]

-- Bulk-upserts addresses into peers, then bulk-inserts into selected_peers. Idempotent.
PinPeers
  :: UTCTime
  -> [(PeerAddress, Maybe Text)]  -- (address, optional note) pairs
  -> PeerRepo m [Peer]

-- Idempotent — silently succeeds if the peer is not found or not pinned.
UnpinPeer :: PeerAddress -> PeerRepo m ()

-- Replenishment query: joins peers with selected_peers, applies the same
-- cooldown and connection-exclusion filters as GetEligiblePeers.
GetEligiblePinnedPeers
  :: NominalDiffTime  -- failure cooldown
  -> Set (ID Peer)    -- currently connected peer IDs (to exclude)
  -> Word             -- limit
  -> PeerRepo m (Set Peer)
```

Both `PinPeers` and `UnpinPeer` perform their work in a single DB transaction with no per-peer round-trips.

---

## API

A new `/peers` route group is added to the existing `Routes` record in `Hoard.API`.

```
GET    /peers/pinned
POST   /peers/pinned
DELETE /peers/pinned
```

### Request / response shapes

**`GET /peers/pinned`**

Response: `[Peer]` (the existing peer type; pin metadata is stored in the table but not exposed).

```json
[
  {
    "id": "a1b2c3d4-...",
    "address": "192.0.2.1",
    "port": 3001,
    "first_discovered": "2024-01-01T00:00:00Z",
    "last_seen": "2024-01-02T00:00:00Z",
    "last_connected": "2024-01-02T00:00:00Z",
    "last_failure_time": null,
    "discovered_via": "pinned"
  }
]
```

**`POST /peers/pinned`**

Request body: a list of peers to pin. We don't take `(ID Peer)` for ergonomics — peers are upserted on the fly. Idempotent.

```json
[
  {"peer": {"host": "192.0.2.1", "port": 3001}, "note": "optional annotation"},
  {"peer": {"host": "192.0.2.2", "port": 3002}, "note": null}
]
```

Response: `[Peer]` (200 OK), one entry per submitted address in the same order returned by the DB upsert.

**`DELETE /peers/pinned`**

Request body: same structure as `POST` (identifies peers to unpin by address; `note` is ignored).

```json
[{"peer": {"host": "192.0.2.1", "port": 3001}, "note": null}]
```

Response: 204 No Content. Idempotent — silently succeeds if a peer is not found or not pinned.

---

## Peer Manager Configuration

`PeerManager.Config` gains two new fields that replace the old three-state `PeerMode`:

```haskell
data Config = Config
  { ...
  , peerMode        :: PeerMode  -- default: Automatic
  , discoverNewPeers :: Bool     -- default: True
  }

data PeerMode
  = Automatic
    -- ^ Connect to any eligible peer as well as any pinned peers.
  | Manual
    -- ^ Only connect to pinned peers; ignore all other discovered peers.
```

`PeerMode` serialises to/from JSON as `"automatic"` / `"manual"` via `QuietSnake`.

### Behaviour matrix

|                                        | `Automatic`    | `Manual`         |
|----------------------------------------|----------------|------------------|
| Peer sharing protocol runs             | if `discoverNewPeers = True` | if `discoverNewPeers = True` |
| Eligible peers sourced from            | `peers` (pinned first) | `selected_peers` only |
| Pinned peers always attempted first    | ✓              | ✓                |

The two knobs are orthogonal:

| Goal | Config |
|------|--------|
| Current behaviour (fully automatic) | `peerMode = Automatic`, `discoverNewPeers = True` |
| Connect only to pinned peers, still discover | `peerMode = Manual`, `discoverNewPeers = True` |
| Connect only to pinned peers, no discovery | `peerMode = Manual`, `discoverNewPeers = False` |
| Automatic connections, no discovery | `peerMode = Automatic`, `discoverNewPeers = False` |

### Replenishment

`replenishCollectors` always tries pinned peers first:

1. Fetch eligible pinned peers (`GetEligiblePinnedPeers`)
2. If `peerMode = Automatic` and slots remain, fetch additional eligible peers from the general pool (`GetEligiblePeers`), excluding already-handled peers

This ensures pinned peers are always preferred when both pools are available.

### Peer sharing

`discoverNewPeers` controls two things in `NodeToNode`:

- The `peerSharing` field in the version handshake data (`PeerSharingEnabled` / `PeerSharingDisabled`)
- Whether the `PeerSharing` mini-protocol is included in the mux application

When `discoverNewPeers = False` the remote peer is informed via the handshake that we won't participate in peer sharing, and the mini-protocol is not started.

### Bootstrap behaviour in Manual mode

On startup in `Manual` mode, if `selected_peers` is empty, the node falls back to the bootstrap snapshot to seed it — identical to the existing bootstrap flow, but entries land in `selected_peers` as well as `peers`. This ensures the node is not stranded with no pinned peers on first run.

If `selected_peers` is already populated, bootstrap is skipped.

---

## Implementation Plan

### New files

| File | Purpose |
|------|---------|
| `src/Hoard/DB/Schemas/SelectedPeers.hs` | Rel8 table schema for `selected_peers` |
| `src/Hoard/API/Peers.hs` | Servant route types, `PinPeerRequest`, and handlers |
| `src/Hoard/Effects/PeerRepo/State.hs` | In-memory state interpreter (for tests) |

### Modified files

| File | Change |
|------|--------|
| `src/Hoard/API.hs` | Add `peers :: PeersAPI mode` to `Routes` |
| `src/Hoard/DB/Schema.hs` | Register `selected_peers` table |
| `src/Hoard/Effects/PeerRepo.hs` | Add `GetPinnedPeers`, `PinPeers`, `UnpinPeer`, `GetEligiblePinnedPeers`; extract shared `applyEligibilityFilter` |
| `src/Hoard/PeerManager/Config.hs` | Replace three-state `PeerMode` with `Automatic \| Manual`; add `discoverNewPeers :: Bool` |
| `src/Hoard/PeerManager.hs` | Priority replenishment; mode-conditional bootstrap |
| `src/Hoard/Bootstrap.hs` | Extract `resolveBootstrapAddresses`; add `bootstrapPinnedPeers` using `PinPeers` |
| `src/Hoard/Effects/NodeToNode.hs` | Gate peer sharing mini-protocol and handshake flag on `discoverNewPeers` |
| `app/hoard/Main.hs` | No new effects to wire; `PeerRepo` and `PeerManager.Config` already in the stack |

---
