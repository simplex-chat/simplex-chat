# Test Chat Relay — Context Notes

## Task Summary

Implement `APITestChatRelay` command that tests relay liveness, retrieves relay profile, and stores it on the configuration record. Also remove `name` from relay config, replace with profile from relay.

## Plan Summary

`APITestChatRelay userId chatRelayId`:

1. Fetch relay's short link data via `getShortLinkConnReq` on relay's configured address
2. This validates: SMP server reachability, link data cryptographic integrity, link existence
3. Decode `RelayAddressData` from link's `userData` — contains relay profile (can later add identity key, capabilities)
4. Save relay profile on `chat_relays` config record, set `tested = True` (or `False` with error)
5. When later adding relay to channel, `addRelay` uses saved profile instead of `profileFromName name`

**Changes:**
- New type `RelayAddressData {relayProfile :: Profile}` in Protocol.hs (parallel to `RelayShortLinkData`, can diverge)
- Relay address creation: use `RelayAddressData` instead of `ContactShortLinkData`
- Migration: add `contact_profile_id` to `chat_relays`, drop `name`
- Preset relays: include profile directly (like preset contact cards)
- Extend `UserChatRelay` type: remove `name`, add optional profile
- Simplify `CLINewRelay` to address only
- Implement `APITestChatRelay` command + `CRChatRelayTestResult` response
- Update `addRelay` to use profile from config record
- Rework relay configuration views (iOS, Android/Desktop) to account for name removal

**Limitations:** Tests SMP connectivity AND relay liveness via XGrpRelayTest handshake.

## Liveness Test Protocol — XGrpRelayTest

Single new protocol message `XGrpRelayTest` used in BOTH directions (meaning depends on context).

### Connection Flow (same as XGrpRelayInv/XGrpRelayAcpt)

1. Owner calls `getShortLinkConnReq` on relay address → gets `FixedLinkData` with `connReq`
2. Owner calls `joinConnection` with `XGrpRelayTest` as connInfo (same flow point as `XGrpRelayInv`)
3. Relay receives `REQ` with `XGrpRelayTest` in connInfo (same handler location as `XGrpRelayInv` — Subscriber.hs:1247)
4. Relay accepts request, sends `XGrpRelayTest` back as connInfo with profile (same flow point as `XGrpRelayAcpt`)
5. Owner receives `CONF` with `XGrpRelayTest` — test passed, profile obtained
6. Both sides delete the connection

### Blocking Pattern

- Owner API blocks on TMVar
- Subscriber puts result into TMVar when CONF with XGrpRelayTest arrives
- Connection deleted on both sides after test
- Cleanup of stale test connections on app crash/restart

## Key Code Locations

### Protocol (XGrpRelayInv/XGrpRelayAcpt as reference)
- `src/Simplex/Chat/Protocol.hs:437` — `XGrpRelayInv` / `XGrpRelayAcpt` constructors
- `src/Simplex/Chat/Protocol.hs:965` — event tags `XGrpRelayInv_` / `XGrpRelayAcpt_`
- `src/Simplex/Chat/Protocol.hs:1021` — tag strings "x.grp.relay.inv" / "x.grp.relay.acpt"
- `src/Simplex/Chat/Protocol.hs:1078` — tag parsing
- `src/Simplex/Chat/Protocol.hs:1131` — event to tag mapping
- `src/Simplex/Chat/Protocol.hs:1283` — JSON parsing
- `src/Simplex/Chat/Protocol.hs:1350` — JSON encoding

### Owner sends XGrpRelayInv (reference for sending XGrpRelayTest)
- `src/Simplex/Chat/Library/Commands.hs:3743-3780` — `addRelays`/`addRelay` function
  - Line 3752: `getShortLinkConnReq` to fetch relay's connReq
  - Line 3759: `prepareConnectionToJoin`
  - Line 3775: `encodeConnInfo $ XGrpRelayInv relayInv` → connInfo
  - Line 3776: `joinConnection` with connInfo

### Relay receives XGrpRelayInv (reference for receiving XGrpRelayTest)
- `src/Simplex/Chat/Library/Subscriber.hs:1247` — REQ handler: `XGrpRelayInv groupRelayInv -> xGrpRelayInv invId chatVRange groupRelayInv`
- `src/Simplex/Chat/Library/Subscriber.hs:1450-1453` — `xGrpRelayInv` function: creates relay request group, starts worker
- `src/Simplex/Chat/Library/Subscriber.hs:3568-3677` — relay request worker processes the invitation:
  - Fetches group link data, validates profile/signature
  - Creates relay link
  - Accepts owner connection via `acceptRelayJoinRequestAsync`

### Owner receives relay response (CONF handler — reference for receiving XGrpRelayTest response)
- Need to find where CONF is handled for the relay member connection
- The relay sends `XGrpRelayAcpt relayLink` as connInfo when accepting

### Short Link Data Types
- `src/Simplex/Chat/Protocol.hs:1415` — `ContactShortLinkData {profile, message, business}`
- `src/Simplex/Chat/Protocol.hs:1439` — `RelayShortLinkData {relayProfile}` (for per-group relay links)
- New: `RelayAddressData {relayProfile}` (for relay's contact address)

### Relay Address Creation
- `src/Simplex/Chat/Library/Commands.hs:2162-2176` — `APICreateMyAddress` for relay users
  - Line 2168: `-- TODO [relays] relay: add relay profile, identity, key to link data?`
  - Line 2169: currently uses `contactShortLinkData` — should use `RelayAddressData`

### UserChatRelay Type
- `src/Simplex/Chat/Operators.hs:263-273` — `UserChatRelay` type: `chatRelayId, address, name, domains, preset, tested, enabled, deleted`
- `src/Simplex/Chat/Operators.hs:277-284` — `GroupRelay` type (per-channel relay record)
- `src/Simplex/Chat/Operators.hs:287-291` — `CLINewRelay` type: `address, name`

### DB Schema
- `chat_relays` table: `chat_relay_id, address, name, domains, preset, tested, enabled, user_id, deleted`
- `group_relays` table: `group_relay_id, group_id, group_member_id, chat_relay_id, relay_status, relay_link, conf_id`
- Migration file: `src/Simplex/Chat/Store/SQLite/Migrations/M20260222_chat_relays.hs`

### Store Operations
- `src/Simplex/Chat/Store/Groups.hs:1333` — `createRelayForOwner`: uses `profileFromName name` (should use cached profile)
- `src/Simplex/Chat/Store/Profiles.hs` — relay DB operations (get/set chat relays)

### Views (need rework for name removal)
- `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/networkAndServers/ChatRelayView.kt`
- `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/newchat/AddChannelView.kt`
- `apps/ios/Shared/Views/UserSettings/NetworkAndServers/ChatRelayView.swift`
- `apps/ios/Shared/Views/NewChat/AddChannelView.swift`

### Server Test Reference (testProtocolServer pattern)
- `simplexmq/src/Simplex/Messaging/Agent.hs:549` — `testProtocolServer` function
- `simplexmq/src/Simplex/Messaging/Agent/Client.hs:1231` — `runSMPServerTest` implementation
- `src/Simplex/Chat/Library/Commands.hs:1488` — `APITestProtoServer` handler in chat

### Existing TODO Comments
- `Commands.hs:3749-3751` — "TODO [relays] owner: track and reuse relay profiles"
- `Commands.hs:2168` — "TODO [relays] relay: add relay profile, identity, key to link data?"

### FixedLinkData (from simplexmq master)
- `simplexmq/src/Simplex/Messaging/Agent/Protocol.hs:1731-1736`:
  ```
  data FixedLinkData c = FixedLinkData
    { agentVRange :: VersionRangeSMPA,
      rootKey :: C.PublicKeyEd25519,
      linkConnReq :: ConnectionRequestUri c,
      linkEntityId :: Maybe ByteString
    }
  ```

### Important Design Decisions
- `name` field removed from `UserChatRelay` — relay's profile `displayName` serves as identity
- Before testing, show address/domain as placeholder in UI
- Preset relays can include profile directly (like preset contact cards)
- `RelayAddressData` is separate from `ContactShortLinkData` and `RelayShortLinkData` — can evolve independently with relay identity key, capabilities, etc.
