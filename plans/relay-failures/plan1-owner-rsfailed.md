# Plan 1: Owner-Side — Add RSFailed to RelayStatus

## Problem Statement

After a channel owner creates a channel, relay connections progress asynchronously through RSNew → RSInvited → RSAccepted → RSActive. If a relay connection permanently fails after `joinConnection` (e.g. agent ERR with AUTH error), the relay stays at RSInvited or RSAccepted forever — indistinguishable from "still connecting." The owner sees a perpetual orange indicator with no way to know the relay has permanently failed.

**Note:** Initial channel creation (`addRelays` in Commands.hs:3689) uses `mapConcurrently` — if ANY relay fails during creation, the entire group is deleted and an error is thrown. Partial failure during creation is impossible. This plan addresses **post-creation async failures** only.

## Solution Summary

Add `RSFailed` case to `RelayStatus`. When a permanent ERR event arrives for a relay member connection on the owner side (Subscriber.hs:1121), set the relay's status to `RSFailed` and emit `CEvtGroupLinkRelaysUpdated` so iOS receives the updated status. UI shows red "Failed" indicator.

## Detailed Technical Design

### Data Flow

```
Owner creates channel → addRelays (Commands.hs:3689) → all relays RSInvited
  → async: relay sends CONF with XGrpRelayAcpt → RSAccepted (Subscriber.hs:738)
  → async: relay CON → setGroupLinkDataAsync → LINK → RSActive (Subscriber.hs:830,1230)

Failure path (NEW):
  → async: agent delivers ERR for relay member connection (Subscriber.hs:1121)
  → check: isRelay m && owner && permanent error
  → update relay to RSFailed in DB
  → emit CEvtGroupLinkRelaysUpdated
  → iOS: ChannelRelaysModel.update() → UI shows red "Failed"
```

### What Changes

| Layer | File | Change |
|-------|------|--------|
| Haskell types | Types/Shared.hs:78-83 | Add `RSFailed` case |
| Haskell encoding | Types/Shared.hs:85-107 | Add "failed" to `relayStatusText`, `textEncode`, `textDecode` |
| Haskell logic | Library/Subscriber.hs:1121-1123 | Handle ERR for relay members on owner side |
| Haskell store | Store/Groups.hs | `updateRelayStatus` already generic — no change needed |
| Swift types | ChatTypes.swift:2505-2510 | Add `.rsFailed = "failed"` case |
| Swift text | ChatTypes.swift:2563-2572 | Add `case .rsFailed: "Failed"` |
| Swift UI | AddChannelView.swift:362 | Update color ternary for `.rsFailed` |
| Swift UI | ComposeView.swift:844 | Same color change |

### Key Design Decisions

**When to set RSFailed**: On ERR event in `processGroupMessage` (Subscriber.hs:1121) where:
1. Local user is owner (`memberRole' membership == GROwner`)
2. Remote member is a relay (`isRelay m`)
3. Error is permanent (`not (temporaryOrHostError err)`)

The agent's async command worker already retries temporary errors automatically — only permanent errors are delivered as ERR. `temporaryOrHostError` (already imported at Subscriber.hs:76) classifies NETWORK, TIMEOUT, HOST as temporary. Permanent errors include AUTH, CONN NOT_ACCEPTED, INTERNAL, etc.

**Event emission**: `CEvtGroupLinkRelaysUpdated` is already used in Subscriber.hs (line 1230) for the LINK event. Same pattern reused here. All required store functions (`getGroupRelayByGMId`, `updateRelayStatus`, `getGroupRelays`, `getGroupLink`) are already imported and used elsewhere in Subscriber.hs.

**Scope of variables at ERR handler** (line 1121): `user@User{userId}` bound at line 353, `gInfo@GroupInfo{..., membership, ...}` and `m` bound at line 678. All in scope.

## Detailed Implementation Steps

### Step 1: Haskell — Add RSFailed to RelayStatus

**File:** `src/Simplex/Chat/Types/Shared.hs`

Add `RSFailed` after `RSActive` (line 82):
```haskell
data RelayStatus
  = RSNew
  | RSInvited
  | RSAccepted
  | RSActive
  | RSFailed
  deriving (Eq, Show)
```

Update `relayStatusText` (line 86-90): add `RSFailed -> "failed"`
Update `textEncode` (line 93-97): add `RSFailed -> "failed"`
Update `textDecode` (line 98-103): add `"failed" -> Just RSFailed`

### Step 2: Haskell — Handle ERR for relay members

**File:** `src/Simplex/Chat/Library/Subscriber.hs` (lines 1121-1123)

Current:
```haskell
ERR err -> do
  eToView $ ChatErrorAgent err (AgentConnId agentConnId) (Just connEntity)
  when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
```

Change to:
```haskell
ERR err -> do
  eToView $ ChatErrorAgent err (AgentConnId agentConnId) (Just connEntity)
  when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
  when (memberRole' membership == GROwner && isRelay m && not (temporaryOrHostError err)) $ do
    relay <- withStore $ \db -> getGroupRelayByGMId db (groupMemberId' m)
    void $ withStore' $ \db -> updateRelayStatus db relay RSFailed
    gLink <- withStore $ \db -> getGroupLink db user gInfo
    updatedRelays <- withStore $ \db -> liftIO $ getGroupRelays db gInfo
    toView $ CEvtGroupLinkRelaysUpdated user gInfo gLink updatedRelays
```

**Why this location**: The ERR event is the agent's signal that a command permanently failed (temporary errors are retried by the agent's async command worker). After `joinConnection` succeeds (Commands.hs:3722), the connection lifecycle continues asynchronously — CONF, CON events arrive later. If any of these fail permanently, ERR is delivered here.

**Pattern precedent**: The CON handler at line 830 already uses `isRelay m` to branch relay-specific logic (update member status, trigger `setGroupLinkDataAsync`). The CONF handler at line 738 uses `memberRole' membership == GROwner && isRelay m` — same guard.

### Step 3: Swift — Add .rsFailed

**File:** `SimpleXChat/ChatTypes.swift`

Add case to enum (line 2509):
```swift
case rsFailed = "failed"
```

Add to `text` property (line 2563-2572) — exhaustive switch, compiler-enforced:
```swift
case .rsFailed: "Failed"
```

### Step 4: Swift UI — Update relay status indicators

**AddChannelView.swift** `relayStatusIndicator` (line 362):
Current: `.fill(status == .rsActive ? .green : status == .rsNew ? .red : .orange)`
Change to: `.fill(status == .rsActive ? .green : (status == .rsFailed || status == .rsNew) ? .red : .orange)`

**ComposeView.swift** `relayStatusIndicator` (line 844):
Same color change.

**AddChannelView progress step** (line 232): `activeCount` already counts `.rsActive` only — failed relays won't count as active. The "Channel link" button requires `activeCount > 0`, so partial failure still allows proceeding.

### Step 5: Verify no other RelayStatus pattern matches

`relayStatusText` and `textEncode`/`textDecode` in Shared.hs are exhaustive — compiler-enforced. Swift `text` property is exhaustive — compiler-enforced. All other RelayStatus usage is `==` comparisons — no changes needed.

## Complexity Assessment

| Aspect | Count |
|--------|-------|
| Haskell files to modify | 2 (Types/Shared.hs, Subscriber.hs) |
| Swift files to modify | 3 (ChatTypes.swift, AddChannelView.swift, ComposeView.swift) |
| New functions | 0 |
| Pattern matches to update | 3 Haskell (relayStatusText, textEncode, textDecode) + 1 Swift (text) |
| DB migration | None (relay_status stored as TEXT in group_relays) |

## Test Plan

1. After `joinConnection` succeeds in `addRelay` (Commands.hs:3722), simulate a permanent ERR by having the relay reject the connection
2. Create channel with 3 relays
3. Verify: progress step shows 2 relays progressing to active, 1 stays at RSInvited then transitions to red "Failed"
4. Verify: ComposeView owner relay bar shows failed relay with red indicator and "Failed" text
5. Verify: ChannelRelaysView shows "Failed" for broken relay
6. Remove test break, verify normal flow unaffected

## Limitations

- **Post-creation only**: Initial creation failures (`addRelays` using `mapConcurrently`) cause the entire group to be deleted — no partial failure. RSFailed only applies to async failures after creation.
- **No recovery path**: Once RSFailed, no automatic mechanism to retry or recover. Owner would need to remove and re-add the relay.
- **Async retry gap**: `retryRelayConnectionAsync` (Commands.hs:2051) is fire-and-forget. If the async retry also permanently fails, that ERR will also be caught by this handler — which is correct behavior.
- **Race condition**: If ERR arrives before the relay record is fully created (unlikely — `addRelay` creates DB records before `joinConnection`), `getGroupRelayByGMId` could fail. This is extremely unlikely since `joinConnection` must succeed first for ERR to be delivered later.
