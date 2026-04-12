# Plan: Relay Leaving Group (Moderation Capability)

## Context

SimpleX Chat channels use chat relays to forward messages from owners to subscribers. When a channel hosts prohibited content, the relay operator needs the ability to make their relay leave the group — a moderation capability. Currently `APILeaveGroup` doesn't work correctly for relay members: it sends `XGrpLeave` only to other relays (via `getGroupRelayMembers`), but the relay needs to notify the owner so the owner can update channel link data and forward the event to subscribers through remaining relays.

## Flow

1. **Relay** calls `APILeaveGroup` → sends `XGrpLeave` to owners only → deletes all connections (owners + subscribers)
2. **Owner** receives `XGrpLeave` → updates relay member status to `GSMemLeft` → updates `GroupRelay.relayStatus` to `RSLeft` → removes relay link from channel link data via `updatePublicGroupData` → delivery system forwards event to subscribers via remaining relays
3. **Subscribers** receive forwarded `XGrpLeave` → update relay member status to `GSMemLeft` → delete connection to relay (already dead)

## Changes

### 1. Add `RSLeft` to `RelayStatus`

**`src/Simplex/Chat/Types/Shared.hs`** (~L81-112)

Add `RSLeft` after `RSActive` in the `RelayStatus` data type, and add `"left"` encoding in `relayStatusText`, `TextEncoding` instance.

### 2. Fix `APILeaveGroup` recipients for relay

**`src/Simplex/Chat/Library/Commands.hs`** (~L2838-2844)

Change `getRecipients`:

```haskell
-- CURRENT:
getRecipients user gInfo
  | useRelays' gInfo = do
      relays <- withFastStore' $ \db -> getGroupRelayMembers db vr user gInfo
      pure (relays, relays)
  | otherwise = do
      ms <- withFastStore' $ \db -> getGroupMembers db vr user gInfo
      pure (ms, filter memberCurrentOrPending ms)

-- NEW:
getRecipients user gInfo@GroupInfo {membership}
  | useRelays' gInfo, isRelay membership = do
      -- Relay leaving: notify owners, clean up all connections
      ms <- withFastStore' $ \db -> getGroupMembers db vr user gInfo
      let owners = filter (\m -> memberRole' m >= GROwner && memberCurrent m) ms
      pure (ms, owners)
  | useRelays' gInfo = do
      relays <- withFastStore' $ \db -> getGroupRelayMembers db vr user gInfo
      pure (relays, relays)
  | otherwise = do
      ms <- withFastStore' $ \db -> getGroupMembers db vr user gInfo
      pure (ms, filter memberCurrentOrPending ms)
```

`members` (first tuple element) = all members → used by `deleteMembersConnections'` for cleanup.
`recipients` (second tuple element) = owners only → efficient, delivery system forwards to subscribers.

Existing functions reused: `getGroupMembers` (Store/Groups.hs), `isRelay` (Types.hs:1063), `memberRole'` (Types.hs:1066), `memberCurrent` (Types.hs — need to verify existence).

### 3. Update `xGrpLeave` to set relay status on owner

**`src/Simplex/Chat/Library/Subscriber.hs`** (~L3099-3110)

After `updateMemberRecordDeleted` and before `updatePublicGroupData`, add relay status update:

```haskell
xGrpLeave gInfo m msg@RcvMessage {msgSigned} brokerTs = do
  deleteMemberConnection m
  gInfo' <- updateMemberRecordDeleted user gInfo m GSMemLeft
  -- NEW: update relay status when a relay member leaves
  when (isRelay m) $
    withStore' $ \db -> do
      relay_ <- runExceptT $ getGroupRelayByGMId db (groupMemberId' m)
      forM_ relay_ $ \relay -> void $ updateRelayStatus db relay RSLeft
  gInfo'' <- updatePublicGroupData user gInfo'
  -- ... rest unchanged
```

On owner: `getGroupRelayByGMId` succeeds → relay status updated to `RSLeft`.
On subscriber: `getGroupRelayByGMId` fails (no `GroupRelay` record) → `forM_` on `Left` is no-op.

Existing functions reused: `isRelay` (Types.hs:1063), `getGroupRelayByGMId` (Store/Groups.hs:1296), `updateRelayStatus` (Store/Groups.hs:1418), `groupMemberId'` (Types.hs).

### 4. Client type updates

**`apps/ios/SimpleXChat/ChatTypes.swift`** (~L2558-2563, 2628-2637)

Add `case rsLeft = "left"` to `RelayStatus` enum and `case .rsLeft: "left"` to `text` property.

**`apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt`** (~L2266-2276)

Add `@SerialName("left") RsLeft` to `RelayStatus` enum and `RsLeft -> generalGetString(MR.strings.relay_status_left)` to `text` property.

**`apps/multiplatform/common/src/commonMain/resources/MR/base/strings.xml`** (~L2879)

Add `<string name="relay_status_left">left</string>`.

**`packages/simplex-chat-client/types/typescript/src/types.ts`** (~L3608-3613)

Add `Left = "left"` to `RelayStatus` enum.

### 5. Test

**`tests/ChatTests/Groups.hs`**

Add `testChannelRelayLeave` test. Structure:

1. Create channel with 2 relays (`relay1`, `relay2`) and 2 subscribers (`dan`, `eve`) using `prepareChannel2Relays` + `memberJoinChannel`
2. Subscribers discover each other (send a message so relay forwards)
3. `relay1` leaves: `relay1 ##> "/leave #team"`
4. Verify relay1 output: `"#team: you left the group"`
5. Verify owner output: `"#team: <relay1> left the group (signed)"`
6. Wait for async link data update
7. Verify owner relay list: relay1 status = left, relay2 status = active
8. Verify subscribers receive forwarded `XGrpLeave` via relay2 (subscriber member status for relay1 updated)
9. Verify channel still works: owner sends message → relay2 forwards → subscribers receive

Register test in test list at ~L261 (after `testChannelSubscriberLeave`).

## Files Modified

| File | Change |
|------|--------|
| `src/Simplex/Chat/Types/Shared.hs` | Add `RSLeft` to `RelayStatus` |
| `src/Simplex/Chat/Library/Commands.hs` | Fix `getRecipients` for relay in `APILeaveGroup` |
| `src/Simplex/Chat/Library/Subscriber.hs` | Update `xGrpLeave` to set relay status |
| `apps/ios/SimpleXChat/ChatTypes.swift` | Add `rsLeft` case |
| `apps/multiplatform/.../ChatModel.kt` | Add `RsLeft` case |
| `apps/multiplatform/.../strings.xml` | Add `relay_status_left` string |
| `packages/.../types.ts` | Add `Left` to enum |
| `tests/ChatTests/Groups.hs` | Add `testChannelRelayLeave` test |

## Verification

```bash
cabal build --ghc-options=-O0
cabal test simplex-chat-test --test-options='-m "channels"'
```

## Adversarial Review

**Pass 1:**
- Relay's `sendGroupMessage'` signs `XGrpLeave` (relay has `groupKeys` with `memberPrivKey`; `requiresSignature XGrpLeave_ = True`). Owner verifies signature. Forwarded copies retain signature for subscriber verification. OK.
- `deleteMembersConnections' user members True` with `waitDelivery=True` ensures `XGrpLeave` reaches owner's SMP queue before relay deletes connections. OK.
- `updatePublicGroupData` reads `getConnectedGroupRelays` which filters by `member_status = GSMemConnected AND relay_status IN (RSAccepted, RSActive)`. Since member status is already `GSMemLeft`, relay link is excluded. OK.
- `memberEventDeliveryScope` for relay (`GRRelay < GRModerator`): returns `DJDeliveryJob {includePending = False}`. Delivery worker forwards to subscribers via remaining relays. OK.
- `muteEventInChannel` for relay member: muted for subscribers (no chat item shown), but DB is updated. Owner sees event (not muted since `GROwner >= GRModerator`). OK.
- Relay's own `deleteGroupLinkIfExists` is no-op (relay doesn't own group link). OK.
- `getGroupRelayByGMId` on subscriber returns `Left` (no `GroupRelay` record) → `forM_` skips. No error. OK.

**Pass 2:**
- Race condition: owner removes relay + relay leaves simultaneously. Both paths delete connection and update member status. `GroupRelay.relayStatus` ends as `RSLeft` from `xGrpLeave` or remains as-is from `xGrpMemDel` (which doesn't update relay status). No data corruption. OK.
- Relay leaves when it's the only relay: owner receives `XGrpLeave`, updates link data (no relay links), delivery task has no relays to forward through — subscribers don't receive the event. Subscribers see dead connection. Channel is broken. Acceptable for MVP.
- `getGroupMembers` for relay may return thousands of subscriber records (for `members` cleanup list). This is unavoidable — connections must be deleted. `deleteAgentConnectionsAsync'` handles batching. OK.
- Relay's `membership` record doesn't have `activeConn` in `members` list (can't connect to self). `mapMaybe` in `deleteMembersConnections'` filters it out. OK.

**Result: 2 consecutive clean passes.**
