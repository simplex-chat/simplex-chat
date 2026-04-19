# Plan: Relay Leaving Group (Moderation Capability)

## Context

SimpleX Chat channels use chat relays to forward messages from owners to subscribers. When a channel hosts prohibited content, the relay operator needs the ability to make their relay leave the group. Currently `APILeaveGroup` doesn't work correctly for relay members: the `getRecipients` helper always uses `getGroupRelayMembers` for channel groups, which returns only other relays (members with `GRRelay` role) — the owner is excluded. The relay needs to notify the owner (so it can update channel link data) and all subscribers directly (relay has connections to all of them).

## Flow

1. **Relay** calls `APILeaveGroup` → sends `XGrpLeave` directly to all members (owners + subscribers) → deletes all connections
2. **Owner** receives `XGrpLeave` → updates relay member status to `GSMemLeft` → updates `GroupRelay.relayStatus` to `RSInactive` → updates channel link relay list via `updatePublicGroupData` → `setGroupLinkDataAsync` → `setAgentConnShortLinkAsync` (excludes left relay from link data) → relay bar shows status
3. **Subscribers** receive `XGrpLeave` directly from relay → update relay member status to `GSMemLeft` → delete connection to relay → relay bar shows status

## Changes

### 1. Add `RSInactive` to `RelayStatus`

**`src/Simplex/Chat/Types/Shared.hs`** (~L81-112)

`GroupMemberStatus` already carries left/removed semantics (`GSMemLeft`, `GSMemRemoved`), so `RelayStatus` should not duplicate that. Add `RSInactive` as a generic terminal status meaning "no longer operational", complementing `RSActive`. Add `"inactive"` encoding in `relayStatusText` and `TextEncoding` instance.

```haskell
data RelayStatus
  = RSNew
  | RSInvited
  | RSAccepted
  | RSActive
  | RSInactive
  deriving (Eq, Show)
```

### 2. Fix `APILeaveGroup` recipients for relay

**`src/Simplex/Chat/Library/Commands.hs`** (~L2838-2844)

Use nested condition inside `useRelays'` guard. When relay leaves, it sends `XGrpLeave` to all current/pending members directly (relay has connections to all of them).

```haskell
getRecipients user gInfo@GroupInfo {membership}
  | useRelays' gInfo =
      if isRelay membership
        then do
          -- Relay leaving: notify all members directly, clean up all connections
          ms <- withFastStore' $ \db -> getGroupMembers db vr user gInfo
          pure (ms, filter memberCurrentOrPending ms)
        else do
          relays <- withFastStore' $ \db -> getGroupRelayMembers db vr user gInfo
          pure (relays, relays)
  | otherwise = do
      ms <- withFastStore' $ \db -> getGroupMembers db vr user gInfo
      pure (ms, filter memberCurrentOrPending ms)
```

- `members` (first tuple) = all members → used by `deleteMembersConnections'` for connection cleanup
- `recipients` (second tuple) = all current/pending members → XGrpLeave sent directly

Existing functions: `isRelay` (Types.hs:1063), `getGroupMembers` (Store/Groups.hs), `memberCurrentOrPending` (Types.hs:1308).

### 3. Update `xGrpLeave` to set relay status and channel link on owner

**`src/Simplex/Chat/Library/Subscriber.hs`** (~L3113-3124)

After `updateMemberRecordDeleted` and before `updatePublicGroupData`, set `RSInactive` on the owner's `GroupRelay` record. On subscribers this is a no-op because they have no `GroupRelay` record (`getGroupRelayByGMId` returns `Left`, `forM_` skips).

```haskell
xGrpLeave gInfo m msg@RcvMessage {msgSigned} brokerTs = do
  deleteMemberConnection m
  gInfo' <- updateMemberRecordDeleted user gInfo m GSMemLeft
  -- Set relay status to inactive (owner-only; subscriber has no GroupRelay record)
  when (isRelay m) $
    withStore' $ \db -> do
      relay_ <- runExceptT $ getGroupRelayByGMId db (groupMemberId' m)
      forM_ relay_ $ \relay -> void $ updateRelayStatus db relay RSInactive
  gInfo'' <- updatePublicGroupData user gInfo'
  -- ... rest unchanged
```

The channel link update chain on owner: `updatePublicGroupData` (Internal.hs:1317) → `setGroupLinkDataAsync` (Internal.hs:1309) → `getConnectedGroupRelays` (filters `member_status = GSMemConnected AND relay_status IN (RSAccepted, RSActive)`) → `groupLinkData` (builds `UserContactLinkData` with remaining relay links only) → `setAgentConnShortLinkAsync` (updates SMP short link). The left relay is excluded by the `member_status` filter, so its link is removed from the channel link data.

Existing functions: `isRelay` (Types.hs:1063), `getGroupRelayByGMId` (Store/Groups.hs:1296), `updateRelayStatus` (Store/Groups.hs:1418), `groupMemberId'` (Types.hs).

### 4. Client type updates

**`apps/ios/SimpleXChat/ChatTypes.swift`** (~L2558-2563, L2628-2637)

Add `case rsInactive = "inactive"` to `RelayStatus` enum and `case .rsInactive: "inactive"` to `text` property.

**`apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt`** (~L2266-2276)

Add `@SerialName("inactive") RsInactive` to `RelayStatus` enum and `RsInactive -> generalGetString(MR.strings.relay_status_inactive)` to `text` property.

**`apps/multiplatform/common/src/commonMain/resources/MR/base/strings.xml`** (~L2879)

Add `<string name="relay_status_inactive">inactive</string>`.

**`packages/simplex-chat-client/types/typescript/src/types.ts`** (~L3608-3613)

Add `Inactive = "inactive"` to `RelayStatus` enum.

### 5. UI: relay bar updates

The relay bar (above compose area) currently shows for owners when `activeCount < relays.count`, and for subscribers in steady state. After relay leaves, the bar shows but needs better indication of what happened and whether delivery is broken.

#### 5a. Fix `relayStatusIndicator` color for `RSInactive`

Currently `relayStatusIndicator` shows yellow for any non-active status — yellow implies "connecting/in progress", which is wrong for an inactive relay.

**iOS** (`apps/ios/Shared/Views/NewChat/AddChannelView.swift` L431-432):
```swift
// CURRENT:
let color: Color = connFailed ? .red : (status == .rsActive ? .green : .yellow)
// NEW:
let color: Color = connFailed ? .red : (status == .rsActive ? .green : (status == .rsInactive ? .red : .yellow))
```

**Kotlin** (`apps/multiplatform/.../views/newchat/AddChannelView.kt` L551):
```kotlin
// CURRENT:
val color = if (connFailed) Color.Red else if (status == RelayStatus.RsActive) Color.Green else WarningYellow
// NEW:
val color = if (connFailed) Color.Red else when (status) {
    RelayStatus.RsActive -> Color.Green
    RelayStatus.RsInactive -> Color.Red
    else -> WarningYellow
}
```

#### 5b. Owner relay bar: "no active relays" message

When `activeCount == 0`, show a warning in the relay bar that delivery is broken and adding new relays is coming.

**iOS** (`apps/ios/Shared/Views/Chat/ComposeMessage/ComposeView.swift` ~L730-738):

In `ownerChannelRelayBar`, when expanded and `activeCount == 0`, add footer text:
```swift
"Messages can't be delivered to subscribers. Adding new relay will be available in a future update."
```

**Kotlin** (`apps/multiplatform/.../views/chat/ComposeView.kt` ~L1647-1657): same logic.

New string: `relay_bar_owner_no_delivery` = "Messages can't be delivered to subscribers. Adding new relay will be available in a future update."

#### 5c. Subscriber relay bar: show disconnection in steady state

Currently in steady state (`showProgress = false`), the subscriber relay bar header shows only "N relays" with no error indication. When relay connections are deleted (relay left), the subscriber sees no issue in collapsed view.

**iOS** (`apps/ios/Shared/Views/Chat/ComposeMessage/ComposeView.swift` ~L780-792):

When `!showProgress`, check error state:
```swift
if !showProgress {
    if errorCount == total {
        Text("All relays disconnected – messages can't be delivered")
    } else if errorCount > 0 {
        Text(String.localizedStringWithFormat("%d/%d relays connected, %d errors", connectedCount, total, errorCount))
    } else {
        Text(String.localizedStringWithFormat("%d relays", total))
    }
}
```

**Kotlin** (`apps/multiplatform/.../views/chat/ComposeView.kt` ~L1695-1707): same logic.

New strings:
- `relay_bar_all_disconnected` = "All relays disconnected – messages can't be delivered"
- `relay_bar_connected_with_errors_steady` = "%1$d/%2$d relays connected, %3$d errors"

### 6. Test

**`tests/ChatTests/Groups.hs`**

Add `testChannelRelayLeave` test:

1. Create channel with 2 relays (`relay1`, `relay2`) and 2 subscribers (`dan`, `eve`) via `prepareChannel2Relays` + `memberJoinChannel`
2. Verify channel works: owner sends message → subscribers receive via relay forwarding
3. `relay1` leaves: `relay1 ##> "/leave #team"`
4. Verify relay1 output: `"#team: you left the group"`
5. Verify owner output: `"#team: <relay1_name> left the group (signed)"`
6. Verify subscribers receive `XGrpLeave` directly — check relay1 member status is `"left"` on subscribers via `checkMemberStatus`
7. Wait for async link data update
8. Verify channel still works with remaining relay: owner sends message → relay2 forwards → subscribers receive
9. `relay2` leaves: `relay2 ##> "/leave #team"`
10. Verify relay2 output and owner/subscriber leave events
11. **Verify no delivery**: owner sends message, `threadDelay`, check subscribers' last item is still the previous message (not the new one) — pattern from `testChannelSubscriberLeave` L9237

Register test in test list at ~L261 (after `testChannelSubscriberLeave`).

## Files Modified

| File | Change |
|------|--------|
| `src/Simplex/Chat/Types/Shared.hs` | Add `RSInactive` to `RelayStatus` |
| `src/Simplex/Chat/Library/Commands.hs` | Fix `getRecipients` for relay in `APILeaveGroup` |
| `src/Simplex/Chat/Library/Subscriber.hs` | Update `xGrpLeave` to set relay status |
| `apps/ios/SimpleXChat/ChatTypes.swift` | Add `rsInactive` case |
| `apps/ios/Shared/Views/NewChat/AddChannelView.swift` | Fix `relayStatusIndicator` color for inactive |
| `apps/ios/Shared/Views/Chat/ComposeMessage/ComposeView.swift` | Owner/subscriber relay bar messages |
| `apps/multiplatform/.../ChatModel.kt` | Add `RsInactive` case |
| `apps/multiplatform/.../AddChannelView.kt` | Fix `RelayStatusIndicator` color for inactive |
| `apps/multiplatform/.../ComposeView.kt` | Owner/subscriber relay bar messages |
| `apps/multiplatform/.../strings.xml` | Add `relay_status_inactive` + relay bar strings |
| `packages/.../types.ts` | Add `Inactive` to enum |
| `tests/ChatTests/Groups.hs` | Add `testChannelRelayLeave` test |

## Verification

```bash
cabal build --ghc-options=-O0
cabal test simplex-chat-test --test-options='-m "channels"'
```

Manual: verify relay bar appearance on iOS simulator and Android emulator after relay leaves.

## Adversarial Review

**Pass 1:**
- Relay's `sendGroupMessage'` signs `XGrpLeave` (`requiresSignature XGrpLeave_ = True`; relay has `groupKeys` with `memberPrivKey`). Owner and subscribers verify signature. OK.
- `deleteMembersConnections' user members True` with `waitDelivery=True` ensures `XGrpLeave` reaches SMP queues before relay deletes connections. OK.
- Owner's channel link update: `updatePublicGroupData` → `setGroupLinkDataAsync` → `getConnectedGroupRelays` (excludes left relay by `member_status = GSMemLeft`) → `groupLinkData` (builds link with remaining relays) → `setAgentConnShortLinkAsync` (updates SMP short link). Left relay's link removed. OK.
- `muteEventInChannel` for relay member (`GRRelay < GRModerator`): muted for subscribers (no chat item), but DB is updated. Owner sees event (`GROwner >= GRModerator`). OK.
- Relay's `deleteGroupLinkIfExists` is no-op (relay doesn't own group link). OK.
- `getGroupRelayByGMId` on subscriber returns `Left` (no `GroupRelay` record) → `forM_` skips. No error. OK.
- `memberEventDeliveryScope` returns `DJSGroup {jobSpec = DJDeliveryJob {includePending = False}}` for relay member. On subscriber, creates a delivery task but subscriber has no forwarding role — delivery worker finds no eligible connections. Harmless no-op. OK.
- Owner relay bar: `activeCount` drops (RSInactive ≠ RSActive) → bar shows → `relayStatusIndicator` shows red dot with "inactive". When `activeCount == 0`: "Messages can't be delivered... Adding new relay will be available in a future update." OK.
- Subscriber relay bar: `connStatus = .deleted` → `deletedCount` increases → `errorCount` increases. When `errorCount == total` in steady state: "All relays disconnected – messages can't be delivered". OK.

**Pass 2:**
- Race condition: owner removes relay + relay leaves simultaneously. Both paths delete connection and update member status. No data corruption — idempotent. `GroupRelay.relayStatus` ends as `RSInactive` from `xGrpLeave` or unchanged from `xGrpMemDel` (which doesn't update relay status). OK.
- `getGroupMembers` for relay may return thousands of subscribers. `deleteMembersConnections'` uses `deleteAgentConnectionsAsync'` which handles batching. `sendGroupMessage'` also handles sending to many members. OK.
- Relay's own `membership` record has no `activeConn` in members list (can't connect to self). `mapMaybe` in `deleteMembersConnections'` filters it out. OK.
- Both relays leave: after last relay leaves, owner sends message. Delivery system has no connected relays to forward through — message saved locally but not delivered. Subscribers' last chat item remains unchanged. Test verifies this. OK.
- `getGroupRelays` (used by `apiGetGroupRelays`) returns ALL GroupRelay records including RSInactive — owner UI correctly includes left relays in bar. OK.
- Subscriber `groupMembers` filter (`memberRole == .relay`) includes left members (memberRole unchanged) — subscriber UI correctly shows left relays. OK.

**Result: 2 consecutive clean passes.**
