# Relay Management Improvements

## Problem Statement

Channel owners currently can only add relays during channel creation (`APINewPublicGroup`). Once a channel is created, there is no way to:
1. Add a new relay to an existing channel.
2. Remove a relay from an existing channel.
3. Have relays and subscribers automatically detect and synchronize state changes by checking the authoritative group link data on the SMP server.

Several TODO markers in the codebase (`[relays]`) confirm these are planned but unimplemented. The `runRelayGroupLinkChecks` function (Commands.hs:4729) is a stub. The LINK event handler (Subscriber.hs:1308-1309) has a TODO for relay deletion detection. No `APIAddGroupRelay` or `APIRemoveGroupRelay` commands exist.

## Solution Summary

### Add relay to existing channel

Reuse the existing `addRelays` function (Commands.hs:3887, in `processChatCommand`'s `where` block) from a new `APIAddGroupRelay` command. The `addRelays` flow is asynchronous: after the invitation is sent (RSNew→RSInvited), the relay responds with its relay link (→RSAccepted), and the CON event handler (Subscriber.hs:861-864) calls `setGroupLinkDataAsync` to publish the new relay link. The LINK callback then promotes RSAccepted→RSActive. No synchronous `setGroupLinkData` call is needed in the add handler.

### Remove relay from existing channel

New `APIRemoveGroupRelay` command that:
1. Marks the relay as removed (new `RSRemoved` status).
2. Updates group link data on SMP server (removes relay link).
3. Sends `x.grp.mem.del` to the relay to notify it (best effort, via member's active connection).
4. Deletes the relay member connection.

**Relation to `APIRemoveMembers`**: The existing `APIRemoveMembers` command partially handles relay removal (sends `XGrpMemDel` to all group members, calls `updatePublicGroupData` for channels). A dedicated `APIRemoveGroupRelay` is preferred because: (a) it explicitly manages the `GroupRelay` status, (b) it avoids the complexity of group-wide message broadcasting (relay removal only needs to notify the relay directly), (c) it provides a clean API surface for the UI.

**Per the RFC** (docs/rfcs/2025-10-20-chat-relays.md), the relay itself is responsible for forwarding the "relay is deleted" notification to its connected members. This is relay-side behavior — the owner's command only needs to notify the relay; member notification is the relay's responsibility.

### State synchronization

Three actors synchronize via the group link data:
- **Owner**: publishes the authoritative relay list in link data.
- **Relay**: periodically fetches group link data to confirm its link is present (self-check). If absent → clean up.
- **Member/subscriber**: periodically (or on reconnect) fetches group link data. If new relay links appear → connect. If relay links disappear → mark relay member inactive/removed.

---

## Detailed Technical Design

### 1. New RelayStatus: RSRemoved

**File**: `src/Simplex/Chat/Types/Shared.hs`

Add `RSRemoved` constructor to `RelayStatus`:

```haskell
data RelayStatus
  = RSNew
  | RSInvited
  | RSAccepted
  | RSActive
  | RSInactive
  | RSRemoved   -- NEW
```

Update `relayStatusText`, `TextEncoding`, JSON instances. No database migration needed — `RelayStatus` is stored as TEXT, so `"removed"` just works.

**Forward compatibility note**: Older clients that encounter `"removed"` in the database will have `textDecode` return `Nothing`, causing a `fromField` failure when loading the `GroupRelay` record. This is acceptable because: (a) relay management is a new feature, so older clients would not have `RSRemoved` records, and (b) the same pattern is used for other enums where new values are added without migration.

### 2. New ChatCommands and ChatResponses

**File**: `src/Simplex/Chat/Controller.hs`

```haskell
-- New commands
| APIAddGroupRelay GroupId (NonEmpty Int64)     -- group ID, relay chat_relay_ids
| APIRemoveGroupRelay GroupId Int64             -- group ID, group_relay_id

-- New responses (additions)
| CRGroupRelayAdded { user :: User, groupInfo :: GroupInfo, groupLink :: GroupLink, groupRelays :: [GroupRelay] }
| CRGroupRelayAddFailed { user :: User, addRelayResults :: [AddRelayResult] }
| CRGroupRelayRemoved { user :: User, groupInfo :: GroupInfo, groupLink :: GroupLink, groupRelays :: [GroupRelay] }

-- New events (additions)
| CEvtGroupRelaysChanged { user :: User, groupInfo :: GroupInfo, groupLink :: GroupLink, groupRelays :: [GroupRelay] }
```

`CEvtGroupRelaysChanged` is emitted by the subscriber-side sync flows (relay or member discovering changes).

### 3. Backend: Add Relay to Existing Channel

**File**: `src/Simplex/Chat/Library/Commands.hs`

New handler for `APIAddGroupRelay`:

```
APIAddGroupRelay groupId relayIds -> withUser $ \user -> withGroupLock "addGroupRelay" groupId $ do
  -- 1. Validate: user is owner, group uses relays
  gInfo <- withFastStore $ \db -> getGroupInfo db vr user groupId
  assertUserGroupRole gInfo GROwner
  unless (useRelays' gInfo) $ throwCmdError "group does not use relays"

  -- 2. Get group link (needed for relay invitation)
  gLink <- withFastStore $ \db -> getGroupLink db user gInfo
  sLnk <- case connShortLink' (connLinkContact gLink) of
    Just sl -> pure sl
    Nothing -> throwChatError $ CEException "group link has no short link"

  -- 3. Load requested relay configs
  relays <- withFastStore $ \db -> mapM (getChatRelayById db user) (L.toList relayIds)

  -- 4. Reuse existing addRelays function
  results <- addRelays user gInfo sLnk relays

  -- 5. Check results; at least one success required
  let toRelayResult (r, Left e) = AddRelayResult r (Just e)
      toRelayResult (r, Right _) = AddRelayResult r Nothing
  case partitionEithers (map snd results) of
    ([], _groupRelays) -> do
      -- Relay connection is asynchronous: relay invitation was sent (RSNew→RSInvited).
      -- When relay responds (RSAccepted) and connects (CON event at Subscriber.hs:861-864),
      -- setGroupLinkDataAsync is called automatically to add the relay link to group link data.
      -- The LINK callback then promotes RSAccepted→RSActive.
      relays' <- withFastStore $ \db -> liftIO $ getGroupRelays db gInfo
      pure $ CRGroupRelayAdded user gInfo gLink relays'
    (_errors, _) -> do
      let relayResults = map toRelayResult results
      pure $ CRGroupRelayAddFailed user relayResults
```

Key points:
- Uses `withGroupLock` to prevent concurrent relay modifications.
- Reuses `addRelays` unchanged — it already handles the full invitation flow.
- No synchronous `setGroupLinkData` call needed: the CON event handler (Subscriber.hs:861-864) calls `setGroupLinkDataAsync` when the relay connects, which adds the relay link to group link data.
- Error response uses `CRGroupRelayAddFailed` (new response type) rather than `CRPublicGroupCreationFailed` which is semantically specific to channel creation.

### 4. Backend: Remove Relay from Existing Channel

**File**: `src/Simplex/Chat/Library/Commands.hs`

New handler for `APIRemoveGroupRelay`:

```
APIRemoveGroupRelay groupId groupRelayId -> withUser $ \user -> withGroupLock "removeGroupRelay" groupId $ do
  -- 1. Validate: user is owner
  gInfo <- withFastStore $ \db -> getGroupInfo db vr user groupId
  assertUserGroupRole gInfo GROwner

  -- 2. Get the group relay and its member
  (groupRelay, relayMember) <- withFastStore $ \db -> do
    gr <- getGroupRelayById db groupRelayId
    m <- getGroupMemberById db vr user (groupMemberId gr)
    pure (gr, m)

  -- 3. Mark relay as removed in DB (both GroupRelay and GroupMember records)
  withFastStore' $ \db -> do
    updateRelayStatus db groupRelay RSRemoved
    updateGroupMemberStatus db (userId user) relayMember GSMemRemoved

  -- 4. Update group link data (removes this relay's link from SMP server)
  gLink <- withFastStore $ \db -> getGroupLink db user gInfo
  gLink' <- setGroupLinkData nm user gInfo gLink

  -- 5. Notify relay of removal via x.grp.mem.del (best effort)
  -- sendDirectMemberMessage takes (Connection, ChatMsgEvent, GroupId), not (User, GroupMember, ...)
  -- XGrpMemDel takes (MemberId, Bool) — Bool is withMessages, False for relay removal
  void $ tryAllErrors $ forM_ (activeConn relayMember) $ \conn ->
    sendDirectMemberMessage conn (XGrpMemDel (memberId' relayMember) False) groupId

  -- 6. Delete the relay member connection
  -- deleteMemberConnection :: GroupMember -> CM () (Internal.hs:1795)
  deleteMemberConnection relayMember

  -- 7. Return updated state
  relays <- withFastStore $ \db -> liftIO $ getGroupRelays db gInfo
  pure $ CRGroupRelayRemoved user gInfo gLink' relays
```

Key points:
- `RSRemoved` prevents the relay from being included in `getConnectedGroupRelays` (which filters by RSAccepted/RSActive), so `setGroupLinkData` automatically excludes it.
- Both `GroupRelay` (RSRemoved) and `GroupMember` (GSMemRemoved) statuses must be updated. The UI's `ownerRelayStatusText` checks both relay and member status for display.
- `x.grp.mem.del` notifies the relay. If delivery fails (relay offline), the relay will discover removal via its own periodic link check.
- Connection deletion is necessary to clean up agent-level resources.

**Error handling consideration**: If `setGroupLinkData` (step 4) fails, the entire handler throws and steps 5-7 don't execute. The relay is marked RSRemoved locally but the link remains on the SMP server. This is self-healing: the next `updatePublicGroupData` call (triggered by any profile/member-count change) will exclude the RSRemoved relay from link data. During implementation, consider whether to make step 4 best-effort (wrap in `tryAllErrors`) so cleanup always proceeds.

**New store function needed** in `Store/Groups.hs`:

```haskell
deleteGroupRelayRecord :: DB.Connection -> GroupRelay -> IO ()
```

However, we may prefer to keep the record with RSRemoved status for audit/recovery rather than hard-deleting. The `getConnectedGroupRelays` already filters by status, so removed relays won't appear in link data.

### 5. Backend: LINK Event Handler — Detect Relay Removal (Owner)

**File**: `src/Simplex/Chat/Library/Subscriber.hs` (lines 1308-1317)

Replace the TODO with actual relay removal detection:

```haskell
updateRelay :: DB.Connection -> GroupRelay -> ([GroupRelay], Bool) -> IO ([GroupRelay], Bool)
updateRelay db relay@GroupRelay {relayLink, relayStatus} (acc, changed) =
  case relayLink of
    Just rLink
      | rLink `elem` relayLinks && relayStatus == RSAccepted -> do
          -- Relay link present, promote to active
          relay' <- updateRelayStatus db relay RSActive
          pure (relay' : acc, True)
      | rLink `elem` relayLinks -> pure (relay : acc, changed)
      | relayStatus `elem` [RSAccepted, RSActive, RSInactive] -> do
          -- Relay link ABSENT from link data — relay was removed (e.g., by another owner)
          -- RSInactive is included: an inactive relay whose link is removed has been dropped
          relay' <- updateRelayStatus db relay RSRemoved
          pure (relay' : acc, True)
    _ -> pure (relay : acc, changed)
```

Note: If the owner themselves just called `APIRemoveGroupRelay`, the relay is already RSRemoved before `setGroupLinkData` triggers the LINK callback. The `relayStatus `elem` [RSAccepted, RSActive, RSInactive]` guard will NOT match RSRemoved, avoiding a double-update. This detection is primarily for the multi-owner scenario where another owner removes a relay.

### 6. Backend: Relay Self-Check (runRelayGroupLinkChecks)

**File**: `src/Simplex/Chat/Library/Commands.hs` (lines 4729-4735)

Implement the stub:

```
runRelayGroupLinkChecks :: User -> CM ()
runRelayGroupLinkChecks user = do
  vr <- chatVersionRange
  -- Get all groups where this client is a relay (relay_own_status is set and not RSRemoved)
  relayGroups <- withFastStore' $ \db -> getRelayOwnGroups db vr user
  forM_ relayGroups $ \gInfo -> tryAllErrors $ do
    -- Get the group link from GroupProfile.publicGroup.groupLink
    case publicGroup (groupProfile gInfo) of
      Just PublicGroupProfile {groupLink = sLnk} -> do
        -- Fetch current link data from SMP server
        -- getShortLinkConnReq' returns (FixedLinkData, ConnLinkData) where ConnLinkData
        -- pattern matches as UserContactLinkData (see Subscriber.hs:1298)
        (_, UserContactLinkData UserContactData {relays = relayLinks}) <- getShortLinkConnReq' NRMBackground user sLnk
        -- Get our own relay link from the group link connection
        gLink_ <- withFastStore' $ \db -> runExceptT $ getGroupLink db user gInfo
        case gLink_ of
          Right GroupLink {connLinkContact = CCLink _ (Just ourLink)} ->
            if ourLink `elem` relayLinks
              then do
                -- Our link is present — update to RSActive if not already
                gInfo' <- withFastStore' $ \db -> updateRelayOwnStatusFromTo db gInfo RSAccepted RSActive
                when (relayOwnStatus gInfo' /= relayOwnStatus gInfo) $
                  toView $ CEvtGroupRelayUpdated user gInfo' (membership gInfo') -- TODO: verify event params
              else do
                -- Our link is ABSENT — we have been removed
                withFastStore' $ \db -> updateRelayOwnStatus_ db gInfo RSRemoved
                -- Relay should clean up: per RFC, relay forwards "relay is deleted"
                -- notification to its connected members, then deletes its relay link.
                -- The x.grp.mem.del from owner may also arrive and trigger cleanup.
          _ -> pure ()
      _ -> pure ()
```

**New store function needed**:

```haskell
getRelayOwnGroups :: DB.Connection -> VersionRangeChat -> User -> IO [GroupInfo]
-- SELECT groups WHERE relay_own_status IS NOT NULL AND relay_own_status NOT IN ('removed')
```

**Scheduling**: `runRelayGroupLinkChecks` should be called periodically. It can be added to the existing periodic tasks loop (likely in `startChat` or the background worker). Check existing patterns for `expireChatItems` or similar periodic functions.

### 7. Backend: Member/Subscriber Synchronization

When a member reconnects or periodically checks, they should discover relay changes via group link data.

**Approach**: Add a `syncGroupRelays` function called:
- On app foreground / subscription resume (existing `subscribeGroups` flow)
- Periodically (similar to relay self-check)

**File**: `src/Simplex/Chat/Library/Commands.hs` or `Internal.hs`

```
syncMemberRelays :: User -> GroupInfo -> CM ()
syncMemberRelays user gInfo = when (useRelays' gInfo && memberRole' (membership gInfo) /= GROwner) $ do
  vr <- chatVersionRange
  -- Get group link from GroupProfile.publicGroup.groupLink
  case publicGroup (groupProfile gInfo) of
    Just PublicGroupProfile {groupLink = sLnk} -> tryAllErrors $ do
      (_, UserContactLinkData UserContactData {relays = currentRelayLinks}) <- getShortLinkConnReq' NRMBackground user sLnk
      -- Get local relay state: GroupRelay records have relayLink :: Maybe ShortLinkContact
      -- Note: getGroupRelayMembers returns [GroupMember] (no relay links),
      -- so we need getGroupRelays which returns [GroupRelay] with relayLink field.
      localGroupRelays <- withFastStore $ \db -> liftIO $ getGroupRelays db gInfo
      let localRelayLinks = mapMaybe (\GroupRelay {relayLink} -> relayLink) localGroupRelays
      -- Discover new relays (in link data but not local)
      let newRelayLinks = filter (`notElem` localRelayLinks) currentRelayLinks
      forM_ newRelayLinks $ \rlnk -> tryAllErrors $
        connectToNewRelay user gInfo rlnk
      -- Discover removed relays (local but not in link data)
      let removedLinks = filter (`notElem` currentRelayLinks) localRelayLinks
      forM_ localGroupRelays $ \gr@GroupRelay {relayLink, groupMemberId} ->
        when (maybe False (`elem` removedLinks) relayLink) $ do
          relayMember <- withFastStore $ \db -> getGroupMemberById db vr user groupMemberId
          markRelayMemberRemoved user gInfo relayMember
    _ -> pure ()
```

Where:
- `connectToNewRelay` reuses `getCreateRelayForMember` + `connectViaContact` (same pattern as Commands.hs:2168-2178). New helper function to be created.
- `markRelayMemberRemoved` updates member status and deletes the connection. New helper function to be created.
- Note: on the member side, `getGroupRelays` may return an empty list if the member is not the owner. **Need to verify**: whether `group_relays` records exist for members or only for owners. If only for owners, member-side sync would need to compare `getGroupRelayMembers` (GroupMember list) with the relay links from the SMP server, matching by some identifying property (e.g., relay member's connection short link). This requires further investigation during implementation.

**When to call `syncMemberRelays`**:
- During `subscribeGroups` / group subscription (on app start or foreground).
- Could also be triggered when a relay connection fails (reconnection triggers re-check).

### 8. Database Changes

**No migration needed for RSRemoved** — RelayStatus is stored as TEXT.

**New store functions** (in `Store/Groups.hs`):

```haskell
getRelayOwnGroups :: DB.Connection -> VersionRangeChat -> User -> IO [GroupInfo]
-- For relay self-check: groups WHERE relay_own_status IS NOT NULL AND relay_own_status NOT IN ('removed')
```

Note: `publicGroupLink` is not needed as a separate function — the access path is `publicGroup (groupProfile gInfo)` which gives `Maybe PublicGroupProfile`, and `PublicGroupProfile` has `groupLink :: ShortLinkContact`.

### 9. iOS UI Changes

#### 9.1 ChannelRelaysView.swift — Add/Remove Buttons

Add an "Add relay" button (owner only) and swipe-to-delete on relay rows:

```swift
struct ChannelRelaysView: View {
    // ... existing state ...
    @State private var showAddRelay = false

    var body: some View {
        List {
            relaysList()
            if groupInfo.isOwner {
                Section {
                    Button {
                        showAddRelay = true
                    } label: {
                        Label("Add chat relay", systemImage: "plus")
                    }
                }
            }
        }
        .sheet(isPresented: $showAddRelay) {
            AddGroupRelayView(groupInfo: groupInfo) { relays in
                groupRelays = relays
            }
        }
        // ... existing onAppear ...
    }

    // In relaysList, add swipe delete for owner:
    // .swipeActions { if groupInfo.isOwner { deleteRelayButton(relay) } }
}
```

#### 9.2 New AddGroupRelayView.swift

A sheet that:
1. Loads user's configured chat relays via `apiGetUserChatRelays()`.
2. Filters out relays already in this channel.
3. Shows a selectable list.
4. On confirm, calls new `apiAddGroupRelay(groupId, relayIds)`.
5. Shows progress/results.

#### 9.3 New API Functions in SimpleXAPI.swift

```swift
func apiAddGroupRelay(_ groupId: Int64, _ relayIds: [Int64]) async throws -> [GroupRelay] { ... }
func apiRemoveGroupRelay(_ groupId: Int64, _ groupRelayId: Int64) async throws -> [GroupRelay] { ... }
```

#### 9.4 New ChatResponse/ChatEvent Cases

In `AppAPITypes.swift`, add handling for `CRGroupRelayAdded`, `CRGroupRelayRemoved`, `CEvtGroupRelaysChanged`.

#### 9.5 Relay Status Display Enhancement

Update `ChannelRelaysView` and `GroupMemberInfoView` to show `RSRemoved` status with appropriate styling (red, "removed").

In `ChatTypes.swift`, update `RelayStatus` enum to include `.removed` case.

---

## State Synchronization Summary

```
                  SMP Server (group link data)
                 ┌──────────────────────────────┐
                 │  UserContactData {           │
                 │    relays: [relay1, relay2]   │
                 │  }                            │
                 └──────────┬───────────────────┘
                            │
            ┌───────────────┼───────────────┐
            │ writes        │ reads          │ reads
            ▼               ▼                ▼
         Owner          Relay (self)      Member
    setGroupLinkData   runRelayGroup     syncMemberRelays
                       LinkChecks

    Triggers:          Triggers:          Triggers:
    - Add relay        - Periodic         - App foreground
    - Remove relay     - On subscription  - On subscription
    - Profile update                      - Relay conn failure
```

**Owner writes** → SMP server is updated → **Relays and Members read** → discover changes → adjust local state.

---

## Edge Cases and Failure Recovery

1. **Add relay fails (network)**: `addRelays` already handles temporary errors. The relay remains in RSInvited; owner can retry or the relay will process the pending invitation when it comes online.

2. **Remove relay, notification fails**: The relay won't receive `x.grp.mem.del`, but its periodic self-check will discover its link is missing from group link data → self-cleanup.

3. **Owner removes last relay**: Members lose message delivery. Owner must add a new relay. Members will discover the new relay via `syncMemberRelays` on their next link data check.

4. **Relay goes offline permanently**: Owner removes it via `APIRemoveGroupRelay`. New members won't see it in link data. Existing members with connections to this relay will experience connection failures → `syncMemberRelays` discovers the relay link is gone → marks it removed locally.

5. **Race: relay still connecting when owner removes it**: The group lock prevents concurrent add/remove. If relay is in RSInvited/RSAccepted, removal still works — `setGroupLinkData` won't include it (it's marked RSRemoved before the link data update).

6. **Member discovers new relay but can't connect**: Treated same as initial join — temporary failures are retried. The relay member remains in a connecting state.

7. **Concurrent owners adding/removing relays**: The group lock serializes operations within one client. Cross-client conflicts are resolved by the SMP server — the last `setConnShortLink` call wins. Both owners' LINK callbacks will see the final state and reconcile.

---

## Implementation Order

1. **RSRemoved status** — trivial, foundational for everything else.
2. **APIRemoveGroupRelay** — simpler than add (no async relay handshake), and the LINK handler relay-removal detection.
3. **APIAddGroupRelay** — reuses `addRelays`, needs group lock and post-connection link data update.
4. **runRelayGroupLinkChecks** — relay self-check implementation.
5. **syncMemberRelays** — member-side synchronization.
6. **iOS UI** — ChannelRelaysView add/remove buttons, AddGroupRelayView sheet, API functions, response handling.

## Files Changed (Backend)

| File | Change |
|------|--------|
| `src/Simplex/Chat/Types/Shared.hs` | Add `RSRemoved` to `RelayStatus` |
| `src/Simplex/Chat/Controller.hs` | Add `APIAddGroupRelay`, `APIRemoveGroupRelay` commands; add response/event types |
| `src/Simplex/Chat/Library/Commands.hs` | Implement command handlers; implement `runRelayGroupLinkChecks` |
| `src/Simplex/Chat/Library/Internal.hs` | Add `syncMemberRelays`, `connectToNewRelay`, `markRelayMemberRemoved` |
| `src/Simplex/Chat/Library/Subscriber.hs` | Fix LINK handler relay removal detection; integrate `syncMemberRelays` into subscription |
| `src/Simplex/Chat/Store/Groups.hs` | Add `getRelayOwnGroups`, `deleteGroupRelayRecord` (if needed) |

## Files Changed (iOS)

| File | Change |
|------|--------|
| `apps/ios/SimpleXChat/ChatTypes.swift` | Add `RelayStatus.removed` |
| `apps/ios/Shared/Model/AppAPITypes.swift` | Add response/event types |
| `apps/ios/Shared/Model/SimpleXAPI.swift` | Add `apiAddGroupRelay`, `apiRemoveGroupRelay` |
| `apps/ios/Shared/Views/Chat/Group/ChannelRelaysView.swift` | Add/remove buttons, swipe actions |
| `apps/ios/Shared/Views/Chat/Group/AddGroupRelayView.swift` | NEW: relay selection sheet |
| `apps/ios/Shared/Views/Chat/Group/GroupMemberInfoView.swift` | Show RSRemoved status |
