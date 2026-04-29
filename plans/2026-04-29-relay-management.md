# Relay Management Improvements

## Problem Statement

Channel owners currently can only add relays during channel creation (`APINewPublicGroup`). Once a channel is created, there is no way to:
1. Add a new relay to an existing channel.
2. Remove a relay from an existing channel.
3. Have relays and subscribers automatically detect and synchronize relay state changes.

Several TODO markers in the codebase (`[relays]`) confirm these are planned but unimplemented. The `runRelayGroupLinkChecks` function (Commands.hs:4729) is a stub. The LINK event handler (Subscriber.hs:1308-1309) has a TODO for relay deletion detection. No `APIAddGroupRelay` command exists.

## Solution Summary

### Add relay to existing channel

New `APIAddGroupRelay` command that reuses the existing `addRelays` function (Commands.hs:3887, in `processChatCommand`'s `where` block). The `addRelays` flow is asynchronous: after the invitation is sent (RSNew→RSInvited), the relay responds with its relay link (→RSAccepted), and the CON event handler (Subscriber.hs:861-864) calls `setGroupLinkDataAsync` to publish the new relay link. The LINK callback then promotes RSAccepted→RSActive.

### Remove relay from existing channel

Use the existing `APIRemoveMembers` command, extended with relay-specific handling. In channels, `APIRemoveMembers` already sends `XGrpMemDel` to all relay members via `sendGroupMessages` (the `memberSendAction` routing ensures the message goes to relays only, which forward it to subscribers). This is the correct approach: broadcasting the removal through *other* relays ensures all subscribers learn about the removal even if the removed relay is malicious and refuses to notify them. Link data synchronization serves as a backup mechanism.

The extension needed: when removing a relay member, also update its `GroupRelay.relay_status` to `RSInactive`. Currently `APIRemoveMembers` updates `GroupMember` status (via `deleteOrUpdateMemberRecordIO`) and calls `updatePublicGroupData` (which updates link data), but does not touch the `GroupRelay` record.

### State synchronization

Three actors synchronize via the group link data on the SMP server:

- **Owner**: publishes the authoritative relay list in link data via `setGroupLinkData`. The `getConnectedGroupRelays` function (which filters by `member_status = GSMemConnected AND relay_status IN (RSAccepted, RSActive)`) determines which relays appear in link data.
- **Relay**: `runRelayGroupLinkChecks` (implement the stub) periodically fetches group link data to confirm its own link is present. If absent → self-cleanup.
- **Subscriber**: when opening a channel, the UI already calls `APIGetUpdatedGroupLinkData` (Commands.hs:1777) which fetches link data from the SMP server. This handler will be extended to also synchronize relay state: connect to newly discovered relays, disconnect from removed relays.

---

## Detailed Technical Design

### 1. Extend `APIRemoveMembers` for Relay Removal

**File**: `src/Simplex/Chat/Library/Commands.hs` (lines 2804-2900)

`APIRemoveMembers` already handles relay members correctly for message delivery:
- `deleteMemsSend` sends `XGrpMemDel` via `sendGroupMessages` (line 2870)
- `memberSendAction` (Internal.hs:2161-2170) routes the message to relay members only (in channels)
- Other relays forward the `XGrpMemDel` to all subscribers
- `deleteOrUpdateMemberRecordIO` sets `GSMemRemoved` on the member (line 2896)
- `updatePublicGroupData` updates link data (line 2828-2829), which excludes the removed relay because `getConnectedGroupRelays` requires `member_status = GSMemConnected`

**What needs to be added**: after `deleteOrUpdateMemberRecordIO` removes the member, also update the `GroupRelay.relay_status` to `RSInactive`. This mirrors the existing pattern in `xGrpLeave` (Subscriber.hs:3169-3172) where a relay leaving sets its relay status to RSInactive.

The change is in the `delMember` helper (line 2891-2897):

```haskell
delMember db m = do
  void $ deleteOrUpdateMemberRecordIO db user gInfo m
  -- When removing a relay member, also set GroupRelay status to RSInactive
  when (isRelay m) $ do
    relay_ <- runExceptT $ getGroupRelayByGMId db (groupMemberId' m)
    forM_ relay_ $ \relay -> void $ updateRelayStatus db relay RSInactive
  pure m {memberStatus = GSMemRemoved}
```

This uses existing `RSInactive` status (no new enum values needed) and follows the same pattern as `xGrpLeave`. The `getConnectedGroupRelays` query filters by both `member_status = GSMemConnected` and `relay_status IN (RSAccepted, RSActive)`, so either status update alone would suffice, but setting both keeps state consistent and aligns with the leave-relay pattern.

**iOS UI**: the relay removal is triggered by calling the existing `apiRemoveMembers` from `ChannelRelaysView` (new swipe-to-delete action on relay rows). No new API command needed for removal.

### 2. New `APIAddGroupRelay` Command

**File**: `src/Simplex/Chat/Controller.hs`

```haskell
-- New command
| APIAddGroupRelay GroupId (NonEmpty Int64)     -- group ID, chat_relay_ids

-- New responses
| CRGroupRelayAdded { user :: User, groupInfo :: GroupInfo, groupLink :: GroupLink, groupRelays :: [GroupRelay] }
| CRGroupRelayAddFailed { user :: User, addRelayResults :: [AddRelayResult] }
```

**File**: `src/Simplex/Chat/Library/Commands.hs`

New handler:

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

  -- 4. Reuse existing addRelays function (Commands.hs:3887)
  results <- addRelays user gInfo sLnk relays

  -- 5. Check results
  case partitionEithers (map snd results) of
    ([], _) -> do
      -- Relay connection is asynchronous: invitation sent (RSNew→RSInvited).
      -- When relay responds (RSAccepted) and connects (CON at Subscriber.hs:861-864),
      -- setGroupLinkDataAsync is called automatically to add the relay link.
      -- The LINK callback then promotes RSAccepted→RSActive.
      relays' <- withFastStore $ \db -> liftIO $ getGroupRelays db gInfo
      pure $ CRGroupRelayAdded user gInfo gLink relays'
    _ -> do
      let toRelayResult (r, Left e) = AddRelayResult r (Just e)
          toRelayResult (r, Right _) = AddRelayResult r Nothing
      pure $ CRGroupRelayAddFailed user (map toRelayResult results)
```

Key points:
- Uses `withGroupLock` to prevent concurrent relay modifications.
- Reuses `addRelays` unchanged — it handles the full invitation flow (create relay member, create GroupRelay record, send `XGrpRelayInv`, update status RSNew→RSInvited).
- No synchronous `setGroupLinkData` call needed: the CON event handler calls `setGroupLinkDataAsync` when the relay connects.

### 3. Extend `APIGetUpdatedGroupLinkData` for Subscriber Relay Sync

**File**: `src/Simplex/Chat/Library/Commands.hs` (lines 1777-1787)

Currently this handler fetches link data from the SMP server and updates group profile and member count. It is called by the iOS UI when a non-owner subscriber opens a channel (ChatView.swift:750). The `ConnLinkData` it receives already contains the relay list in `UserContactData.relays`.

Extend the handler to also synchronize relay state:

```
APIGetUpdatedGroupLinkData groupId -> withUser $ \user -> do
  gInfo@GroupInfo {groupProfile = p} <- withFastStore $ \db -> getGroupInfo db vr user groupId
  case p of
    GroupProfile {publicGroup = Just PublicGroupProfile {groupLink = sLnk}} | useRelays' gInfo -> do
      (_, cData@(ContactLinkData _ UserContactData {relays = currentRelayLinks})) <-
        getShortLinkConnReq nm user sLnk
      groupSLinkData_ <- liftIO $ decodeLinkUserData cData
      gInfo' <- case groupSLinkData_ of
        Just sLinkData -> fst <$> updateGroupFromLinkData user gInfo sLinkData
        _ -> pure gInfo
      -- Sync relay state for non-owner subscribers
      when (memberRole' (membership gInfo) /= GROwner) $
        syncSubscriberRelays nm user gInfo' currentRelayLinks
      pure $ CRGroupInfo user gInfo'
    _ -> throwCmdError "group link data not available"
```

**New functions** `syncSubscriberRelays` and `connectToNewRelay` in `Commands.hs` (must be in `processChatCommand`'s scope to access `connectViaContact`, which is a local function in its `where` block at line 3496):

```
syncSubscriberRelays :: NetworkRequestMode -> User -> GroupInfo -> [ShortLinkContact] -> CM ()
syncSubscriberRelays nm user gInfo currentRelayLinks = tryAllErrors $ do
  vr <- chatVersionRange
  -- Get local relay members (all members with GRRelay role, regardless of status)
  localRelayMembers <- withFastStore' $ \db -> getGroupRelayMembers db vr user gInfo
  -- GroupMember.relayLink :: Maybe ShortLinkContact (Types.hs:1041)
  -- Set by getCreateRelayForMember (Store/Groups.hs:1392) when subscriber connects to a relay.
  let activeRelayMembers = filter memberCurrent localRelayMembers
      localRelayLinks = mapMaybe relayLink activeRelayMembers

  -- Discover new relays (in link data but not among active local relay members)
  let newRelayLinks = filter (`notElem` localRelayLinks) currentRelayLinks
  forM_ newRelayLinks $ \rlnk -> tryAllErrors $
    connectToNewRelay nm user gInfo rlnk

  -- Discover removed relays (active local relay member whose link is absent from link data)
  forM_ activeRelayMembers $ \m ->
    case relayLink m of
      Just rlnk | rlnk `notElem` currentRelayLinks ->
        tryAllErrors $ do
          deleteMemberConnection m
          void $ updateMemberRecordDeleted user gInfo m GSMemRemoved
      _ -> pure ()
```

Where `connectToNewRelay` follows the same pattern as `connectToRelay` in `APIConnectPreparedGroup` (Commands.hs:2165-2181):

```
connectToNewRelay :: NetworkRequestMode -> User -> GroupInfo -> ShortLinkContact -> CM ()
connectToNewRelay nm user gInfo relayLink = do
  vr <- chatVersionRange
  gVar <- asks random
  -- getCreateRelayForMember is idempotent — returns existing member if relay_link matches
  relayMember <- withFastStore $ \db -> getCreateRelayForMember db vr gVar user gInfo relayLink
  (fd@FixedLinkData {rootKey = relayKey, linkEntityId}, cData) <-
    getShortLinkConnReq nm user relayLink
  relayLinkData_ <- liftIO $ decodeLinkUserData cData
  case (relayLinkData_, linkEntityId) of
    (Just RelayShortLinkData {relayProfile = p}, Just entityId) ->
      withFastStore $ \db -> updateRelayMemberData db user relayMember (MemberId entityId) (MemberKey relayKey) p
    _ -> throwChatError $ CEException "relay link: no relay link data or entity id"
  let cReq = linkConnReq fd
      relayLinkToConnect = CCLink cReq (Just relayLink)
  -- connectViaContact handles incognito internally for relay groups (Commands.hs:3545-3546):
  -- when PCEGroup gInfo is passed and useRelays' gInfo, it uses incognitoMembershipProfile gInfo
  -- regardless of the incognito parameter.
  void $ connectViaContact user (Just $ PCEGroup gInfo relayMember) False relayLinkToConnect Nothing Nothing
```

**Note on `getCreateRelayForMember` idempotency**: This function queries `WHERE m.relay_link = ?` without filtering by member status (Store/Groups.hs:1379). If a relay was previously removed (GSMemRemoved) and is later re-added by the owner, `getCreateRelayForMember` will return the old removed member. During implementation, verify whether the member status needs to be reset before reconnecting, or whether `connectViaContact` handles this correctly.

### 4. LINK Event Handler — Detect Relay Removal (Owner)

**File**: `src/Simplex/Chat/Library/Subscriber.hs` (lines 1308-1317)

Replace the TODO with relay removal detection. This handles the multi-owner scenario where another owner removes a relay:

```haskell
updateRelay :: DB.Connection -> GroupRelay -> ([GroupRelay], Bool) -> IO ([GroupRelay], Bool)
updateRelay db relay@GroupRelay {relayLink, relayStatus} (acc, changed) =
  case relayLink of
    Just rLink
      | rLink `elem` relayLinks && relayStatus == RSAccepted -> do
          -- Relay link present in link data, promote to active
          relay' <- updateRelayStatus db relay RSActive
          pure (relay' : acc, True)
      | rLink `elem` relayLinks -> pure (relay : acc, changed)
      | relayStatus `elem` [RSAccepted, RSActive, RSInactive] -> do
          -- Relay link ABSENT from link data — removed (e.g., by another owner)
          relay' <- updateRelayStatus db relay RSInactive
          pure (relay' : acc, True)
    _ -> pure (relay : acc, changed)
```

**Why `RSInactive`**: After the owner's own `APIRemoveMembers` call, the relay is already `RSInactive` before `updatePublicGroupData` triggers the LINK callback. The `relayStatus `elem` [RSAccepted, RSActive, RSInactive]` guard will still match `RSInactive` but `updateRelayStatus` is idempotent (RSInactive→RSInactive is a no-op write). The `RSInactive` guard is included to handle the case where another owner removed a relay that was already inactive on this owner's side — the link absence confirms it should stay/become inactive.

In the same-owner removal case: `APIRemoveMembers` → `delMember` sets RSInactive → `updatePublicGroupData` → LINK callback → relay is already RSInactive, `rLink `notElem` relayLinks` matches, `updateRelayStatus db relay RSInactive` is effectively a no-op.

### 5. Relay Self-Check (`runRelayGroupLinkChecks`)

**File**: `src/Simplex/Chat/Library/Commands.hs` (lines 4729-4735)

Implement the stub. When this client acts as a relay for a channel, it periodically verifies its link is present in the group link data:

```
runRelayGroupLinkChecks :: User -> CM ()
runRelayGroupLinkChecks user = do
  vr <- chatVersionRange
  -- Get all groups where this client is a relay (relay_own_status is set and not RSInactive)
  relayGroups <- withFastStore' $ \db -> getRelayOwnGroups db vr user
  forM_ relayGroups $ \gInfo -> tryAllErrors $ do
    case publicGroup (groupProfile gInfo) of
      Just PublicGroupProfile {groupLink = sLnk} -> do
        -- getShortLinkConnReq' returns (FixedLinkData, ConnLinkData m).
        -- ConnLinkData 'CMContact = ContactLinkData VersionRangeSMPA UserContactData
        -- (NOT UserContactLinkData which is for the LINK event's auData)
        (_, ContactLinkData _ UserContactData {relays = relayLinks}) <-
          getShortLinkConnReq' NRMBackground user sLnk
        -- Check if our own relay link is present
        gLink_ <- withFastStore' $ \db -> runExceptT $ getGroupLink db user gInfo
        case gLink_ of
          Right GroupLink {connLinkContact = CCLink _ (Just ourLink)} ->
            if ourLink `elem` relayLinks
              then do
                -- Our link is present — promote to RSActive if still RSAccepted
                gInfo' <- withFastStore' $ \db -> updateRelayOwnStatusFromTo db gInfo RSAccepted RSActive
                when (relayOwnStatus gInfo' /= relayOwnStatus gInfo) $
                  toView $ CEvtGroupRelayUpdated user gInfo' (membership gInfo')
              else do
                -- Our link is ABSENT — we have been removed
                withFastStore' $ \db -> updateRelayOwnStatus_ db gInfo RSInactive
                -- Per RFC: relay should forward "relay is deleted" notification to
                -- connected members, then clean up. The x.grp.mem.del from owner
                -- may also arrive and trigger cleanup independently.
          _ -> pure ()
      _ -> pure ()
```

**New store function** in `Store/Groups.hs`:

```haskell
getRelayOwnGroups :: DB.Connection -> VersionRangeChat -> User -> IO [GroupInfo]
-- SELECT groups WHERE relay_own_status IS NOT NULL AND relay_own_status != 'inactive'
```

**Scheduling**: Add to the existing periodic tasks loop (check patterns for `expireChatItems` or similar periodic functions in `startChat`).

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
         Owner          Relay (self)      Subscriber
    setGroupLinkData   runRelayGroup     syncSubscriber
    (via updatePublic  LinkChecks        Relays (in
     GroupData)                          APIGetUpdated
                                        GroupLinkData)
    Triggers:          Triggers:          Triggers:
    - Add relay        - Periodic         - Opening channel
    - Remove member    - On subscription    (existing UI flow)
    - Profile update
```

**Owner writes** → SMP server is updated → **Relays and Subscribers read** → discover changes → adjust local state.

**Key design principle**: The `XGrpMemDel` message broadcast through other relays is the primary notification mechanism for relay removal. Subscribers receive it promptly via their connected relays. Link data synchronization via `APIGetUpdatedGroupLinkData` is the backup mechanism — it catches cases where the `XGrpMemDel` was missed (subscriber offline, relay connection issues) and handles new relay discovery.

---

## Edge Cases and Failure Recovery

1. **Add relay fails (network)**: `addRelays` handles temporary errors. The relay remains in RSInvited; owner can retry or the relay will process the pending invitation when it comes online.

2. **Removed relay is malicious / refuses to notify subscribers**: Not a problem. `APIRemoveMembers` sends `XGrpMemDel` to all relay members. Other (non-malicious) relays forward it to subscribers. Subscribers learn about the removal regardless of the removed relay's behavior.

3. **Remove relay, all relays offline**: `XGrpMemDel` is queued for delivery. Link data is still updated. Subscribers will discover the change via `APIGetUpdatedGroupLinkData` next time they open the channel.

4. **Owner removes last relay**: Subscribers lose message delivery. Owner must add a new relay. Subscribers will discover the new relay via `syncSubscriberRelays` when they next open the channel.

5. **Relay goes offline permanently**: Owner removes it via `APIRemoveMembers`. New subscribers won't see it in link data. Existing subscribers with connections to this relay will experience connection failures. On next channel open, `syncSubscriberRelays` discovers the relay link is gone and marks it removed locally.

6. **Subscriber discovers new relay via link data**: `connectToNewRelay` follows the same pattern as `APIConnectPreparedGroup`'s `connectToRelay` — calls `getCreateRelayForMember` (idempotent), fetches relay link data, connects via `connectViaContact`.

7. **Concurrent owners adding/removing relays**: The group lock serializes operations within one client. Cross-client conflicts are resolved by the SMP server — the last `setConnShortLink` call wins. Both owners' LINK callbacks see the final state and reconcile.

---

## Implementation Order

1. **Extend `APIRemoveMembers`** — add `GroupRelay` status update to `RSInactive` when removing relay members. Small, contained change in `delMember`.
2. **LINK handler relay-removal detection** — implement the TODO in Subscriber.hs to detect absent relay links.
3. **`APIAddGroupRelay`** — new command, reuses `addRelays`.
4. **`runRelayGroupLinkChecks`** — relay self-check implementation.
5. **Extend `APIGetUpdatedGroupLinkData`** — add `syncSubscriberRelays` for subscriber relay synchronization.
6. **iOS UI** — ChannelRelaysView add/remove buttons, AddGroupRelayView sheet, API functions.

## Files Changed (Backend)

| File | Change |
|------|--------|
| `src/Simplex/Chat/Controller.hs` | Add `APIAddGroupRelay` command; add `CRGroupRelayAdded`, `CRGroupRelayAddFailed` responses |
| `src/Simplex/Chat/Library/Commands.hs` | Extend `delMember` in `APIRemoveMembers` for relay status; implement `APIAddGroupRelay` handler; implement `runRelayGroupLinkChecks`; extend `APIGetUpdatedGroupLinkData`; add `syncSubscriberRelays`, `connectToNewRelay` (must be in `processChatCommand` scope for `connectViaContact` access) |
| `src/Simplex/Chat/Library/Subscriber.hs` | Fix LINK handler relay removal detection |
| `src/Simplex/Chat/Store/Groups.hs` | Add `getRelayOwnGroups` |

## Files Changed (iOS)

| File | Change |
|------|--------|
| `apps/ios/Shared/Model/AppAPITypes.swift` | Add `APIAddGroupRelay` command, `CRGroupRelayAdded`/`CRGroupRelayAddFailed` responses |
| `apps/ios/Shared/Model/SimpleXAPI.swift` | Add `apiAddGroupRelay` function |
| `apps/ios/Shared/Views/Chat/Group/ChannelRelaysView.swift` | Add relay button, swipe-to-delete (calls existing `apiRemoveMembers`) |
| `apps/ios/Shared/Views/Chat/Group/AddGroupRelayView.swift` | NEW: relay selection sheet |
