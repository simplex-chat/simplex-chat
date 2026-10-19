# Relay Management Improvements

## Problem Statement

Channel owners currently can only add relays during channel creation (`APINewPublicGroup`). Once a channel is created, there is no way to:
1. Add a new relay to an existing channel.
2. Remove a relay from an existing channel.
3. Have relays and subscribers automatically detect and synchronize relay state changes.

Several TODO markers in the codebase (`[relays]`) confirm these are planned but unimplemented. The `runRelayGroupLinkChecks` function (Commands.hs:4729) is a stub. The LINK event handler (Subscriber.hs:1308-1309) has a TODO for relay deletion detection. No `APIAddGroupRelays` command exists.

## Solution Summary

### Add relay to existing channel

New `APIAddGroupRelays` command that reuses the existing `addRelays` function (Commands.hs:3887, in `processChatCommand`'s `where` block). The `addRelays` flow is asynchronous: after the invitation is sent (RSNew→RSInvited), the relay responds with its relay link (→RSAccepted), and the CON event handler (Subscriber.hs:861-864) calls `setGroupLinkDataAsync` to publish the new relay link. The LINK callback then promotes RSAccepted→RSActive.

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

### 1. Relay Deactivation on Member Removal

**File**: `src/Simplex/Chat/Library/Internal.hs` (lines 1804-1821)

Two member-removal primitives exist: `deleteOrUpdateMemberRecordIO` (IO, line 1808) and `updateMemberRecordDeleted` (CM, line 1816). Both run in DB context. Relay deactivation belongs inside these functions so it runs in the same transaction as the member status change.

**New helper** in Internal.hs:

```haskell
deactivateRelayIfNeeded :: DB.Connection -> GroupMember -> IO ()
deactivateRelayIfNeeded db m =
  when (isRelay m) $ do
    relay_ <- runExceptT $ getGroupRelayByGMId db (groupMemberId' m)
    forM_ relay_ $ \relay -> void $ updateRelayStatus db relay RSInactive
```

**Extend `deleteOrUpdateMemberRecordIO`** (line 1808):

```haskell
deleteOrUpdateMemberRecordIO db user@User {userId} gInfo m = do
  (gInfo', m') <- deleteSupportChatIfExists db user gInfo m
  checkGroupMemberHasItems db user m' >>= \case
    Just _ -> updateGroupMemberStatus db userId m' GSMemRemoved
    Nothing -> deleteGroupMember db user m'
  deactivateRelayIfNeeded db m
  pure gInfo'
```

**Extend `updateMemberRecordDeleted`** (line 1816):

```haskell
updateMemberRecordDeleted user@User {userId} gInfo m newStatus =
  withStore' $ \db -> do
    (gInfo', m') <- deleteSupportChatIfExists db user gInfo m
    updateGroupMemberStatus db userId m' newStatus
    deactivateRelayIfNeeded db m
    pure gInfo'
```

This covers all four call sites:
- `delMember` in `deleteMemsSend` (Commands.hs:2896) — owner removing relay via `APIRemoveMembers`
- `deleteOrUpdateMemberRecord` in `xGrpMemDel` (Subscriber.hs:3123) — receiving relay deletion notification
- `updateMemberRecordDeleted` in `xGrpMemDel` (Subscriber.hs:3121) — relay deletion with forwarding
- `updateMemberRecordDeleted` in `xGrpLeave` (Subscriber.hs:3168) — relay leaves voluntarily

For subscribers who have no `GroupRelay` records, `getGroupRelayByGMId` returns `Left`, `forM_` on `Left` is a no-op — safe.

**Cleanup**: remove the now-redundant separate relay deactivation in `xGrpLeave` (Subscriber.hs:3169-3172):

```haskell
-- Before:
gInfo' <- updateMemberRecordDeleted user gInfo m GSMemLeft
when (isRelay m) $
  withStore' $ \db -> do
    relay_ <- runExceptT $ getGroupRelayByGMId db (groupMemberId' m)
    forM_ relay_ $ \relay -> void $ updateRelayStatus db relay RSInactive
gInfo'' <- updatePublicGroupData user gInfo'

-- After:
gInfo' <- updateMemberRecordDeleted user gInfo m GSMemLeft
gInfo'' <- updatePublicGroupData user gInfo'
```

**`APIRemoveMembers` requires no changes** — `delMember` (line 2891) already calls `deleteOrUpdateMemberRecordIO` which now handles relay deactivation internally. The `getConnectedGroupRelays` query filters by both `member_status = GSMemConnected` and `relay_status IN (RSAccepted, RSActive)`, so the removed relay is excluded from link data when `updatePublicGroupData` runs (line 2828-2829).

**iOS UI**: The remove button is currently hidden on the relay member info page by an explicit guard in `adminDestructiveSection` (GroupMemberInfoView.swift:646: `mem.memberRole != .relay`). Changes needed:

1. **Remove the relay guard** — change the condition to allow relay members to be removed. The `canBeRemoved()` permission check (ChatTypes.swift:2868) already validates that the user has sufficient role.

2. **Relay-specific button text** — the `removeMemberButton` (line 708) currently shows `"Remove subscriber"` for channels (`groupInfo.useRelays`). Add a relay branch: when `mem.memberRole == .relay`, show `"Remove relay"` instead.

3. **Relay-specific alert text** — `showRemoveMemberAlert` (GroupChatInfoView.swift:926) currently shows `"Remove subscriber?"` / `"Subscriber will be removed from channel"` for channels. Add a relay branch: `"Remove relay?"` / `"Relay will be removed from channel"`.

4. **Last active relay warning** — when removing a relay, check if it's the last active relay (count relay members with `memberCurrent` status in `chatModel.groupMembers`). If so, show a warning: `"This is the last active relay. Removing it will prevent message delivery to subscribers."` The count is available client-side from `chatModel.groupMembers.filter { $0.wrapped.memberRole == .relay && $0.wrapped.memberCurrent }`.

No new API command needed for removal — the existing `apiRemoveMembers` is used.

### 2. New `APIAddGroupRelays` Command

**File**: `src/Simplex/Chat/Controller.hs`

```haskell
-- New command
| APIAddGroupRelays GroupId (NonEmpty Int64)     -- group ID, chat_relay_ids

-- New responses
| CRGroupRelaysAdded { user :: User, groupInfo :: GroupInfo, groupLink :: GroupLink, groupRelays :: [GroupRelay] }
| CRGroupRelaysAddFailed { user :: User, addRelayResults :: [AddRelayResult] }
```

**File**: `src/Simplex/Chat/Library/Commands.hs`

New handler:

```
APIAddGroupRelays groupId relayIds -> withUser $ \user -> withGroupLock "addGroupRelays" groupId $ do
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
      pure $ CRGroupRelaysAdded user gInfo gLink relays'
    _ -> do
      let toRelayResult (r, Left e) = AddRelayResult r (Just e)
          toRelayResult (r, Right _) = AddRelayResult r Nothing
      pure $ CRGroupRelaysAddFailed user (map toRelayResult results)
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

**Parameterize `connectToRelay`** — move from `APIConnectPreparedGroup`'s `where` block to `processChatCommand`'s `where` block so both `APIConnectPreparedGroup` and subscriber sync can use it. The captured closure variables become explicit parameters or are derived internally:

```
-- In processChatCommand's where block (for connectViaContact access).
-- connectViaContact ignores incognito param for relay groups (Commands.hs:3545-3546),
-- using incognitoMembershipProfile gInfo instead.
connectToRelay :: User -> GroupInfo -> ShortLinkContact -> CM (ShortLinkContact, GroupMember, Either ChatError ())
connectToRelay user gInfo relayLink = do
  vr <- chatVersionRange
  gVar <- asks random
  relayMember <- withFastStore $ \db -> getCreateRelayForMember db vr gVar user gInfo relayLink
  r <- tryAllErrors $ do
    (fd@FixedLinkData {rootKey = relayKey, linkEntityId}, cData) <-
      getShortLinkConnReq nm user relayLink
    relayLinkData_ <- liftIO $ decodeLinkUserData cData
    case (relayLinkData_, linkEntityId) of
      (Just RelayShortLinkData {relayProfile = p}, Just entityId) ->
        withFastStore $ \db -> updateRelayMemberData db user relayMember (MemberId entityId) (MemberKey relayKey) p
      _ -> throwChatError $ CEException "relay link: no relay link data or entity id"
    let cReq = linkConnReq fd
        relayLinkToConnect = CCLink cReq (Just relayLink)
    void $ connectViaContact user (Just $ PCEGroup gInfo relayMember) False relayLinkToConnect Nothing Nothing
  relayMember' <- withFastStore $ \db -> getGroupMember db vr user (groupId' gInfo) (groupMemberId' relayMember)
  pure (relayLink, relayMember', r)
```

`getCreateRelayForMember` stays outside `tryAllErrors` — the member must be available for re-read even on failure (for `RelayConnectionResult` reporting). `APIConnectPreparedGroup` calls `mapConcurrently (connectToRelay user gInfo') relays` as before.

**New function** `syncSubscriberRelays` in `processChatCommand`'s scope (reuses `connectToRelay`):

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
    void $ connectToRelay user gInfo rlnk

  -- Discover removed relays (active local relay member whose link is absent from link data)
  forM_ activeRelayMembers $ \m ->
    case relayLink m of
      Just rlnk | rlnk `notElem` currentRelayLinks ->
        tryAllErrors $ do
          deleteMemberConnection m
          void $ updateMemberRecordDeleted user gInfo m GSMemRemoved
      _ -> pure ()
```

**Note on `getCreateRelayForMember` idempotency**: This function queries `WHERE m.relay_link = ?` without filtering by member status (Store/Groups.hs:1379). If a relay was previously removed (GSMemRemoved) and is later re-added by the owner, `getCreateRelayForMember` will return the old removed member. During implementation, verify whether the member status needs to be reset before reconnecting, or whether `connectViaContact` handles this correctly.

### 4. LINK Event Handler — Detect Relay Removal (Owner)

**File**: `src/Simplex/Chat/Library/Subscriber.hs` (lines 1308-1317)

Replace the TODO with relay removal detection. The LINK callback fires when this owner updates link data (via `setGroupLinkData` / `setConnShortLink`). Currently multi-owner channels are not supported, so this only fires after the same owner's own actions (add/remove relay, profile update). When multi-owner support is added, another owner's link data update on the SMP server would need a separate mechanism (e.g., periodic link data fetch or subscription) for this owner to learn about it — the LINK callback only fires in response to this client's own `setConnShortLink` calls.

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
          -- Relay link ABSENT from link data — set to inactive.
          -- TODO [multi-owner] When multi-owner channels are supported, another owner removing
          -- a relay updates link data on the SMP server, but this owner won't receive a LINK
          -- callback for it (LINK only fires in response to own setConnShortLink calls).
          -- A separate mechanism will be needed for cross-owner relay state synchronization.
          relay' <- updateRelayStatus db relay RSInactive
          pure (relay' : acc, True)
    _ -> pure (relay : acc, changed)
```

After the same owner's `APIRemoveMembers` call, the relay is already `RSInactive` before `updatePublicGroupData` triggers the LINK callback. The guard matches `RSInactive` but `updateRelayStatus` is idempotent (RSInactive→RSInactive is a no-op write).

### 5. Relay Self-Check (`runRelayGroupLinkChecks`)

**File**: `src/Simplex/Chat/Library/Commands.hs` (lines 4729-4735)

Implement the stub. The existing `startRelayChecks` (Commands.hs:225-233) already launches `runRelayGroupLinkChecks` as an async task via `relayGroupLinkChecksAsync`. The stub currently does `pure ()` and exits immediately. Replace with a periodic loop following the `cleanupManager` pattern (Commands.hs:4643):

```
runRelayGroupLinkChecks :: User -> CM ()
runRelayGroupLinkChecks user = do
  initialDelay <- asks (initialCleanupManagerDelay . config)
  liftIO $ threadDelay' initialDelay
  interval <- asks (cleanupManagerInterval . config)  -- or a dedicated config field
  forever $ do
    flip catchAllErrors eToView $ do
      lift waitChatStartedAndActivated
      checkRelayGroups
    liftIO $ threadDelay' $ diffToMicroseconds interval
  where
    checkRelayGroups = do
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
    - Add relay        - Periodic check   - Opening channel
    - Remove member                         (existing UI flow)
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

6. **Subscriber discovers new relay via link data**: `syncSubscriberRelays` calls `connectToRelay` (same function used by `APIConnectPreparedGroup`).

---

## Implementation Order

1. **Relay deactivation in member-removal primitives** — add `deactivateRelayIfNeeded` helper to `deleteOrUpdateMemberRecordIO` and `updateMemberRecordDeleted` in Internal.hs; remove redundant code from `xGrpLeave`.
2. **LINK handler relay-removal detection** — implement the TODO in Subscriber.hs to detect absent relay links.
3. **`APIAddGroupRelays`** — new command, reuses `addRelays`.
4. **`runRelayGroupLinkChecks`** — relay self-check implementation.
5. **Extend `APIGetUpdatedGroupLinkData`** — add `syncSubscriberRelays` for subscriber relay synchronization.
6. **iOS UI** — ChannelRelaysView add/remove buttons, AddGroupRelayView sheet, API functions.

## Files Changed (Backend)

| File | Change |
|------|--------|
| `src/Simplex/Chat/Controller.hs` | Add `APIAddGroupRelays` command; add `CRGroupRelaysAdded`, `CRGroupRelaysAddFailed` responses |
| `src/Simplex/Chat/Library/Internal.hs` | Add `deactivateRelayIfNeeded` helper; extend `deleteOrUpdateMemberRecordIO` and `updateMemberRecordDeleted` to call it |
| `src/Simplex/Chat/Library/Commands.hs` | Parameterize and move `connectToRelay` to `processChatCommand` scope; implement `APIAddGroupRelays` handler; implement `runRelayGroupLinkChecks`; extend `APIGetUpdatedGroupLinkData`; add `syncSubscriberRelays` (all in `processChatCommand` scope for `connectViaContact` access) |
| `src/Simplex/Chat/Library/Subscriber.hs` | Fix LINK handler relay removal detection; remove redundant relay deactivation from `xGrpLeave` |
| `src/Simplex/Chat/Store/Groups.hs` | Add `getRelayOwnGroups` |

## Files Changed (iOS)

| File | Change |
|------|--------|
| `apps/ios/Shared/Model/AppAPITypes.swift` | Add `APIAddGroupRelays` command, `CRGroupRelaysAdded`/`CRGroupRelaysAddFailed` responses |
| `apps/ios/Shared/Model/SimpleXAPI.swift` | Add `apiAddGroupRelays` function |
| `apps/ios/Shared/Views/Chat/Group/GroupMemberInfoView.swift` | Remove `.relay` guard from `adminDestructiveSection` (line 646); add relay-specific button/alert text; add last-active-relay warning |
| `apps/ios/Shared/Views/Chat/Group/GroupChatInfoView.swift` | Add relay branch to `showRemoveMemberAlert` text |
| `apps/ios/Shared/Views/Chat/Group/ChannelRelaysView.swift` | Add relay button |
| `apps/ios/Shared/Views/Chat/Group/AddGroupRelayView.swift` | NEW: relay selection sheet |
