# Plan: Channel Subscriber Counts

## Context

Channel subscribers cannot see how many people are in a channel. Due to channel topology (subscribers connect only to the relay, not to each other), subscribers only discover other members when those members perform visible actions. This means most subscribers are invisible to each other, giving no indication of channel size.

Users need subscriber counts to:
- Evaluate channels before joining (is this channel active?)
- Understand the audience size of channels they're in
- Channel owners want to see their reach

This feature adds:
1. Subscriber count embedded in the channel's short link data (visible pre-join)
2. Owner maintains the count on join/leave/removal events
3. Subscribers refresh the count by re-resolving the short link when opening the channel

## Part 1: Data Types

### 1a. New `PublicGroupData` type + extend `GroupShortLinkData`

**File**: `src/Simplex/Chat/Protocol.hs:1421-1428`

```haskell
data PublicGroupData = PublicGroupData
  { publicMemberCount :: Int64
  }
  deriving (Eq, Show)

data GroupShortLinkData = GroupShortLinkData
  { groupProfile :: GroupProfile,
    publicGroupData :: Maybe PublicGroupData
  }
  deriving (Show)

$(JQ.deriveJSON defaultJSON ''PublicGroupData)
$(JQ.deriveJSON defaultJSON ''GroupShortLinkData)
```

`publicGroupData :: Maybe PublicGroupData` (not bare `PublicGroupData`) for backward compatibility — old clients decoding new link data must handle the missing field gracefully. With `defaultJSON` the field is optional when missing from JSON, decoding to `Nothing`.

### 1b. Extend `GroupSummary`

**File**: `src/Simplex/Chat/Types.hs:534-537`

```haskell
data GroupSummary = GroupSummary
  { currentMembers :: Int64,
    publicMemberCount :: Maybe Int64
  }
  deriving (Eq, Show)
```

Update `omittedField` (Types.hs:2039):
```haskell
omittedField = Just GroupSummary {currentMembers = 0, publicMemberCount = Nothing}
```

### 1c. DB column — add `public_member_count` to existing chat_relays migration

**File**: `src/Simplex/Chat/Store/SQLite/Migrations/M20260222_chat_relays.hs`

Add to the existing migration (after `member_priv_key` line):
```sql
ALTER TABLE groups ADD COLUMN public_member_count INTEGER;
```

**File**: `src/Simplex/Chat/Store/Postgres/Migrations/M20260222_chat_relays.hs`

Same addition to the Postgres variant.

Also add to `down_m20260222_chat_relays`:
```sql
ALTER TABLE groups DROP COLUMN public_member_count;
```

No new migration file — this column belongs with the chat_relays feature.

### 1d. Update `toGroupInfo` to read new column

**File**: `src/Simplex/Chat/Store/Shared.hs`

Add `public_member_count` to `groupInfoQueryFields` SQL (line 763, after `summary_current_members_count`).

Update `GroupInfoRow` type (line 668) to include `Maybe Int64` for the new column.

Update `toGroupInfo` (line 682):
```haskell
groupSummary = GroupSummary {currentMembers, publicMemberCount}
```

### 1e. Update hardcoded `GroupSummary` constructors

**File**: `src/Simplex/Chat/Store/Groups.hs`

```haskell
-- line 397: GroupSummary 1 → GroupSummary {currentMembers = 1, publicMemberCount = Nothing}
-- line 476: GroupSummary 2 → GroupSummary {currentMembers = 2, publicMemberCount = Nothing}
```

### 1f. Swift types

**File**: `apps/ios/SimpleXChat/ChatTypes.swift`

Add `GroupSummary` struct:
```swift
public struct GroupSummary: Decodable, Hashable {
    public var currentMembers: Int64
    public var publicMemberCount: Int64?
}
```

Add to `GroupInfo`:
```swift
public var groupSummary: GroupSummary
```

Add `PublicGroupData` struct:
```swift
public struct PublicGroupData: Decodable, Hashable {
    public var publicMemberCount: Int64
}
```

Extend `GroupShortLinkData`:
```swift
public struct GroupShortLinkData: Codable, Hashable {
    public var groupProfile: GroupProfile
    public var publicGroupData: PublicGroupData?
}
```

## Part 2: Owner Maintains Count

### 2a. DB function `updatePublicMemberCount`

**File**: `src/Simplex/Chat/Store/Groups.hs`

```haskell
-- | Computes public member count = total "current" members minus relay members.
-- "Current" statuses match the group_member_status_predicates trigger table
-- (introduced, intro-inv, accepted, announced, connected, complete, creator).
updatePublicMemberCount :: DB.Connection -> VersionRangeChat -> User -> GroupInfo -> ExceptT StoreError IO GroupInfo
updatePublicMemberCount db vr user gInfo@GroupInfo {groupId} = do
  liftIO $ do
    [Only totalCount] <- DB.query db
      "SELECT summary_current_members_count FROM groups WHERE group_id = ?"
      (Only groupId)
    [Only relayCount] <- DB.query db
      [sql|SELECT COUNT(*) FROM group_members
           WHERE group_id = ? AND member_role = ?
             AND member_status IN (?,?,?,?,?,?,?)|]
      (groupId, GRRelay, GSMemIntroduced, GSMemIntroInvited, GSMemAccepted, GSMemAnnounced, GSMemConnected, GSMemComplete, GSMemCreator)
    let publicCount = totalCount - relayCount
    currentTs <- getCurrentTime
    DB.execute db
      "UPDATE groups SET public_member_count = ?, updated_at = ? WHERE group_id = ?"
      (publicCount, currentTs, groupId)
  getGroupInfo db vr user groupId
```

Returns updated `GroupInfo` by re-reading from DB after the update. Uses `ExceptT StoreError IO` since `getGroupInfo` returns in that monad.

### 2b. Helper to update count + link data

**File**: `src/Simplex/Chat/Library/Internal.hs`

```haskell
updatePublicGroupData :: User -> GroupInfo -> CM GroupInfo
updatePublicGroupData user gInfo
  | useRelays' gInfo && memberRole' (membership gInfo) == GROwner = do
      vr <- chatVersionRange
      gInfo' <- withStore $ \db -> updatePublicMemberCount db vr user gInfo
      updatePublicGroupLinkDataAsync user gInfo'
      pure gInfo'
  | otherwise = pure gInfo
```

Single inverted condition: only owner of a channel updates. Re-reads `GroupInfo` from DB (via `updatePublicMemberCount`).

### 2c. Update owner event handlers

**xGrpMemNew** (Subscriber.hs:2753): After member creation, call `updatePublicGroupData`. Must be called in BOTH the unknown→announced branch and the new member branch, but NOT in the duplicate branch (which returns early):
```haskell
-- In both `Right unknownMember` and `Left _` branches, after memberAnnouncedToView:
void $ updatePublicGroupData user gInfo'
```
(`updatePublicGroupData` internally guards on `useRelays' && memberRole == GROwner`, so it's safe to call unconditionally.)

**xGrpLeave** (Subscriber.hs:3028): After `updateMemberRecordDeleted`:
```haskell
gInfo' <- updateMemberRecordDeleted user gInfo m GSMemLeft
gInfo'' <- updatePublicGroupData user gInfo'
```

**xGrpMemDel** (Subscriber.hs:2954): After member status update:
```haskell
-- After deleteOrUpdateMemberRecord / updateMemberRecordDeleted
gInfo'' <- updatePublicGroupData user gInfo'
```

**APIRemoveMembers** (Commands.hs:2646): After all deletions. `updatePublicGroupData` re-reads `GroupInfo` internally, so no separate `getGroupInfo` call is needed:
```haskell
gInfo' <- updatePublicGroupData user gInfo
-- use gInfo' in response
```

## Part 3: Async Mechanism for Link Data Updates

### 3a. Keep `CFSetShortLink` — no new command tag

Keep the existing `CFSetShortLink` command function as-is. Both relay-initiated and count-initiated link data updates use the same `CFSetShortLink` tag.

`setAgentConnShortLinkAsync` signature unchanged — it already hardcodes `CFSetShortLink`.

### 3b. Add `updatePublicGroupLinkDataAsync`

**File**: `src/Simplex/Chat/Library/Internal.hs`

```haskell
updatePublicGroupLinkDataAsync :: User -> GroupInfo -> CM ()
updatePublicGroupLinkDataAsync user gInfo = do
  vr <- chatVersionRange
  (conn, groupRelays) <- withStore $ \db ->
    (,) <$> getGroupLinkConnection db vr user gInfo <*> liftIO (getGroupRelays db gInfo)
  gLink <- withStore $ \db -> getGroupLink db user gInfo
  let (userLinkData, crClientData) = groupLinkData gInfo gLink groupRelays
  setAgentConnShortLinkAsync user conn userLinkData (Just crClientData)
```

This reuses `groupLinkData` to construct the full link data (including the updated `publicGroupData`).

### 3c. Update `groupLinkData` to include `publicGroupData`

**File**: `src/Simplex/Chat/Library/Internal.hs:1315-1322`

```haskell
groupLinkData gInfo@GroupInfo {groupProfile, groupSummary = GroupSummary {publicMemberCount}} GroupLink {groupLinkId} groupRelays =
  let direct = not $ useRelays' gInfo
      relays = mapMaybe (\GroupRelay {relayLink} -> relayLink) groupRelays
      publicGroupData_ = PublicGroupData <$> publicMemberCount
      userData = encodeShortLinkData $ GroupShortLinkData {groupProfile, publicGroupData = publicGroupData_}
      userLinkData = UserContactLinkData UserContactData {direct, owners = [], relays, userData}
      crClientData = encodeJSON $ CRDataGroup groupLinkId
   in (userLinkData, crClientData)
```

### 3d. Rename `CEvtGroupLinkRelaysUpdated` → `CEvtGroupLinkDataUpdated` with `relaysChanged` flag

**File**: `src/Simplex/Chat/Controller.hs:857`

```haskell
CEvtGroupLinkDataUpdated {user :: User, groupInfo :: GroupInfo, groupLink :: GroupLink, groupRelays :: [GroupRelay], relaysChanged :: Bool}
```

**File**: `src/Simplex/Chat/Library/Subscriber.hs:1230-1256`

In the `CFSetShortLink` continuation handler, track whether any relay actually changed status in the `updateRelay` loop:

```haskell
CFSetShortLink ->
  case (ucGroupId_, auData) of
    (Just groupId, UserContactLinkData UserContactData {relays = relayLinks}) -> do
      (gInfo, gLink, relays, relaysChanged) <- withStore $ \db -> do
        gInfo <- getGroupInfo db vr user groupId
        gLink <- getGroupLink db user gInfo
        relays <- liftIO $ getGroupRelays db gInfo
        (relays', changed) <- liftIO $ updateRelays db relays
        liftIO $ setGroupInProgressDone db gInfo
        pure (gInfo, gLink, relays', changed)
      toView $ CEvtGroupLinkDataUpdated user gInfo gLink relays relaysChanged
      where
        updateRelays :: DB.Connection -> [GroupRelay] -> IO ([GroupRelay], Bool)
        updateRelays db = foldM updateRelay' ([], False)
          where
            updateRelay' (!acc, !changed) relay@GroupRelay {relayLink, relayStatus} =
              case relayLink of
                Just rLink
                  | rLink `elem` relayLinks && relayStatus == RSAccepted -> do
                      relay' <- updateRelayStatus db relay RSActive
                      pure (acc <> [relay'], True)
                _ -> pure (acc <> [relay], changed)
    _ -> throwChatError $ CECommandError "LINK event expected for a group link only"
```

**File**: `src/Simplex/Chat/View.hs:468, 1177-1188`

Mute CLI output when `relaysChanged = False`. Keep `viewGroupLinkRelaysUpdated` name (it only prints for relay changes):
```haskell
-- line 468:
CEvtGroupLinkDataUpdated u g groupLink relays relaysChanged
  | relaysChanged -> ttyUser u $ viewGroupLinkRelaysUpdated g groupLink relays
  | otherwise -> []
```
No rename of `viewGroupLinkRelaysUpdated` — it prints relay-specific info and is only called when relays changed.

**File**: `bots/src/API/Docs/Events.hs:101`

Rename reference from `CEvtGroupLinkRelaysUpdated` to `CEvtGroupLinkDataUpdated`. Update description to "Group link data updated."

## Part 4: Subscriber Receives and Refreshes Count

### 4a. Save count on `APIPrepareGroup`

**File**: `src/Simplex/Chat/Library/Commands.hs:1911-1926`

`APIPrepareGroup` already receives `groupSLinkData :: GroupShortLinkData`. After `createPreparedGroup`, save the `publicMemberCount` if present:

```haskell
APIPrepareGroup userId ccLink direct groupSLinkData -> withUserId userId $ \user -> do
  let GroupShortLinkData {groupProfile = gp@GroupProfile {description}, publicGroupData = publicGroupData_} = groupSLinkData
  ...
  (gInfo, hostMember_) <- withStore $ \db -> createPreparedGroup db gVar vr user gp False ccLink welcomeSharedMsgId useRelays subRole
  -- Save public member count from short link data
  gInfo' <- case publicGroupData_ of
    Just PublicGroupData {publicMemberCount} ->
      withFastStore $ \db -> updateGroupPublicMemberCount db vr user gInfo publicMemberCount
    Nothing -> pure gInfo
  ... -- use gInfo' instead of gInfo for the rest
```

### 4b. Update count on `APIConnectPreparedGroup`

**File**: `src/Simplex/Chat/Library/Commands.hs:1994-2035`

`APIConnectPreparedGroup` already calls `getShortLinkConnReq` (line 2002) which returns fresh link data. After resolving, extract and save the updated count:

Extract `publicMemberCount_` from fresh link data and pass it through `updatePreparedRelayedGroup` → `setPreparedGroupLinkInfo_`:

```haskell
(FixedLinkData {linkConnReq = mainCReq@(CRContactUri crData), linkEntityId, rootKey}, cData@(ContactLinkData _ UserContactData {owners, relays})) <- getShortLinkConnReq nm user sLnk
groupSLinkData_ <- liftIO $ decodeLinkUserData cData
let publicMemberCount_ = groupSLinkData_ >>= publicGroupData >>= Just . publicMemberCount
```

Pass `publicMemberCount_` to `updatePreparedRelayedGroup` which forwards it to `setPreparedGroupLinkInfo_` (same mechanism as Part 4a).

### 4c. Subscriber refresh API — new command

**File**: `src/Simplex/Chat/Controller.hs`

Add to `ChatCommand`:
```haskell
APIGetUpdatedGroupLinkData GroupId
```

### 4d. Command parser

**File**: `src/Simplex/Chat/Library/Commands.hs` (parser section)

```haskell
"/_get group link data #" *> (APIGetUpdatedGroupLinkData <$> A.decimal)
```

### 4e. Handler implementation

**File**: `src/Simplex/Chat/Library/Commands.hs`

Uses `groupLink` from `GroupProfile` (already guaranteed to be a `ShortLinkContact` for channels):

```haskell
APIGetUpdatedGroupLinkData groupId -> withUser $ \user -> do
  gInfo@GroupInfo {groupProfile = GroupProfile {groupLink = groupLink_}} <- withFastStore $ \db -> getGroupInfo db vr user groupId
  case groupLink_ of
    Just sLnk | useRelays' gInfo -> do
      (_, cData) <- getShortLinkConnReq nm user sLnk
      groupSLinkData_ <- liftIO $ decodeLinkUserData cData
      case groupSLinkData_ of
        Just GroupShortLinkData {publicGroupData = Just PublicGroupData {publicMemberCount}} -> do
          gInfo' <- withFastStore $ \db -> updateGroupPublicMemberCount db vr user gInfo publicMemberCount
          pure $ CRGroupInfo user gInfo'
        _ -> throwCmdError "no public group data in short link"
    _ -> throwCmdError "group link data not available"
```

Errors propagate naturally — `getShortLinkConnReq` throws on network failure, store errors throw on DB failure. UI ignores errors and doesn't update group info.

### 4f. DB function for subscriber-side refresh

**File**: `src/Simplex/Chat/Store/Groups.hs`

Used only by `APIGetUpdatedGroupLinkData` (Part 4e):
```haskell
updateGroupPublicMemberCount :: DB.Connection -> VersionRangeChat -> User -> GroupInfo -> Int64 -> ExceptT StoreError IO GroupInfo
updateGroupPublicMemberCount db vr user gInfo@GroupInfo {groupId} publicCount = do
  currentTs <- liftIO getCurrentTime
  liftIO $ DB.execute db
    "UPDATE groups SET public_member_count = ?, updated_at = ? WHERE group_id = ?"
    (publicCount, currentTs, groupId)
  getGroupInfo db vr user groupId
```

### 4g. Response type

Use existing `CRGroupInfo` response (Controller.hs:672), which returns the updated `GroupInfo` containing `groupSummary` with the new `publicMemberCount`.

## Part 5: Haskell Tests

### 5a. Helper to query `public_member_count`

**File**: `tests/ChatTests/Utils.hs`

```haskell
checkPublicMemberCount :: TestCC -> String -> IO (Maybe Int64)
checkPublicMemberCount cc groupName = withCCTransaction cc $ \db -> do
  r <- DB.query db
    "SELECT public_member_count FROM groups WHERE local_display_name = ?"
    (Only groupName) :: IO [Only (Maybe Int64)]
  pure $ case r of
    [Only cnt] -> cnt
    _ -> Nothing
```

### 5b. Add checks to existing tests

Subscribers already receive `public_member_count` at join time (saved in `APIPrepareGroup` from `GroupShortLinkData`, updated in `APIConnectPreparedGroup`). So after joining, the subscriber's DB already has the correct count — no API call needed.

The `/_get group link data` API is only needed to refresh stale counts after membership changes (leave/remove) that happened while the subscriber was already in the channel.

**testChannels1RelayDeliver** (Groups.hs:8397): After all subscribers have joined and messages are delivered. With alice (owner), bob (relay), cath, dan, eve as subscribers, expect count = 4 (owner + 3 subscribers, excluding relay):
```haskell
-- Owner's count is maintained automatically:
checkPublicMemberCount alice "channel" >>= (`shouldBe` Just 4)
-- Subscriber already has count from join flow (APIPrepareGroup / APIConnectPreparedGroup):
checkPublicMemberCount cath "channel" >>= (`shouldBe` Just 4)
```

**testChannelRemoveMemberSigned** (Groups.hs:8909): Check before and after member removal:
```haskell
-- Before removal — owner and subscriber both have count:
checkPublicMemberCount alice "channel" >>= (`shouldBe` Just 4)
checkPublicMemberCount cath "channel" >>= (`shouldBe` Just 4)
-- After removal — owner updated automatically, subscriber refreshes:
checkPublicMemberCount alice "channel" >>= (`shouldBe` Just 3)
cath ##> "/_get group link data #1"
checkPublicMemberCount cath "channel" >>= (`shouldBe` Just 3)
```

**testChannelSubscriberLeave** (Groups.hs:9049): Check before any leave, after 1 leave, after 2 leaves:
```haskell
-- Before any leave — both have count:
checkPublicMemberCount alice "channel" >>= (`shouldBe` Just 4)
checkPublicMemberCount cath "channel" >>= (`shouldBe` Just 4)
-- After 1 subscriber leaves — owner updated, subscriber refreshes:
checkPublicMemberCount alice "channel" >>= (`shouldBe` Just 3)
cath ##> "/_get group link data #1"
checkPublicMemberCount cath "channel" >>= (`shouldBe` Just 3)
-- After 2nd subscriber leaves — owner updated, subscriber refreshes:
checkPublicMemberCount alice "channel" >>= (`shouldBe` Just 2)
cath ##> "/_get group link data #1"
checkPublicMemberCount cath "channel" >>= (`shouldBe` Just 2)
```

Note: `##>` sends the command and consumes the echo. `CRGroupInfo` produces view output (`"group ID: ..."`, `"current members: ..."`); additional output lines may need to be consumed with `<##` depending on the exact CLI view output. Exact assertions will be determined during test iteration.

### 5c. Build and iterate

Build and run the specific tests until they pass:
```bash
cabal build --ghc-options=-O0
cabal test simplex-chat-test --test-options='-m "channels 1 relay deliver"'
cabal test simplex-chat-test --test-options='-m "channel remove member signed"'
cabal test simplex-chat-test --test-options='-m "channel subscriber leave"'
```

Fix any failures and re-run until all pass.

## Part 6: iOS UI

### 6a. Pre-join alert — show subscriber count

**File**: `apps/ios/Shared/Views/NewChat/NewChatView.swift:1100-1148` (`showPrepareGroupAlert`)

The function already receives `groupShortLinkData: GroupShortLinkData`. Add count display:
```swift
if let pgd = groupShortLinkData.publicGroupData, pgd.publicMemberCount > 0 {
    Text("\(pgd.publicMemberCount) subscribers")
        .foregroundColor(theme.colors.secondary)
}
```

Similarly in `showOpenKnownGroupAlert` (line 1176), which receives `GroupInfo`:
```swift
if let count = groupInfo.groupSummary.publicMemberCount, count > 0 {
    Text("\(count) subscribers")
        .foregroundColor(theme.colors.secondary)
}
```

### 6b. ChatView top bar — show subscriber count

**File**: `apps/ios/Shared/Views/Chat/ChatInfoToolbar.swift`

For channels, show subscriber count below the channel name:
```swift
if case let .group(groupInfo, _) = chat.chatInfo,
   groupInfo.useRelays,
   let count = groupInfo.groupSummary.publicMemberCount,
   count > 0 {
    Text("\(count) subscribers")
        .font(.caption)
        .foregroundColor(theme.colors.secondary)
}
```

### 6c. Trigger refresh on ChatView open

**File**: `apps/ios/Shared/Views/Chat/ChatView.swift:689-698`

In the existing channel-specific block that loads group members:
```swift
if case let .group(groupInfo, _) = cInfo, groupInfo.useRelays {
    Task { await chatModel.loadGroupMembers(groupInfo) }
    // Refresh subscriber count for non-owner
    if groupInfo.membership.memberRole != .owner {
        Task {
            if let gInfo = await apiGetUpdatedGroupLinkData(groupInfo.groupId) {
                await MainActor.run {
                    chatModel.updateGroup(gInfo)
                }
            }
        }
    }
    // existing owner relay loading...
}
```

### 6d. Add `groupInfo` case to Swift `ChatResponse0`

**File**: `apps/ios/Shared/Model/AppAPITypes.swift`

`CRGroupInfo` exists in Haskell (Controller.hs:672) but not in iOS. Add to `ChatResponse0` (after `contactInfo` on line 675):
```swift
case groupInfo(user: UserRef, groupInfo: GroupInfo)
```
Add `responseType`, `details` entries following the `contactInfo` pattern.

### 6e. Swift API function

**File**: `apps/ios/Shared/Model/SimpleXAPI.swift`

```swift
func apiGetUpdatedGroupLinkData(_ groupId: Int64) async -> GroupInfo? {
    let r: APIResult<ChatResponse0>? = await chatApiSendCmd(.apiGetUpdatedGroupLinkData(groupId: groupId))
    if case let .result(.groupInfo(_, groupInfo)) = r { return groupInfo }
    return nil
}
```

**File**: `apps/ios/SimpleXChat/APITypes.swift` — add command encoding:
```swift
case apiGetUpdatedGroupLinkData(groupId: Int64)
// cmdString: "/_get group link data #\(groupId)"
```

## Part 7: Implementation Order

1. **Part 1** — Data types (Haskell types, migration column, Swift types)
2. **Part 3** — Async mechanism (update `groupLinkData`, add `updatePublicGroupLinkDataAsync`, rename event, update continuation handler)
3. **Part 2** — Owner count maintenance (DB function, helper, event handler updates)
4. **Part 4** — Subscriber refresh API
5. **Build** — `cabal build --ghc-options=-O0`, fix compile errors
6. **Part 5** — Haskell tests (add checks, iterate with `cabal test` until passing)
7. **Part 6** — iOS UI
8. **Final build + test**

## Files to Modify

| File | Changes |
|---|---|
| `src/Simplex/Chat/Protocol.hs` | Add `PublicGroupData` type, extend `GroupShortLinkData` |
| `src/Simplex/Chat/Types.hs` | Extend `GroupSummary` with `publicMemberCount` |
| `src/Simplex/Chat/Controller.hs` | Add `APIGetUpdatedGroupLinkData` command, rename `CEvtGroupLinkRelaysUpdated` → `CEvtGroupLinkDataUpdated` with `relaysChanged` |
| `src/Simplex/Chat/Library/Internal.hs` | `updatePublicGroupData`, `updatePublicGroupLinkDataAsync`, update `groupLinkData` |
| `src/Simplex/Chat/Library/Subscriber.hs` | Track `relaysChanged` in continuation handler, update event handlers (`xGrpMemNew`, `xGrpLeave`, `xGrpMemDel`) |
| `src/Simplex/Chat/Library/Commands.hs` | `APIGetUpdatedGroupLinkData` handler + parser, save count in `APIPrepareGroup` and `APIConnectPreparedGroup`, update `APIRemoveMembers` |
| `src/Simplex/Chat/Store/Groups.hs` | `updatePublicMemberCount`, `updateGroupPublicMemberCount`, update hardcoded `GroupSummary` constructors |
| `src/Simplex/Chat/Store/Shared.hs` | Update `toGroupInfo`, `GroupInfoRow` type, SQL query |
| `src/Simplex/Chat/Store/SQLite/Migrations/M20260222_chat_relays.hs` | Add `public_member_count` column |
| `src/Simplex/Chat/Store/Postgres/Migrations/M20260222_chat_relays.hs` | Add `public_member_count` column |
| `src/Simplex/Chat/View.hs` | Update event pattern, mute output when `relaysChanged = False` |
| `bots/src/API/Docs/Events.hs` | Rename event reference |
| `apps/ios/SimpleXChat/ChatTypes.swift` | `GroupSummary`, `PublicGroupData`, extend `GroupShortLinkData`, `groupSummary` in `GroupInfo` |
| `apps/ios/SimpleXChat/APITypes.swift` | `apiGetUpdatedGroupLinkData` command |
| `apps/ios/Shared/Model/AppAPITypes.swift` | Add `groupInfo` case to `ChatResponse0` |
| `apps/ios/Shared/Model/SimpleXAPI.swift` | `apiGetUpdatedGroupLinkData` function |
| `apps/ios/Shared/Views/NewChat/NewChatView.swift` | Show count in prepare/known alerts |
| `apps/ios/Shared/Views/Chat/ChatInfoToolbar.swift` | Show count in top bar |
| `apps/ios/Shared/Views/Chat/ChatView.swift` | Trigger refresh on channel open |
| `tests/ChatTests/Utils.hs` | `checkPublicMemberCount` helper |
| `tests/ChatTests/Groups.hs` | Add count checks to `testChannels1RelayDeliver`, `testChannelRemoveMemberSigned`, `testChannelSubscriberLeave` |

## Verification

1. Build: `cabal build --ghc-options=-O0`
2. Run Haskell tests and iterate until passing:
   - `cabal test simplex-chat-test --test-options='-m "channels 1 relay deliver"'`
   - `cabal test simplex-chat-test --test-options='-m "channel remove member signed"'`
   - `cabal test simplex-chat-test --test-options='-m "channel subscriber leave"'`
3. iOS UI manual verification:
   - Scan channel short link → see count in pre-join alert
   - Owner sees subscriber count in ChatView top bar
   - Subscriber sees count in ChatView top bar after refresh
   - Subscriber leaves → owner count decreases
   - Owner removes member → owner count decreases
