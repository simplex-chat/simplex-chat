Plan rewritten for conciseness with fresh-context re-evaluation; supersedes earlier revisions.

# Plan: relay refuses to rejoin a channel it left

## 1. Identifier

Gating key: `GroupRelayInvitation.groupLink :: ShortLinkContact` (Types.hs:884-889). Available at `xGrpRelayInv` (Subscriber.hs:1524-1528) before any DB write or network call. The relay already stores this exact value on every `groups` row it processes (column `relay_request_group_link`, M20260222:38), so the rejection lookup is a single SELECT against `groups` keyed on `(user_id, relay_request_group_link)`. Link rotation by the owner bypasses refusal; `publicGroupId` (Types.hs:790) would resist that but is only known after `getShortLinkConnReq'` — defer that gating to a follow-up.

## 2. Storage

New column on `groups`. The refusal is naturally part of the group's existing state on the relay; a separate table was rejected to keep one source of truth and surface refusal in the existing `/gs` listing.

New `RelayRejection` type in `src/Simplex/Chat/Types/Shared.hs` next to `RelayStatus`. Single-constructor enum because the only meaningful state is "the relay has rejected this channel"; absence (`Nothing`) is the default and what operator-allow restores. Matches the shape of the existing `relayOwnStatus :: Maybe RelayStatus` at Types.hs:470:

```haskell
data RelayRejection = RJRejected
  deriving (Eq, Show)

instance TextEncoding RelayRejection where
  textEncode RJRejected = "rejected"
  textDecode "rejected" = Just RJRejected
  textDecode _ = Nothing

instance FromField RelayRejection where fromField = fromTextField_ textDecode
instance ToField RelayRejection where toField = toField . textEncode
$(JQ.deriveJSON (enumJSON $ dropPrefix "RJ") ''RelayRejection)
```

Add `relayRejection :: Maybe RelayRejection` to `GroupInfo` (Types.hs:467-491) next to `relayOwnStatus`. `Nothing` is the default for every group on every install.

New migration `M20260514_relay_rejection`. SQLite:

```sql
ALTER TABLE groups ADD COLUMN relay_rejection TEXT;
CREATE INDEX idx_groups_relay_request_group_link
  ON groups(user_id, relay_request_group_link)
  WHERE relay_request_group_link IS NOT NULL;
```

Postgres:

```sql
ALTER TABLE groups ADD COLUMN relay_rejection TEXT;
CREATE INDEX idx_groups_relay_request_group_link
  ON groups(user_id, relay_request_group_link)
  WHERE relay_request_group_link IS NOT NULL;
```

Column is nullable, no default — `NULL` is the natural "no relay rejection state on this row" sentinel and avoids spurious writes during the ALTER. The index is partial-on-`IS NOT NULL` because most rows on owner-only or p2p installs have no `relay_request_group_link`; the index size stays minimal on those installs and the lookup remains O(log n) on relay installs. Both engines support partial indexes.

Down: `DROP INDEX idx_groups_relay_request_group_link; ALTER TABLE groups DROP COLUMN relay_rejection;`. Tests regenerate `chat_schema.sql`.

Two helpers, added to `src/Simplex/Chat/Store/Groups.hs` alongside the existing `relay_*` helpers:

```haskell
isRelayGroupRefused :: DB.Connection -> User -> ShortLinkContact -> IO Bool
isRelayGroupRefused db User {userId} groupLink =
  fromOnly . head <$> DB.query db
    [sql|
      SELECT EXISTS (
        SELECT 1 FROM groups
        WHERE user_id = ?
          AND relay_request_group_link = ?
          AND relay_rejection = ?
        LIMIT 1
      )
    |]
    (userId, groupLink, RJRejected)

setGroupRelayRejection :: DB.Connection -> User -> GroupId -> Maybe RelayRejection -> IO ()
setGroupRelayRejection db User {userId} groupId rejection = do
  currentTs <- getCurrentTime
  DB.execute db
    "UPDATE groups SET relay_rejection = ?, updated_at = ? WHERE user_id = ? AND group_id = ?"
    (rejection, currentTs, userId, groupId)
```

`isRelayGroupRefused` uses `EXISTS … LIMIT 1` because more than one `groups` row may share `relay_request_group_link` (`createRelayRequestGroup` at Store/Groups.hs:1526 INSERTs unconditionally — pre-existing behavior). If *any* matching row has `relay_rejection = 'rejected'`, the channel is refused. The equality check naturally excludes `NULL` rows.

`setGroupRelayRejection` takes `Maybe RelayRejection` so the same helper writes both directions: `Just RJRejected` on leave (§6), `Nothing` on operator-allow (§7). The `updated_at = ?` clause matches the convention used by `updateRelayOwnStatus_` (Store/Groups.hs:1597) and `updateRelayStatus_` (1444-1447); `groups.updated_at` is defined at chat_schema.sql:137 with `CHECK(updated_at NOT NULL)`.

## 3. Rejection point — `xGrpRelayInv` (Subscriber.hs:1524)

```haskell
xGrpRelayInv :: InvitationId -> VersionRangeChat -> GroupRelayInvitation -> CM ()
xGrpRelayInv invId chatVRange groupRelayInv@GroupRelayInvitation {groupLink} = do
  refused <- withStore' $ \db -> isRelayGroupRefused db user groupLink
  if refused
    then sendRelayRejection
    else do
      initialDelay <- asks $ initialInterval . relayRequestRetryInterval . config
      (_gInfo, _ownerMember) <- withStore $ \db ->
        createRelayRequestGroup db vr user groupRelayInv invId chatVRange initialDelay
      lift $ void $ getRelayRequestWorker True
  where
    sendRelayRejection = do
      let pqSup = PQSupportOff
      subMode <- chatReadVar subscriptionMode
      chatVR <- chatVersionRange
      let chatV = chatVR `peerConnChatVersion` chatVRange
      connId <- withAgent $ \a -> prepareConnectionToAccept a (aUserId user) False invId pqSup
      dm <- encodeConnInfoPQ pqSup chatV XGrpRelayReject
      void $ withAgent $ \a ->
        acceptContact a NRMBackground (aUserId user) connId False invId dm pqSup subMode
      deleteAgentConnectionAsync' connId False
```

**Why synchronous `acceptContact` (not `acceptContactAsync`).** `acceptContactAsync` enqueues a JOIN agent command; the CONF send and the snd-queue creation happen later inside the agent's command worker (Agent.hs:1826-1830). If we immediately call `deleteAgentConnectionAsync' acId True`, `setConnDeleted` runs, `prepareDeleteConnections_` finds zero rcv queues (no JOIN yet), `deleteConn db (Just timeout) connId` finds zero `snd_message_deliveries` and calls `deleteConnRecord`. The connection record is gone before the JOIN worker can send the CONF — the rejection signal is silently dropped.

`acceptContact` (Internal.hs:881-912 precedent; Agent.hs:1437-1442 → `joinConn` 1263 → `joinConnSrv` 1358-1369 for CRContactUri → `sendInvitation` Agent/Client.hs:1796-1799 → `sendOrProxySMPMessage` 1084-1094 → `sendSMPMessage`/`proxySMPMessage`) hands the CONF to the SMP server via a direct SMP client call. The CONF does NOT go through `snd_message_deliveries` — it is transmitted inline. Subsequent `deleteAgentConnectionAsync' connId False` is therefore safe; `waitDelivery=True` would be a no-op because no delivery row exists for this CONF. The cost is one SMP round-trip blocking the receive loop, which the refusal path can absorb.

No chat-layer `Connection` row is persisted for the refused contact — the agent owns the connection state, and `deleteAgentConnectionAsync'` cleans it up.

## 4. Wire format — `XGrpRelayReject`

Empty-payload event, owner-relay direct contact channel only. Not group-signed (the direct contact connection is authenticated at the agent layer). Naming matches the existing `XGrpLinkReject` precedent (Protocol.hs:440, tag:985, string:1043).

`src/Simplex/Chat/Protocol.hs`:

- GADT constructor (after `XGrpRelayNew`, line 446):
  ```haskell
  XGrpRelayReject :: ChatMsgEvent 'Json
  ```
- Tag GADT (after `XGrpRelayNew_`, line 991):
  ```haskell
  XGrpRelayReject_ :: CMEventTag 'Json
  ```
- `strEncode` (line 1049): `XGrpRelayReject_ -> "x.grp.relay.reject"`.
- `strDecode` lookup map (line 1108): `"x.grp.relay.reject" -> XGrpRelayReject_`.
- `toCMEventTag` (line 1163): `XGrpRelayReject -> XGrpRelayReject_`.
- JSON parse (line 1321): `XGrpRelayReject_ -> pure XGrpRelayReject`.
- JSON encode (line 1391): `XGrpRelayReject -> JM.empty` — matches `XGrpLeave -> JM.empty` (1402) and `XDirectDel -> JM.empty` (1379).
- **No** entry in `isForwardedGroupMsg` (485-505) — not forwarded.
- **No** entry in `requiresSignature` (1227-1238) — not a group event.

Older owner clients parse the unknown tag as `XUnknown` (default branch at line 1134) and hit the CONF handler's catch-all `_ -> messageError "CONF from invited member must have x.grp.acpt"`. No state change, no crash; the GroupRelay stays at `RSInvited` — the same end state as today's "relay never responds" mode. The owner UI shows the relay as permanently "invited" with no progress; documented degradation.

`docs/protocol/channels-protocol.md`: insert a `### Relay refusal` subsection between `### Relay addition` (lines 61-73) and `### Subscriber connection` (line 75). Three short paragraphs: (1) trigger — `APILeaveGroup` flips `relay_rejection` to `rejected`; (2) signal — empty-payload `x.grp.relay.reject` over the direct contact channel; (3) owner handling — `GroupRelay` transitions `RSInvited → RSRejected`; final; cleared only by the relay operator running `/relay allow <groupId>`.

## 5. Owner-side state

New `RelayStatus` variant in `src/Simplex/Chat/Types/Shared.hs:81-114`:

```haskell
data RelayStatus = … | RSInactive | RSRejected
relayStatusText RSRejected = "rejected"
-- textEncode/textDecode add "rejected" / RSRejected entries
```

CONF handler arm in `src/Simplex/Chat/Library/Subscriber.hs:760-773` (immediately after the existing `XGrpRelayAcpt` clause):

```haskell
XGrpRelayReject
  | memberRole' membership == GROwner && isRelay m -> do
      relay <- withStore $ \db -> do
        liftIO $ updateGroupMemberStatus db userId m GSMemRejected
        relay <- getGroupRelayByGMId db (groupMemberId' m)
        liftIO $ updateRelayStatusFromTo db relay RSInvited RSRejected
      let m' = m {memberStatus = GSMemRejected}
      deleteMemberConnection m'
      toView $ CEvtGroupRelayUpdated user gInfo m' relay
  | otherwise -> messageError "x.grp.relay.reject: only owner can receive relay rejection"
```

Both `getGroupRelayByGMId` (Store/Groups.hs:1307, returns `ExceptT StoreError IO`) and `updateRelayStatusFromTo` (1438-1442, returns `IO`) are already exported; no new helper. `updateRelayStatusFromTo` is conditional on the current status equalling `RSInvited` — racing CONFs cannot regress an already-rejected or already-active row. `deleteMemberConnection` (Internal.hs:1807-1808) safely no-ops when `activeConn` is `Nothing`. `CEvtGroupRelayUpdated` (Controller.hs:900) carries `(user, groupInfo, member, groupRelay)` — exactly the iOS payload.

`addRelays` (Commands.hs:3942-3976) already persists `GroupRelay` with `RSNew → RSInvited` before sending `XGrpRelayInv`, so by the time `XGrpRelayReject` arrives the row exists. A second user-initiated `addRelays` after rejection creates a fresh row (new `GroupMember`, new `GroupRelay`), independent of the rejected one — there is no automatic retry.

## 6. Refusal write — `APILeaveGroup` (Commands.hs:2919-2935)

```haskell
APILeaveGroup groupId -> withUser $ \user@User {userId} -> do
  gInfo@GroupInfo {membership, groupProfile} <- withFastStore $ \db -> getGroupInfo db vr user groupId
  filesInfo <- withFastStore' $ \db -> getGroupFileInfo db user gInfo
  withGroupLock "leaveGroup" groupId $ do
    cancelFilesInProgress user filesInfo
    msg <- if useRelays' gInfo && isRelay membership
             then leaveChannelRelay gInfo
             else leaveGroupSendMsg user gInfo
    (gInfo', scopeInfo) <- mkLocalGroupChatScope gInfo
    ci <- saveSndChatItem user (CDGroupSnd gInfo' scopeInfo) msg (CISndGroupEvent SGEUserLeft)
    toView $ CEvtNewChatItems user [AChatItem SCTGroup SMDSnd (GroupChat gInfo' scopeInfo) ci]
    deleteGroupLinkIfExists user gInfo'
    withFastStore' $ \db -> updateGroupMemberStatus db userId membership GSMemLeft
    -- NEW: flip relay_rejection on the relay's local groups row
    when (useRelays' gInfo && isRelay membership) $ do
      let GroupProfile {publicGroup} = groupProfile
      case publicGroup of
        Just PublicGroupProfile {} ->
          withFastStore' $ \db -> setGroupRelayRejection db user groupId (Just RJRejected)
        Nothing ->
          throwChatError $ CEInternalError "APILeaveGroup: relay-served channel has no publicGroup"
    pure $ CRLeftMemberUser user gInfo' {membership = membership {memberStatus = GSMemLeft}}
```

The throw is a structural assertion (the placeholder profile is replaced by `updateGroupProfile` inside `getLinkDataCreateRelayLink` at Subscriber.hs:3847 before the relay ever becomes a current member). It catches schema inconsistency without papering over it.

## 7. Operator command — relay side

One API command. Operator discovers rejected channels through the existing `/gs` listing (see §7.2 below); no separate list command.

`src/Simplex/Chat/Controller.hs` (after `APITestChatRelay` at ~408):

```haskell
| APIAllowRelayGroup {groupId :: GroupId}
-- response (after CRGroupRelays at ~737):
| CRRelayGroupAllowed {user :: User, groupInfo :: GroupInfo}
```

Parser entries (`src/Simplex/Chat/Library/Commands.hs:5033+`). `GroupId = Int64` is a type alias (Types.hs:449), so `A.decimal` decodes directly — matches the `APILeaveGroup <$> A.decimal` precedent at Commands.hs:5021:

```haskell
"/_relay allow " *> (APIAllowRelayGroup <$> A.decimal),
"/relay allow "  *> (APIAllowRelayGroup <$> A.decimal),
```

Handler:

```haskell
APIAllowRelayGroup groupId -> withUser $ \user -> do
  gInfo <- withFastStore $ \db -> getGroupInfo db vr user groupId
  withStore' $ \db -> setGroupRelayRejection db user groupId Nothing
  let gInfo' = gInfo {relayRejection = Nothing}
  pure $ CRRelayGroupAllowed user gInfo'
```

Operator-allow writes `NULL` to the column, restoring the row to "no relay rejection state". This is the minimal-state design: the only meaningful value the column ever holds is `'rejected'`. The alternative (storing a `Just RJAllowed` tombstone) was rejected because no caller needs the "previously rejected" history — `addRelays` already creates fresh `GroupRelay` rows on retry (Commands.hs:3942-3976), so audit visibility lives on those rows, not on the cleared `groups` row.

No event to the owner. The owner's next user-initiated `addRelays` succeeds normally. Operator authorization is the chat-relay binary's process-level access; no protocol-level auth needed.

### 7.1 Guard against deleting a rejected group

`APIDeleteChat CTGroup` at Commands.hs:1242-1246 lets the operator delete the group once `memberCurrent membership` is false (post-leave). That path would silently clear the refusal — an accidental `/d` should not undo a moderation decision. Add a guard immediately after the existing `unless canDelete` check (line 1246):

```haskell
when (relayRejection gInfo == Just RJRejected) $
  throwChatError $ CECommandError "cannot delete a rejected channel; run /_relay allow <groupId> first"
```

`checkRelayInactiveGroups` at Commands.hs:4812-4817 only deletes connections (`deleteGroupConnections`), not the `groups` row, so no guard is needed there.

### 7.2 Surface `[rejected]` in `/gs`

`viewGroupsList` in `src/Simplex/Chat/View.hs:1432-1459` renders one line per group. Extend `groupSS`'s destructure to pull `relayRejection` while keeping the existing `GroupSummary {currentMembers}` pattern (used at line 1456 by `memberCount`), and append `[rejected]` between status and alias:

```haskell
groupSS g@GroupInfo { membership
                    , chatSettings = ChatSettings {enableNtfs}
                    , groupSummary = GroupSummary {currentMembers}
                    , relayRejection
                    } =
  case memberStatus membership of
    GSMemInvited -> groupInvitation' g
    s -> membershipIncognito g <> ttyFullGroup g <> viewMemberStatus s <> rejectionSuffix <> alias g
  where
    rejectionSuffix = case relayRejection of
      Just RJRejected -> " [rejected]"
      Nothing         -> ""
    …
```

One token, recognizable in the existing list output. No new column, no template change.

## 8. iOS

No iOS changes from the rev-4 plan — the owner-side `RSRejected` surface is independent of the relay-side storage shape.

`apps/ios/SimpleXChat/ChatTypes.swift:2637-2643 + 2708-2718`:

```swift
public enum RelayStatus: String, Decodable, Equatable, Hashable {
  …
  case rsRejected = "rejected"
}
extension RelayStatus { public var text: LocalizedStringKey {
  switch self { … case .rsRejected: "rejected" }
}}
```

`apps/ios/Shared/Views/NewChat/AddChannelView.swift:487-504` (`relayStatusIndicator`):

```swift
let isRejected = status == .rsRejected
let color: Color =
  connFailed || removed || isRejected ? .red
  : (status == .rsActive ? .green : .yellow)
let text: LocalizedStringKey =
  connFailed ? "failed"
  : memberStatus == .memLeft ? "removed by operator"
  : isRejected ? "rejected"
  : removed ? "removed"
  : status.text
```

`apps/ios/Shared/Views/Chat/Group/GroupMemberInfoView.swift`, inside the existing `Section` after the `Relay address` block at line 195:

```swift
if groupRelay?.relayStatus == .rsRejected {
  infoRow("Status", "rejected by relay operator")
}
```

`ChannelRelaysView.swift` requires no change — the existing fall-through in `ownerRelayStatusText` (line 114-127) to `groupRelays.first(…)?.relayStatus.text` already renders `"rejected"` via the new `RelayStatus.text` case.

`GroupMemberStatus.memRejected` already exists in `apps/ios/SimpleXChat/ChatTypes.swift:3002` (`case memRejected = "rejected"`) and is the JSON-decoded form of Haskell's `GSMemRejected`. No Swift enum change needed; cited here so an iOS-only reviewer doesn't drop the case if the enum is touched.

Per `apps/ios/CODE.md` Change Protocol, the implementer also updates `apps/ios/spec/state.md`, `apps/ios/spec/api.md`, `apps/ios/spec/client/chat-view.md`, `apps/ios/product/views/group-info.md`, `apps/ios/spec/impact.md`, and `apps/ios/product/concepts.md`.

Kotlin/Android/desktop port is a separate PR.

## 9. Tests — `tests/ChatTests/RelayRefused.hs`

All tests use the existing channel harness (`prepareChannel2Relays`, `relayN ##> "/leave #..."`) and block on chat events from the test queue, not `threadDelay`.

- **`testRelayRefuseAfterLeave`** — relay1 leaves; owner re-adds; owner blocks on `CEvtGroupRelayUpdated`; assert `relayStatus == RSRejected`, member `GSMemRejected`, channel link data excludes relay1 (`getConnectedGroupRelays` filters by `relay_status IN (RSAccepted, RSActive)` at Store/Groups.hs:1334). Also: query relay's `groups` table and assert `relay_rejection = 'rejected'`. Deterministic delivery check for the sync-accept-then-delete path.
- **`testRelayAllowAcceptsAgain`** — operator runs `/relay allow <groupId>`; owner re-adds; relay reaches `RSActive`. Relay's `groups.relay_rejection` flips back to `allowed`.
- **`testRelayDoesNotRefuseUnrelatedChannel`** — relay1 leaves channel A; owner of unrelated channel B issues `XGrpRelayInv`; relay1 accepts B; only A's `groups` row has `relay_rejection = 'rejected'`.
- **`testRelayRefuseRaceConcurrentInvitations`** — owner sends two `XGrpRelayInv` for the same channel concurrently after the relay has left; both refuse; relay's `groups` table acquires no placeholder row for the second invitation (both queries match the same rejected row).
- **`testRelayForwardCompatOldOwner`** — owner's `chatVersionRange` excludes `x.grp.relay.reject`; relay refuses; owner emits `messageError` and the GroupRelay row stays at `RSInvited`; no crash.
- **`testRelayDeleteRejectedBlocked`** — relay1 leaves channel A; operator runs `/d #A`; deletion fails with the guard error from §7.1; channel still exists; operator runs `/relay allow <groupId>` then `/d #A`; deletion succeeds.

## 10. Adversarial review

- **Timing side channel** — refusal path takes one synchronous SMP round-trip via `acceptContact`; accepted path is much longer (worker, link-data fetch). Passive SMP-server observation can distinguish refusal from acceptance by latency. SMP server already infers relay-channel relationships from connection patterns; marginal additional leak. Not mitigated; documented residual risk.
- **Information leakage in `XGrpRelayReject`** — empty payload. No reason, no timestamp, no other channel identifiers.
- **Concurrent leave-then-rejoin** — operator-facing contract: invitations arriving before the leave commits locally are processed normally; invitations after are refused. Window bounded by the duration of `withGroupLock "leaveGroup" groupId`. No cross-group lock added.
- **Operator deletes a `RJRejected` group** — blocked at `APIDeleteChat CTGroup` (Commands.hs:1242-1246) per §7.1. Operator must explicitly `/relay allow <id>` before deletion. Mental model: an accidental `/d` doesn't undo a moderation decision.
- **Two concurrent `XGrpRelayInv` for the same rejected channel** — both lookups query the same indexed `groups` row, both find `relay_rejection = 'rejected'`, both refuse. No race in the rejection path.
- **Duplicate `groups` rows for the same `relay_request_group_link`** — pre-existing behavior (`createRelayRequestGroup` INSERTs unconditionally). `isRelayGroupRefused` uses `EXISTS … LIMIT 1` so any one `RJRejected` row blocks future invitations. The delete-guard in §7.1 keeps rejected rows alive against accidental deletion.
- **Schema migration on a relay with unusual group states** — column is nullable with no default; every existing row gets `NULL`. Owner-only groups, business chats, p2p groups all read `Nothing` and never write the column. No upgrade hazard.
- **Migration on a database without `relay_request_group_link` populated** — the partial index `WHERE relay_request_group_link IS NOT NULL` has zero entries on non-relay installs; the lookup path is unused on those installs.
- **Operator-allow vs. concurrent invitation** — UPDATE-SELECT race resolves to either "still refused" (lookup ran first) or "slipped through with accept" (UPDATE ran first); both match operator intent.
- **`getGroupRelayByGMId` failure on owner side** — propagates as `ChatErrorStore`. Cannot happen in normal operation (`addRelays` creates the row before invitation); if it ever does, the error surfaces visibly.
- **Multi-user relay binary** — `groups.user_id` scopes both the lookup and the update. `withUser` for the CLI command. No cross-user pollution.
- **Forward compat (old relay)** — old relay binary lacks the column and the gate. Until migrated, it processes `XGrpRelayInv` as before. Feature is enforced by relay behavior at the binary level.

## 11. Files changed

| File | Change |
|---|---|
| `src/Simplex/Chat/Types/Shared.hs` | `RSRejected` variant + encodings; new `RelayRejection` type |
| `src/Simplex/Chat/Types.hs` | Add `relayRejection` field to `GroupInfo` |
| `src/Simplex/Chat/Protocol.hs` | `XGrpRelayReject` constructor, tag, str enc/dec, JSON enc/dec |
| `src/Simplex/Chat/Store/Groups.hs` | `isRelayGroupRefused`, `setGroupRelayRejection`; read `relay_rejection` into `GroupInfo` |
| `src/Simplex/Chat/Store/SQLite/Migrations/M20260514_relay_rejection.hs` | NEW. ALTER + partial index |
| `src/Simplex/Chat/Store/SQLite/Migrations.hs` | Register migration |
| `src/Simplex/Chat/Store/Postgres/Migrations/M20260514_relay_rejection.hs` | NEW |
| `src/Simplex/Chat/Store/Postgres/Migrations.hs` | Register migration |
| `src/Simplex/Chat/Controller.hs` | `APIAllowRelayGroup` command; `CRRelayGroupAllowed` response |
| `src/Simplex/Chat/Library/Commands.hs` | Parser entries; handler; refusal write in `APILeaveGroup`; delete guard in `APIDeleteChat CTGroup` |
| `src/Simplex/Chat/Library/Subscriber.hs` | Gate in `xGrpRelayInv`; `XGrpRelayReject` arm in CONF handler |
| `src/Simplex/Chat/View.hs` | `[rejected]` suffix in `viewGroupsList` |
| `simplex-chat.cabal` | Register new migration modules |
| `docs/protocol/channels-protocol.md` | Insert "Relay refusal" subsection |
| `apps/ios/SimpleXChat/ChatTypes.swift` | `rsRejected` case + text |
| `apps/ios/Shared/Views/NewChat/AddChannelView.swift` | Red dot + "rejected" in `relayStatusIndicator` |
| `apps/ios/Shared/Views/Chat/Group/GroupMemberInfoView.swift` | "Status: rejected by relay operator" row |
| `tests/ChatTests/RelayRefused.hs` | NEW. Six tests |
| Test list registration | Add the new module |

`chat_schema.sql` is auto-regenerated by tests; do not hand-edit.

## 12. Out of scope

- Kotlin/Android/desktop UI port.
- New alerts, modals, banners, compose-bar changes.
- Refusal triggered by `xGrpMemDel` (owner removing relay).
- Pre-emptive blocking of unseen channels.
- Owner-side independent clear of `RSRejected`.
- `publicGroupId`-keyed refusal (defer to a follow-up that ships its own migration).
- Timing-uniform refusal.
