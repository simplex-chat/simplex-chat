Plan rewritten for conciseness with fresh-context re-evaluation; supersedes earlier revisions.

# Plan: relay refuses to rejoin a channel it left

## 1. Identifier

Gating key: `GroupRelayInvitation.groupLink :: ShortLinkContact` (Types.hs:884-889). Available at `xGrpRelayInv` (Subscriber.hs:1524-1528) before any DB write or network call. The relay already stores this value on every `groups` row it processes (column `relay_request_group_link`, M20260222:38), and the existing `relay_own_status` column already carries the relay's lifecycle for the channel — refusal slots into that state machine as a new `RSRejected` variant. Lookup is a single SELECT against `groups`. Link rotation by the owner bypasses refusal; `publicGroupId` (Types.hs:790) would resist that but is only known after `getShortLinkConnReq'` — defer that gating to a follow-up.

## 2. Storage

No new column, no new type, no new field on `GroupInfo`. The existing `relay_own_status TEXT` (M20260222:37) is the carrier.

`RelayStatus` (`src/Simplex/Chat/Types/Shared.hs:81-114`) gains an `RSRejected` constructor (encoded as `"rejected"`). It is reused on both sides: on the relay it is the row's own state after `APILeaveGroup`; on the owner it is the `GroupRelay.relayStatus` after `XGrpRelayReject` arrives in §5.

State-machine slot for `RSRejected` on the relay:

- `updateRelayOwnStatus_` (Store/Groups.hs:1593-1597) writes `relay_inactive_at = Just currentTs` only when the new status is `RSInactive`. `RSRejected` therefore correctly leaves `relay_inactive_at = NULL`, so the row is NOT eligible for `checkRelayInactiveGroups` cleanup (Commands.hs:4812-4817).
- `checkRelayServedGroups` (Commands.hs:4795-4810) iterates only `getRelayServedGroups` rows — `relay_own_status IN (RSAccepted, RSActive)` (Store/Groups.hs:1607). RSRejected rows are not iterated.
- `xGrpMemDel` writer at Subscriber.hs:3132 currently flips any non-NULL `relay_own_status` to `RSInactive` when the owner removes the relay member. That would silently regress `RSRejected → RSInactive` and let a subsequent `XGrpRelayInv` slip through (the lookup checks `'rejected'`). The write at line 3132 is tightened to skip when the row is already `RSRejected`:

```haskell
when (maybe False (/= RSRejected) (relayOwnStatus gInfo)) $
  updateRelayOwnStatus_ db gInfo RSInactive
```

New migration `M20260514_relay_request_group_link_index` adds a partial index — the column is unindexed today and the new gate SELECTs on it. SQLite:

```sql
CREATE INDEX idx_groups_relay_request_group_link
  ON groups(user_id, relay_request_group_link)
  WHERE relay_request_group_link IS NOT NULL;
```

Postgres mirror. Partial-on-`IS NOT NULL` because most rows on owner-only or p2p installs leave the column NULL. Both engines support partial indexes. Down: `DROP INDEX idx_groups_relay_request_group_link`.

One helper, added next to the existing `relay_*` helpers in `src/Simplex/Chat/Store/Groups.hs`:

```haskell
isRelayGroupRefused :: DB.Connection -> User -> ShortLinkContact -> IO Bool
isRelayGroupRefused db User {userId} groupLink =
  fromOnly . head <$> DB.query db
    [sql|
      SELECT EXISTS (
        SELECT 1 FROM groups
        WHERE user_id = ?
          AND relay_request_group_link = ?
          AND relay_own_status = ?
        LIMIT 1
      )
    |]
    (userId, groupLink, RSRejected)
```

`EXISTS … LIMIT 1` because more than one `groups` row may share `relay_request_group_link` (`createRelayRequestGroup` at Store/Groups.hs:1526 INSERTs unconditionally). If any matching row has `relay_own_status = 'rejected'`, the channel is refused. The equality check naturally excludes other states (NULL, RSInvited, RSAccepted, RSActive, RSInactive).

All other operator-allow and leave writes reuse existing helpers `updateRelayOwnStatus_` and `updateRelayOwnStatusFromTo` (Store/Groups.hs:1587-1597). No new write helpers.

## 3. Rejection point — `xGrpRelayInv` (Subscriber.hs:1524)

```haskell
xGrpRelayInv :: InvitationId -> VersionRangeChat -> GroupRelayInvitation -> CM ()
xGrpRelayInv invId chatVRange groupRelayInv@GroupRelayInvitation {groupLink} = do
  refused <- withStore' $ \db -> isRelayGroupRefused db user groupLink
  if refused
    then sendRelayRejection `catchAllErrors` eToView
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

`acceptContact` (Internal.hs:881-912 precedent; Agent.hs:1437-1442 → `joinConn` 1263 → `joinConnSrv` 1358-1369 for CRContactUri → `sendInvitation` Agent/Client.hs:1796-1799 → `sendOrProxySMPMessage` 1084-1094 → `sendSMPMessage`/`proxySMPMessage`) hands the CONF to the SMP server via a direct SMP client call. The CONF does NOT go through `snd_message_deliveries` — it is transmitted inline. Subsequent `deleteAgentConnectionAsync' connId False` is therefore safe. The cost is one SMP round-trip blocking the receive loop, which the refusal path can absorb.

No chat-layer `Connection` row is persisted for the refused contact — the agent owns the connection state, and `deleteAgentConnectionAsync'` cleans it up.

If `sendInvitation` throws (SMP server unreachable), `acceptContact` throws before reaching its internal `acceptInvitation` step and the agent-allocated rcv queue from `newRcvConnSrv` is left for the agent's eventual cleanup. The owner receives no rejection and falls back to the silent-degradation path (GroupRelay stuck at `RSInvited`). The outer `catchAllErrors eToView` prevents the receive loop from being held by the bubbled-up exception.

## 4. Wire format — `XGrpRelayReject`

Empty-payload event, owner-relay direct contact channel only. Not group-signed. Naming matches the existing `XGrpLinkReject` precedent (Protocol.hs:440, tag:985, string:1043).

`src/Simplex/Chat/Protocol.hs`:

- GADT constructor (after `XGrpRelayNew`, line 446): `XGrpRelayReject :: ChatMsgEvent 'Json`
- Tag GADT (after `XGrpRelayNew_`, line 991): `XGrpRelayReject_ :: CMEventTag 'Json`
- `strEncode` (line 1049): `XGrpRelayReject_ -> "x.grp.relay.reject"`
- `strDecode` (line 1108): `"x.grp.relay.reject" -> XGrpRelayReject_`
- `toCMEventTag` (line 1163): `XGrpRelayReject -> XGrpRelayReject_`
- JSON parse (line 1321): `XGrpRelayReject_ -> pure XGrpRelayReject`
- JSON encode (line 1391): `XGrpRelayReject -> JM.empty` — matches `XGrpLeave -> JM.empty` (1402) and `XDirectDel -> JM.empty` (1379).
- **No** entry in `isForwardedGroupMsg` (485-505) or `requiresSignature` (1227-1238).

Older owner clients parse the unknown tag as `XUnknown` (default branch at 1134) and hit the CONF handler's catch-all `_ -> messageError "CONF from invited member must have x.grp.acpt"`. No state change, no crash; the GroupRelay stays at `RSInvited` — the same end state as today's "relay never responds" mode. The owner UI shows the relay as permanently "invited" with no progress; documented degradation.

`docs/protocol/channels-protocol.md`: insert a `### Relay refusal` subsection between `### Relay addition` (61-73) and `### Subscriber connection` (75). Paragraphs:

1. **Trigger** — relay's `APILeaveGroup` sets `relay_own_status = 'rejected'` on the relay's local `groups` row for the channel.
2. **Signal** — empty-payload `x.grp.relay.reject` over the owner-relay direct contact channel.
3. **Owner handling** — `GroupRelay` transitions `RSInvited → RSRejected`; final. Cleared only by the relay operator running `/group allow <groupId>`.
4. **Limitations** — (a) older owner clients log a CONF parse error and leave their `GroupRelay` at `RSInvited` indefinitely (same UX as a relay that doesn't respond); (b) older relay binaries do not enforce refusal — mixed-version deployments where some relays are old behave asymmetrically.

## 5. Owner-side state

`RelayStatus` gains `RSRejected` (§2). Add to `relayStatusText`, `textEncode`, `textDecode`.

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

`getGroupRelayByGMId` (Store/Groups.hs:1307) and `updateRelayStatusFromTo` (1438-1442) are already exported. `updateRelayStatusFromTo` is conditional on the current status equalling `RSInvited` — racing CONFs cannot regress an already-rejected or already-active row. `deleteMemberConnection` (Internal.hs:1807-1808) safely no-ops when `activeConn` is `Nothing`. `CEvtGroupRelayUpdated` (Controller.hs:900) carries exactly the iOS payload.

`addRelays` (Commands.hs:3942-3976) persists `GroupRelay` with `RSNew → RSInvited` before sending `XGrpRelayInv`, so the row exists when the CONF arrives. A second user-initiated `addRelays` after rejection creates a fresh row, independent of the rejected one — no automatic retry.

## 6. Refusal write — `APILeaveGroup` (Commands.hs:2919-2935)

Currently `leaveChannelRelay` does NOT touch `relay_own_status` — verified at Commands.hs:2938-2947. The new write is added to the existing leave path, unconditionally on the relay-leave branch:

```haskell
APILeaveGroup groupId -> withUser $ \user@User {userId} -> do
  gInfo@GroupInfo {membership} <- withFastStore $ \db -> getGroupInfo db vr user groupId
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
    -- NEW: mark the relay's local groups row as refused
    when (useRelays' gInfo && isRelay membership) $
      withFastStore' $ \db -> updateRelayOwnStatus_ db gInfo RSRejected
    pure $ CRLeftMemberUser user gInfo' {membership = membership {memberStatus = GSMemLeft}}
```

`updateRelayOwnStatus_` (Store/Groups.hs:1593) writes unconditionally. The prior status can legitimately be any of `RSInvited` (operator leaves mid-request, placeholder profile still in place — verified at Store/Groups.hs:1531-1541), `RSAccepted` (waiting for health-check), `RSActive` (steady state), or `RSInactive` (already inactive — re-leaving). The earlier rev's `publicGroup == Nothing` throw was wrong: `RSInvited` is a real lifecycle state with `publicGroup = Nothing` (`createRelayRequestGroup` at Store/Groups.hs:1531 uses a placeholder profile until `updateGroupProfile` at Subscriber.hs:3847 runs inside the relay-request worker). Writing `RSRejected` unconditionally on the relay-leave path correctly cancels an in-progress invitation: `getNextPendingRelayRequest` (Store/RelayRequests.hs:60-72) selects only rows where `relay_own_status = 'invited'`, so the flip to `RSRejected` removes the row from the worker queue.

## 7. Operator command — relay side

One API command. Operator discovers rejected channels through `/gs` (see §7.2).

`src/Simplex/Chat/Controller.hs` (after `APITestChatRelay` at ~408):

```haskell
| APIAllowRelayGroup {groupId :: GroupId}
-- response (after CRGroupRelays at ~737):
| CRRelayGroupAllowed {user :: User, groupInfo :: GroupInfo}
```

Parser entries (`src/Simplex/Chat/Library/Commands.hs:5033+`). `GroupId = Int64` is a type alias (Types.hs:449), so `A.decimal` decodes directly — matches `APILeaveGroup <$> A.decimal` at 5021:

```haskell
"/_relay allow " *> (APIAllowRelayGroup <$> A.decimal),
"/group allow "  *> (APIAllowRelayGroup <$> A.decimal),
```

Handler:

```haskell
APIAllowRelayGroup groupId -> withUser $ \user -> do
  gInfo <- withFastStore $ \db -> getGroupInfo db vr user groupId
  gInfo' <- withStore' $ \db -> updateRelayOwnStatusFromTo db gInfo RSRejected RSInactive
  pure $ CRRelayGroupAllowed user gInfo'
```

`updateRelayOwnStatusFromTo` (Store/Groups.hs:1587-1591) atomically transitions only if the current status equals the from-state — a non-rejected row stays unchanged and the response reports the unchanged `gInfo`. The transition to `RSInactive` writes `relay_inactive_at = currentTs` via `updateRelayOwnStatus_` (1593-1597), so the row becomes eligible for `checkRelayInactiveGroups` connection cleanup on TTL — the correct hygiene state for a previously-rejected, now-cleared row.

No event to the owner. The owner's next user-initiated `addRelays` succeeds normally (the relay's `xGrpRelayInv` finds no `'rejected'` row for the link). Operator authorization is the chat-relay binary's process-level access.

### 7.1 Guard against deleting a rejected group

`APIDeleteChat CTGroup` at Commands.hs:1242-1246 lets the operator delete the group once `memberCurrent membership` is false (post-leave). That path would silently clear the refusal — an accidental `/d` should not undo a moderation decision. Add a guard immediately after the existing `unless canDelete` check:

```haskell
when (relayOwnStatus gInfo == Just RSRejected) $
  throwChatError $ CECommandError "cannot delete a rejected channel; run /_relay allow <groupId> first"
```

`checkRelayInactiveGroups` (Commands.hs:4812-4817) only deletes connections via `deleteGroupConnections`, not group rows, so no guard is needed there.

### 7.2 Surface `[rejected]` in `/gs`

`viewGroupsList` in `src/Simplex/Chat/View.hs:1432-1459`. Extend `groupSS`'s destructure to pull `relayOwnStatus` while keeping the existing `GroupSummary {currentMembers}` pattern (used at line 1456 by `memberCount`), and append `[rejected]` between status and alias:

```haskell
groupSS g@GroupInfo { membership
                    , chatSettings = ChatSettings {enableNtfs}
                    , groupSummary = GroupSummary {currentMembers}
                    , relayOwnStatus
                    } =
  case memberStatus membership of
    GSMemInvited -> groupInvitation' g
    s -> membershipIncognito g <> ttyFullGroup g <> viewMemberStatus s <> rejectionSuffix <> alias g
  where
    rejectionSuffix = case relayOwnStatus of
      Just RSRejected -> " [rejected]"
      _               -> ""
    …
```

## 8. iOS

No iOS storage-side change. The owner-side `RSRejected` rendering is the same as the rev-4 plan.

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

`ChannelRelaysView.swift` requires no change — the existing fall-through in `ownerRelayStatusText` (line 114-127) to `groupRelays.first(…)?.relayStatus.text` already renders `"rejected"`.

`GroupMemberStatus.memRejected` already exists at ChatTypes.swift:3002. No iOS enum change; cited here so an iOS-only reviewer doesn't drop the case.

Per `apps/ios/CODE.md` Change Protocol, the implementer updates `apps/ios/spec/state.md`, `apps/ios/spec/api.md`, `apps/ios/spec/client/chat-view.md`, `apps/ios/product/views/group-info.md`, `apps/ios/spec/impact.md`, and `apps/ios/product/concepts.md`.

Kotlin/Android/desktop port is a separate PR.

## 9. Tests — `tests/ChatTests/RelayRefused.hs`

All tests use the existing channel harness and block on chat events, not `threadDelay`.

- **`testRelayRefuseAfterLeave`** — relay1 leaves; owner re-adds; owner blocks on `CEvtGroupRelayUpdated`; assert owner `relayStatus == RSRejected`, member `GSMemRejected`, channel link data excludes relay1. Also assert relay's `groups.relay_own_status = 'rejected'`. Deterministic delivery check for the sync-accept-then-delete path.
- **`testRelayAllowAcceptsAgain`** — operator runs `/group allow <groupId>`; relay's `groups.relay_own_status` becomes `'inactive'`; owner re-adds; relay reaches `RSActive` on a fresh `GroupRelay` row.
- **`testRelayDoesNotRefuseUnrelatedChannel`** — relay1 leaves channel A; owner of unrelated channel B issues `XGrpRelayInv`; relay1 accepts B; only A's `groups` row has `relay_own_status = 'rejected'`.
- **`testRelayRefuseRaceConcurrentInvitations`** — owner sends two `XGrpRelayInv` for the same channel concurrently after the relay has left; both refuse; relay's `groups` table acquires no placeholder row for the second invitation (both lookups match the same rejected row).
- **`testRelayForwardCompatOldOwner`** — owner's `chatVersionRange` excludes `x.grp.relay.reject`; relay refuses; owner emits `messageError` and the GroupRelay row stays at `RSInvited`; no crash.
- **`testRelayDeleteRejectedBlocked`** — relay1 leaves channel A; operator runs `/d #A`; deletion fails with the guard error from §7.1; channel still exists; operator runs `/group allow <groupId>` then `/d #A`; deletion succeeds.
- **`testRelayRejectSurvivesOwnerRemoveRelayMember`** — relay1 leaves channel A (sets `RSRejected`); owner sends `XGrpMemDel` removing relay1; assert relay's `groups.relay_own_status` is still `'rejected'`, not flipped to `'inactive'`. Covers the §2 tightening of `xGrpMemDel` at Subscriber.hs:3132.
- **`testNonOwnerXGrpRelayRejectIgnored`** — owner-side negative case: deliver an `XGrpRelayReject` CONF on a connection where either `memberRole' membership /= GROwner` or the sender member is not `isRelay`; assert the owner emits `messageError` and neither the GroupRelay row nor the member status changes.

## 10. Adversarial review

- **Existing `RSInactive` consumers.** Three call sites filter on `Just RSInactive` to mean "relay not serving — drop normal delivery":
  - Subscriber.hs:936 (`MSG` handler filters delivery tasks).
  - Subscriber.hs:3571 (delivery-task worker rejects `DJDeliveryJob`).
  - Subscriber.hs:3641 (delivery-job worker errors `DJDeliveryJob`).
  All three must broaden to also match `Just RSRejected` — both states share the "not serving" semantic. `DJRelayRemoved` is handled in a separate branch and remains status-independent. Add a small predicate (e.g., `relayNotServing :: Maybe RelayStatus -> Bool`) near the existing `relayOwnStatus` accessors.
- **`xGrpMemDel` writer at Subscriber.hs:3132** — this is also a writer of `relay_own_status`, not a filter. It flips any non-NULL status to `RSInactive` when the owner removes the relay member. Tightened in §2 to skip when the row is already `RSRejected`; otherwise the refusal would be silently undone by a normal protocol event.
- **Health-check loop never touches RSRejected.** `getRelayServedGroups` filters `relay_own_status IN (RSAccepted, RSActive)` (Store/Groups.hs:1607); RSRejected rows are not iterated. `updateRelayOwnStatusFromTo` calls in Commands.hs:4808-4809 only transition RSAccepted↔RSActive↔RSInactive. With the §2 tightening of `xGrpMemDel` line 3132, no path can silently undo a refusal.
- **Operator deletes a rejected group** — blocked at `APIDeleteChat CTGroup` per §7.1.
- **Timing side channel** — refusal path is one synchronous SMP round-trip; accepted path is much longer. Passive SMP-server observation can distinguish, though relay load adds variance to both paths. SMP server already infers relay-channel relationships from connection patterns; marginal additional leak.
- **Information leakage in `XGrpRelayReject`** — empty payload.
- **Concurrent leave-then-rejoin** — operator-facing contract: invitations arriving before the leave commits locally are processed normally; invitations after are refused. Note that `xGrpRelayInv` does NOT take `withGroupLock "leaveGroup" groupId` (no group ID is known at REQ time); the bound is the SQL commit of the `relay_own_status = 'rejected'` write, not a lock. Sibling rows already at `RSInvited` from before the leave are not retroactively rejected — they are processed normally by the worker. See §12 for follow-up scope.
- **Two concurrent `XGrpRelayInv` for the same rejected channel** — both lookups hit the same indexed row, both refuse. No race.
- **Duplicate `groups` rows for the same `relay_request_group_link`** — pre-existing (`createRelayRequestGroup` INSERTs unconditionally; no uniqueness on `relay_request_inv_id` or `relay_request_group_link`). Any `RSRejected` row blocks *future invitations from creating new rows that progress to acceptance* (the lookup uses `EXISTS … LIMIT 1`). Sibling rows already in `RSInvited` continue to be processed by the worker — see §12.
- **Operator-allow vs. concurrent invitation** — UPDATE-SELECT race resolves to either "still refused" or "slipped through with accept"; both match operator intent.
- **`getGroupRelayByGMId` failure on owner side** — propagates as `ChatErrorStore`; cannot happen in normal operation.
- **Multi-user relay binary** — `groups.user_id` scopes both lookup and write. `withUser` for the CLI. No cross-user pollution.
- **`sendRelayRejection` SMP failure** — wrapped in `catchAllErrors eToView` per §3 so a single SMP failure during refusal does not propagate to the agent receive loop. The owner falls back to silent-degradation (GroupRelay stuck at RSInvited), matching today's "relay unresponsive" mode.
- **Forward compat — mixed-version relays.** An old relay binary leaves a channel by writing `RSInactive`, not `RSRejected`, and does not enforce refusal at `xGrpRelayInv`. Mixed-version deployments (some relays new, some old) have asymmetric behavior: new relays refuse, old relays accept. Acceptable v1 limitation; document in `docs/protocol/channels-protocol.md`. Operator on an upgraded relay can `/leave` again under the new binary to re-establish refusal.
- **Forward compat (old owner)** — old owner's CONF handler lands in the `_ -> messageError "CONF from invited member must have x.grp.acpt"` catch-all (Subscriber.hs:773). GroupRelay stays at `RSInvited`; same end state as today's "relay never responds" mode. Documented in the protocol doc.

## 11. Files changed

| File | Change |
|---|---|
| `src/Simplex/Chat/Types/Shared.hs` | `RSRejected` variant + text encodings |
| `src/Simplex/Chat/Protocol.hs` | `XGrpRelayReject` constructor, tag, str enc/dec, JSON enc/dec |
| `src/Simplex/Chat/Store/Groups.hs` | `isRelayGroupRefused` helper |
| `src/Simplex/Chat/Store/SQLite/Migrations/M20260514_relay_request_group_link_index.hs` | NEW. Partial index |
| `src/Simplex/Chat/Store/SQLite/Migrations.hs` | Register migration |
| `src/Simplex/Chat/Store/Postgres/Migrations/M20260514_relay_request_group_link_index.hs` | NEW |
| `src/Simplex/Chat/Store/Postgres/Migrations.hs` | Register migration |
| `src/Simplex/Chat/Controller.hs` | `APIAllowRelayGroup` command; `CRRelayGroupAllowed` response |
| `src/Simplex/Chat/Library/Commands.hs` | Parser; handler; refusal write in `APILeaveGroup`; delete guard in `APIDeleteChat CTGroup` |
| `src/Simplex/Chat/Library/Subscriber.hs` | Gate in `xGrpRelayInv`; `XGrpRelayReject` arm in CONF handler; broaden three RSInactive filters to also match RSRejected (lines 936, 3571, 3641); tighten `xGrpMemDel` writer at 3132 to skip when row is `RSRejected` |
| `src/Simplex/Chat/View.hs` | `[rejected]` suffix in `viewGroupsList` |
| `simplex-chat.cabal` | Register new migration modules |
| `docs/protocol/channels-protocol.md` | Insert "Relay refusal" subsection |
| `apps/ios/SimpleXChat/ChatTypes.swift` | `rsRejected` case + text |
| `apps/ios/Shared/Views/NewChat/AddChannelView.swift` | Red dot + "rejected" in `relayStatusIndicator` |
| `apps/ios/Shared/Views/Chat/Group/GroupMemberInfoView.swift` | "Status: rejected by relay operator" row |
| `tests/ChatTests/RelayRefused.hs` | NEW. Eight tests |
| Test list registration | Add the new module |

`chat_schema.sql` is auto-regenerated by tests.

## 12. Out of scope

- Kotlin/Android/desktop UI port.
- New alerts, modals, banners, compose-bar changes.
- Refusal triggered by `xGrpMemDel` (owner removing relay).
- Pre-emptive blocking of unseen channels.
- Owner-side independent clear of `RSRejected`.
- `publicGroupId`-keyed refusal.
- Timing-uniform refusal.
- **Sibling-row worker race.** When a relay leaves a channel for which it has a sibling `groups` row in `RSInvited` (e.g., the owner re-sent `XGrpRelayInv` and `createRelayRequestGroup` created a second row), only the row whose ID `APILeaveGroup` targets is flipped to `RSRejected`; sibling `RSInvited` rows continue through the worker. Pre-existing behavior — `leaveChannelRelay` doesn't touch sibling rows today either. Cheapest future closure: in §6, also `UPDATE groups SET relay_request_failed = 1 WHERE user_id = ? AND relay_request_group_link = ? AND relay_own_status = 'invited'` in the same transaction (the worker filters on `relay_request_failed = 0` at Store/RelayRequests.hs:67). Deferred to a follow-up.
- **`XGrpRelayInv` re-delivery duplicates.** `createRelayRequestGroup` has no uniqueness on `relay_request_inv_id` or `relay_request_group_link`; an owner retry of `XGrpRelayInv` creates duplicate rows. Pre-existing; closure ties to the sibling-row item above.

The mixed-version-relay asymmetry and the old-owner stuck-RSInvited UI degradation are documented in `docs/protocol/channels-protocol.md` alongside the new `### Relay refusal` subsection.
