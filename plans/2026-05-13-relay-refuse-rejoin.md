Plan rewritten for conciseness with fresh-context re-evaluation; supersedes earlier revisions.

# Plan: relay refuses to rejoin a channel it left

## 1. Identifier

Gating key: `GroupRelayInvitation.groupLink :: ShortLinkContact` (Types.hs:884-889). Available at `xGrpRelayInv` (Subscriber.hs:1524-1528) before any DB write or network call, which is what the task requires. Link rotation by the owner bypasses refusal; `publicGroupId` (Types.hs:790) would resist that but is only known after `getShortLinkConnReq'` — defer that gating to a follow-up.

## 2. Storage

New table `refused_relay_groups`, separate from `groups` so the refusal outlives the relay's local GroupInfo (which the operator may delete after leaving).

New migration `M20260514_refused_relay_groups`. SQLite (matches M20260507 conventions and M20260222 BLOB columns for ShortLinkContact):

```sql
CREATE TABLE refused_relay_groups (
  refused_relay_group_id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  group_link BLOB NOT NULL,
  group_display_name TEXT NOT NULL DEFAULT '',
  group_short_descr TEXT,
  group_image TEXT,
  left_at TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
) STRICT;
CREATE UNIQUE INDEX idx_refused_relay_groups_user_group_link
  ON refused_relay_groups(user_id, group_link);
```

Postgres (matches M20260222 `BIGINT … GENERATED ALWAYS AS IDENTITY` and M20260429 `TIMESTAMPTZ`):

```sql
CREATE TABLE refused_relay_groups (
  refused_relay_group_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  group_link BYTEA NOT NULL,
  group_display_name TEXT NOT NULL DEFAULT '',
  group_short_descr TEXT,
  group_image TEXT,
  left_at TIMESTAMPTZ NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE UNIQUE INDEX idx_refused_relay_groups_user_group_link
  ON refused_relay_groups(user_id, group_link);
```

Down: `DROP TABLE refused_relay_groups`. Tests regenerate `chat_schema.sql`.

New module `Simplex/Chat/Store/RefusedRelayGroups.hs` (mirrors `Store/RelayRequests.hs`). Exports:

```haskell
insertRefusedRelayGroup
  :: DB.Connection -> User -> ShortLinkContact -> Text -> Maybe Text -> Maybe ImageData -> IO ()
isRelayGroupRefused    :: DB.Connection -> User -> ShortLinkContact -> IO Bool
listRefusedRelayGroups :: DB.Connection -> User -> IO [RefusedRelayGroup]
deleteRefusedRelayGroup :: DB.Connection -> User -> Int64 -> IO Bool
```

`insert` uses `ON CONFLICT (user_id, group_link) DO UPDATE` so a repeated leave refreshes display fields and `left_at` while preserving `created_at`. `delete` is SELECT-EXISTS-then-DELETE because `DB.execute` returns `()` in this codebase.

New record in `Simplex/Chat/Types.hs` next to `RelayRequestData`:

```haskell
data RefusedRelayGroup = RefusedRelayGroup
  { refusedRelayGroupId :: Int64,
    groupLink :: ShortLinkContact,
    groupDisplayName :: Text,
    groupShortDescr :: Maybe Text,
    groupImage :: Maybe ImageData,
    leftAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Eq, Show)

$(JQ.deriveJSON defaultJSON ''RefusedRelayGroup)
```

`defaultJSON` matches `GroupRelay` (Operators.hs:616).

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
      dm <- encodeConnInfoPQ pqSup chatV XGrpRelayRej
      void $ withAgent $ \a ->
        acceptContact a NRMBackground (aUserId user) connId False invId dm pqSup subMode
      deleteAgentConnectionAsync' connId False
```

**Why synchronous `acceptContact` (not `acceptContactAsync`).** `acceptContactAsync` enqueues a JOIN agent command; the CONF send and the snd-queue creation happen later inside the agent's command worker (Agent.hs:1826-1830). If we immediately call `deleteAgentConnectionAsync' acId True`, `setConnDeleted` runs, `prepareDeleteConnections_` finds zero rcv queues (no JOIN yet), `deleteConn db (Just timeout) connId` finds zero `snd_message_deliveries` and calls `deleteConnRecord`. The connection record is gone before the JOIN worker can send the CONF — the rejection signal is silently dropped.

`acceptContact` (Internal.hs:881-912 precedent; Agent.hs:1437-1442 → `joinConn` 1263 → `joinConnSrv` 1358-1369 for CRContactUri → `sendInvitation` Agent/Client.hs:1796-1799 → `sendOrProxySMPMessage` 1084-1094 → `sendSMPMessage`/`proxySMPMessage`) hands the CONF to the SMP server via a direct SMP client call. The CONF does NOT go through `snd_message_deliveries` — it is transmitted inline. Subsequent `deleteAgentConnectionAsync' connId False` is therefore safe; `waitDelivery=True` would be a no-op because no delivery row exists for this CONF. The cost is one SMP round-trip blocking the receive loop, which the refusal path can absorb.

No chat-layer `Connection` row is persisted for the refused contact — the agent owns the connection state, and `deleteAgentConnectionAsync'` cleans it up.

## 4. Wire format — `XGrpRelayRej`

Empty-payload event, owner-relay direct contact channel only. Not group-signed (the direct contact connection is authenticated at the agent layer).

`src/Simplex/Chat/Protocol.hs`:

- GADT constructor (after `XGrpRelayNew`, line 446):
  ```haskell
  XGrpRelayRej :: ChatMsgEvent 'Json
  ```
- Tag GADT (after `XGrpRelayNew_`, line 991):
  ```haskell
  XGrpRelayRej_ :: CMEventTag 'Json
  ```
- `strEncode` (line 1049): `XGrpRelayRej_ -> "x.grp.relay.rej"`.
- `strDecode` lookup map (line 1108): `"x.grp.relay.rej" -> XGrpRelayRej_`.
- `toCMEventTag` (line 1163): `XGrpRelayRej -> XGrpRelayRej_`.
- JSON parse (line 1321): `XGrpRelayRej_ -> pure XGrpRelayRej`.
- JSON encode (line 1391): `XGrpRelayRej -> JM.empty` — matches `XGrpLeave -> JM.empty` (line 1402) and `XDirectDel -> JM.empty` (line 1379).
- **No** entry in `isForwardedGroupMsg` (485-505) — not forwarded.
- **No** entry in `requiresSignature` (1227-1238) — not a group event.

Older owner clients parse the unknown tag as `XUnknown` (default branch at line 1134) and hit the CONF handler's catch-all `_ -> messageError "CONF from invited member must have x.grp.acpt"`. No state change, no crash; the GroupRelay stays at `RSInvited` — the same end state as today's "relay never responds" mode. The owner UI shows the relay as permanently "invited" with no progress; documented degradation.

`docs/protocol/channels-protocol.md`: insert a `### Relay refusal` subsection between `### Relay addition` (lines 61-73) and `### Subscriber connection` (line 75). Three short paragraphs: (1) trigger — `APILeaveGroup` persists a refusal; (2) signal — empty-payload `x.grp.relay.rej` over the direct contact channel; (3) owner handling — `GroupRelay` transitions `RSInvited → RSRejected`; final; cleared only by the relay operator re-issuing.

## 5. Owner-side state

New `RelayStatus` variant in `src/Simplex/Chat/Types/Shared.hs:81-114`:

```haskell
data RelayStatus = … | RSInactive | RSRejected
relayStatusText RSRejected = "rejected"
-- textEncode/textDecode add "rejected" / RSRejected entries
```

CONF handler arm in `src/Simplex/Chat/Library/Subscriber.hs:760-773` (immediately after the existing `XGrpRelayAcpt` clause):

```haskell
XGrpRelayRej
  | memberRole' membership == GROwner && isRelay m -> do
      relay <- withStore $ \db -> do
        liftIO $ updateGroupMemberStatus db userId m GSMemRejected
        relay <- getGroupRelayByGMId db (groupMemberId' m)
        liftIO $ updateRelayStatusFromTo db relay RSInvited RSRejected
      let m' = m {memberStatus = GSMemRejected}
      deleteMemberConnection m'
      toView $ CEvtGroupRelayUpdated user gInfo m' relay
  | otherwise -> messageError "x.grp.relay.rej: only owner can receive relay rejection"
```

Both `getGroupRelayByGMId` (Store/Groups.hs:1307, returns `ExceptT StoreError IO`) and `updateRelayStatusFromTo` (1438-1442, returns `IO`) are already exported; no new helper. `updateRelayStatusFromTo` is conditional on the current status equalling `RSInvited` — racing CONFs cannot regress an already-rejected or already-active row. `deleteMemberConnection` (Internal.hs:1807-1808) safely no-ops when `activeConn` is `Nothing`. `CEvtGroupRelayUpdated` (Controller.hs:900) carries `(user, groupInfo, member, groupRelay)` — exactly the iOS payload.

`addRelays` (Commands.hs:3942-3976) already persists `GroupRelay` with `RSNew → RSInvited` before sending `XGrpRelayInv`, so by the time `XGrpRelayRej` arrives the row exists. A second user-initiated `addRelays` after rejection creates a fresh row (new `GroupMember`, new `GroupRelay`), independent of the rejected one — there is no automatic retry.

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
    -- NEW
    when (useRelays' gInfo && isRelay membership) $ do
      let GroupProfile {displayName, shortDescr, image, publicGroup} = groupProfile
      case publicGroup of
        Just PublicGroupProfile {groupLink} ->
          withFastStore' $ \db ->
            insertRefusedRelayGroup db user groupLink displayName shortDescr image
        Nothing ->
          throwChatError $ CEInternalError "APILeaveGroup: relay-served channel has no publicGroup"
    pure $ CRLeftMemberUser user gInfo' {membership = membership {memberStatus = GSMemLeft}}
```

The throw is structural assertion, not a runtime hazard: a relay-served channel always has `publicGroup = Just _` by the time the relay becomes a current member (the placeholder profile from `createRelayRequestGroup` is replaced by `updateGroupProfile` inside `getLinkDataCreateRelayLink` at Subscriber.hs:3847).

## 7. Operator commands — relay side

`src/Simplex/Chat/Controller.hs` (after `APITestChatRelay` at ~407):

```haskell
| APIListRefusedRelayGroups
| APIClearRefusedRelayGroup {refusedRelayGroupId :: Int64}
-- responses (after CRGroupRelays at ~737):
| CRRefusedRelayGroups {user :: User, refusedRelayGroups :: [RefusedRelayGroup]}
| CRRefusedRelayGroupCleared {user :: User, refusedRelayGroupId :: Int64}
```

Parser entries (`src/Simplex/Chat/Library/Commands.hs:5033+`, longer prefixes first because attoparsec `<|>` is left-biased):

```haskell
"/_relay refused clear " *> (APIClearRefusedRelayGroup <$> A.decimal),
"/_relay refused"        $> APIListRefusedRelayGroups,
"/relay refused clear "  *> (APIClearRefusedRelayGroup <$> A.decimal),
"/relay refused"         $> APIListRefusedRelayGroups,
```

Handlers (alongside `APITestChatRelay`):

```haskell
APIListRefusedRelayGroups -> withUser $ \user -> do
  rs <- withStore' $ \db -> listRefusedRelayGroups db user
  pure $ CRRefusedRelayGroups user rs
APIClearRefusedRelayGroup refusedId -> withUser $ \user -> do
  deleted <- withStore' $ \db -> deleteRefusedRelayGroup db user refusedId
  unless deleted $ throwChatError $ CECommandError "no refused relay-group record with that id"
  pure $ CRRefusedRelayGroupCleared user refusedId
```

Clear deletes only the row. No event to the owner. The owner's next user-initiated `addRelays` for the same relay/channel succeeds normally.

## 8. iOS

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

- **`testRelayRefuseAfterLeave`** — relay1 leaves; owner re-adds; owner blocks on `CEvtGroupRelayUpdated`; assert `relayStatus == RSRejected`, member `GSMemRejected`, channel link data excludes relay1 (`getConnectedGroupRelays` filters by `relay_status IN (RSAccepted, RSActive)` at Store/Groups.hs:1334). This is the deterministic delivery check for the sync-accept-then-delete path.
- **`testRelayClearRefusedAcceptsAgain`** — operator runs `/relay refused`, then `/relay refused clear <id>`; owner re-adds; relay reaches `RSActive`.
- **`testRelayDoesNotRefuseUnrelatedChannel`** — relay1 leaves channel A; owner of unrelated channel B issues `XGrpRelayInv`; relay1 accepts B; refused-table contains only A.
- **`testRelayRefuseRaceConcurrentInvitations`** — owner sends two `XGrpRelayInv` for the same channel concurrently after the relay has left; both refuse; relay's `groups` table acquires no placeholder row; refused-table still has exactly one row (UNIQUE index).
- **`testRelayForwardCompatOldOwner`** — owner's `chatVersionRange` excludes `x.grp.relay.rej`; relay refuses; owner emits `messageError` and the GroupRelay row stays at `RSInvited`; no crash.

## 10. Adversarial review

- **Timing side channel** — the refusal path takes one synchronous SMP round-trip (the new `acceptContact`); the accepted path takes much longer (worker, link-data fetch). An attacker with passive SMP-server observation can distinguish refusal from acceptance by latency. The SMP server already infers relay-channel relationships from connection patterns; marginal additional leak. Not mitigated; documented residual risk.
- **Information leakage in `XGrpRelayRej`** — empty payload. No reason, no timestamp, no other channel identifiers.
- **Concurrent leave-then-rejoin** — explicit operator-facing contract: invitations arriving before the leave commits locally are processed normally; invitations arriving after are refused. The window is bounded by the duration of `withGroupLock "leaveGroup" groupId` plus the refused-record write. No cross-group lock added.
- **Operator-clear vs. concurrent invitation** — DELETE-SELECT race resolves to either "still refused" or "slipped through with accept"; both match operator intent.
- **`getGroupRelayByGMId` failure on owner side** — would propagate as `ChatErrorStore` to the dispatch. Cannot happen in normal operation (the row is created by `addRelays` before `XGrpRelayInv` goes out, so it exists when the CONF arrives); if it ever did, the error surfaces visibly rather than being silently lost.
- **Multi-user relay binary** — refused-records scoped by `user_id`; `withUser` for the CLI commands. No cross-user pollution.
- **Forward compat (old relay)** — old relay binary doesn't enforce refusal but doesn't break either: it processes `XGrpRelayInv` as before. Feature is enforced by relay behavior, not by a protocol invariant on every node.

## 11. Files changed

| File | Change |
|---|---|
| `src/Simplex/Chat/Types/Shared.hs` | `RSRejected` variant + encodings |
| `src/Simplex/Chat/Types.hs` | `RefusedRelayGroup` record |
| `src/Simplex/Chat/Protocol.hs` | `XGrpRelayRej` constructor, tag, str enc/dec, JSON enc/dec |
| `src/Simplex/Chat/Store/RefusedRelayGroups.hs` | NEW. Four exported helpers |
| `src/Simplex/Chat/Store/SQLite/Migrations/M20260514_refused_relay_groups.hs` | NEW |
| `src/Simplex/Chat/Store/SQLite/Migrations.hs` | Register migration |
| `src/Simplex/Chat/Store/Postgres/Migrations/M20260514_refused_relay_groups.hs` | NEW |
| `src/Simplex/Chat/Store/Postgres/Migrations.hs` | Register migration |
| `src/Simplex/Chat/Controller.hs` | Two API commands, two responses |
| `src/Simplex/Chat/Library/Commands.hs` | Parser entries, two handlers, refusal write in `APILeaveGroup` |
| `src/Simplex/Chat/Library/Subscriber.hs` | Gate in `xGrpRelayInv`; `XGrpRelayRej` arm in CONF handler |
| `simplex-chat.cabal` | Register new modules |
| `docs/protocol/channels-protocol.md` | Insert "Relay refusal" subsection |
| `apps/ios/SimpleXChat/ChatTypes.swift` | `rsRejected` case + text |
| `apps/ios/Shared/Views/NewChat/AddChannelView.swift` | Red dot + "rejected" in `relayStatusIndicator` |
| `apps/ios/Shared/Views/Chat/Group/GroupMemberInfoView.swift` | "Status: rejected by relay operator" row |
| `tests/ChatTests/RelayRefused.hs` | NEW. Five tests |
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
