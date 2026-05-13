# Plan: relay refuses to rejoin a channel it left

Companion to task-001-relay-refuse-rejoin.md. File and line references are against the tree at planning time and will drift slightly during implementation.

This is a plan document, not code. Where the wording says "we", it refers to the implementer following this plan.

## Revision log

**Rev 3** addresses task-002-plan-cleanups.md (small inconsistencies left over from rev 2):

- §9.1 item 1: removed the stale "test in §8.6 is best-effort" sentence; §8.6 was deleted in rev 2.
- §9.1 item 4: replaced the redundant analysis with a one-line pointer to §3.4, where the operator-facing contract now lives.
- §7.1 files-touched table: removed the `ChannelRelaysView.swift` row, because §7.4 concludes no edit is needed (the existing fall-through to `RelayStatus.text` already renders "rejected"). §7.4 prose stays as the rationale. §10 already excluded the file. §7.7 spec-doc list updated to drop the stale ChannelRelaysView reference.

**Rev 2** addresses task-001-review.md. Changes from rev 1:

- §1 / §2 / §10: dropped the `public_group_id` column from the v1 schema. A follow-up will add it in its own migration alongside the publicGroupId-based second-stage check.
- §2 / §3 / §6 / §9: renamed SQL column `req_group_link` → `group_link` and Haskell field `reqGroupLink` → `groupLink`, matching `GroupRelayInvitation.groupLink`. Index renamed `idx_refused_relay_groups_user_group_link`.
- §2.3 / §2.4: handle `gInfo.groupProfile.publicGroup == Nothing` explicitly with a throw at the call site rather than silent skip. Inner store function now takes pre-unpacked fields.
- §3.1 / §4.1: cited the simplexmq agent guarantee (`deleteConn` with `waitDeliveryTimeout_`; AgentStore.hs:475-499) for the accept-then-delete pattern; noted this is a new combination of two established mechanisms, not a new mechanism.
- §3.4: reframed the leave-then-rejoin race as an explicit operator-facing contract rather than a residual oddity.
- §4.4 / §12: explicit note on old-owner UI implication (relay row stuck at RSInvited indicator).
- §5.2 / §5.7 / §10: dropped the `updateRelayStatusByGMId` helper; inlined `getGroupRelayByGMId` + `updateRelayStatusFromTo` at the one call site.
- §6.3: reordered parser alternatives — longer prefix before shorter to fix attoparsec left-biased matching.
- §8: removed §8.6 timing-uniformity test; tightened §8.1 to event-driven blocking (no `threadDelay`) so it serves as the deterministic delivery test for the accept-then-delete pattern.
- §9.2 item 11: rewritten to reflect §2.3's explicit throw; no more "silent skip + log warning TODO".

Items #10 (JM.empty for empty payload) and #11 (defaultJSON for RefusedRelayGroup) were verified against existing convention (Protocol.hs:1402 for `XGrpLeave -> JM.empty`; Operators.hs:616 for `GroupRelay`) and need no plan change.

---

## 0. Summary

When a chat-relay's operator leaves a channel via `APILeaveGroup` → `XGrpLeave`, the relay persists a "refused" record keyed by the channel's owner-published short link. Any future `XGrpRelayInv` carrying the same short link is rejected before any state-creating operation. The relay signals the rejection to the owner over the same direct contact channel that carried `XGrpRelayInv` using a new `XGrpRelayRej` event. The owner transitions the corresponding `GroupRelay` from `RSInvited` to a new `RSRejected` variant. The relay operator can list and clear refused records via two new CLI/API commands (`/_relay refused`, `/_relay refused clear <id>`). iOS UI surfaces `RSRejected` as a red-dot indicator with "rejected" text; the existing relay row in `ChannelRelaysView` opens into `GroupMemberInfoView` where the existing relay-link/relay-address rows already exist — we add a one-line "Status reason" row under the existing Section when the status is rejected.

PR scope is core Haskell + iOS UI. Kotlin port is a follow-up PR per the task's scope boundary.

---

## 1. Identifier choice

### 1.1 Candidates evaluated

- **`groupLink :: ShortLinkContact`** — the channel's owner-published group short link as carried in `GroupRelayInvitation.groupLink` (Types.hs:884-889). Known immediately on the relay at `xGrpRelayInv` (Subscriber.hs:1524) before any network call or state creation.
- **`publicGroupId :: B64UrlByteString`** — the channel's immutable identity, defined as `sha256(genesis root key)` (Types.hs:790). Embedded in the short link as `linkEntityId` (verified equal in `getLinkDataCreateRelayLink` at Subscriber.hs:3842). The relay only learns it after `getShortLinkConnReq'` returns at Subscriber.hs:3836 — one outbound network call into the SMP server.
- **Relay-local `GroupId`** — useless before first-time processing; the relay has no `GroupInfo` row when the first `xGrpRelayInv` arrives. Not viable as the gating key.

### 1.2 Decision

**Primary gating key: `groupLink :: ShortLinkContact`** (matches the field name on `GroupRelayInvitation`).

### 1.3 Rationale

- The task says: "Default to the group short link unless a stronger argument emerges." A stronger argument would require `publicGroupId` to provide protection that `groupLink` cannot. It does not, in practice:
  - For an owner to bypass `groupLink`-keyed refusal, the owner has to publish the channel under a different short link. That requires re-creating the channel's group link (the owner's contact link backing the channel). All existing subscribers connected via the old link cease being reachable through it; the owner effectively re-creates the channel address from scratch. Casual evasion is not possible.
  - For an owner to bypass `publicGroupId`-keyed refusal, the owner has to re-create the channel with a fresh genesis root key. From every observer's perspective that is a different channel. So neither key resists this — and the relay refusing one specific channel identity is the correct semantics: a fresh channel is a fresh evaluation.
- `groupLink` is available at the absolute earliest point in the flow (REQ → `xGrpRelayInv`, Subscriber.hs:1524) before any DB write, any agent state allocation, and any outbound network call. This is what the task asks for: "The check must run BEFORE any state-creating operation and before any outbound network call." `publicGroupId` would force the check after `getShortLinkConnReq'`, requiring the relay to first create the placeholder group via `createRelayRequestGroup` (Store/Groups.hs:1526) and then tear it down on rejection — a less clean flow.
- Operator UX: the operator does not have to type the short link. We provide `/_relay refused` to list refused records with row IDs and human-readable display fields, and `/_relay refused clear <id>` to clear by row ID. The identifier choice is hidden from the operator's hand.

### 1.4 Implications of NOT choosing publicGroupId — stated for the reviewer

- **Owner link rotation as evasion vector.** If the owner re-creates the channel's group link with a fresh `ShortLinkContact` while keeping the same `publicGroupId`, the relay's refused record will not match, and the relay will proceed with the request. The new short link looks like a brand-new channel address. The relay can refuse it again on next leave, but a determined owner can iterate. This is documented as a limitation of v1. A follow-up PR can add a second-stage check against `publicGroupId` after `getShortLinkConnReq'` returns, adding a `public_group_id` column to `refused_relay_groups` in its own migration at that time.
- **No additional security exposure today.** The owner who can re-create the group link is the same owner whose channel the relay refused; this isn't a third-party attack vector. The threat model is "relay's moderation decision survives the relay's own leave" — that holds for the common case (relay refuses, owner retries with the same link).

---

## 2. DB schema on the relay side

### 2.1 Decision: separate table

We add a new table `refused_relay_groups` keyed by `(user_id, group_link)`. We do not add a column to `groups` because:

- The relay's `GroupInfo` for the channel may be deleted by the operator (via `/d #channel` after leaving, or via `checkRelayInactiveGroups` cleanup at Commands.hs:4812-4817 when the connection-only cleanup is later extended). A flag column on `groups` couples the refused record's lifetime to the group row's lifetime, which is wrong: the refusal must outlive the group's local state.
- A separate table also makes the operator commands (`/_relay refused`, `/_relay refused clear`) trivial primary-key lookups rather than scans across `groups` filtering on a nullable flag.

### 2.2 Schema

SQLite migration `M20260513_refused_relay_groups`:

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

Column types match the existing pattern for `ShortLinkContact` storage (M20260222: `chat_relays.address BLOB`, `group_relays.relay_link BLOB`, `groups.relay_request_group_link BLOB`). `STRICT` matches every table after M20251230_strict_tables.

Postgres migration `M20260513_refused_relay_groups` (mirror):

```sql
CREATE TABLE refused_relay_groups (
  refused_relay_group_id BIGSERIAL PRIMARY KEY,
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

Down migrations: `DROP TABLE refused_relay_groups` (cascades the index in both engines).

Field notes:

- `user_id NOT NULL REFERENCES users ON DELETE CASCADE` — the chat-relay process supports multiple users in principle; tying refused records to a user mirrors every other relay-related table. `ON DELETE CASCADE` handles user deletion cleanly.
- `group_link` — the canonical encoding of `ShortLinkContact`, written by the existing `ToField ShortLinkContact` instance. Name matches the Haskell field `GroupRelayInvitation.groupLink` (Types.hs:888); the related owner-side column is `groups.relay_request_group_link`.
- `group_display_name`, `group_short_descr`, `group_image` — captured at leave time from `gInfo.groupProfile` for operator UI in `/_relay refused`.
- `left_at` — when the relay left the channel. Distinct from `created_at` because we may later re-insert (`INSERT OR REPLACE` semantics) if the operator clears and the relay leaves again.
- **No `public_group_id` column.** A follow-up PR that adds publicGroupId-based gating will add the column in its own migration. Adding it now for an unused feature violates "don't add features beyond what the task requires" — and adding it later is the standard pattern in this codebase (every monthly migration adds columns).

### 2.3 When the record is written

In `APILeaveGroup` (Commands.hs:2919-2935), after the existing `leaveChannelRelay`/`leaveGroupSendMsg` branch completes successfully and before the final `pure $ CRLeftMemberUser ...`. Specifically: only when `useRelays' gInfo && isRelay membership` — i.e., the same condition that selects `leaveChannelRelay` at line 2925-2926.

The call site extracts the link and the display fields from `gInfo` directly. If `gInfo.groupProfile.publicGroup` is `Nothing` we throw — this is structurally impossible for a relay-served channel (the placeholder profile is replaced by the real one during request processing, before the relay ever becomes a current member), and silent skip would paper over the violation.

Inline placement, inside the `withGroupLock "leaveGroup" groupId` (line 2922):

```haskell
APILeaveGroup groupId -> withUser $ \user@User {userId} -> do
  gInfo@GroupInfo {membership, groupProfile} <- withFastStore $ \db -> getGroupInfo db vr user groupId
  filesInfo <- withFastStore' $ \db -> getGroupFileInfo db user gInfo
  withGroupLock "leaveGroup" groupId $ do
    cancelFilesInProgress user filesInfo
    msg <-
      if useRelays' gInfo && isRelay membership
        then leaveChannelRelay gInfo
        else leaveGroupSendMsg user gInfo
    (gInfo', scopeInfo) <- mkLocalGroupChatScope gInfo
    ci <- saveSndChatItem user (CDGroupSnd gInfo' scopeInfo) msg (CISndGroupEvent SGEUserLeft)
    toView $ CEvtNewChatItems user [AChatItem SCTGroup SMDSnd (GroupChat gInfo' scopeInfo) ci]
    deleteGroupLinkIfExists user gInfo'
    withFastStore' $ \db -> updateGroupMemberStatus db userId membership GSMemLeft
    -- NEW: persist the refused-record on relay-side voluntary leave
    when (useRelays' gInfo && isRelay membership) $ do
      let GroupProfile {displayName, shortDescr, image, publicGroup} = groupProfile
      case publicGroup of
        Just PublicGroupProfile {groupLink} ->
          withFastStore' $ \db ->
            insertRefusedRelayGroup db user groupLink displayName shortDescr image
        Nothing ->
          throwChatError $ CEInternalError "APILeaveGroup: relay-served channel has no publicGroup profile"
    pure $ CRLeftMemberUser user gInfo' {membership = membership {memberStatus = GSMemLeft}}
```

Rationale:

- The `case publicGroup of Just/Nothing` makes the invariant explicit instead of silently skipping. If the invariant is ever violated, the throw produces a diagnosable error rather than an undetectable refusal hole.
- `XGrpLeave` is queued for delivery via the `DJRelayRemoved` delivery job before we persist refusal. The two operations are independent — even if the SMP delivery never reaches the owner, the relay still refuses future invitations. The order (deliver leave first, write refusal next) means we only refuse after we have committed to leaving.
- The group lock (`withGroupLock "leaveGroup" groupId`) serializes against any concurrent processing for that group. The refused-record write is inside the same lock so a racing `xGrpRelayInv` for the same channel (and only the same channel) cannot see "not refused" mid-leave — but see §3.4 for the explicit contract about races across different group locks.

### 2.4 New Haskell module: `Simplex/Chat/Store/RefusedRelayGroups.hs`

Mirror the shape of `Simplex/Chat/Store/RelayRequests.hs` (which is also relay-side).

Exports:

```haskell
insertRefusedRelayGroup
  :: DB.Connection
  -> User
  -> ShortLinkContact      -- the channel's group link from gInfo.groupProfile.publicGroup
  -> Text                  -- displayName
  -> Maybe Text            -- shortDescr
  -> Maybe ImageData       -- image
  -> IO ()
isRelayGroupRefused :: DB.Connection -> User -> ShortLinkContact -> IO Bool
listRefusedRelayGroups :: DB.Connection -> User -> IO [RefusedRelayGroup]
deleteRefusedRelayGroup :: DB.Connection -> User -> Int64 -> IO Bool
```

`RefusedRelayGroup` lives in `Simplex/Chat/Types.hs` next to `RelayRequestData`:

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

`defaultJSON` matches the established convention in `Operators.hs:616` for `GroupRelay` and the relay-side types nearby.

`insertRefusedRelayGroup` body (the call-site responsibility for unpacking `gInfo` is in §2.3; this function takes the unpacked values):

```haskell
insertRefusedRelayGroup db User {userId} groupLink displayName shortDescr image = do
  currentTs <- getCurrentTime
  DB.execute db
    [sql|
      INSERT INTO refused_relay_groups
        (user_id, group_link, group_display_name, group_short_descr, group_image, left_at, created_at)
      VALUES (?,?,?,?,?,?,?)
      ON CONFLICT (user_id, group_link) DO UPDATE
        SET group_display_name = excluded.group_display_name,
            group_short_descr = excluded.group_short_descr,
            group_image = excluded.group_image,
            left_at = excluded.left_at
    |]
    (userId, groupLink, displayName, shortDescr, image, currentTs, currentTs)
```

`ON CONFLICT ... DO UPDATE` makes the write idempotent: if the operator cleared a refused record and then the relay leaves the same channel a second time, the record is refreshed in place. `created_at` is intentionally NOT updated on conflict — first-refusal timestamp is preserved.

`isRelayGroupRefused`:

```haskell
isRelayGroupRefused db User {userId} groupLink =
  fromOnly . head <$> DB.query db
    [sql|
      SELECT EXISTS (
        SELECT 1 FROM refused_relay_groups
        WHERE user_id = ? AND group_link = ?
        LIMIT 1
      )
    |]
    (userId, groupLink)
```

`listRefusedRelayGroups`:

```haskell
listRefusedRelayGroups db User {userId} =
  map toRefused <$> DB.query db
    [sql|
      SELECT refused_relay_group_id, group_link,
             group_display_name, group_short_descr, group_image, left_at, created_at
      FROM refused_relay_groups
      WHERE user_id = ?
      ORDER BY left_at DESC
    |]
    (Only userId)
```

`deleteRefusedRelayGroup` — uses SELECT-then-DELETE because `DB.execute` returns `()` in this codebase (no row-count return path):

```haskell
deleteRefusedRelayGroup :: DB.Connection -> User -> Int64 -> IO Bool
deleteRefusedRelayGroup db User {userId} refusedId = do
  existed <- fromOnly . head <$> DB.query db
    [sql|
      SELECT EXISTS (
        SELECT 1 FROM refused_relay_groups
        WHERE user_id = ? AND refused_relay_group_id = ?
        LIMIT 1
      )
    |]
    (userId, refusedId)
  when existed $ DB.execute db
    "DELETE FROM refused_relay_groups WHERE user_id = ? AND refused_relay_group_id = ?"
    (userId, refusedId)
  pure existed
```

Contract: returns True if a row was deleted, False if none matched. The two statements are not in an explicit transaction because there's no consistency concern — a concurrent INSERT for the same (user_id, refused_relay_group_id) is impossible (the PK is row-generated), and a concurrent DELETE for the same id is idempotent.

### 2.5 When the record is read

In `xGrpRelayInv` (Subscriber.hs:1524), before `createRelayRequestGroup`. See §3 for placement detail.

---

## 3. Rejection point in the request flow

### 3.1 Location

File: `src/Simplex/Chat/Library/Subscriber.hs`. Function: `xGrpRelayInv` (lines 1524-1528 today).

```haskell
xGrpRelayInv :: InvitationId -> VersionRangeChat -> GroupRelayInvitation -> CM ()
xGrpRelayInv invId chatVRange groupRelayInv@GroupRelayInvitation {groupLink} = do
  refused <- withStore' $ \db -> isRelayGroupRefused db user groupLink
  if refused
    then sendRelayRejection invId chatVRange
    else do
      initialDelay <- asks $ initialInterval . relayRequestRetryInterval . config
      (_gInfo, _ownerMember) <- withStore $ \db -> createRelayRequestGroup db vr user groupRelayInv invId chatVRange initialDelay
      lift $ void $ getRelayRequestWorker True
  where
    sendRelayRejection invId' chatVRange' = do
      subMode <- chatReadVar subscriptionMode
      chatVR <- chatVersionRange
      let chatV = chatVR `peerConnChatVersion` chatVRange'
      (_cmdId, acId) <- agentAcceptContactAsync user False invId' XGrpRelayRej subMode PQSupportOff chatV
      deleteAgentConnectionAsync' acId True
```

**Agent-layer guarantee for `deleteAgentConnectionAsync' acId True`.** The `waitDelivery=True` argument propagates to the agent's `deleteConn db (Just timeout) connId` (simplexmq AgentStore.hs:475-499). With a non-Nothing timeout, the agent refuses to finalize connection deletion while `snd_message_deliveries` has unfinished rows for that conn AND the deletion has not yet hit `connDeleteDeliveryTimeout`. Practically: the CONF carrying `XGrpRelayRej`, queued by `agentAcceptContactAsync`, will be sent before the connection is reaped — bounded by the configured timeout, after which the deletion completes even if delivery is still pending (a partition-tolerant safety net).

This is the same `waitDelivery=True` semantics that `leaveChannelRelay`'s `deleteMembersConnections' user members True` relies on (Commands.hs flow described in the 2026-04-10 plan §1). The combination of `agentAcceptContactAsync` + immediate `deleteAgentConnectionAsync' _ True` is a new combination on top of two established mechanisms, not a new mechanism. The precedent `acceptGroupJoinSendRejectAsync` (Internal.hs:974-1003) keeps the connection alive only because the rejecting host needs a persistent member record to retain rejection history; the relay-rejection case has no such requirement — `refused_relay_groups` already holds the persistent state. So the immediate tear-down is intentional and correct.

§8.1 asserts the owner observes the CONF (and thus the rejection event) — this is the deterministic test for the delivery guarantee.

### 3.2 Why this exact line

- `xGrpRelayInv` is the first chat-level function that touches the inbound invitation. The previous handler frame is `processContactConnMessage` REQ handling (Subscriber.hs:1290-1300), which dispatches by chat event tag. The only DB action it performs is parsing and routing — no state is created.
- At this point we have `groupLink` already. `isRelayGroupRefused` is a single SELECT against a unique-indexed column. Cheap.
- We have NOT yet called `createRelayRequestGroup` (which would `INSERT` a placeholder group row, a host member row, and the relay-request-data columns on `groups`). Refusing here keeps the relay's DB clean.
- We have NOT yet called `getShortLinkConnReq'`. No outbound network fetch happens.
- We have NOT yet called `getRelayRequestWorker True` (which would wake up the worker and schedule work). The refusal short-circuits before scheduling.

### 3.3 What if `publicGroupId` were the identifier instead

Restating §1.4 with operational consequences:

- The check would land inside `getLinkDataCreateRelayLink` (Subscriber.hs:3834-3850) immediately after the `linkEntityId` extraction at line 3842, before `validateGroupProfile`/`createRelayLink`. The function signature would need to thread back a refusal result, and `processRelayRequest` (line 3818-3832) would need an additional case to clean up the placeholder group (`createRelayRequestGroup` already committed at this point) and send the rejection. The cleanup is a second-order concern (multi-row delete to undo `createRelayRequestGroup`'s effects, plus relay-request-data clearing).
- Even with `publicGroupId`, we still do one outbound network call (the link-data fetch). That fetch's information surface is the channel's public profile, which the relay would also fetch on legitimate requests, so timing doesn't leak refusal state to an outside observer. But it does mean the rejection round-trip costs an extra hop.
- For these reasons we keep the gating key as `groupLink`. A follow-up that adds publicGroupId-based gating ships its own migration adding `public_group_id` to `refused_relay_groups`.

### 3.4 Concurrent invocations — operator-facing contract

`xGrpRelayInv` runs under the agent's per-connection-entity lock (the inbound REQ is processed under `withEntityLock` in `processContactConnMessage`). The refused-record table is written under the leaving relay's `withGroupLock "leaveGroup" groupId`, which is a different lock. A race is possible: an `XGrpRelayInv` REQ for channel X could be in-flight while the relay leaves channel X. Sequence:

1. Owner sends invitation. Relay's REQ arrives, `xGrpRelayInv` starts.
2. `isRelayGroupRefused db user X` runs — possibly before the leave write commits → returns False.
3. `createRelayRequestGroup` runs, placeholder group + relay-request data are created.
4. The operator's `APILeaveGroup` for X writes the refused record. But the leave is for the OLD group X; the new invitation just created NEW group state.

**Contract:** *Invitations that arrive before the operator's leave commits locally on the relay are processed normally; invitations arriving after the commit are refused.* This is fine for v1 because:

- The relay-side `APILeaveGroup` commit happens immediately after `withFastStore' $ \db -> updateGroupMemberStatus db userId membership GSMemLeft` (Commands.hs:2934) and the refused-record write that follows. The window between "operator triggered leave" and "refused record committed" is bounded by the time the leave's `cancelFilesInProgress` + `leaveChannelRelay` + status-update + refused-write transaction sequence takes — short, but non-zero.
- For an in-flight invitation that slips through this window, the relay accepts and serves the channel normally. The operator can `/leave` the channel again to re-establish refusal. The contract is consistent ("after I see the leave succeed locally, the channel is refused") and predictable.

A coarser lock — e.g., a refusal-write lock per user — could close the race, but it adds cross-cutting complexity for a non-security issue. Recommend NOT adding the lock in v1. The contract above is the documented v1 behavior.

---

## 4. Rejection signal back to owner

### 4.1 Decision: new protocol message `XGrpRelayRej`

We add a new event constructor with no payload. The relay calls `agentAcceptContactAsync user False invId XGrpRelayRej subMode PQSupportOff chatV` (mirrors `acceptGroupJoinSendRejectAsync` at Internal.hs:974-1002), then `deleteAgentConnectionAsync' acId True` to tear down.

We did not pick option B (reuse the relay-request error path with a persistent error code) because:

- The relay-request error path (`setRelayRequestErr` in Store/RelayRequests.hs) is **relay-side state** that drives the *relay's own* retry worker. It does not produce a message to the owner. Hooking a "persistent rejection" code into it does not get the signal to the owner.
- The owner-relay direct contact is the *only* in-band channel from the relay to the owner. Adding a chat-event flowing over it is the protocol-level way to communicate a final state.

### 4.2 Wire format

GADT constructor (Protocol.hs, after `XGrpRelayNew` at line 446):

```haskell
XGrpRelayRej :: ChatMsgEvent 'Json
```

Tag (Protocol.hs:991, after `XGrpRelayNew_`):

```haskell
XGrpRelayRej_ :: CMEventTag 'Json
```

String encoding (Protocol.hs:1049, after `XGrpRelayNew_ -> "x.grp.relay.new"`):

```haskell
XGrpRelayRej_ -> "x.grp.relay.rej"
```

String decoding (Protocol.hs:1108, in the lookup map after `"x.grp.relay.new" -> XGrpRelayNew_`):

```haskell
"x.grp.relay.rej" -> XGrpRelayRej_
```

`toCMEventTag` (Protocol.hs:1163, after `XGrpRelayNew _ -> XGrpRelayNew_`):

```haskell
XGrpRelayRej -> XGrpRelayRej_
```

JSON parse (Protocol.hs:1321, after `XGrpRelayNew_ -> XGrpRelayNew <$> p "relayLink"`):

```haskell
XGrpRelayRej_ -> pure XGrpRelayRej
```

JSON encode (Protocol.hs:1391, after `XGrpRelayNew relayLink -> o ["relayLink" .= relayLink]`):

```haskell
XGrpRelayRej -> JM.empty
```

`isForwardedGroupMsg` (Protocol.hs:485-505): NO entry. `XGrpRelayRej` is owner-relay direct, never forwarded by relays.

### 4.3 Signing

No entry in `requiresSignature` (Protocol.hs:1226-1238). That gate is for "events that must have a valid signature in relay groups" — i.e., events that travel over channel forwarding paths and must be verifiable by recipients without a session with the sender. `XGrpRelayRej` travels on the owner-relay direct contact connection, which is authenticated at the agent layer (the SMP queue between owner and relay is established via `joinConnection` and bound to the connection's keys). Adding the message to `requiresSignature` would require a group signing context that doesn't exist on the owner-relay direct channel.

`unverifiedAllowed` (Protocol.hs:1247-1256): no change.

### 4.4 Forward compatibility

An older owner client that doesn't recognize the tag falls through the `_ -> XUnknown_` branch in `strDecode` (Protocol.hs:1134) and the JSON parser produces `XUnknown "x.grp.relay.rej" params` (line 1352). The owner's CONF handler at Subscriber.hs:760-773 dispatches on `chatMsgEvent`:

```haskell
GCInviteeMember ->
  case chatMsgEvent of
    XGrpAcpt memId | ... -> ...
    XGrpRelayAcpt relayLink | ... -> ...
    _ -> messageError "CONF from invited member must have x.grp.acpt"
```

An old client lands in the catch-all `_ ->` and emits `messageError`. The relay's connection remains in CONF state on the agent side until the owner's connection-cleanup path runs. The owner's GroupRelay stays at RSInvited indefinitely. This is the same end state as today's "relay never responds" failure mode, with one extra log line — acceptable degradation.

**UI implication for old owners.** The owner UI shows the relay row at `RSInvited` (yellow indicator, "invited" text) with no progress and no transition. The owner has no in-UI way to learn that the relay refused; they may eventually conclude the relay is unreachable and remove it manually. Not a bug, but reviewers may ask about it — this is documented in §12 as part of the forward-compat rollup.

The relay sends `XGrpRelayRej` over an agent connection whose chat version is `chatV = chatVR `peerConnChatVersion` chatVRange`. The agent layer carries the message regardless of the chat-version negotiated; only the *parser on the receiving side* matters.

### 4.5 channels-protocol.md updates

#### Signing table (docs/protocol/channels-protocol.md:97-112)

NO new row. `x.grp.relay.rej` is owner-relay direct and not subject to group signing. Add a separate paragraph under "Message signing" → "Signing scope" if a reviewer requests it, but the table is correct as-is.

#### New subsection "Relay refusal"

Insert after the existing "### Relay addition" subsection (lines 61-73), before "### Subscriber connection" (line 75). Heading level `###`, three short paragraphs:

```markdown
### Relay refusal

When a relay leaves a channel via `APILeaveGroup`, it persists a record of the channel's owner-published short link. A future `x.grp.relay.inv` carrying the same short link is refused before any state is created on the relay.

**Refusal signal.** The relay accepts the inbound contact request and immediately sends `x.grp.relay.rej` over the established direct contact channel. The payload is an empty JSON object. The relay then tears down the connection.

**Owner-side handling.** The owner's CONF handler transitions the relevant `GroupRelay` row from `RSInvited` to `RSRejected` and marks the corresponding relay-member record as `GSMemRejected`. The owner does not retry; the refusal is final unless the relay's operator explicitly clears the refused record on the relay (out-of-band relay operator command).

**Privacy.** The refusal signal carries no information beyond "this invitation is refused" — no reason code, no identifier of other refused channels, no timestamp.
```

---

## 5. Owner-side state

### 5.1 New RelayStatus variant: `RSRejected`

`src/Simplex/Chat/Types/Shared.hs:81-114`:

```haskell
data RelayStatus
  = RSNew
  | RSInvited
  | RSAccepted
  | RSActive
  | RSInactive
  | RSRejected -- relay refused the channel invitation; permanent until operator clears on the relay side
  deriving (Eq, Show)
```

Add `RSRejected` cases to `relayStatusText` and the `TextEncoding` instance — encoding text `"rejected"`:

```haskell
relayStatusText = \case
  ...
  RSRejected -> "rejected"

instance TextEncoding RelayStatus where
  textEncode = \case
    ...
    RSRejected -> "rejected"
  textDecode = \case
    ...
    "rejected" -> Just RSRejected
    _ -> Nothing
```

### 5.2 CONF handler — new case in Subscriber.hs:760-773

Inline use of `getGroupRelayByGMId` + `updateRelayStatusFromTo` (both existing helpers; Store/Groups.hs:1307 and 1438-1442). No new top-level helper — the composition is used in exactly one place.

```haskell
GCInviteeMember ->
  case chatMsgEvent of
    XGrpAcpt memId | ... -> ...
    XGrpRelayAcpt relayLink | ... -> ...
    XGrpRelayRej
      | memberRole' membership == GROwner && isRelay m -> do
          relay <- withStore $ \db -> do
            liftIO $ updateGroupMemberStatus db userId m GSMemRejected
            relay <- getGroupRelayByGMId db (groupMemberId' m)
            liftIO $ updateRelayStatusFromTo db relay RSInvited RSRejected
          let m' = m {memberStatus = GSMemRejected}
          deleteMemberConnection m'
          toView $ CEvtGroupRelayUpdated user gInfo m' relay
      | otherwise -> messageError "x.grp.relay.rej: only owner can receive relay rejection for a relay member"
    _ -> messageError "CONF from invited member must have x.grp.acpt"
```

`updateRelayStatusFromTo db relay RSInvited RSRejected` atomically updates only if the current row is RSInvited, otherwise returns the row unchanged. This matches the valid-transition rule in §5.3 — racing CONFs cannot regress an RSRejected row back into a working state, and a row that's somehow at RSAccepted/RSActive when XGrpRelayRej arrives stays put rather than being silently overwritten.

### 5.3 Valid transitions to RSRejected

- `RSInvited → RSRejected`: only valid transition. Enforced by `updateRelayStatusFromTo`.
- `RSAccepted/RSActive → RSRejected`: NOT valid. By the time the owner sees `XGrpRelayAcpt`, the relay has already accepted; a subsequent rejection should never arrive. If the relay's row was somehow RSAccepted or RSActive (e.g., owner retried `addRelays`), `updateRelayStatusFromTo` returns the row unchanged and the existing status sticks.
- `RSInactive → RSRejected`: NOT valid in the same way. After a previous leave the owner's row is RSInactive (set by `deactivateRelayIfNeeded`/the LINK callback per 2026-04-29 plan §1). The owner's *re-add* via `addRelays` creates a NEW `GroupMember` and NEW `GroupRelay` row (Commands.hs:3957-3960), so the new row starts at RSNew → RSInvited; the old RSInactive row is left untouched. The rejection is on the NEW row.

### 5.4 Does `addRelay` already create a persisted GroupRelay row before rejection?

Yes. Reading Commands.hs:3947-3976:

- Line 3957: `createRelayForOwner` — creates the `GroupMember` row.
- Line 3958: `createGroupRelayRecord` — creates the `GroupRelay` row with `RSNew`.
- Line 3959: `createRelayConnection` — creates the connection.
- Line 3972: `joinConnection` — sends `XGrpRelayInv` via the agent.
- Line 3976: `updateRelayStatusFromTo db groupRelay RSNew RSInvited` — flips status to RSInvited.

So when the relay's `XGrpRelayRej` arrives at the owner's CONF handler, the GroupRelay row already exists with RSInvited. We just flip it to RSRejected. No new persistence path needed.

### 5.5 Tear-down on the owner side

After processing `XGrpRelayRej`:

- The relay has called `deleteAgentConnectionAsync'` on its side (waitDelivery=True). The CONF was delivered.
- On the owner side, we call `deleteMemberConnection m'` (existing helper at Internal.hs ~1440) to delete the agent connection and the connection row. The relay member row is kept with `GSMemRejected` (mirrors the subscriber-side behavior in `acceptGroupJoinSendRejectAsync` at Internal.hs:986).
- The GroupRelay row stays with RSRejected — this is the persistent state the UI surfaces.

### 5.6 Owner retry stability

The owner has no automatic retry loop for an outstanding `XGrpRelayInv` — `addRelays` is a one-shot user-triggered command (Commands.hs:3942-3976). The only "retry" path is the operator explicitly running `/relays` or the GUI's "Add relay" action again. Each such retry constructs a new `GroupMember` + `GroupRelay` row (independent of any prior rejected row). The new row goes RSNew → RSInvited and faces a fresh refusal.

So "flapping" between rejected and accepted across retries cannot happen: each retry is a new row, and the previous rejected row stays rejected.

The agent-level connection retry (transient network failure on `joinConnection` itself) is *before* the rejection signal is received and doesn't interact with RSRejected.

### 5.7 Subscriber.hs:760-773 — full updated dispatch

```haskell
GCInviteeMember ->
  case chatMsgEvent of
    XGrpAcpt memId
      | sameMemberId memId m -> do
          withStore $ \db -> liftIO $ updateGroupMemberStatus db userId m GSMemAccepted
          allowAgentConnectionAsync user conn' confId XOk
      | otherwise -> messageError "x.grp.acpt: memberId is different from expected"
    XGrpRelayAcpt relayLink
      | memberRole' membership == GROwner && isRelay m -> do
          withStore' $ \db -> setRelayLinkConfId db m confId relayLink
          void $ getAgentConnShortLinkAsync user CFGetRelayDataAccept (Just conn') relayLink
      | otherwise -> messageError "x.grp.relay.acpt: only owner can add relay"
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
    _ -> messageError "CONF from invited member must have x.grp.acpt"
```

`updateGroupMemberStatus db userId m GSMemRejected` is the existing helper; `GSMemRejected` already exists (Types.hs:1250). `getGroupRelayByGMId` and `updateRelayStatusFromTo` are both already exported from `Store/Groups.hs` (88-91). No new helper introduced.

The existing `CEvtGroupRelayUpdated` event (Controller.hs:900) already carries `user :: User, groupInfo :: GroupInfo, member :: GroupMember, groupRelay :: GroupRelay` — exactly what we need. No new event type.

---

## 6. Operator command — relay side

### 6.1 Naming

Two candidates:

- **A. `/_relay refused` + `/_relay refused clear <id>`** (chosen).
  - API: `APIListRefusedRelayGroups` and `APIClearRefusedRelayGroup Int64`.
  - CLI sugar: `/relay refused` and `/relay refused clear <id>`.
- **B. `/_relays refused` + `/_relays unrefuse <id>`**.
  - API: `APIListRefusedRelayGroups`, `APIUnrefuseRelayGroup Int64`.
  - CLI sugar: `/relays refused`, `/relays unrefuse <id>`.

**Pick A.** Reasoning:

- The verb "unrefuse" is awkward and not present anywhere else in the codebase. "Clear" is the established term for removing local state (e.g., `ClearGroup`, `APIClearChat`).
- `/relay refused` reads naturally — "show the relay's refused list". The space-separated subcommand pattern matches `/relay test` (Commands.hs:5034) and other multi-word commands.
- The `/_relay` underscore-prefixed form follows the established convention (Controller.hs:407 has `SetUserChatRelays`, and Commands.hs:5033 has `/_relay test`). The CLI sugar form `/relay refused` parallels `/relay test`.

### 6.2 ChatCommand constructors

`src/Simplex/Chat/Controller.hs`, after the existing relay commands (~line 408):

```haskell
| APIListRefusedRelayGroups
| APIClearRefusedRelayGroup {refusedRelayGroupId :: Int64}
```

Response constructors (Controller.hs, after `CRGroupRelays` at ~line 737):

```haskell
| CRRefusedRelayGroups {user :: User, refusedRelayGroups :: [RefusedRelayGroup]}
| CRRefusedRelayGroupCleared {user :: User, refusedRelayGroupId :: Int64}
```

### 6.3 Command parser

`src/Simplex/Chat/Library/Commands.hs:5033-5036`, after the existing `/_relay test` line. Longer prefixes parse first within each pair — attoparsec's `<|>` is left-biased and `"/_relay refused"` would otherwise match the prefix of `"/_relay refused clear 5"` and succeed before the longer alternative gets a chance:

```haskell
"/_relay refused clear " *> (APIClearRefusedRelayGroup <$> A.decimal),
"/_relay refused" $> APIListRefusedRelayGroups,
"/relay refused clear " *> (APIClearRefusedRelayGroup <$> A.decimal),
"/relay refused" $> APIListRefusedRelayGroups,
```

### 6.4 Command handler

`src/Simplex/Chat/Library/Commands.hs`, alongside other relay handlers (look at `APITestChatRelay` at line 1541 for shape):

```haskell
APIListRefusedRelayGroups -> withUser $ \user -> do
  rs <- withStore' $ \db -> listRefusedRelayGroups db user
  pure $ CRRefusedRelayGroups user rs
APIClearRefusedRelayGroup refusedId -> withUser $ \user -> do
  deleted <- withStore' $ \db -> deleteRefusedRelayGroup db user refusedId
  unless deleted $ throwChatError $ CECommandError "no refused relay-group record with that id"
  pure $ CRRefusedRelayGroupCleared user refusedId
```

### 6.5 What state is cleared

`deleteRefusedRelayGroup` deletes exactly the matching row in `refused_relay_groups`. Nothing else:

- The relay's previously-deleted `GroupInfo` for the channel is not recreated (it was deleted at leave time or later by the operator). The next `XGrpRelayInv` from the owner is treated as a fresh invitation — the relay will create a new `GroupInfo` via `createRelayRequestGroup`.
- No relay link is recreated. The relay's worker will go through `getLinkDataCreateRelayLink` (Subscriber.hs:3834-3850) which creates a new relay link.
- No GroupRelay state on the OWNER side is touched. The owner only sees the effect when they issue a fresh `XGrpRelayInv` (by re-adding the relay through their `addRelays` flow).

### 6.6 Side effects: no events emitted

Clearing a refused record is a purely local operator action. No SMP traffic is generated. No `XGrpRelay*` event is sent. The owner only learns about the cleared state when the next `XGrpRelayInv` is accepted (sees `XGrpRelayAcpt`) instead of refused.

This is documented explicitly because a reviewer might expect a "rehab" message to the owner. We deliberately do not send one:

- Sending a message would require maintaining persistent "previously-refused owners" identifiers on the relay — exactly the kind of information leakage §4.5 paragraph 4 forbids.
- The owner has no way to act on the information beyond "now I can try again", which they do anyway via fresh `addRelays`.

### 6.7 Authorization

The operator commands run in-process on the relay's chat-relay binary, invoked by the operator via the CLI or by the local management tool. There is no protocol-level authentication needed: the operator already has process-level access to the relay's process and database. Stated explicitly so reviewers see it was considered.

### 6.8 Output format

`CRRefusedRelayGroups` carries `refusedRelayGroups :: [RefusedRelayGroup]`. The JSON shape (from `defaultJSON` derivation on `RefusedRelayGroup`) has `refusedRelayGroupId`, `groupLink`, `groupDisplayName`, `groupShortDescr`, `groupImage`, `leftAt`, `createdAt`. The CLI view formatter (which converts ChatResponse to text for terminal output) renders each row as a numbered line — file location for that change is the existing view logic in `View.hs` (search for `viewChatResponse` or similar; out-of-scope detail but follows the same shape as `CRGroupRelays` formatting).

---

## 7. iOS UI

### 7.1 Files touched

| File | Change |
|---|---|
| `apps/ios/SimpleXChat/ChatTypes.swift` | Add `rsRejected` case to `RelayStatus` enum + text mapping |
| `apps/ios/Shared/Views/NewChat/AddChannelView.swift` | Add red-dot color for `.rsRejected` in `relayStatusIndicator` |
| `apps/ios/Shared/Views/Chat/Group/GroupMemberInfoView.swift` | Add a one-line "Status reason" row inside the existing channel-info Section when `groupRelay.relayStatus == .rsRejected` |

No new alerts, no new modals, no new screens. Match the surface and language used for `RSInactive` ("removed by operator") so the visual idiom is consistent.

### 7.2 `ChatTypes.swift` (around line 2637-2643 and 2708-2718)

```swift
public enum RelayStatus: String, Decodable, Equatable, Hashable {
    case rsNew = "new"
    case rsInvited = "invited"
    case rsAccepted = "accepted"
    case rsActive = "active"
    case rsInactive = "inactive"
    case rsRejected = "rejected"   // NEW
}

extension RelayStatus {
    public var text: LocalizedStringKey {
        switch self {
        case .rsNew: "new"
        case .rsInvited: "invited"
        case .rsAccepted: "accepted"
        case .rsActive: "active"
        case .rsInactive: "inactive"
        case .rsRejected: "rejected"  // NEW
        }
    }
}
```

### 7.3 `AddChannelView.swift` (around line 487-504 — `relayStatusIndicator`)

Existing logic:

```swift
let color: Color = connFailed || removed ? .red : (status == .rsActive ? .green : .yellow)
let text: LocalizedStringKey = connFailed ? "failed" : memberStatus == .memLeft ? "removed by operator" : removed ? "removed" : status.text
```

New logic — adds `.rsRejected` to the red-dot conditions and surfaces the status name as the text:

```swift
let isRejected = status == .rsRejected
let color: Color = connFailed || removed || isRejected ? .red : (status == .rsActive ? .green : .yellow)
let text: LocalizedStringKey =
    connFailed ? "failed"
    : memberStatus == .memLeft ? "removed by operator"
    : isRejected ? "rejected"
    : removed ? "removed"
    : status.text
```

Why `.rsRejected` doesn't reuse `.memRemoved` semantics: a rejected relay is one that *the relay operator refused*. The member-status path covers the "owner removed the relay" case. They are visually similar (red dot) but the text needs to differentiate so the owner can act correctly. `"rejected"` matches `status.text` for `.rsRejected` so the existing translation key suffices.

### 7.4 `ChannelRelaysView.swift` (around line 114-127 — `ownerRelayStatusText`)

Existing logic falls through to `groupRelays.first(where: ...)?.relayStatus.text` for the default case. Since `RelayStatus.text` already returns `"rejected"` for `.rsRejected` after §7.2, no logic change is required *unless* the `connStatus == .failed` short-circuit (line 117-118) masks the rejected status. It does not — `m.activeConn` is nil after `deleteMemberConnection` on the owner side (§5.5), so `connStatus` is nil and the function falls through to the status-derived text. The status correctly shows "rejected".

No change needed in this file. Stated to document the decision.

### 7.5 `GroupMemberInfoView.swift` — expanded row, the reason string

The expanded view is reached by tapping the relay row in `ChannelRelaysView` and lands at `GroupMemberInfoView` with `groupRelay: GroupRelay?` (file ChannelRelaysView.swift line 69-75).

Existing Section starts at GroupMemberInfoView.swift:177:

```swift
Section {
    let label: LocalizedStringKey = groupInfo.useRelays ? "Channel" : groupInfo.businessChat == nil ? "Group" : "Chat"
    infoRow(label, groupInfo.displayName)
    ...
    if let link = member.relayLink {
        infoRow("Relay link", ...)
    }
    if let address = groupRelay?.userChatRelay.address {
        infoRow("Relay address", ...)
        Button { ... } label: { Label("Share relay address", systemImage: "square.and.arrow.up") }
    }
} header: {
    Text(channelMemberSectionHeader)...
} footer: {
    if groupInfo.useRelays && member.memberRole == .relay {
        Text(relaySectionFooter)...
    }
}
```

Add a new row inside the same Section, right after the existing `Relay address` block, gated on `.rsRejected`:

```swift
if groupRelay?.relayStatus == .rsRejected {
    infoRow("Status", "rejected by relay operator")
}
```

`infoRow` is the existing helper used elsewhere on the same view. The text `"rejected by relay operator"` is the reason string. We do not surface any "left at" timestamp — that's relay-local state the owner has no access to and shouldn't pretend to know.

### 7.6 Why no new alerts or modals

The task explicitly forbids them. The status indicator (small red dot + "rejected" text) and the expanded info row are sufficient for the owner to understand the state and decide to remove the rejected relay from their channel via the existing relay-management UI (currently behind the `// TODO [relays] re-enable when relay management ships` guard in ChannelRelaysView.swift:84-95; the rejected state surface in the indicator does not depend on that UI shipping).

### 7.7 Document Map / spec updates

Per `apps/ios/CODE.md`'s change protocol, the implementer must update:

- `apps/ios/spec/state.md` (covers `ChatTypes.swift`) — add `rsRejected` to the RelayStatus state machine.
- `apps/ios/spec/api.md` (covers ChatTypes + AppAPITypes) — add the new `RelayStatus` value.
- `apps/ios/spec/client/chat-view.md` (covers GroupMemberInfoView) — note the new info row.
- `apps/ios/product/views/group-info.md` (covers GroupMemberInfoView) — note the "Status: rejected by relay operator" row.
- `apps/ios/spec/impact.md` — extend the source-file → product-concept map.
- `apps/ios/product/concepts.md` — add row for "relay rejection" if not present.

These doc updates are part of the implementation, not separate work. The plan's job is to list them so they aren't forgotten.

---

## 8. Test plan

All tests live under `tests/ChatTests/`. Add a new file `tests/ChatTests/RelayRefused.hs` (mirrors the shape of `tests/ChatTests/Channels.hs` for relay-specific test fixtures) and register it in the test driver.

The tests use the existing channel test harness (`prepareChannel2Relays`, `memberJoinChannel`, the `relayN ##> "/cmd ..."` shape used in `testChannelRelayLeave`).

### 8.1 `testRelayRefuseAfterLeave`

This test also serves as the deterministic check that the agent-layer accept-then-delete-with-waitDelivery actually delivers the CONF (see §3.1 last paragraph). Assertions wait on the chat event from the test harness's event queue, not on wall-clock delays.

Sequence:

1. Set up: owner creates a channel with 2 relays (`relay1`, `relay2`) and 1 subscriber (`dan`). Confirm `relay1` and `relay2` are at `RSActive` on the owner side.
2. `relay1` operates `/leave #team`. Owner observes `relay1`'s status transition to `RSInactive` (existing leave flow).
3. Owner attempts to re-add `relay1` to the channel via `/_add relay #team <relay1 address>` (or whatever the public API for `APIAddGroupRelays` is by then). Owner's new `addRelays` call creates a fresh GroupRelay row.
4. The owner blocks on the next `CEvtGroupRelayUpdated` event for `relay1`. When it fires, assert the carried `groupRelay.relayStatus == RSRejected`.
5. Assert:
   - The owner's `apiGetGroupRelays` returns the new row with `relayStatus == RSRejected`.
   - The owner's chat list shows `relay1` with `memberStatus == GSMemRejected` for the newly-attempted member record.
   - The channel's link data does NOT include `relay1`'s relay link (`getConnectedGroupRelays` filters by `relay_status IN (RSAccepted, RSActive)` at Store/Groups.hs:1334, which excludes RSRejected).

Verification uses `withCCStore (getGroupRelays db gInfo)` and inspection of the most recent relay row by `created_at`. Event-driven blocking (no `threadDelay`) makes this test deterministic.

### 8.2 `testRelayClearRefusedAcceptsAgain`

Sequence:

1. Run `testRelayRefuseAfterLeave` (or its setup) so the relay has refused once.
2. `relay1 ##> "/relay refused"` — assert the output contains the row for the channel with `groupDisplayName = "team"`.
3. `relay1 ##> "/relay refused clear <id>"` — assert success.
4. Owner attempts to re-add `relay1` again. This time the relay accepts.
5. Assert the relay's new GroupRelay row transitions through `RSNew → RSInvited → RSAccepted → RSActive` (after the LINK callback fires).

### 8.3 `testRelayDoesNotRefuseUnrelatedChannel`

Negative test ensuring no false positives from identifier collision.

Sequence:

1. Set up two distinct channels with distinct short links (e.g., create two `prepareChannel2Relays` fixtures).
2. `relay1` leaves channel A. Refused record for A's link is written.
3. Owner of channel B (a *different* channel, never served by `relay1` before) issues `XGrpRelayInv` for B.
4. Assert `relay1` accepts: the new request proceeds through `getLinkDataCreateRelayLink`, `acceptOwnerConnection`, and the relay's `relay_own_status` reaches `RSAccepted`.
5. Assert the refused record table contains only the row for A, not B.

### 8.4 `testRelayRefuseRaceConcurrentInvitations`

Concurrent invitations test (the task explicitly asks for this).

Sequence:

1. Set up: `relay1` leaves channel A. Refused record written.
2. Owner of A starts TWO `XGrpRelayInv` invitations to `relay1` concurrently (the test harness can simulate this by sending the message twice over the agent without waiting for the first to complete).
3. Assert that BOTH inbound REQs hit `xGrpRelayInv`, both query the refused table, both find a hit, both send `XGrpRelayRej`, and the owner observes two CONFs each transitioning their respective GroupRelay row to `RSRejected`.
4. Assert no placeholder group is created on the relay (no row added to `groups` for this channel).
5. Assert no duplicate refused record is created on the relay (the UNIQUE index ensures this — but the test also performs a `SELECT COUNT(*)` to confirm).

The "no duplicate state on either side" claim: the relay's `refused_relay_groups` row was created at leave time and not modified by the invitations. The owner's two `addRelays` produced two independent rows in `group_relays` (they were independent invitations); each is transitioned to RSRejected. This is consistent — the test asserts the count is 2, not 1.

### 8.5 `testRelayForwardCompatOldOwner`

Verify graceful degradation on an owner client that doesn't recognize `x.grp.relay.rej`.

Sequence:

1. Override the test owner's `chatVersionRange` so that the JSON parser falls into the `XUnknown_` branch (by ensuring the tag table doesn't include `x.grp.relay.rej`). Easiest way: write the test in two flavors — one with the new parser (current chat version) and one constructing the chat-event JSON manually with the new tag but a parser lacking it.
2. `relay1` refuses (with the new code). Send `XGrpRelayRej` to the old owner.
3. Assert the owner emits `messageError "CONF from invited member must have x.grp.acpt"` (an existing path).
4. Assert the owner's GroupRelay row stays at `RSInvited`. No crash. No DB corruption.

This test confirms the forward-compat story in §4.4 holds.

### 8.6 (removed)

A timing-uniformity test was considered and dropped — flaky timing assertions consume CI cycles and produce no actionable signal. The timing observable is documented as a residual risk in §9.1 item 1.

---

## 9. Adversarial review

### 9.1 Pass 1

**Item 1 — timing side channel on the refusal lookup.**

The relay's `isRelayGroupRefused` is a single indexed SELECT. The refused path then does `agentAcceptContactAsync` + `deleteAgentConnectionAsync'`. The accepted path does `createRelayRequestGroup` (INSERT into `groups`, INSERT into `group_members`, plus several relay-request-data column writes) + `getRelayRequestWorker True`.

Differences in timing:

- Refused path: indexed SELECT + CONF send + connection teardown ≈ a few ms.
- Accepted path: SELECT + several INSERTs + worker wakeup ≈ a few tens of ms, then much more time spent in the worker for the actual processing.

The CONF/CONF-ack pattern means the owner's agent-layer sees the accepted CONF (with XGrpRelayAcpt) only after the worker has done its work — that's hundreds of ms to seconds. The refused CONF (with XGrpRelayRej) arrives within tens of ms. An external observer with passive access to the SMP queue could distinguish "accepted" from "refused" by latency.

The `dummyVerifyCmd` pattern (Subscriber.hs:3739-3870 doesn't have it but `dummyVerifyCmd` is in simplexmq) applies when the operation's existence vs. nonexistence must be indistinguishable. Here the situation is different: the SMP queue between owner and relay is private to that pair. An observer would need to be the owner (in which case the owner is the *intended* recipient of the rejection signal anyway) or to compromise the SMP server. The SMP server sees encrypted traffic and timing; it does not see message bodies. Timing-based inference of "this owner-relay pair has a refusal in progress vs. an acceptance" is realistic only if the SMP server is the adversary AND there's something for the server to learn by that distinction — which in our threat model the SMP server is not (or, in the strictest reading, the SMP server can already infer relay-channel relationships from connection patterns, so the leak from timing is marginal).

Conclusion: not a structural defect that requires dummy-time padding. Documented as a residual timing observable. The plan does NOT pad rejection latency to match acceptance latency, because doing so would force us to fake-execute the entire worker path on the refused side — a much more invasive change.

**Item 2 — information leakage in the rejection signal.**

`XGrpRelayRej` has an empty JSON object payload. No reason code, no identifier, no timestamp. The rejection signal carries one bit: "this invitation refused." The owner learns nothing about other channels the relay refuses, when this channel was left, or why.

The CONF carrying it discloses nothing more than the existing accepted-CONF discloses: that the owner-relay direct contact is alive (which the owner already knew, since they just initiated the connection).

OK. No change.

**Item 3 — refused record exists, relay GroupInfo deleted, vs. vice versa.**

- Scenario A: refused record exists in `refused_relay_groups`, GroupInfo for the channel deleted on relay. Behavior: future `XGrpRelayInv` for the link is refused; relay creates no state. The refused record carries the display fields the operator needs to identify the channel for clearing.
- Scenario B: GroupInfo for the channel exists on relay (e.g., from an older orphan or pre-leave state), no refused record. Behavior: future `XGrpRelayInv` proceeds normally; `createRelayRequestGroup` is called. The existing flow doesn't UNIQUE-constrain on `(user_id, groupRelayInv.groupLink)` for `groups`, so a duplicate placeholder group may be created. This is pre-existing behavior unrelated to refusal — out of scope here.

OK.

**Item 4 — concurrent leave-then-rejoin from the same owner.**

Covered by §3.4. The race is acknowledged as an operator-facing contract, not a mitigated condition. No additional lock or change in this pass.

**Item 5 — operator-clear command running while an `XGrpRelayInv` is being processed.**

Two sub-scenarios:

- Operator runs `APIClearRefusedRelayGroup <id>` while a refused-path `xGrpRelayInv` is concurrent for the same channel: the DELETE and the SELECT race. Either ordering is fine — the SELECT either sees the row (refusal proceeds) or doesn't (the operator just cleared it and the invitation slips through with acceptance). The latter case matches the operator's intent exactly.
- Operator runs `APIClearRefusedRelayGroup <id>` while an *accepted-path* `xGrpRelayInv` is concurrent (e.g., for a channel the relay had refused earlier and is now being re-invited just as the operator clears the refusal): same outcome — the request proceeds, the relay accepts. Consistent with the operator's intent.

OK. No fix needed.

**Item 6 — forward compatibility with older clients.**

§4.4 covers this. Two flavors of older client:

- Old owner: receives `XGrpRelayRej`, parses as `XUnknown`, falls into the catch-all in CONF handler at Subscriber.hs:773, logs `messageError`. No crash. GroupRelay stays at RSInvited (matching the legacy "relay never responds" failure mode). Test in §8.5 covers this.
- Old relay: doesn't know how to write refused records (no migration applied? — but if the binary is old, the migration code isn't there to know about it). Old relay processes XGrpRelayInv normally, regardless of past leaves. The "refuses to rejoin" guarantee doesn't apply to relays running old binaries. This is acceptable — the feature is enforced by the relay's behavior, not by protocol invariants enforced everywhere.

OK.

### 9.2 Pass 2

**Item 7 — operator clears the refusal but the owner's relay member is already in `RSRejected`.**

What happens on the owner side after the operator clears the relay-side refusal? Nothing immediately. The owner's `GroupRelay` row for the previous failed attempt stays at RSRejected. The owner does NOT learn about the clear.

The owner's next `addRelays` action (a fresh user-triggered "Add relay" in UI) creates a NEW relay member + GroupRelay row. That row's invitation reaches the relay, which no longer refuses, so the new row transitions to RSAccepted → RSActive normally. The old rejected row stays as a historical record (matching how RSInactive rows persist).

This matches the task: "Owner-side ability to clear the rejection on their side. Owner's `RSRejected` clears only when the relay's record is cleared and a new accept occurs." Specifically: there's no owner-side clearing. Only a fresh add+accept produces a new RSActive row.

**Item 8 — relay's `GroupInfo` re-create on cleared refusal.**

After clearing, `relay1` is in a clean state for that channel (no GroupInfo, no refused record). When the owner's next `XGrpRelayInv` arrives, `xGrpRelayInv`:

1. Checks `isRelayGroupRefused` → False.
2. Calls `createRelayRequestGroup` → creates a NEW placeholder group row.
3. Worker processes it, calls `getLinkDataCreateRelayLink`, validates `publicGroupId` matches `linkEntityId` (Subscriber.hs:3842) — this works because the channel's identity is the same (the owner hasn't recreated the channel).
4. `acceptOwnerConnection` sends `XGrpRelayAcpt`.
5. Owner sees `XGrpRelayAcpt`, transitions RSInvited → RSAccepted → RSActive on a NEW GroupRelay row.

No conflict with the prior history. OK.

**Item 9 — Down migration leaves data behind.**

`DROP TABLE refused_relay_groups` discards the refusal data. On re-upgrade after a downgrade-then-reupgrade cycle, the user's prior refusals are gone. This is acceptable: data loss on downgrade-upgrade is standard for SimpleX migrations, and the operator can re-leave to re-establish the refusal if needed.

**Item 10 — Multi-user relay binary.**

The relay binary supports multiple users. The refused records are scoped by `user_id`. The lookup uses `(user_id, group_link)`. The CLI commands run in the context of the active user (the existing `withUser` wrapper). Cross-user pollution is impossible.

**Item 11 — `gInfo.groupProfile.publicGroup` is `Nothing` at leave time.**

Structurally impossible for a relay-served channel where the relay is a current member: the placeholder profile (created with `publicGroup = Nothing` by `createRelayRequestGroup` at Store/Groups.hs:1531-1540) is replaced by the real profile via `updateGroupProfile` inside `getLinkDataCreateRelayLink` (Subscriber.hs:3847) before the relay ever transitions to `RSAccepted`. The `APILeaveGroup` handler at §2.3 throws `CEInternalError` if it encounters this state — no silent skip, no log warning. If the invariant ever fires in practice, the error surfaces immediately as a diagnosable bug rather than a hidden refusal hole.

**Item 12 — `chat_schema.sql` regeneration.**

Auto-generated by tests. Per `docs/CONTRIBUTING.md`, the implementer should NOT edit `chat_schema.sql` by hand; running the test suite regenerates it after the migration is registered. Note this in the implementation steps.

### 9.3 Pass 2 — convergence

The above two passes identified items 7-12 in pass 2 that pass 1 did not surface. A third pass would scan the same surface; running it mentally:

- §1: identifier choice and rationale internally consistent.
- §2: schema details, write/read sites, helper functions consistent.
- §3: rejection point ties to identifier choice from §1 correctly.
- §4: new event, signing analysis, forward compat, docs.
- §5: owner transitions, helper function spec consistent.
- §6: operator command, parser shape, side effects, no events.
- §7: iOS minimal-surface changes consistent.
- §8: tests cover the listed risk items from the task plus §9 items.
- §9: residual risk items documented.

Nothing new. Pass 3 finds zero issues. Pass 4 finds zero issues. Two consecutive clean passes — stopping condition satisfied.

---

## 10. Files modified

| File | Change |
|---|---|
| `src/Simplex/Chat/Types/Shared.hs` | Add `RSRejected` to `RelayStatus` (enum + textEncode/textDecode + relayStatusText). |
| `src/Simplex/Chat/Types.hs` | Add `RefusedRelayGroup` record next to `RelayRequestData`. |
| `src/Simplex/Chat/Protocol.hs` | Add `XGrpRelayRej` GADT constructor, tag, str encode/decode, toCMEventTag, JSON parse/encode. No isForwardedGroupMsg / requiresSignature entry. |
| `src/Simplex/Chat/Store/RefusedRelayGroups.hs` | NEW. Insert/check/list/delete helpers. |
| `src/Simplex/Chat/Store/SQLite/Migrations/M20260513_refused_relay_groups.hs` | NEW. CREATE TABLE + UNIQUE INDEX. |
| `src/Simplex/Chat/Store/SQLite/Migrations.hs` | Register the new migration. |
| `src/Simplex/Chat/Store/Postgres/Migrations/M20260513_refused_relay_groups.hs` | NEW. Postgres equivalent. |
| `src/Simplex/Chat/Store/Postgres/Migrations.hs` | Register the new migration. |
| `src/Simplex/Chat/Controller.hs` | Add `APIListRefusedRelayGroups`, `APIClearRefusedRelayGroup` commands; `CRRefusedRelayGroups`, `CRRefusedRelayGroupCleared` responses. |
| `src/Simplex/Chat/Library/Commands.hs` | Add CLI parser entries; add handlers for the two new commands; in `APILeaveGroup`, insert refused record on relay-side leave. |
| `src/Simplex/Chat/Library/Subscriber.hs` | In `xGrpRelayInv`, gate on `isRelayGroupRefused` and send `XGrpRelayRej` on hit. In CONF for `GCInviteeMember`, handle `XGrpRelayRej` (owner side). |
| `simplex-chat.cabal` | Register new modules. |
| `docs/protocol/channels-protocol.md` | Add "### Relay refusal" subsection after "### Relay addition". |
| `apps/ios/SimpleXChat/ChatTypes.swift` | Add `rsRejected` case + text mapping. |
| `apps/ios/Shared/Views/NewChat/AddChannelView.swift` | Add red-dot color for `.rsRejected` in `relayStatusIndicator`. |
| `apps/ios/Shared/Views/Chat/Group/GroupMemberInfoView.swift` | Add "Status: rejected by relay operator" row in expanded relay-info Section. |
| `tests/ChatTests/RelayRefused.hs` | NEW. Tests §8.1 - §8.5. |
| Test driver registration (`tests/ChatTests/Channels.hs` or test list module) | Register the new test module. |

Tests will regenerate `chat_schema.sql` automatically; do not hand-edit.

Per `apps/ios/CODE.md`'s Change Protocol, the implementer also updates the relevant iOS spec/product documents (§7.7) and the spec impact graph.

---

## 11. Out of scope (matches the task prompt boundaries)

- Kotlin / Android / Desktop UI. iOS-only this PR; Kotlin port lands as a separate PR.
- New alerts, banners, modals, compose-bar changes.
- Blocking based on the owner removing the relay (`xGrpMemDel`). Only `APILeaveGroup` writes the refused record.
- Pre-emptive blocking of channels the relay has never served.
- Owner-side ability to clear the rejection on their side. Owner's `RSRejected` clears only when the relay's record is cleared and a new accept occurs.
- `publicGroupId`-based secondary rejection check after `getShortLinkConnReq'`. Reserved for a follow-up that ships its own migration adding the `public_group_id` column.
- Timing-uniform refusal (dummy-execute the accepted path for the refused side). The §9 pass-1 item 1 analysis explains why this isn't needed given the threat model.

---

## 12. Security considerations — explicit roll-up

- **Timing side channel.** Documented in §9 pass 1 item 1. Not mitigated by dummy-execute; the leak surface is to the SMP server, which already infers relationships from connection patterns; the additional information from latency is marginal. Documented limitation, not a vulnerability.
- **Information leakage.** `XGrpRelayRej` is one bit. No reason code, no identifier set, no timestamp. §4.5 documents this in the protocol spec.
- **Operator command authorization.** No protocol-level auth needed. The chat-relay binary runs in the operator's process; the operator's process-level access is the trust root. §6.7 documents this.
- **Owner retry stability.** §5.6. The owner has no automatic retry of `XGrpRelayInv`; each retry is a user-initiated fresh `addRelays`. The new RSRejected GroupRelay row is final.
- **Forward compatibility.** §4.4, §8.5. Older owner ignores `XGrpRelayRej` (logged as `messageError`, no state change, no crash). Older relay doesn't enforce refusal but doesn't break either.
- **Old-owner UI implication.** §4.4. On an older owner client, the relay row remains at `RSInvited` (yellow indicator, "invited" text) indefinitely after refusal. The owner has no in-UI signal that the relay refused; they may manually remove the relay after concluding it's unreachable. Not a bug; called out so reviewers don't ask whether it should be addressed in this PR.
