# Orphan `prepared` connections after a failed connect via a long link

## Problem

Connecting via an **old-format (long) link** whose server is unreachable — contact
address, one-time invitation, or group link — leaves a row in `connections` with
`conn_status = prepared`, `conn_type = contact`, `contact_id IS NULL`. The user
sees the connect error, but the connection stays in the database forever:

- it is **not shown in the chat list**, so there is no way to delete it from the UI;
- for address/group links the receive queue was already created on the user's own
  server, so it is counted in server stats and re-subscribed on every app start.

A new orphan is produced per distinct link — a repeat attempt with the *same* link
reuses the existing prepared connection, found by `via_contact_uri_hash`.

Why the queue exists for some kinds and not others: for a contact URI the agent's
`joinConnSrv` creates our queue with `newRcvConnSrv` *before* `sendInvitation`
reaches the dead server. For an invitation URI the queue is created by
`createReplyQueue` inside `mkAgentConfirmation`, which runs *after*
`agentSecureSndQueue`, so whether a queue leaks there depends on whether the peer's
queue supports sender-securing.

In the apps, short links do not produce this orphan: `getShortLinkConnReq` fails
before anything is created, and a successful short-link flow creates a *prepared
contact/group* chat entity, which is visible in the chat list. See
"Effect on short links" for the one path where a short link behaves like a long one.

## Cause

`connectViaContact` → `connect'` (`src/Simplex/Chat/Library/Commands.hs`) commits
the connection row **before** the only step that can fail:

```haskell
(connId, chatV) <- prepareContact user cReq pqSup       -- agent record only, no network
conn  <- withFastStore' $ \db -> createConnReqConnection ...  -- row committed here
conn' <- joinContact user conn cReq ...                 -- network: throws on unreachable server
```

`createConnReqConnection` (`src/Simplex/Chat/Store/Direct.hs`) writes `ConnPrepared`
and, when `preparedEntity_` is `Nothing`, `Nothing -> (ConnContact, Nothing, Nothing,
Nothing)` — hence `conn_type = contact` even for a group link. `preparedEntity_` is
`Nothing` for every `APIConnect`, which is how a plain long link is connected. (A long
link is not *by itself* entity-less: `connectContactViaAddress`, reached by
`APIConnectContactViaAddress`, passes `Just (PCEContact ct)` with the contact's saved
full link, and that connection carries `contact_id`.) The invitation-link path
(`connectViaInvitation` → `joinNewConn`, with `createDirectConnection'`) has the same
shape.

Keeping the row is **intentional**: it is the retry mechanism introduced in
`9a87f344b` (#5018, "do not regenerate key when accepting connection to avoid
invalidating invitation link on bad networks"). A retry with the same link finds it
by `cReqHash` and re-joins with the same keys and `xContactId`;
`testRetryConnectingViaContactLink` covers this.

What makes it an orphan is the chat-list preview query
(`getContactConnectionChatPreviews_` in `src/Simplex/Chat/Store/Messages.hs`):

```sql
WHERE user_id = ? AND conn_type = ? AND conn_status != ?   -- ConnContact, ConnPrepared
  AND contact_id IS NULL AND conn_level = 0 AND via_contact IS NULL
```

`conn_status != 'prepared'` came from the same #5018 commit ("exclude prepared
connections from API responses"). Because the query already requires
`conn_type = contact AND contact_id IS NULL`, the only rows that filter excludes are
this orphan shape and, for as long as it stays `prepared`, the chat relay test
connection (see below) — prepared connections belonging to a prepared contact or group
have `contact_id` / `group_member_id` set and are shown as their own chats. On success the
same row becomes `ConnJoined` and *is* listed, so the connection is invisible only
while it is prepared, i.e. exactly when the join failed.

## Fix

Show entity-less prepared connections in the chat list so they can be deleted, and
keep the retry behaviour of #5018 unchanged.

```diff
         WHERE user_id = ?
           AND conn_type = ?
-          AND conn_status != ?
+          AND relay_test = 0
           AND contact_id IS NULL
           AND conn_level = 0
           AND via_contact IS NULL
           AND (via_group_link = 0 OR (via_group_link = 1 AND group_link_id IS NOT NULL))
           AND LOWER(local_alias) LIKE '%' || LOWER(?) || '%'
-    params search = (userId, ConnContact, ConnPrepared, search)
+    params search = (userId, ConnContact, search)
```

Nothing else is needed:

- **Deletion already works.** `apiDeleteChat CTContactConnection` uses
  `getPendingContactConnection` / `deletePendingContactConnection`, neither of which
  filters on status, and it calls `deleteAgentConnectionAsync` first — so the agent
  connection and the queue on the user's own server go too, which is what stops the
  per-restart traffic and the stats count. The chat-list context menu already offers
  Delete (`ChatListNavLinkView.kt`), as does the iOS swipe action
  (`ChatListNavLink.swift`).
- **Both clients render `prepared` pending connections correctly.**
  `ConnStatus.prepared → initiated = false` (`ChatModel.kt`, `ChatTypes.swift`), so
  the item reads "requested to connect" / "via contact address link" (or "accepted
  invitation" / "via one-time link" for an invitation link), and the link/QR section —
  which would otherwise show the *peer's* link, since a joining connection stores it
  in `conn_req_inv` — is gated on `initiated` in both apps
  (`ContactConnectionInfoView.kt`, `ContactConnectionInfo.swift`). No client change is
  required.
- **The remaining conditions already hold for these rows.** `createConnReqConnection`
  writes `via_group_link = isJust groupLinkId` and `group_link_id = groupLinkId`
  together, so `via_group_link = 0 OR group_link_id IS NOT NULL` holds for old group
  links (which carry `CRDataGroup`) and for addresses/business links (which do not).
- **No migration.** Orphans already in users' databases become visible and deletable
  immediately.

### Why `relay_test = 0` is required (separate commit)

`APITestChatRelay` creates a connection via `createRelayTestConnection` with
`ConnPrepared`, `conn_type = ConnContact`, `contact_id NULL`, `conn_level = 0`,
`via_contact NULL`, and the schema defaults `via_group_link = 0`, `local_alias = ''`
— i.e. it satisfies every condition of this query except the status filter.

The status filter does not actually hold it out. `processAgentMessageConn` runs
`updateConnStatus` on the entity **before** dispatching to `processDirectMessage`,
and `agentMsgConnStatus` maps `CONF{}` to `ConnRequested`; the relay-test
interception happens inside `processDirectMessage`, i.e. after the status has already
been written. A relay-test connection resolves to `RcvDirectMsgConnection c Nothing`
(`getConnectionEntity`, `ConnContact` with no entity id), so this applies to it. So as
soon as the relay responds, the row is `requested`, not `prepared`, and **today it
already appears in the chat list** as a phantom "connecting" entry — for the duration
of the test (up to 40s), and permanently if the app is killed before
`deleteConnectionRecord` runs, until `cleanupStaleRelayTestConns` removes it 5 minutes
later.

That is a pre-existing bug unrelated to prepared connections, so it is a **separate
commit**. It has to come first: without it, dropping the status filter would also make
the prepared phase of every relay test visible, turning a transient leak into a
permanent one.

Checked against **every** `INSERT INTO connections` site; only these can produce
`conn_type = contact` with `contact_id IS NULL`:

| site | shape | in the list? |
|---|---|---|
| `createConnReqConnection`, `preparedEntity_ = Nothing` | ConnContact, contact_id NULL | **yes** — the orphan being fixed |
| `createConnReqConnection`, `PCEContact` / `PCEGroup` | contact_id set / ConnMember | no |
| `createDirectConnection_`, `contactId_ = Nothing` | ConnContact, contact_id NULL | **yes** — the orphan being fixed |
| `createRelayTestConnection` | ConnContact, contact_id NULL, `relay_test = 1` | no — excluded by the new guard |
| `createRelayConnection`, `createRelayMemberConnectionAsync` | ConnMember | no |
| `createMemberContactConn`, `createMemberContact` | contact_id set | no |
| `createConnection_` | sets `contact_id` whenever connType is ConnContact | no |

No code path updates an existing row *to* `prepared`: `ConnPrepared` appears only at
row creation and as the *from* status of `updateConnectionStatusFromTo`, and
`agentMsgConnStatus` never returns it.

## Effect on short links

None on the normal short-link flows, and no new orphan shape.

- The apps route on **presence of short link data, not link format**
  (`ConnectPlan.kt`): when the link server returned link data they call
  `apiPrepareContact` / `apiPrepareGroup`, which create a *prepared contact or group*;
  the connection then carries `contact_id` (or is `ConnMember`) and is excluded by the
  query's existing conditions, showing as its own chat as today.
- If the link server is unreachable, `connectPlan` → `getShortLinkConnReq` fails
  before anything is written, so a broken short link creates nothing at all.
- The one path where a short link can still reach `connectViaContact` with no prepared
  entity is `APIConnect`, which passes `Nothing` regardless of link format — used by
  the terminal `/c <link>` and by the app's fallback branch when the plan returns `Ok`
  with no link data. There the row is written only because the user attempted a
  connect, and if the join then fails it is the same orphan; showing it is the intended
  fix, not a regression.

## Rejected alternative: delete the connection when the join fails

- It reintroduces #5018 for one-time links: `agentSecureSndQueue` may already have
  secured the peer's queue with our key, so regenerating keys on retry breaks the link
  permanently — the scenario covered by `testRetryConnectingClientTimeout`.
- For addresses and group links it discards the stable `xContactId`, so a retry after a
  *timeout* (where the invitation may in fact have been delivered) creates a duplicate
  contact request on the peer.
- It does nothing for orphans already in users' databases without a migration.

## Cost of the fix

Every failed long-link connect now leaves a visible "connecting" entry that the user
deletes or retries, instead of an invisible row.

## Verification

A SQLite database built from `chat_schema.sql` with one row per affected shape, run
against the old and new predicates:

| row | shape | old query | new query | new query *without* `relay_test = 0` |
|---|---|---|---|---|
| 1 | failed long **contact address** join | hidden | **shown** | shown |
| 2 | failed long **group link** join (`via_group_link=1`, `group_link_id` set) | hidden | **shown** | shown |
| 3 | failed long **one-time invitation** join (`conn_req_inv` set) | hidden | **shown** | shown |
| 4 | **relay test** connection (`relay_test=1`), still `prepared` | hidden | hidden | **leaks** |
| 4b | same row once the relay has responded (`requested`) | **leaks today** | hidden | leaks |
| 5 | healthy `joined` pending connection | shown | shown | shown |
| 6 | prepared **contact** (short link, `contact_id` set) | hidden | hidden | hidden |
| 7 | prepared **group** member (short link, `conn_type=member`) | hidden | hidden | hidden |

`EXPLAIN QUERY PLAN` is identical before and after for all three pagination variants
and matches what is already recorded in `chat_query_plans.txt`, so only the three
predicate lines change there:
`SEARCH connections USING INDEX idx_connections_contact_id (contact_id=?)` +
`USE TEMP B-TREE FOR ORDER BY` for `PTLast`, and
`SEARCH connections USING INDEX idx_connections_updated_at (user_id=? AND updated_at>?/<?)`
for `PTAfter` / `PTBefore`.

Postgres is unaffected: `relay_test smallint DEFAULT 0 NOT NULL`, matching SQLite's
`INTEGER NOT NULL DEFAULT 0`; the literal `0` is valid on both, as the query already
compares `via_group_link` the same way.

No existing test changes behaviour. All 8 explicit `/_get chats … pcc=on` assertions
and all 124 `@@@` / `@@@!` uses were checked against every failed raw-link connect in
the suite — `testRetryConnecting`, `testRetryConnectingClientTimeout`,
`testRetryConnectingViaContactLink`, `testRejectContactAndDeleteUserContact`,
`testPlanShortLinkInvitation` — none of which asserts on a chat list after the failure.
The four `BROKER` retry tests that do assert around a failure use
`/_connect contact @N` / `/_connect group #N`, i.e. prepared entities excluded by the
query's existing conditions.

`getContactConnectionChatPreviews_` is only reached with `withPCC = True`; the terminal
`/chats` passes `False`, so the CLI listing is unchanged.

The new test, `testFailedConnectViaContactLinkInChatList`, connects to a contact
address with the server down, asserts that the pending connection is listed, deletes
it, and asserts the list is empty.

## Known limitations and follow-ups (not in this change)

1. **The entry appears only after the chat list is reloaded.** A failed `APIConnect`
   returns an error, and every constructor with a `PendingContactConnection` field is a
   `CR*` command response, not a `CEvt*` event (the one event that could carry one
   structurally, `CEvtChatInfoUpdated`, is only ever emitted with `SCTDirect` or
   `SCTGroup`) — so the client never learns of the connection until the next
   `getUserChatData` / `startChat` load. Orphans already in
   the database therefore appear at next start, but one created now is not visible in
   the session that created it. Surfacing it live needs a new response field or event.
2. **A profile with no real conversations still hides the list.** `shouldShowOnboarding`
   (`OnboardingCards.kt`) and iOS `hasConversations` (`ChatListView.swift`) count
   `ContactConnection` as *not* a conversation. Pre-existing — it applies to today's
   `ConnJoined` pending connections too — and unaffected by this change in practice,
   since `CreateActiveUser` gives a profile a note folder and (best-effort, inside
   `catchAllErrors`) preset contact cards, which already make `shouldShowOnboarding`
   true on a fresh profile.
3. **The wording is misleading for a permanently failed attempt.** The entry reads
   "requested to connect" / "via contact address link" with "You will be connected when
   your connection request is accepted, please wait or check later!" — the same text as
   a genuinely pending request. Branching on `pccConnStatus == prepared` in
   `PendingContactConnection.displayName` / `description` would fix this in both clients
   with a couple of new strings.
4. **Pre-existing: the peer's one-time link can be presented as the user's own.**
   `ContactConnectionInfoView.kt` writes `chatModel.showingInvitation` with no
   `initiated` gate (only the layout below it is gated); `NewChatView.kt` seeds itself
   from that state, skips `createInvitation`, and `InviteView` renders the link under
   "Share this 1-time link" with a QR and share button. This is reachable today with any
   `ConnJoined` invitation-join pending connection, which stays listed while the peer is
   offline; this change adds permanent rows of the same kind, so it is worth a one-line
   guard on `initiated`. iOS is unaffected.
5. **Delete during an in-flight join.** `deleteChat CTContactConnection` takes
   `withConnectionLock` while the connect path holds only `withInvitationLock`, so a
   delete can land between row creation and the join. `updateConnectionStatusFromTo`
   then finds no row and returns the connection unchanged, and `CRSentInvitation` hands
   the client a pending connection that no longer exists — a ghost row until the next
   reload. Harmless and self-correcting.
