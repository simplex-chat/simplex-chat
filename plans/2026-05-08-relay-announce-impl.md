# Implementation plan: owner-pushed relay announcement (`XGrpRelayNew`)

Companion to `/workspace/plans/2026-05-08-relay-announce.md` (overview). This file is the
file-and-symbol-level diff guide. Read the overview first.

All file/line references are against the working tree at the start of the implementation;
they will drift slightly as edits land. Cite this plan when something looks unfamiliar.

---

## 1. Step ordering and commit shape

Compilation must hold after every step. The order below is the smallest reviewable
sequence; steps S1–S5 are intentionally split into two PRs: a wire-format-only PR and a
behaviour PR, so reviewers can evaluate the new event in isolation.

PR 1 — wire format (compiles, no behaviour change)

- S1 `Protocol.hs`: add `XGrpRelayNew` constructor, tag `x.grp.relay.new`,
  `toCMEventTag`, JSON encode/parse, `isForwardedGroupMsg` row.
- S2 `Protocol.hs`: extend `requiresSignature` to include `XGrpRelayNew_`.
- S3 `docs/protocol/channels-protocol.md`: signing-table row + new "Relay addition"
  subsection.

PR 2 — receive + send + forward (one logical change)

- S4 `Store/Groups.hs`: add active-status filter in place to the inner
  `getGroupMemberByRelayLink` lookup inside `getCreateRelayForMember`.
- S5 `Library/Internal.hs`: introduce `connectToRelayAsync`. Move `syncSubscriberRelays`
  from `Commands.hs` to `Internal.hs` and pivot its add-half to `connectToRelayAsync`.
- S6 `Library/Commands.hs`: drop the now-unused sync `connectToRelay`; `APIConnectPreparedGroup`
  keeps the existing sync call (see §6 — left in place); update import of
  `syncSubscriberRelays`. Keep `retryRelayConnectionAsync` as-is.
- S7 `Library/Subscriber.hs`: add forward-only case to `processEvent`, add
  `XGrpRelayNew` case to `processForwardedMsg`, add owner send at end of LINK callback.
- S8 Tests in `tests/ChatTests/Channels.hs` (or split across files per §11).

S1–S3 land in PR 1; S4–S8 in PR 2. PR 2 must not be split: the owner-side send and the
subscriber-side handler must ship together to avoid asymmetry where one direction is
emitted but not consumed.

---

## 2. `Protocol.hs` — wire format (S1, S2)

### 2.1 GADT constructor (Protocol.hs:443-445)

Add at line 446 (immediately after `XGrpRelayTest`, before `XGrpMemNew`):

```
XGrpRelayNew :: ShortLinkContact -> ChatMsgEvent 'Json
```

Rationale: keeps the relay-related events grouped. Single `ShortLinkContact` field, no
record syntax, mirrors `XGrpRelayAcpt :: ShortLinkContact -> ChatMsgEvent 'Json` at
Protocol.hs:444. **Do not** introduce a record wrapper or `RelayInfo` envelope — the
overview locked the shape to a single field; the receiver looks the link up locally.

### 2.2 Tag GADT and string encoding (Protocol.hs:986-988, 1043-1045, 1101-1103)

- Insert `XGrpRelayNew_ :: CMEventTag 'Json` after `XGrpRelayTest_` (line 988).
- In `strEncode`, add `XGrpRelayNew_ -> "x.grp.relay.new"` after `XGrpRelayTest_` (line 1045).
- In `strDecode` map (line 1103), add `"x.grp.relay.new" -> XGrpRelayNew_` after
  `"x.grp.relay.test" -> XGrpRelayTest_`.

The `_` -> `XUnknown_` fallback at line 1129 already gives correct old-client behaviour;
no change there.

### 2.3 `toCMEventTag` (Protocol.hs:1133-1184)

Add `XGrpRelayNew _ -> XGrpRelayNew_` after the `XGrpRelayTest` line (1157).

### 2.4 JSON parse / encode (Protocol.hs:1308-1314, 1378-1382)

- `appJsonToCM`/`msg` parser (1271-1344): add
  `XGrpRelayNew_ -> XGrpRelayNew <$> p "relayLink"`
  immediately after `XGrpRelayAcpt_` (line 1309). Field name `"relayLink"` matches the
  `XGrpRelayAcpt` precedent (1309) — do not invent a new key.
- `chatToAppMessage`/`params` encoder (1354-1410): add
  `XGrpRelayNew relayLink -> o ["relayLink" .= relayLink]`
  after the `XGrpRelayAcpt` clause (1379). Same key.

### 2.5 `isForwardedGroupMsg` (Protocol.hs:484-503)

Add a single case `XGrpRelayNew _ -> True` in the listed group of `True` cases (e.g.
between `XGrpMemNew {} -> True` (495) and `XGrpMemRole {} -> True` (496)). Rationale:
relays must forward this event to subscribers; it is the entire point. The comment
above the function (line 482) already says actual filtering happens in `processEvent`;
the listing here is for the send-side `memberSendAction` decisions about pre-member
forwarding (Internal.hs:2202), which we want to behave the same as `XGrpMemNew`.

### 2.6 `requiresSignature` (Protocol.hs:1221-1231)

Add `XGrpRelayNew_ -> True` to the list. Rationale: this is an administrative event;
must reuse the existing required-signature gate. Without this, `withVerifiedMsg`
(Subscriber.hs:3385-3407) would treat a missing signature as acceptable
(`signatureOptional` becomes `True`), breaking the threat model from
`channels-protocol.md` §"Message signing".

### 2.7 What NOT to change

- Do not touch `hasNotification` or `hasDeliveryReceipt` — relay-add is administrative,
  not a notification surface for the user. The relay's delivery pipeline (delivery_task /
  delivery_job) already handles forwarding without an entry in either table.
- Do not touch `unverifiedAllowed` (Protocol.hs:1240-1249). Owners always know their own
  key; subscribers always have the owner key from link data. The "no key" branch is for
  member-to-member events, not for owner-signed administrative events.

---

## 3. `Store/Groups.hs` — active-status filter on relay-link lookup (S4)

### 3.1 The current shape (Store/Groups.hs:1376-1407)

`getCreateRelayForMember` runs `getGroupMemberByRelayLink` (an inner `let` at 1380-1385),
falls back to `createRelayMember`. The inner SQL filters on `group_id = ? AND relay_link
= ?` only — no status filter. The schema permits multiple rows with the same
`(group_id, relay_link)` over time: when a relay is removed by the owner, its row is
preserved with `GSMemLeft` (this drives the "removed by operator" UI on the subscriber
side). For the existing subscriber-join flow (`APIConnectPreparedGroup → connectToRelay`,
Commands.hs:2141 / 3597-3613) the unfiltered lookup happens to work because rows in that
path are recent and active. For the new subscriber receive path we must filter to *active*
rows so that a re-add after a `GSMemLeft` creates a fresh row instead of resurrecting the
historical one.

### 3.2 The change

Add an active-status filter in place to the existing inner `let`. No extraction, no new
top-level function:

```
getGroupMemberByRelayLink =
  maybeFirstRow (toContactMember vr user) $
    DB.query
      db
      (groupMemberQuery <> " WHERE m.group_id = ? AND m.relay_link = ? AND m.member_status IN (?,?,?,?,?,?,?)")
      ( (groupId, relayLink)
          :. (GSMemIntroduced, GSMemIntroInvited, GSMemAccepted, GSMemAnnounced)
          :. (GSMemConnected, GSMemComplete, GSMemCreator)
      )
```

The seven statuses are the `memberCurrent'`-true set from Types.hs:1318-1334:
`GSMemIntroduced`, `GSMemIntroInvited`, `GSMemAccepted`, `GSMemAnnounced`,
`GSMemConnected`, `GSMemComplete`, `GSMemCreator`. Tuple shape is illustrative — match the
existing `:.` chaining convention used elsewhere in the module.

Justification for SQL-level filter (vs. Haskell post-filter): `maybeFirstRow` returns
whatever row the engine yields first. With `GSMemLeft` history rows preserved alongside
active rows, an unfiltered query is non-deterministic without `ORDER BY`. Filtering in
SQL eliminates the ambiguity at the query level. The list of statuses is tiny and
stable.

### 3.3 Existing call site unaffected

`getCreateRelayForMember`'s lone existing caller is `connectToRelay` (Commands.hs:3597-3613),
invoked from `APIConnectPreparedGroup` (Commands.hs:2141). Rows it creates are inserted
with `GSMemAccepted` (line 1403), which is `memberCurrent`. The filtered lookup still
finds them on retry, so the subscriber-join flow's reuse-on-retry behaviour is preserved.
No signature or call-site change is needed in `Commands.hs`.

### 3.4 What NOT to change

- Do not extract `getGroupMemberByRelayLink` to a top-level function. The
  filter-in-place shape is the minimal diff; both call sites (existing
  `APIConnectPreparedGroup → connectToRelay` and new `connectToRelayAsync`) share one
  definition by going through `getCreateRelayForMember`.
- Do not modify `getGroupMember`, `getGroupMembers`, or other lookups. The change is
  scoped to the relay-link lookup inside `getCreateRelayForMember`.
- Do not delete the historical `GSMemLeft` row when re-adding a relay. The
  delete-or-update logic in `syncSubscriberRelays` removes only when the link is no
  longer in the channel's link data (Commands.hs:3623-3633); on re-add it remains in
  link data, so the historical row stays untouched and is filtered out by the new
  lookup.

---

## 4. `Library/Internal.hs` — `connectToRelayAsync` and moved `syncSubscriberRelays` (S5)

### 4.1 New helper

Place near the existing relay/group plumbing (e.g. after `setGroupLinkDataAsync` at
Internal.hs:1316-1322) so that all relay-link async helpers cluster together.

```
connectToRelayAsync :: User -> GroupInfo -> ShortLinkContact -> CM ()
```

Body — described, not coded:

1. `vr <- chatVersionRange`.
2. `gVar <- asks random` — needed by `getCreateRelayForMember` via the create branch.
3. `relayMember <- withFastStore $ \db -> getCreateRelayForMember db vr gVar user gInfo relayLink`.
   With the active-status filter from §3.2, this atomically returns the existing active
   row (if any) or creates a fresh `GSMemAccepted` row. `GSMemLeft` history rows are
   invisible to the lookup, so re-add after removal creates a new row beside the
   historical one.
4. Idempotence check on `activeConn relayMember`:

   - `Just _` → `pure ()` (skip; an earlier path already bound a connection on this
     row. The agent layer handles transient failures internally; permanent-failure
     recovery is deferred to explicit retry paths and channel re-join.)
   - `Nothing` → either a freshly created row or a leftover row from an attempt that
     never bound a connection; proceed to step 5.
5. `subMode <- chatReadVar subscriptionMode`.
6. `newConnIds <- getAgentConnShortLinkAsync user CFGetRelayDataJoin Nothing relayLink`
   (Commands.hs:2479 — already returns `(CommandId, ConnId)` for binding).
7. `withFastStore' $ \db -> createRelayMemberConnectionAsync db user gInfo relayMember relayLink newConnIds subMode`
   (Direct.hs:225-244).
8. Return. Continuation is the existing `CFGetRelayDataJoin` LDATA callback at
   Subscriber.hs:1131-1160 — unchanged.

Store-call conventions: `getCreateRelayForMember` is `ExceptT StoreError IO`, so use
`withFastStore`. `createRelayMemberConnectionAsync` is `IO`, so `withFastStore'`. Both
match what `retryRelayConnectionAsync` (Commands.hs:2168-2174) and `connectToRelay`
(Commands.hs:3597-3613) already use.

### 4.2 Locking argument

`connectToRelayAsync` is called from two sites (after this PR):
- The forwarded `XGrpRelayNew` handler in `processForwardedMsg`. The entire receive path
  is wrapped in `withEntityLock "processAgentMessage" lockEntity` (Subscriber.hs:117) and
  the lock entity for any group connection is `CLGroup groupId` (Connections.hs:51-72).
- `syncSubscriberRelays`, called from `APIGetUpdatedGroupLinkData` inside
  `withGroupLock "syncSubscriberRelays" groupId` (Commands.hs:1787) — also `CLGroup groupId`.

Both paths therefore hold the same lock for the same group. The `getCreateRelayForMember`
call (lookup-or-create, atomic within its own transaction) and the `activeConn` check on
its result are performed under that lock, and any subsequent agent commands
(`getAgentConnShortLinkAsync`, `createRelayMemberConnectionAsync`) only persist state
that will be observed under the same lock by the next event's check. No additional lock
is needed. No `justCreated` flag, no per-link mutex.

### 4.3 Move `syncSubscriberRelays` from `Commands.hs:3614-3633` to `Internal.hs`

Place right below `connectToRelayAsync`. Body changes:

- Replace the single `connectToRelay` call inside the `forM_ newRelayLinks` loop
  (Commands.hs:3621-3622) with `connectToRelayAsync user gInfo rlnk`. Keep the
  per-relay `void . tryAllErrors` wrapping verbatim — equivalent to the existing
  pattern at Commands.hs:3621-3622 with only the connect helper substituted:

  ```
  forM_ newRelayLinks $ \rlnk -> void . tryAllErrors $
    connectToRelayAsync user gInfo rlnk
  ```

  `connectToRelayAsync` can fail at three local operations
  (`getCreateRelayForMember` → store error if creating; `getAgentConnShortLinkAsync`
  → agent error; `createRelayMemberConnectionAsync` → store error). Per-relay error
  isolation costs nothing and ensures a failure on relay R1 does not short-circuit
  attempts for R2, R3 in the same batch. The outer `void . tryAllErrors` (3615) is
  preserved as well; it remains the catch-all for the whole sync operation.
- Remove half: keep verbatim — `deleteMemberConnection`, `deleteOrUpdateMemberRecord`
  calls (3631-3632), the `null activeRelayMembers` guard (3629), and the
  `localRelayMembers` filter (3617).

Type signature after move (matches current except for module location):

```
syncSubscriberRelays :: User -> GroupInfo -> [ShortLinkContact] -> CM ()
```

### 4.4 Imports / exports

- `Internal.hs` likely already imports the relevant `Store.Groups`/`Store.Direct`
  symbols; if `getCreateRelayForMember` or `createRelayMemberConnectionAsync` are not
  imported, add them.
- Export `connectToRelayAsync` and `syncSubscriberRelays` from `Internal.hs` (it is a
  module without an explicit export list — see "module Simplex.Chat.Library.Internal where"
  near top — so any new top-level binding is automatically exported).

### 4.5 What NOT to change

- Do not change `connectToRelay` (sync, Commands.hs:3597-3613) signature. PR 2 keeps it
  alive for the subscriber's initial channel-join — see §5.1.
- Do not touch `retryRelayConnectionAsync` (Commands.hs:2168-2174). Its retry semantics
  are tied to the subscriber's initial channel-join (`APIConnectPreparedGroup`,
  Commands.hs:2108-2161) and remain on that path.
- Do not introduce any new `withGroupLock` inside `connectToRelayAsync`. The caller's
  lock is sufficient (see §4.2).

---

## 5. `Library/Commands.hs` — drop unused sync helper, fix imports (S6)

### 5.1 Decide what to delete

Audit `connectToRelay` callers: only `APIConnectPreparedGroup` (Commands.hs:2108-2161)
uses it. That command is the **subscriber's** initial channel-join entry point
(not owner channel creation — owner-side relay invitation flows through
`APIAddGroupRelays` and `x.grp.relay.inv`/`x.grp.relay.acpt`, see channels-protocol.md
§"Relay acceptance"). At join time, the subscriber does
`mapConcurrently (connectToRelay user gInfo') relays` (Commands.hs:2141) to connect to
all relays in parallel during the join handshake.

The sync flow is intentional there:
- the user is on a "joining channel" spinner;
- failures must surface immediately to UI so the user sees a meaningful error
  instead of a stuck spinner;
- the existing flow already chains async retry via `retryRelayConnectionAsync`
  (Commands.hs:2159) for the relays that fail with temporary errors — sync handles
  the immediate-feedback path, async handles tail recovery.

**Default; reviewer to confirm**: keep `connectToRelay` for the
`APIConnectPreparedGroup` path. The overview's "deletable once event-driven path is
wired" was conditional ("once no caller remains"). Subscriber join has different UX
semantics from event-driven relay sync; convergence onto async-only is a separate
concern and is out of scope for this PR.

### 5.2 Move `syncSubscriberRelays` reference

`APIGetUpdatedGroupLinkData` at Commands.hs:1787-1788 currently references
`syncSubscriberRelays` as a local where-binding inside `processChatCommand` (it is the
inner `where`-defined function at 3614). After moving it to `Internal.hs`, the call site
at 1788 unchanged but the local binding at 3614-3633 deleted. Imports auto-rerouted via
`Simplex.Chat.Library.Internal` (already imported at the top of Commands.hs).

### 5.3 What NOT to change

- Do not change `APIGetUpdatedGroupLinkData`'s `withGroupLock` wrapper or the `gInfo'`
  it passes to the sync function. The lock and the link-data refresh are still required.
- Do not change `retryRelayConnectionAsync`. It is the right primitive for the
  subscriber-join retry use case (`APIConnectPreparedGroup` tail recovery,
  Commands.hs:2159); the new event-driven path is independent.

---

## 6. `Library/Subscriber.hs` — owner send, relay forward, subscriber receive (S7)

### 6.1 Owner — send site in LINK callback (Subscriber.hs:1300-1333)

The relevant block:

```
LINK _link auData ->
  withCompletedCommand conn agentMsg $ \CommandData {cmdFunction} ->
    case cmdFunction of
      CFSetShortLink ->
        case (ucGroupId_, auData) of
          (Just groupId, UserContactLinkData UserContactData {relays = relayLinks}) -> do
            (gInfo, gLink, relays, relaysChanged) <- withStore $ \db -> do
              gInfo <- getGroupInfo db vr user groupId
              gLink <- getGroupLink db user gInfo
              relays <- liftIO $ getGroupRelays db gInfo
              (relays', changed) <- liftIO $ foldrM (updateRelay db) ([], False) relays
              liftIO $ setGroupInProgressDone db gInfo
              pure (gInfo, gLink, relays', changed)
            toView $ CEvtGroupLinkDataUpdated user gInfo gLink relays relaysChanged
            where
              updateRelay db relay@GroupRelay {relayLink, relayStatus} (acc, changed) =
                case relayLink of
                  Just rLink
                    | rLink `elem` relayLinks && relayStatus == RSAccepted -> do
                        relay' <- updateRelayStatus db relay RSActive
                        pure (relay' : acc, True)
                    ...
```

Plan:

1. Extend the `updateRelay` accumulator from `([GroupRelay], Bool)` to
   `([GroupRelay], Bool, [ShortLinkContact])`: keep the existing `Bool` for the
   `CEvtGroupLinkDataUpdated`'s `relaysChanged` flag, and add a new
   `[ShortLinkContact]` collecting the links of relays that just transitioned
   `RSAccepted → RSActive`. In the `RSAccepted → RSActive` branch, replace
   `pure (relay' : acc, True)` with `pure (relay' : acc, True, rLink : newlyActiveLinks)`.
   In the `RSActive → RSInactive` branch (which also sets `changed = True` today,
   line 1330), keep the `Bool` flip but pass `newlyActiveLinks` through unchanged
   — removals are explicitly out of scope for the announce push (overview
   §"Owner — send site"). Other branches pass both extra fields through unchanged.
2. Bind `(gInfo, gLink, relays, relaysChanged, newlyActiveLinks)` from the `withStore`
   block; pass `relaysChanged` to the existing `CEvtGroupLinkDataUpdated` `toView`
   call so its semantics are preserved exactly; pass `newlyActiveLinks` to the new
   send block in step 3.

3. After the `toView`, add (still inside `(Just groupId, UserContactLinkData ...)`
   case). The send block fetches all relay members and filters inline (see §6.2):

   ```
   let newlyActiveLinks = ...  -- collected from the fold accumulator
   forM_ (L.nonEmpty newlyActiveLinks) $ \newlyActive -> do
     allRelayMembers <- withFastStore' $ \db -> getGroupRelayMembers db vr user gInfo
     let recipients = filter
           (\m -> memberStatus m == GSMemConnected && relayLink m `notElem` newlyActiveLinks)
           allRelayMembers
         events = XGrpRelayNew <$> newlyActive
     unless (null recipients) $
       void $ sendGroupMessages user gInfo Nothing False recipients events
   ```

   - `sendGroupMessages` signature (Internal.hs:2049): `User -> GroupInfo -> Maybe
     GroupChatScope -> ShowGroupAsSender -> [GroupMember] -> NonEmpty (ChatMsgEvent e) -> CM (NonEmpty (Either ChatError SndMessage), GroupSndResult)`.
   - `Nothing` for `Maybe GroupChatScope`: this is administrative, not scoped to a
     support side-channel. Justified by `XGrpInfo` / `XGrpPrefs` send patterns elsewhere
     where signed admin events use `Nothing`.
   - `False` for `ShowGroupAsSender`: this is signed by the owner; relays must verify
     the owner signature via `withVerifiedMsg` (Subscriber.hs:3385). `asGroup = True`
     uses `CBChannel` binding (channels-protocol.md §"Channel-as-sender"), which has no
     member ID and is not what we want — verification needs the owner's member ID.
   - `void` discards the per-member result; logging is handled by the existing send
     pipeline.

### 6.2 Recipients query

No new Store helper. Inline the filter in the LINK callback:

- After the `withStore` block that runs the fold, call
  `withFastStore' $ \db -> getGroupRelayMembers db vr user gInfo` to get
  `[GroupMember]` (Store/Groups.hs:1185-1191).
- Filter in Haskell:
  `filter (\m -> memberStatus m == GSMemConnected && relayLink m \`notElem\` newlyActiveLinks)`.
- `memberStatus == GSMemConnected` already implies `memberCurrent` (Types.hs:1318-1334);
  do not add a redundant `memberCurrent` check.
- Pass the filtered list as the recipients argument to `sendGroupMessages`.

Justification: one-shot use, low frequency (LINK callback only), no benefit
to introducing a new Store function. `vr` and `user` are already in scope at
the LINK callback (Subscriber.hs:1306, inside `processContactConnMessage`).

### 6.3 Defensive batching

Per overview, the receive-loop group lock serializes `XGrpRelayAcpt` handling (which
calls `setGroupLinkDataAsync`) so each LINK callback typically sees a single
`RSAccepted → RSActive` transition. Coding the send as `NonEmpty (XGrpRelayNew _)` keeps
the path correct if the agent ever consolidates `setConnShortLink` writes. The
`L.nonEmpty newlyActive` guard handles the empty case (no transition this callback).

### 6.4 Relay — `processEvent` case (Subscriber.hs:990-1032)

Insert this case before the catch-all `_ -> Nothing <$ messageError ...` at 1032:

```
XGrpRelayNew _ -> pure $ Just (DeliveryTaskContext (DJSGroup {jobSpec = DJDeliveryJob {includePending = False}}) False)
```

Justification by precedent:
- `XMsgNew` (991) → `newGroupContentMessage` returns `Just (ctx js)` where `ctx` is
  `DeliveryTaskContext js False` (line 983). The `False` is the "don't include in
  history" flag — relay forwards but doesn't snapshot.
- `XGrpMemNew` (1011) → `xGrpMemNew` returns `Just (ctx (DJSGroup {…}))`. We want
  identical broadcast scope (all subscribers, no support-only channel).
- `XGrpDel` (1022) is the only event that uses `DJRelayRemoved`; that is for
  relay-removal-by-owner, not relevant here.

`DJDeliveryJob {includePending = False}` matches `XMsgNew`'s default (search
`Delivery.hs` for `DJDeliveryJob` constructor — `includePending = False` is the
non-administrative-state-change default; `XGrpInfo` uses `True` because it changes
group profile state and pending members must learn it on accept). The relay
**stores no member record for the announced relay** (overview §"Relay — forward
only"), so subscribers entering pending state later will instead learn via on-open
`syncSubscriberRelays`. `includePending = False` is correct.

What NOT to do:
- Do not add an `xGrpRelayNew` handler on the relay side — the relay is forward-only.
- Do not create a `GroupMember` record for the announced relay on the relay. Departure
  from `XGrpMemNew` semantics is intentional; relays don't connect to other relays of
  the same channel.

### 6.5 Subscriber — `processForwardedMsg` case (Subscriber.hs:3354-3383)

Add to the inner `case event of` (just before the catch-all `_ -> messageError ...` at
3378):

```
XGrpRelayNew rl -> withAuthor XGrpRelayNew_ $ \_author -> connectToRelayAsync user gInfo rl
```

Notes:
- `withAuthor` (3380-3383) requires `author_ :: Maybe GroupMember` to be `Just` —
  enforces the "must be attributable to a signing owner" invariant. `FwdChannel` (3351
  via `processForwardedMsg (VMUnsigned chatMsg) Nothing`) makes `author_ = Nothing`,
  which `withAuthor` rejects with `messageError`. This is the desired behaviour: the
  event must be owner-signed and attributed.
- Signature verification happens upstream in `withVerifiedMsg` (3385-3407) before
  `processForwardedMsg` is invoked (3348-3349). With `requiresSignature` returning
  `True` for `XGrpRelayNew_` (§2.6), an unsigned forwarded `XGrpRelayNew` triggers the
  bad-signature path at 3389-3391.
- The `_author` is used only as an authorisation token here. The connect helper does
  not need the author identity — the author is the owner whose link data already
  carried the relay key, and the relay member's keys/profile are fetched from the
  relay's own short link.

### 6.6 `xGrpMsgForward` — no change needed

Already validates the forwarder is a relay (`isMemberGrpFwdRelay`, 3340) and dispatches
to `processForwardedMsg`. Adding the new event tag inside that switch is the entirety
of the receive-side change.

### 6.7 What NOT to change in `Subscriber.hs`

- Do not touch the `CFGetRelayDataJoin` LDATA callback (1131-1160). Its end state
  (subscriber-side) is exactly the continuation we want; the helper hands off to it.
- Do not touch the `CON` handler at 823-865 for relay members. The `firstConnectedHost`
  branch (855-859) handles the first-connected-relay UI events; subsequent relays go
  through 859. After `XGrpRelayNew`-driven connect, the new relay's `CON` will land in
  this same handler and get `firstConnectedHost = False` (because at least one relay is
  already connected), which is correct.
- Do not modify the `CONF`/`XGrpRelayAcpt` path at 768-772. That is owner-side.

---

## 7. `docs/protocol/channels-protocol.md` updates (S3)

### 7.1 Signing-required table

Section: `## Protocol → ### Message signing → "Which messages require signatures:"`
table (lines 84-97). Add a row after `x.grp.mem.restrict`:

```
| `x.grp.relay.new` | Announce new relay to subscribers | Required |
```

Phrasing matches existing `Description` cells (verb + object).

### 7.2 New subsection "Relay addition"

Insert after the existing `### Relay acceptance` subsection (lines 42-58). Heading
level `###`, four short paragraphs:

1. **Owner-side trigger.** When the owner has accepted a relay (existing flow,
   `x.grp.relay.acpt` at line 36) and the agent confirms the link-data update by
   delivering the LINK event, the owner promotes the relay locally to active and
   sends `x.grp.relay.new` to every other currently-connected relay of the channel
   (excluding the relay being announced).
2. **Wire format.** Single-field JSON object: `{"relayLink": "<short link>"}`.
   Owner-signed via the same `CBGroup` binding prefix used for all administrative
   events (see [Message signing](#message-signing)).
3. **Relay forwarding semantics.** Each relay forwards `x.grp.relay.new` verbatim to
   all of its subscribers via the standard delivery pipeline (delivery_task /
   delivery_job, see [Delivery pipeline](#delivery-pipeline)). The relay does **not**
   create a member record for the announced relay — relays do not connect to other
   relays of the same channel.
4. **Subscriber receive semantics.** The subscriber resolves the announced short link
   asynchronously, creates a relay-member row (or reuses an existing active row), and
   binds the resulting agent connection without blocking the receive loop. If the
   subscriber's client doesn't recognise the event (older version), it is parsed as
   `XUnknown` and ignored; the next `APIGetUpdatedGroupLinkData` (channel open) reaches
   the same end state via `syncSubscriberRelays`.
5. **Idempotence.** The receive loop wraps each agent message in a per-group entity
   lock (`CLGroup groupId`); the same lock is held by `APIGetUpdatedGroupLinkData`.
   A duplicate `x.grp.relay.new` arriving from a second relay finds an active row +
   active connection and is a no-op.

### 7.3 What NOT to change

- Do not renumber existing sections.
- Do not modify the `Binary batch format` section — `x.grp.relay.new` is a
  `signedElement` like every other administrative event; no new ABNF.
- Do not touch the `Channel-as-sender messages` section — `XGrpRelayNew` is owner-bound,
  `CBGroup`, never `CBChannel`.

---

## 8. Test plan (S8)

All tests live under `tests/ChatTests/Channels.hs` (or a dedicated
`tests/ChatTests/Channels/RelayAnnounce.hs` if the file is getting unwieldy — confirm
with reviewer). Each test maps to a row in the overview's "Test surface".

| Overview test | Concrete test name | Harness |
|---|---|---|
| Owner adds relay → subscribers receive `XGrpRelayNew` and connect without channel open | `testRelayAnnounceOnlineSubscriber` | uses `testChat3` (owner + relay + subscriber); after channel is up, owner adds a second relay; assert subscriber's relay-member count for that group becomes 2 with both `GSMemConnected`, no `APIGetUpdatedGroupLinkData` invoked. |
| Two relays forward the announce; subscriber connects exactly once | `testRelayAnnounceDedupes` | `testChat4` (owner + 2 existing relays + subscriber); owner adds third relay; both existing relays forward; assert exactly one new relay-member row, exactly one connection. Inspect via `withCCStore (getGroupRelayMembers …)`. |
| Race vs. `APIGetUpdatedGroupLinkData` for same relay | `testRelayAnnounceRaceWithSync` | drive `APIGetUpdatedGroupLinkData` and `XGrpRelayNew` concurrently; assert no double row; rely on the existing `withGroupLock` to serialize. |
| `GSMemLeft` row preserved on re-add | `testRelayAnnounceReAddPreservesHistory` | owner adds relay, removes it, adds again with same link; assert two `GroupMember` rows for that link (one `GSMemLeft`, one current); the historical row is what drives the "removed by operator" UI. |
| Old subscriber ignores | `testRelayAnnounceOldSubscriber` | use `chatVersionRange` overrides to simulate an older subscriber; assert event is logged as unknown but produces no error item; `syncSubscriberRelays` invocation on next channel open creates the row. |
| Old relay drops | `testRelayAnnounceOldRelay` | inverse: relay's `chatVersionRange` does not include `XGrpRelayNew_` → `processEvent` default `messageError "unsupported"`. Subscribers fall back to on-open sync. |
| Bad signature | `testRelayAnnounceBadSignature` | inject an unsigned (or wrong-signed) `XGrpRelayNew` directly via the test SMP harness; assert `RGEMsgBadSignature` chat item is created on subscriber. |

Helpers reused: `withSmpServer`, `testChat`, `testChat3`, `testChat4`, `awaitListChat`,
`withCCStore`, `getGroupRelayMembers`. Add a small helper in the test module
`assertRelayMemberCount :: TestCC -> GroupId -> Int -> IO ()` if not already present.

For the dedup test specifically, assertion shape:

```
m <- getGroupRelayMembers db vr user gInfo
let relayRows = filter (\GM -> relayLink GM == Just newRl && memberCurrent GM) m
length relayRows `shouldBe` 1
length (filter (isJust . activeConn) relayRows) `shouldBe` 1
```

---

## 9. Risk register

1. **Race: event arrives during channel open.** The receive loop and
   `APIGetUpdatedGroupLinkData` share `CLGroup groupId`. Whichever path runs first
   creates/uses the row; the second sees an active row + active conn (or creates one
   if not yet) and is a no-op. Tested via `testRelayAnnounceRaceWithSync`.

2. **Agent coalescing of `setConnShortLink` writes.** Today the receive-loop group lock
   serializes `XGrpRelayAcpt` handling, so each LINK callback sees one transition. If
   the agent ever batches multiple writes into one callback, the `NonEmpty
   (XGrpRelayNew _)` send path stays correct: every newly-active relay gets announced.
   No fix needed; defensive shape is already there.

3. **Old relay between owner and subscriber.** Old relay's `processEvent` default branch
   drops the event with `messageError "unsupported"`. Subscribers behind that relay
   recover via on-open `syncSubscriberRelays`. Documented in the protocol doc and
   covered by `testRelayAnnounceOldRelay`.

4. **Malformed signature.** `requiresSignature XGrpRelayNew_ = True` causes
   `withVerifiedMsg` to reject and produce `RGEMsgBadSignature`. Standard path; tested.

5. **Agent error during `getAgentConnShortLinkAsync` (step 3 of
   `connectToRelayAsync`).** If the failure happens before
   `createRelayMemberConnectionAsync` runs, `activeConn` is `Nothing`
   and the next trigger retries automatically. If the call succeeds
   but a later async step (LDATA, CONF, CON) stalls, `activeConn`
   exists in a non-`ConnReady` state; the chat layer does not retry
   by design (Option A simple skip). The agent layer's internal
   retries on subscription resume drive recovery for transient
   network failures. Permanent stalls are recovered via explicit
   retry paths (`retryRelayConnectionAsync`, channel re-join).

6. **Link-data fetch failure after pre-created member row.** Two
   sub-cases. (a) `createRelayMemberConnectionAsync` not yet run:
   `activeConn = Nothing`, next trigger retries (`XGrpRelayNew`
   arrival from another relay or channel re-open via
   `syncSubscriberRelays`). (b) Connection record exists but LDATA
   failed: `activeConn = Just _`, chat layer skips by Option A;
   agent layer retries internally on subscription resume.

7. **Active-status filter on lookup breaks other call sites.** The filter is added in
   place on `getCreateRelayForMember`'s inner lookup. Its lone existing caller is the
   subscriber-join path (`APIConnectPreparedGroup` → `connectToRelay`, Commands.hs:2141
   / 3597-3613); rows there are created with `GSMemAccepted`, which is `memberCurrent`,
   so the filtered lookup still finds them on retry. Observable behaviour unchanged for
   the existing caller. Audit done in §3.3; reviewer to confirm.

8. **Multiple owners (future).** `LINK` callback only fires for the local owner's own
   `setConnShortLink` calls (per existing TODO at Subscriber.hs:1327-1329). A second
   owner adding a relay won't trigger the event from this owner — the second owner
   would emit it themselves. Out of scope for current single-owner channels.

---

## 10. Backward compatibility

- **No schema migration.** The plan adds zero columns and zero tables. The new lookup
  uses an existing column (`group_members.relay_link`) with an existing index path.
- **No protocol-version bump in the chat versioning.** The new tag is parsed as
  `XUnknown` by clients that do not recognise `"x.grp.relay.new"` (Protocol.hs:1129
  default branch in `strDecode`). `XUnknown` is silently ignored when reached by
  `processEvent` (Subscriber.hs:1032 catch-all `messageError`); since this is on the
  receive side of an old client, the message is logged as unsupported and the channel
  state is unaffected.
- **No serialization-compat shim.** The single-field JSON form means old clients fall
  through to `XUnknown_` cleanly without any optional-field hand-rolling.

---

## 11. Out of scope

- **Owner authorization-chain pushes.** Adding/removing owners is governed by the
  multi-owner roadmap (channels-overview.md §"Governance evolution"). `XGrpRelayNew`
  does not carry owner-chain payload.
- **Profile pushes.** Subscriber profile changes are out of scope; relay profile
  arrives via the relay's own link data in the existing `CFGetRelayDataJoin` LDATA
  flow.
- **Content batching beyond the LINK callback.** The `NonEmpty` shape is defensive
  for agent-side coalescing, not a general batching mechanism.
- **Retry-on-failure semantics for the new async path.** Existing
  `retryRelayConnectionAsync` (Commands.hs:2168-2174) covers the subscriber-join
  retry of failed initial connects (`APIConnectPreparedGroup` tail recovery). For
  event-driven re-attempts, on-open `syncSubscriberRelays` is the recovery mechanism;
  per-link retry timers are not added.
- **Deletion of `connectToRelay` (sync).** Default kept. The lone caller is
  `APIConnectPreparedGroup` (subscriber's initial channel-join flow,
  Commands.hs:2108-2161), not owner channel creation. Deletion is a reviewer-confirmed
  follow-up if subscriber-join is converged onto async — see §5.1 for why the sync
  flow is intentional there.

---

## 12. Concerns with overview

- **Overview §"Files touched" lists "remove sync `connectToRelay`".** After tracing
  Commands.hs:2141 (`mapConcurrently (connectToRelay user gInfo') relays` inside
  `APIConnectPreparedGroup`), the sync helper still has a real caller — the
  **subscriber's initial channel-join** (not owner setup). Deleting it now would
  either break that path or force `APIConnectPreparedGroup` onto the async helper,
  which is a separate concern (different UX expectations: spinner-blocking immediate
  feedback vs. fire-and-forget). Plan defers this to a follow-up. Reviewer to confirm.

- **Overview §"Subscriber — receive" mentions `withAuthor XGrpRelayNew_`.** That
  function name (the tag) does not currently exist in `Protocol.hs`; it lands in §2.2
  of this plan. Naming preserved verbatim.

- **Overview §"Test surface" "Owner with old relay → relay drops the event".** The
  current `processEvent` default branch is at Subscriber.hs:1032. Verified: the
  default `_ -> Nothing <$ messageError ("unsupported message: " <> tshow event)`
  drops the event after logging. This matches the overview's expectation.
