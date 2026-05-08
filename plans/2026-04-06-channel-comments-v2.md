# Plan v2: Comments on channel messages

## 1. Context

Channels (PR #6382 chat-relays MVP) are groups where one node relays signed
posts from owners to many subscribers. Channel **comments** add Telegram-style
discussion threads under each post: a `comments(N)` affordance on the post
opens a flat thread with its own composer, where subscribers can comment and
quote both the parent post and other comments.

This plan is v2 for the `f/channel-comments` feature branch. v1
(`plans/2026-04-06-channel-comments.md`) was largely implemented on the
backend side, with several deliberate design deviations. v2 ratifies (or
rejects) each as-built deviation, identifies the small remaining backend gaps,
and slices the unstarted iOS work in light of the as-built Haskell surface.
Kotlin Multiplatform remains deferred. v2 is self-contained: where v1 said
something correct that survives the deviations, the relevant content is
restated here verbatim.

## 2. As-built audit

The branch is at HEAD `912657058`. Three-dot diff against `master` shows
~21 backend files and ~3 test files changed; iOS is untouched.

### 2.1 What landed

**Protocol & messaging** (verified at the line numbers given):

- `MsgContainer` is a single record (Protocol.hs:652-666) with
  `quote :: Maybe QuotedMsg`, `parent :: Maybe MsgRef`, `forward :: Maybe Bool`
  as independent optional fields. `mcEmpty` plus smart constructors `mcSimple`
  / `mcQuote` / `mcComment` / `mcForward` (Protocol.hs:672-697). The four
  `MC*` sum constructors and `ExtMsgContent` are gone. `parseMsgContainer` is
  rewritten to read each discriminator independently (Protocol.hs:867-892);
  `msgContainerJSON` is rewritten to fold three discriminator fields with the
  same field-ordering as before (Protocol.hs:934-943). `isMCForward`,
  `isForwardedGroupMsg`, `cmToQuotedMsg` all rewritten.
- `commentsVersion = VersionChat 18` and `currentChatVersion = 18`
  (Protocol.hs:159, 92).
- `MsgPrefs {commentsDisabled :: Bool}` (Protocol.hs:263-268) is carried as an
  optional field `prefs :: Maybe MsgPrefs` on `XMsgUpdate` (Protocol.hs:433).
  Wire serialized via `("prefs" .=? prefs)` so absent on legacy messages.

**Local types**:

- `ChannelMsgInfo {channelMsgItem :: CChatItem 'CTGroup, channelMsgSharedId
  :: SharedMsgId}` in Messages.hs:348-352. Helper `channelMsgRef
  :: ChannelMsgInfo -> MsgRef` in Messages.hs:358-364 — placed in
  `Messages.hs` (NOT `Protocol.hs` as v1 proposed).
- `CIMeta` extended with `parentChatItemId :: Maybe ChatItemId`,
  `commentsTotal :: Int`, `commentsDisabled :: Bool` (Messages.hs:543-547).
  `mkCIMeta` and `dummyMeta` updated (Messages.hs:556, 592-594).
- `ChatInfo.GroupChat` is **still 2-parameter** (Messages.hs:170);
  `ChannelMsgInfo` is threaded as a separate parameter through send/receive
  paths and embedded into the returned `Chat` only at the iOS layer.
- `ChatRef` is **unchanged** (Messages.hs:161-166); `APIGetChat` carries
  `parentItemId :: Maybe ChatItemId` as a separate field on its record
  (Controller.hs:322).

**Roles, preferences, controller config**:

- `GRCommenter` inserted between `GRObserver` and `GRAuthor`
  (Types/Shared.hs); `"commenter"` text encoding added.
- `CommentsGroupPreference {enable, closeAfter}` fully wired through
  `Types/Preferences.hs` (`GroupFeature`/`SGroupFeature` arms, `groupPrefSel`,
  `toGroupFeature`, `GroupPreferences`/`FullGroupPreferences`,
  `defaultGroupPrefs` (FEOff), `defaultBusinessGroupPrefs`, `emptyGroupPrefs`,
  `setGroupPreference_`, `GroupPreferenceI`, `HasField "enable"`,
  `GroupFeatureI 'GFComments`, `GroupFeatureNoRoleI 'GFComments`,
  `groupParamText_`, `mergeGroupPreferences`, `toGroupPreferences`, JSON with
  `omittedField`).
- `channelSubscriberRole = GRCommenter` default (Chat.hs:117). Channel-level
  override `comments = CommentsGroupPreference {enable = FEOn, closeAfter =
  Nothing}` is set inside the `APINewPublicGroup` handler
  (Commands.hs:2484-2485) — the only owner-side channel-creation path.

**Receive / send / API surface**:

- New commands `APISendComment` (Controller.hs:327, handler at
  Commands.hs:630-646) and `APISetCommentsDisabled` (Controller.hs:328,
  handler at Commands.hs:647-662). CLI parsers `/_comment` and
  `/_comments_disabled` plus `parent=` token on `/_get chat`.
- `prohibitedGroupContent` extended with `Maybe ChannelMsgInfo` and a
  channel-comments arm (Internal.hs:339-352): rejects when not a channel,
  comments feature disabled, parent soft-deleted, or parent
  `commentsDisabled`.
- `commentsClosed :: GroupInfo -> Maybe ChannelMsgInfo -> UTCTime -> Bool`
  helper (Internal.hs:371-379).
- Send-side path `sendGroupContentMessages_` accepts `Maybe ChannelMsgInfo`
  and runs `assertCommentsOpen` plus extended `allowedRole`
  (Commands.hs:4207-4244).
- `prepareGroupMsg` accepts `parentRef_ :: Maybe MsgRef` and folds it into the
  built `MsgContainer.parent` (Internal.hs:204-208).
- Receive path: `newGroupContentMessage` resolves `mc.parent` via
  `getChannelMsgInfoBySharedMsgId` and drops the message on missing/forged
  parent (Subscriber.hs:1985-1991); composes
  `prohibitedGroupContent` plus `commentsClosed` gate
  (Subscriber.hs:1958-1965); threads `Maybe ChannelMsgInfo` through to the
  store.
- `memberCanSend` Nothing-arm corrected from `> GRObserver` to `>= GRAuthor`
  (Subscriber.hs:1527).
- `groupMessageUpdate` (Subscriber.hs:2039-2126) accepts the new optional
  `Maybe MsgPrefs`. The owner branch (line 2089) updates content; the
  moderator branch (line 2090) applies prefs only; the unchanged-content
  branch (line 2114) emits `CEvtChatItemNotChanged` only when prefs were not
  applied. `applyMsgPrefs` runs only on the `CIChannelRcv` arm so non-channel
  groups cannot have `commentsDisabled` set via this path.

**Store**:

- New migrations
  `M20260407_channel_comments` (SQLite + Postgres), registered and exposed.
  Three columns on `chat_items`: `parent_chat_item_id`,
  `comments_total`, `comments_disabled`. Two indexes:
  `idx_chat_items_parent_chat_item_id` and `idx_chat_items_parent_item_ts`.
  Down step also resets `member_role = 'commenter'` to `'observer'`.
- `createNewChatItem_` writes `parent_chat_item_id`
  (Store/Messages.hs:586-607); increments parent's `comments_total` in the
  same transaction (line 606); admits comment items to history via
  `includeInHistory` (Store/Messages.hs:631-635).
- `getChannelMsgInfo`, `getChannelMsgInfoBySharedMsgId`,
  `adjustChannelMsgCommentCount` (with `MAX(0, ...)` clamp),
  `setChannelMsgCommentsDisabled`, `quotedItemInCommentSection`
  (Store/Messages.hs:1462-1509).
- `getGroupChat` and the four pagination helpers
  (`getGroupChatLast_/After_/Before_/Around_/Initial_`) plus
  `getChatItemIDs` accept `Maybe ChatItemId parentChatItemId_`. The new
  arm in `getChatItemIDs` (Store/Messages.hs:1633) filters
  by `parent_chat_item_id = ?` and the default-scope arms add
  `AND parent_chat_item_id IS NULL`.
- `AND parent_chat_item_id IS NULL` added to default-scope SELECTs at
  `findGroupChatPreviews_` (898, 911, 917), `getChatContentTypes` (1209),
  `queryUnreadGroupItems` (1846, 1852),
  `updateGroupChatItemsRead` (2187), `getGroupUnreadTimedItems` (2244),
  `getGroupReportsCount_` (1836), and the per-message-ids audit-list
  predicates referenced in `getChatItemIDs` (1609, 1615, 1621).
- Comment-count decrements: `deleteGroupChatItem` (2918),
  `markGroupChatItemDeleted` (3031), `markGroupCIBlockedByAdmin` (3092),
  `markGroupChatItemBlocked` (3075), `updateGroupChatItemModerated` (2941),
  `updateGroupCIBlockedByAdmin` (2998); each guarded on
  `isNothing wasDeleted` so a second moderator delete on an already-deleted
  comment does not double-decrement. Bulk paths via
  `decrementMemberCommentCounts_` (called from `markMemberCIsDeleted` and
  `updateMemberCIsModerated`) and via inlined `SELECT ... GROUP BY
  parent_chat_item_id` in `deleteGroupExpiredCIs` (Store/Messages.hs:3700)
  and `deleteGroupMember` (Store/Groups.hs:2063-2077).

**Tests** (8 of v1's 16):

`tests/ChatTests/Groups.hs:284-292` block `describe "channel comments"`:
`testChannelCommentSubscriberCanComment`,
`testChannelCommentNotInRegularGroup`,
`testChannelCommentDisabledRejected`,
`testChannelCommentEditDelete`,
`testChannelCommentCountIncrement`,
`testChannelCommentObserverRejected`,
`testChannelCommentMainChatExclusion`,
`testChannelCommentQuote`. Helpers `channelFeaturesNoE2E`
(`tests/ChatTests/Utils.hs:292`) and `lastGroupItemId` (Utils.hs:711). Full
ProtocolTests update for the merged `MsgContainer` and the new
`XMsgUpdate.prefs` field.

**iOS**: zero changes.
`apps/ios/SimpleXChat/ChatTypes.swift`, `apps/ios/Shared/Model/*`,
`apps/ios/Shared/Views/Chat/*` are all untouched — verified no occurrence of
`channelMsgInfo`, `commentsTotal`, `commentsDisabled`, `parentChatItemId`,
`GRCommenter`, or `ChannelMsgInfo`.

### 2.2 As-built decisions

**1. `forward :: Maybe Bool` instead of `Bool` in `MsgContainer`.**

**Decision: ratify.** The serializer (Protocol.hs:943) emits `"forward": true`
only when `forward == Just True`; `Nothing` and `Just False` both omit the
field. The parser (Protocol.hs:884-892) maps absent → `Nothing`, `Bool b` →
`justTrue b`, and `Object _` → `Just True`. Round-trip is byte-identical for
every pre-existing message because no in-flight message ever produced
`"forward": false`. `Maybe Bool` reads slightly worse than the v1-proposed
`Bool`, but reverting it would touch all 50 `mcForward`-adjacent sites for
zero behavioral change. The `Maybe` shape is internally consistent with the
`Maybe`-style discriminator fields `quote` and `parent`.

**2. Per-message comments-disabled in `XMsgUpdate.prefs` instead of a
dedicated `XGrpCommentsDisabled` event.**

**Decision: ratify, with a verification of the receive path.** Carrying the
per-post disable state in an optional `prefs :: Maybe MsgPrefs` sub-object on
the existing `XMsgUpdate` event is strictly better forward-compat than
introducing a new `XGrpCommentsDisabled` event tag: pre-`commentsVersion`
clients silently ignore the unknown field via `omittedField` defaulting,
whereas they would refuse a new event tag entirely. The receive path
(Subscriber.hs:2039-2126) is correct on each branch:

- Owner content edit (line 2089): writes content via `updateCI True ...`
  which itself calls `applyMsgPrefs ci'` (line 2110) so a piggy-backed prefs
  field also takes effect. A content-only edit with no prefs leaves the
  existing `comments_disabled` untouched (`applyMsgPrefs` returns `False`
  when `prefs_ = Nothing` per line 2126).
- Moderator prefs-only update (line 2090): moderator (`memberRole >=
  GRModerator`) is allowed to flip prefs but **not** to update content. The
  branch returns `Just (infoToDeliveryContext ...)` if prefs applied,
  `Nothing` otherwise.
- Unchanged-content branch (line 2114): `prefsApplied <- applyMsgPrefs ci`
  is checked; the legacy `CEvtChatItemNotChanged` only fires when no prefs
  applied. This preserves the old user-visible "not changed" path for
  non-comment edits.
- Non-channel groups: `applyMsgPrefs` is only reachable in the `CIChannelRcv`
  arm (line 2088). A non-channel `XMsgUpdate` with prefs is handled by the
  `CIGroupRcv m'` arm at line 2085, which never calls `applyMsgPrefs`. Wire
  prefs from a non-channel sender are silently discarded by the receiver — a
  defensive no-op.

**3. `APISendComment` as a separate command instead of a parent param on
`APISendMessages`.**

**Decision: ratify.** `APISendComment` carries a comment-specific validation
surface: `useRelays' gInfo` assertion, parent resolution via
`getChannelMsgInfo`, quoted-item containment via
`quotedItemInCommentSection`, and an `assertCommentsOpen` plus extended
`allowedRole` (Commands.hs:4215-4222) that maps `(Nothing, Just _) ->
Just GRCommenter`. Folding all of this into `APISendMessages` would balloon
that handler's branching, hurt readability, and make the comments feature
harder to evolve. The cost is one extra iOS API binding, which is trivial
compared to the iOS UI work either way.

**4. Comments-thread carrier: param-threading vs. embedded in
`ChatInfo.GroupChat`.**

**Decision: option (b) — keep param-threading in Haskell, embed
`channelMsgInfo` as a third associated value of `.group` in iOS.**

The Haskell as-built threads `Maybe ChannelMsgInfo` cleanly through the small
set of paths that need it (`getGroupChat`, `getChatItemIDs`,
`prohibitedGroupContent`, `commentsClosed`, `sendGroupContentMessages_`,
`createNewChatItem_`, `newGroupContentMessage`). Revisiting that to make
`ChatInfo.GroupChat` a 3-tuple would touch dozens of pattern-match sites
across the codebase for no behavioral gain.

iOS has a fundamentally different shape: `ChatInfo` flows through SwiftUI
views and `ChatModel`, where `cInfo.groupChatScope()` is consulted at four
gating sites in `ChatModel.swift` and inside `getCIItemsModel` to decide
secondary-IM routing (`apps/ios/Shared/Model/ChatModel.swift:657, 679, 691,
717, 788`). A "this Chat is a comments thread for parent X" predicate must
be observable from `cInfo` alone at all of those sites; threading a separate
parameter through every caller is impractical. Embedding `channelMsgInfo`
beside the existing `groupChatScope: GroupChatScopeInfo?` mirrors the
existing pattern.

**Cross-platform JSON consequence.** The Haskell `Chat 'CTGroup` returned
from `APIGetChat` JSON-encodes via the existing 2-parameter `GroupChat`
constructor; the wire shape is unchanged. iOS reconstructs the embedded
`channelMsgInfo` locally: when iOS calls `apiGetChat(parent: parentId)`, it
already has the parent post in memory (the parent lives in the main
channel's `ItemsModel`), so it composes the local `ChatInfo.group(_, _,
ChannelMsgInfo(channelMsgItem: parent, channelMsgSharedId: parent.meta.itemSharedMsgId!))`
on receipt. No new JSON field is needed.

The asymmetry is intentional and pragmatic. Documented in `apps/ios/spec/state.md`
under the new `getCIItemsModel` channel-msg branch.

**5. `memberCanComment` defense-in-depth on receive.**

**Decision: add an explicit receive-side guard.** Today the receive path
relies on `prohibitedGroupContent`'s `not (groupFeatureAllowed SGFComments
gInfo)` arm and on the relay's send-side `allowedRole` check. The send-side
check runs on the sender's instance — a malicious or modified subscriber
could bypass it locally; the relay then forwards to all subscribers. Each
receiver should defensively re-check that the message author has
`memberRole >= GRCommenter` before persisting the comment. The cost is a
~10-line helper plus one composition site in `newGroupContentMessage`. The
benefit is mitigation against malicious subscribers running modified
clients that cooperate with a faulty relay.

**6. History replay for new joiners.**

**Decision: defer in v2.** As built, the include-in-history admission for
comment items already lands (Store/Messages.hs:631-635), and
`getGroupHistoryItems` (Store/Messages.hs:3825-3846) returns the top-N items
ordered by `item_ts DESC` with no per-parent cap and no
`parent_chat_item_id IS NULL` filter. The behavior on a new joiner is messy:

- Comments mix into the replay stream by recency. A heavy-comment week can
  push older parent posts out of the cap-N window.
- The receiver's `resolveCommentParent` (Subscriber.hs:1989-1991) drops any
  comment whose parent is not present locally. A comment that lands before
  its parent in the replay stream is silently dropped.
- The parent post's `commentsTotal` arrives at its current (live) value,
  which may exceed the number of comments actually delivered to the joiner.

This is a known limitation; v2 documents it under §6 (Forward compatibility
& threat model) and §8 (Out of scope). The v1 sizing approach — per-parent
cap M plus post-window cap N, both tunable constants — is on record for a
follow-up PR. v2 explicitly excludes the helper
`getChannelMsgCommentsForHistory` and the `getGroupHistoryItems` extension.

**7. Default channel preference at creation site.**

**Decision: ratify.** The override `comments = CommentsGroupPreference
{enable = FEOn, closeAfter = Nothing}` is set in `APINewPublicGroup`
(Commands.hs:2483-2485) — the only owner-side channel-creation path that
sets `useRelays = True`. Verified by grep: the only other `useRelays = True`
caller is `APIPrepareGroup` (Commands.hs:2014), which is the
**subscriber-side join** path; subscribers inherit the published prefs of
the channel they join. There is no "import group converted to channel" path
in the current tree, no business-channel path, and the
`createPreparedGroup` / `createNewGroup` store helpers do not synthesize
prefs themselves. v2 carries the verification step into Slice 1 just so a
future addition to the tree does not silently bypass the override.

**8. Forward-compatibility version gating — VERIFIED GAP.**

The relay-side per-recipient version filter currently only gates by
`groupKnockingVersion` and `contentReportsVersion`
(Internal.hs:1601-1627, Subscriber.hs:3593, 3631). It does **not** gate
`XMsgNew` events whose `MsgContainer.parent` is `Just _`, nor `XMsgUpdate`
events whose `prefs` is `Just _`, against `commentsVersion`.

Forward-compat exposure for pre-`commentsVersion` recipients:

- Receiving an `XMsgNew` with `parent = Just _`: the legacy parser would
  successfully parse the container (the field is optional), but the message
  carries comment semantics that the legacy client renders incorrectly — a
  comment under a channel post would surface as a regular post in the main
  channel chat. **This is a content/UX leak.**
- Receiving an `XMsgUpdate` with `prefs = Just _`: the legacy parser ignores
  the unknown field via `omittedField`. The client doesn't enforce
  `commentsDisabled` locally but won't malfunction. Acceptable degradation.

**Decision: add `commentsVersion` gating in the relay forwarding path for
`parent`-bearing `XMsgNew` only.** Prefs-bearing `XMsgUpdate` is allowed to
degrade silently. The check goes in the delivery-job worker
(Subscriber.hs:3548 / `runDeliveryJobOperation` and the per-recipient
filtering inside `sendLoop` at line 3580) where each recipient's
`peerChatVRange` (or `memberChatVRange`) is already in scope. See §3 step 5.

## 3. Remaining backend work

### 3.1 Receive-side `memberCanComment` (decision 5)

`src/Simplex/Chat/Library/Subscriber.hs` — define alongside `memberCanSend`
(after line 1528):

```haskell
memberCanComment :: Maybe GroupMember -> CM (Maybe a) -> CM (Maybe a)
memberCanComment Nothing a = a
memberCanComment (Just GroupMember {memberRole}) a
  | memberRole >= GRCommenter = a
  | otherwise = messageError "member is not allowed to comment" $> Nothing
```

Compose into `newGroupContentMessage` (Subscriber.hs:1948-1973): when
`mc.parent = Just _` is observed (i.e., `channelMsgInfo_` resolves to
`Just _`), wrap the existing parent-resolved branch in `memberCanComment
m_`. Concretely: after `channelMsgInfo_ <- resolveCommentParent gInfo'
parent_` succeeds with `Just _`, gate the rest of the branch on
`memberCanComment m_`.

The relay's send-side check (`allowedRole = Just GRCommenter` at
Commands.hs:4221) is preserved unchanged; this guard is defense-in-depth on
the receiver.

Test: new `testChannelCommentMemberCanCommentReceiveGuard` (§5).

### 3.2 Default-scope SELECT predicate audit completion

The branch added `AND parent_chat_item_id IS NULL` to most default-scope
SELECTs (enumerated in §2.1 above). The remaining sites identified by
auditing every `chat_items` SELECT in `Store/Messages.hs` against the rule
"every group-context default-scope query that is not explicitly per-parent
must filter `parent_chat_item_id IS NULL`":

- `getGroupNavInfo_.getAfterUnreadCount` (Store/Messages.hs:1875-1890) and
  `getGroupNavInfo_.getAfterTotalCount` (lines 1891-1912) — these compute
  "items after this one" for navigation. **Must filter on the same scope as
  the open chat.** When the open chat is a comments thread (the function
  is called from `getGroupChatAround_` at line 1773, which receives
  `parentChatItemId_`), the predicate must be `parent_chat_item_id = ?`
  with the parent id; when the open chat is the main channel, the
  predicate must be `parent_chat_item_id IS NULL`. As-built has neither
  predicate, so navigation counts in either scope include items from the
  other scope.

  **Fix:** thread the existing `parentChatItemId_` (already available at
  the `getGroupChatAround_` call site) into `getGroupNavInfo_` as a new
  parameter, and apply the appropriate predicate inside both subqueries.
  Mutually exclusive with the existing scope (member-support); both
  predicates added to both subqueries.

- All other `chat_items` queries in `Store/Messages.hs` were verified
  against the audit rule. Single-row lookups by `chat_item_id` (e.g.
  Store/Messages.hs:684, 694, 701, 1504, 3403) are scope-agnostic by id
  and need no predicate. The DELETE queries on `chat_items` for whole-chat
  clear (lines 206, 227, 3683, 3713) are intentionally cross-scope and
  rely on the FK cascade for child comments — no change. Bulk member
  deletion paths (`deleteGroupMember` at Store/Groups.hs:2063 and the
  `decrementMemberCommentCounts_` helper at Store/Messages.hs:1517-1543)
  decrement parent counts before bulk-marking; verified consistent.

Test: extend `testChannelCommentMainChatExclusion` to also assert that
opening the main channel via `CPAround` on a parent post returns nav info
that excludes comments under that post.

### 3.3 Relay forwarding version gate (decision 8)

`src/Simplex/Chat/Library/Subscriber.hs` — `runDeliveryJobOperation` at line
3548, specifically the per-recipient batching inside `deliver` /
`sendLoop`. Today the per-recipient compatibility filter is the
`compatible m` predicate at Internal.hs:1626-1627, called from
`getGroupRecipients` for non-channel groups, and the inline
`maxVersion (memberChatVRange m) >= groupKnockingVersion` checks in the
member-support delivery path (lines 3593, 3631).

Add a per-recipient filter for channel posts:

- Compute, for each `XGrpMsgForward` body about to be batched, the minimum
  required version of any forwarded inner event:
  - For `XMsgNew mc` with `mc.parent = Just _` → `commentsVersion`.
  - Other inner events → existing baseline.
- When iterating recipients in `sendLoop` / `deliver`, skip recipients
  whose `maxVersion (memberChatVRange m)` (or `peerChatVRange` of their
  active connection, matching the convention in
  `getGroupRecipients.compatible`) is below that minimum.

This sits in the relay-to-subscriber path because the relay is the only
node that fans out to many version-heterogeneous subscribers. The
owner-to-relay leg is owner-controlled, and owners share a single
`currentChatVersion` with their relays in practice.

**Why prefs-bearing `XMsgUpdate` is intentionally NOT gated:** the
`prefs` field is dropped silently by older parsers via `omittedField`. The
content portion of `XMsgUpdate` is unaffected. Older subscribers simply
fail to enforce `commentsDisabled` locally, which is acceptable
degradation: the parent post still shows, the disabled state is
non-binding for them, and the relay (which knows the disabled state) still
prevents their comments from being accepted. Documented in §6.

Test: new `testChannelCommentVersionGated` (§5) sets a subscriber's
peer chat version to `commentsVersion - 1` via existing test infrastructure
and asserts that a comment-bearing `XMsgNew` is not delivered.

### 3.4 No other backend changes

Specifically NOT in scope of v2:

- `getChannelMsgCommentsForHistory` helper, per-parent cap M, and the
  `getGroupHistoryItems` extension (decision 6 — deferred).
- `XGrpCommentsDisabled` as a dedicated event tag (decision 2 — replaced
  by `MsgPrefs` on `XMsgUpdate`).
- `ChatInfo.GroupChat` 3-parameter shape (decision 4 — Haskell stays
  param-threaded).
- `ChatRef.channelMsg_` (decision 4).
- Subscriber profile dissemination for comments (out of scope, follow-up).
- Per-comment notifications (out of scope).
- `DeliveryWorkerScope` for comments (out of scope; comments share the
  channel post worker for batching).

## 4. Remaining iOS work

The iOS surface is unstarted. Per `apps/ios/CODE.md`'s three-layer Change
Protocol (read product → spec → source, then implement, then update all
three layers), this section enumerates the iOS API types, model, and view
work refreshed for the as-built Haskell surface (decisions 3, 4, and the
`MsgPrefs`-on-`XMsgUpdate` shape).

### 4.1 iOS API types

`apps/ios/SimpleXChat/ChatTypes.swift`:

1. New struct mirroring the Haskell `ChannelMsgInfo`:
   ```swift
   public struct ChannelMsgInfo: Decodable, Hashable {
       public var channelMsgItem: ChatItem
       public var channelMsgSharedId: String  // SharedMsgId is base64 string
   }
   ```
   Used as a third associated value on `.group` (decision 4 / option b).

2. Extend `ChatInfo.group` (line 1376) from:
   ```swift
   case group(groupInfo: GroupInfo, groupChatScope: GroupChatScopeInfo?)
   ```
   to:
   ```swift
   case group(groupInfo: GroupInfo, groupChatScope: GroupChatScopeInfo?, channelMsgInfo: ChannelMsgInfo?)
   ```
   Every existing `.group` pattern match in `ChatTypes.swift` (~25 sites)
   gains `_` for the new third position. The third arg is decoded as
   `nil` from the wire (the Haskell `Chat` JSON has no field for it) and
   set to `Just _` only by the iOS layer when constructing the local
   "comments thread" Chat object. Use `Decoder` `.decodeIfPresent` with
   default `nil` so older remote-connection clients also decode chats
   normally.

3. **`GroupChatScope` (line 1905) — UNCHANGED.** No `.channelMsg` case.
   Comments are not a scope; they live on the new associated value.

4. `ChatItem` — add three optional fields with `Decodable` defaults:
   ```swift
   public var parentChatItemId: Int64? = nil
   public var commentsTotal: Int = 0
   public var commentsDisabled: Bool = false
   ```
   Decoded via `try container.decodeIfPresent(...)` with documented
   defaults, mirroring the Haskell `omittedField` shape. Forward-compat
   for older remote-connection clients.

5. `ChatItem` computed property:
   ```swift
   public var isChannelPost: Bool {
       if case .channelRcv = chatDir { return true }
       return false
   }
   ```
   Subscriber view only. Owner-side ("owner viewing their own outgoing
   channel post") is composed at call sites as
   `groupInfo.useRelays && chatItem.chatDir == .groupSnd` because there
   is no `.channelSnd` case in `ChatTypes.swift`, and `asGroup` is wire-only,
   not persisted as a column on chat items.

6. `ChatInfo.channelMsgInfo()` helper method, parallel to the existing
   `groupChatScope()`:
   ```swift
   public func channelMsgInfo() -> ChannelMsgInfo? {
       switch self {
       case let .group(_, _, channelMsgInfo): channelMsgInfo
       default: nil
       }
   }
   ```

`apps/ios/Shared/Model/AppAPITypes.swift`:

7. New `ChatCommand` cases:
   - `.apiSendComment(groupId:parentItemId:liveMessage:ttl:composedMessages:)`
     — serializes to `/_comment #<groupId> <parentItemId> [live=on]
     [ttl=<n>] (json|text) <body>` matching the parser at
     Commands.hs:4814.
   - `.apiSetCommentsDisabled(groupId:parentItemId:disabled:)` —
     serializes to `/_comments_disabled #<groupId> <parentItemId>
     (on|off)` matching the parser at Commands.hs:4815.
8. `.apiGetChat` extended to accept an optional `parentItemId: Int64?`
   serialized as `parent=<id>` per the existing parser.

`apps/ios/Shared/Model/SimpleXAPI.swift`:

9. New functions:
   - `apiSendComment(groupId: Int64, parentItemId: Int64, liveMessage: Bool, ttl: Int?, composedMessages: [ComposedMessage]) async throws -> [ChatItem]`
   - `apiSetCommentsDisabled(_ groupId: Int64, _ parentItemId: Int64, _ disabled: Bool) async throws -> ChatItem` — returns the updated parent post.
10. `apiGetChat` (existing, around line 539) extended to accept
    `parentItemId: Int64? = nil`. Default `nil` keeps existing call sites
    unchanged.

### 4.2 iOS state model

`apps/ios/Shared/Model/ChatModel.swift`:

11. Extend `getCIItemsModel` (line 691) to consult both
    `cInfo.groupChatScope()` and `cInfo.channelMsgInfo()`. The two are
    runtime-mutually-exclusive (a `ChatInfo.group` is either the main
    channel, a member-support scope, or a comments thread). Order of
    checks: scope first (preserves existing behavior), then channel-msg.
    ```swift
    func getCIItemsModel(_ cInfo: ChatInfo, _ ci: ChatItem) -> ItemsModel? {
        let cInfoScope = cInfo.groupChatScope()
        let cInfoChannelMsg = cInfo.channelMsgInfo()
        if let cInfoScope { /* existing switch unchanged */ }
        else if let cInfoChannelMsg {
            switch secondaryIM?.secondaryIMFilter {
            case let .some(.groupChannelMsgContext(parent)):
                return (cInfo.id == chatId && cInfoChannelMsg.channelMsgItem.id == parent.id)
                    ? secondaryIM : nil
            default: return nil
            }
        } else {
            return cInfo.id == chatId ? im : nil
        }
    }
    ```
    `SecondaryItemsModelFilter` (the existing local filter sum at
    `ChatModel.swift:75`) gains a new constructor
    `.groupChannelMsgContext(parent: ChatItem)`. The matcher compares the
    parent's local `ChatItem.id` (NOT `SharedMsgId` — local id is
    unambiguous within a single client and is what
    `cInfoChannelMsg.channelMsgItem.id` exposes).

12. Audit and update the four scope gating sites at
    `ChatModel.swift:657, 679, 717, 788`. Each currently gates on
    `cInfo.groupChatScope() == nil`. Change each to
    `cInfo.groupChatScope() == nil && cInfo.channelMsgInfo() == nil` so
    comment items take the same code path as member-support items: they
    do NOT update the main chat preview, do NOT add to the main chat's
    unread counter, and do NOT pop the main chat to the top of the chat
    list.

13. `ItemsModel.loadSecondaryChat(...)` gains a new branch:
    ```swift
    case .groupChannelMsgContext(let parent):
        // calls apiGetChat with parentItemId: parent.id
    ```
    The returned `Chat` has wire `chatInfo` of the form
    `.group(groupInfo, scope: nil, channelMsgInfo: nil)` because the
    Haskell side does not embed `ChannelMsgInfo` into the wire. iOS
    rewrites it locally to
    `.group(groupInfo, scope: nil, channelMsgInfo: ChannelMsgInfo(channelMsgItem: parent, channelMsgSharedId: parent.meta.itemSharedMsgId!))`
    before storing the `Chat` in `secondaryIM`.

14. Inbound `ChatItem` updates from the backend with `chatInfo.group`
    arrive without `channelMsgInfo` set on the wire. `getCIItemsModel`
    needs to recognize that a comment item is a comment by inspecting
    `cItem.parentChatItemId != nil`, NOT by inspecting
    `cInfo.channelMsgInfo()`. **Refinement to step 11:** the inbound
    routing branch must compare `ci.parentChatItemId` against the open
    secondary IM's parent id, not `cInfoChannelMsg.channelMsgItem.id`:
    ```swift
    case let .some(.groupChannelMsgContext(parent)):
        return (cInfo.id == chatId && ci.parentChatItemId == parent.id)
            ? secondaryIM : nil
    ```
    Outbound items the iOS layer constructs locally CAN carry
    `cInfo.channelMsgInfo` (because iOS controls that local construction)
    but for inbound robustness we drive routing off `parentChatItemId`.

### 4.3 iOS view layer

15. Comments button on the channel post bubble. Locate the channel-post
    item view by searching `apps/ios/Shared/Views/Chat/ChatItem/` for
    `.channelRcv` (the only channel-specific direction case in iOS).
    Add a `CommentsButton` view that renders only when
    `(parent.isChannelPost) || (groupInfo.useRelays && groupInfo.membership.memberRole >= .owner && parent.chatDir == .groupSnd)`.
    Body: `bubble.left` icon plus
    `comments` (when `commentsTotal == 0`) or `comments N` (when > 0).
    Disabled when `parent.commentsDisabled || !groupInfo.fullGroupPreferences.comments.on || commentsClosedLocally(parent)`.

16. Hidden `NavigationLink` in `ChatView.swift` driven by
    `@State private var commentsParent: ChatItem?` — same legacy pattern
    used by `NavStackCompat`, `UserSupportChatNavLink`, and
    `GroupReportsChatNavLink`. The codebase has not migrated to
    `NavigationStack`/value-based navigation; the Comments view follows
    the existing legacy `NavigationLink(isActive:)` pattern. iOS-16
    deprecation warning is suppressed locally with `@available`.

17. `openComments: (ChatItem) -> Void` closure plumbed from `ChatView`
    down through the chat-items list and into each item view. Tapping
    the comments button sets `commentsParent = parent`, the binding
    flips, the hidden link pushes `SecondaryChatView` for the locally-
    constructed Chat with embedded `channelMsgInfo`.

18. New `apps/ios/Shared/Views/Chat/Group/ChannelMsgChatToolbar.swift`,
    mirror of `MemberSupportChatToolbar.swift`. Shows "Comments on:"
    with a 1-line preview of the parent post text. Wired via
    `ChatView`'s toolbar selector based on `chat.chatInfo.channelMsgInfo()
    != nil`.

19. Composer gating in `ComposeView.swift`: when
    `chat.chatInfo.channelMsgInfo() != nil`, disable the composer with a
    "Comments are closed" banner whenever any of: parent's
    `commentsDisabled` is true; group's comments preference is off; the
    post's age has exceeded `closeAfter` window.

20. Owner controls: in the channel post item context menu (long-press),
    add "Disable comments" / "Enable comments", gated to
    `groupInfo.membership.memberRole >= .moderator`. Tapping calls
    `apiSetCommentsDisabled` and updates local state via the model's
    standard `upsertChatItem` path (the parent post's `commentsDisabled`
    field is on `ChatItem` JSON and reconciles automatically).

### 4.4 iOS three-layer documentation updates (per apps/ios/CODE.md)

The three-layer Change Protocol mandates that source changes update the
matching `spec/` and `product/` documents. New documents to add:

- `apps/ios/spec/client/comments.md` (new) — comments view, API surface,
  state matching logic, hidden NavigationLink pattern, composer gating.
- `apps/ios/product/views/comments.md` (new) — user-facing flow:
  comments button, navigation push, owner controls.

Existing documents to update:

- `apps/ios/spec/client/chat-view.md` — add comments-button and
  commentsParent NavigationLink sections.
- `apps/ios/spec/api.md` — `apiSendComment`, `apiSetCommentsDisabled`,
  `parent` parameter on `apiGetChat`, `ChannelMsgInfo` type,
  `parentChatItemId/commentsTotal/commentsDisabled` on `ChatItem`.
- `apps/ios/spec/state.md` — `getCIItemsModel` channel-msg branch,
  `ChatInfo.channelMsgInfo()` helper, `SecondaryItemsModelFilter.groupChannelMsgContext`,
  the four audited gating sites (657, 679, 717, 788).
- `apps/ios/spec/impact.md` — map the touched source files to the new
  product concept.
- `apps/ios/spec/client/compose.md` — composer gating banner.
- `apps/ios/product/concepts.md` — new "Channel comments" row pointing to
  the new spec/product docs and to the affected source files and tests.
- `apps/ios/product/glossary.md` — define "channel post comment".
- `apps/ios/product/rules.md` — gating invariants: channels-only,
  `memberRole >= GRCommenter`, `parent.commentsDisabled` false, age
  within `closeAfter` window.

Adversarial self-review per CODE.md: repeat until two consecutive passes
find zero issues.

## 5. Remaining tests

Each test below has a one-line acceptance criterion. v1's #13 (history
replay) is dropped because history replay is deferred. Tests are added to
the existing `describe "channel comments"` block at
`tests/ChatTests/Groups.hs:284`.

1. `testChannelCommentRcvFromAnotherSubscriber` — cath comments; bob
   receives via relay; bob sees cath as unknown member (existing
   `unknownMemberRole` mechanism).
2. `testChannelCommentQuoteAnotherComment` — dan's comment quotes bob's
   earlier comment AND has the same parent post; both `parent` and
   `quote` set on the same `MsgContainer`.
3. `testChannelCommentModerationDelete` — owner deletes a subscriber's
   comment via moderation; parent's `commentsTotal` decrements once.
4. `testChannelCommentClosingWindow` — group has `comments.closeAfter
   = 1`; an old post stops accepting comments after the window.
5. `testChannelCommentPrefOff` — group's `comments.enable = FEOff`
   (rejected on send-side preflight and receive-side
   `prohibitedGroupContent`).
6. `testChannelCommentOwnerSentAsGroupNoLeak` — owner posts a channel
   message and adds an own comment with `sentAsGroup = True`;
   subscribers must NOT see the owner's member identity. Mirrors
   `testChannelOwnerReaction` and `testChannelOwnerQuote`.
7. `testChannelCommentRoundtripJSON` — `tests/JSONTests.hs` round-trip
   for the merged `MsgContainer` carrying `parent` only, `quote` only,
   both, and neither; for `XMsgUpdate` with and without `prefs`.
8. `testChannelCommentDisabledViaPrefs` — owner toggles
   `comments_disabled` via `APISetCommentsDisabled`; the
   `XMsgUpdate.prefs` event reaches subscribers and their local
   `commentsDisabled` flips. Verify that a follow-up non-disabling owner
   content edit does NOT silently reset `commentsDisabled` (Subscriber.hs:2126
   returning False on Nothing prefs is the invariant under test).
9. `testChannelCommentMemberCanCommentReceiveGuard` — defense-in-depth
   from §3.1: a relay forwards a comment whose author has been
   demoted to `GRObserver` since the original send; receivers reject via
   `memberCanComment` even though `prohibitedGroupContent` would have
   accepted (the feature flag is on and the parent is not deleted).
   Achieve via the existing role-change test infrastructure.
10. `testChannelCommentVersionGated` — from §3.3: a subscriber whose
    peer chat version is `commentsVersion - 1` does not receive
    comment-bearing `XMsgNew` from the relay.

`testChannelCommentMainChatExclusion` (already landed) is extended to
also assert that `getGroupNavInfo_` after-counts in the main chat
exclude comments — covers §3.2.

## 6. Forward compatibility & threat model

### 6.1 Forward compatibility

- **`MsgContainer` shape.** The merged record's wire JSON is identical to
  the existing shape. Pre-`commentsVersion` parsers see the same set of
  optional discriminator fields. `forward` is omitted on the wire when
  not `Just True`, matching legacy. No backwards-incompatible change
  here.
- **`MsgContainer.parent` to a pre-`commentsVersion` recipient.** The
  legacy parser would parse the field but render the comment as a regular
  post in the main chat — a content/UX leak. **Mitigation: relay
  forwarding gate at `commentsVersion`** (see §3.3). Verified absent
  today; v2 adds it.
- **`XMsgUpdate.prefs` to a pre-`commentsVersion` recipient.** Older
  parsers ignore the optional sub-object via `omittedField`; content
  update lands. The recipient does not enforce `commentsDisabled`
  locally, but the relay enforces it for them: their attempted comments
  get rejected by the relay's send-side preflight. Acceptable
  degradation; not gated.
- **New `CIMeta` fields (`parentChatItemId`, `commentsTotal`,
  `commentsDisabled`) on iOS / remote-connection JSON.** All three carry
  `omittedField` defaults (`Nothing` / `0` / `False`) so older
  remote-connection clients don't fall back to `CInfoInvalidJSON`.
  Consistent with `docs/CONTRIBUTING.md` "improving compatibility for
  remote desktop connection".
- **History replay limitation.** As built (decision 6), a new joiner can
  receive a comment whose parent has fallen out of the cap-N replay
  window; the comment is dropped silently on
  `resolveCommentParent`. `commentsTotal` on the parent reflects the
  live count, which may exceed what the joiner sees. Documented in
  `apps/ios/product/rules.md` and `apps/ios/product/gaps.md`. Future PR
  per the M/N sizing approach in v1 §15.

### 6.2 Threat model

1. Malicious subscriber tries to flood comments → existing per-relay
   rate limits unchanged. Owner can disable comments per-post or
   group-wide via the comments preference.
2. Malicious subscriber tries to comment on a non-channel group →
   `prohibitedGroupContent` `not (useRelays' gInfo) -> Just GFComments`
   rejects on send-side preflight (Commands.hs:4232) and receive-side
   (`newGroupContentMessage` at Subscriber.hs:1958-1961, which routes to
   the `rejected` helper saving a `CIRcvGroupFeatureRejected GFComments`
   item).
3. Malicious subscriber tries to comment on a soft-deleted parent post →
   `prohibitedGroupContent` `isJust itemDeleted -> Just GFComments`
   rejects. On hard-delete, the FK cascade removes the comment row;
   receive lookup returns `SEChatItemNotFound` and the message is
   dropped at `resolveCommentParent`.
4. Malicious subscriber tries to disable comments without permission →
   `applyMsgPrefs` requires `memberRole' m >= GRModerator` on the
   moderator branch (Subscriber.hs:2090) and only in the `CIChannelRcv`
   arm. Owners satisfy this trivially; non-moderators are rejected with
   "x.msg.update: member attempted to update channel message".
5. Malicious relay forges a parent SharedMsgId →
   `getChannelMsgInfoBySharedMsgId` returns not-found and the message is
   dropped (Subscriber.hs:1989-1991).
6. Malicious owner lies about `commentsDisabled` to the relay → no
   different from any other channel governance event; the relay's role
   check is the trust boundary.
7. Comment author identity leak on owner-as-group comments →
   `sentAsGroup` is preserved through the comment send path. Test #6
   (testChannelCommentOwnerSentAsGroupNoLeak) covers this.
8. Cascade FK and comment-count drift → `parent_chat_item_id ON DELETE
   CASCADE` removes all child comments when a parent is hard-deleted.
   The parent's `comments_total` value disappears with the row. For
   per-comment delete/undelete, `adjustChannelMsgCommentCount` uses
   `MAX(0, comments_total + ?)` so concurrent +1/-1 cannot leave a
   negative value. Bulk member-removal paths
   (`decrementMemberCommentCounts_`, inline equivalents in
   `deleteGroupMember` and `deleteGroupExpiredCIs`) decrement BEFORE the
   bulk delete/update, so the count is always a safe upper bound on
   live comments.
9. Replay attack on a stale `XMsgUpdate.prefs` → if a relay re-broadcasts
   an old `commentsDisabled = false` after a newer `... = true` has
   taken effect, subscribers would silently re-enable a disabled post.
   Mitigation for MVP: trust latest delivery. Channel governance has no
   replay protection today; documented in
   `apps/ios/product/rules.md`.
10. Merged-record forward+parent injection → a message with `forward =
    Just True` and `parent = Just _` is treated as a forwarded comment
    (semantically odd but valid). Forward provenance preserved per
    existing semantics; comment lands on the parent post. Defense:
    `memberCanComment` (§3.1) runs after `memberCanSend`, so a member
    without `GRCommenter` cannot deliver such a message even with the
    forward flag set.
11. Defense-in-depth role check on receive → §3.1's `memberCanComment`
    closes the gap where a malicious sender bypasses
    `allowedRole` locally.

### 6.3 Version gate verification steps

For Slice 1 (§7) acceptance:

a. Spawn alice (chat version `commentsVersion`), bob (a relay, version
   `commentsVersion`), and a subscriber cath whose `peerChatVRange.maxV`
   is forced to `commentsVersion - 1` via the existing test harness
   (used by `testGroupKnocking`/`testContentReports` patterns).
b. Alice posts a channel message; cath receives it as a normal post
   (the message has no `parent` field, so no gate fires).
c. Alice posts another channel message. Bob comments under it from
   bob's subscriber session. The comment-bearing `XMsgNew` is forwarded
   by the relay to dan (`commentsVersion`-capable), but skipped to cath.
d. Verify cath does NOT receive the comment, but DOES still receive
   subsequent main-channel posts. Assertion: cath's chat with the
   channel does not show the comment item; the main channel pagination
   is intact.

For prefs-bearing `XMsgUpdate` (intentionally NOT gated):

e. Alice toggles `commentsDisabled` on a post. The `XMsgUpdate` is
   forwarded to all subscribers including cath. Cath's local parser
   ignores the `prefs` sub-object via `omittedField`; cath's local
   `commentsDisabled` stays `False`. A comment attempt by cath is
   rejected by the relay (which knows the post is disabled), preserving
   the disabled invariant despite cath's stale local view.

## 7. Slices

Each slice ends with a build + test invocation that should pass. Slices
are ordered by dependency: backend gaps → backend tests → iOS API/state
→ iOS UI → docs → adversarial review.

### Slice 1 — Backend gap closure

1. Add `memberCanComment` helper in `Subscriber.hs` and compose it in
   `newGroupContentMessage` per §3.1.
2. Thread `parentChatItemId_` into `getGroupNavInfo_` and apply the
   appropriate predicate inside both subqueries per §3.2.
3. Add `commentsVersion` per-recipient gate in
   `runDeliveryJobOperation` for `parent`-bearing `XMsgNew` per §3.3.
4. Verify channel-creation override at `APINewPublicGroup` is the only
   `useRelays = True` owner path in the tree (one-line grep check
   added as a comment near Commands.hs:2484).

Build: `cabal build --ghc-options=-O0`.
Test: `cabal test simplex-chat-test --test-options='-m "channel comments"'`
must continue to pass (existing 8 tests).

### Slice 2 — Remaining Haskell tests

Add the 10 tests from §5 to `tests/ChatTests/Groups.hs`'s `describe
"channel comments"` block. Extend `testChannelCommentMainChatExclusion`
to cover `getGroupNavInfo_` scope correctness.

Build + test:
- `cabal test simplex-chat-test --test-options='-m "channel comments"'`
- `cabal test simplex-chat-test --test-options='-m "JSON"'`
- `cabal test simplex-chat-test --test-options='-m "Protocol"'`

### Slice 3 — iOS API types and state

`apps/ios/SimpleXChat/ChatTypes.swift`:

1. `ChannelMsgInfo` struct.
2. `ChatInfo.group` third associated value.
3. `ChatItem` `parentChatItemId/commentsTotal/commentsDisabled` with
   `Decodable` defaults.
4. `ChatItem.isChannelPost` computed property.
5. `ChatInfo.channelMsgInfo()` helper.

`apps/ios/Shared/Model/AppAPITypes.swift`:

6. `.apiSendComment(...)` and `.apiSetCommentsDisabled(...)` cases plus
   serialization.
7. `.apiGetChat` `parent: Int64?` parameter plus serialization.

`apps/ios/Shared/Model/SimpleXAPI.swift`:

8. `apiSendComment` and `apiSetCommentsDisabled` async functions.
9. `apiGetChat` extended.

`apps/ios/Shared/Model/ChatModel.swift`:

10. `getCIItemsModel` channel-msg branch using
    `ci.parentChatItemId == parent.id` for inbound routing (§4.2 step 14).
11. `SecondaryItemsModelFilter.groupChannelMsgContext(parent: ChatItem)`
    constructor.
12. Gate audit at lines 657, 679, 717, 788 — change each
    `cInfo.groupChatScope() == nil` to add
    `&& cInfo.channelMsgInfo() == nil`.
13. `ItemsModel.loadSecondaryChat` channel-msg branch with local
    `ChannelMsgInfo` reconstruction.

iOS build via Xcode (out of scope for the Haskell test environment;
built separately).

### Slice 4 — iOS UI

1. Comments button on channel post bubble (locate `.channelRcv` view in
   `apps/ios/Shared/Views/Chat/ChatItem/`).
2. Hidden `NavigationLink` in `ChatView.swift` driven by
   `commentsParent: ChatItem?`.
3. `openComments` closure plumbed through chat-items list.
4. New `ChannelMsgChatToolbar.swift`.
5. Composer gating banner in `ComposeView.swift` when
   `channelMsgInfo != nil` and any disabling condition holds.
6. Owner-only "Disable/Enable comments" item in per-message context
   menu calling `apiSetCommentsDisabled`.

Manual test in iOS simulator:
- Create a channel as alice; join from bob; alice posts; bob comments;
  navigate Channel → Comments → back; verify comments(N) increments;
  bob edits and deletes comments; bob quotes both parent post and
  another comment.
- Alice disables comments on the post: bob's composer goes inactive
  with banner; bob cannot send; alice re-enables; bob can send again.
- Set `comments.closeAfter = 60` in group preferences; post a message;
  wait; verify comment rejected after 60 seconds.
- Run iOS test suite (`xcodebuild test ...`).

### Slice 5 — iOS three-layer documentation updates

Per `apps/ios/CODE.md`:

1. New `apps/ios/spec/client/comments.md`.
2. Update `apps/ios/spec/client/chat-view.md`,
   `apps/ios/spec/api.md`, `apps/ios/spec/state.md`,
   `apps/ios/spec/client/compose.md`, `apps/ios/spec/impact.md`.
3. New `apps/ios/product/views/comments.md`.
4. Update `apps/ios/product/concepts.md`,
   `apps/ios/product/glossary.md`, `apps/ios/product/rules.md`,
   `apps/ios/product/gaps.md` (history replay limitation).
5. Update Document Map in `apps/ios/CODE.md` if any new source file is
   added.
6. Adversarial self-review until two consecutive clean passes.

### Slice 6 — Adversarial review and final test pass

1. Re-read every changed backend file end-to-end.
2. Run the full `cabal test simplex-chat-test` suite (not just
   channels).
3. Run `tests/JSONTests.hs` and `tests/ProtocolTests.hs` to catch JSON /
   protocol regressions.
4. Two consecutive clean adversarial self-review passes.

## 8. Out of scope (deferred)

- **History replay with per-parent cap M and post-window cap N** — see
  decision 6 / §6.1. As-built admits comment items via
  `include_in_history` but does not cap them per parent; new joiners may
  receive comments whose parents were displaced from the cap-N window
  and lose those comments at `resolveCommentParent`. Future PR will add
  `getChannelMsgCommentsForHistory` and extend `getGroupHistoryItems`
  per v1 §15.
- **Kotlin Multiplatform (Android/Desktop) port** — separate follow-up.
- **Subscriber profile dissemination** — comment authors unknown to a
  viewing subscriber appear via the existing "unknown member" path.
- **Per-comment notifications** — no notification routing for comments.
- **Comment count indicator on the channel chat list** — no per-chat
  unread badge for comments.
- **"List of comment threads" view for owners** — no analog to
  `MemberSupportView`.
- **Separate `DeliveryWorkerScope` for comments** — comments share the
  channel post worker for batching.

## 9. Critical files to be modified (v2 scope)

Backend (Slices 1–2):

- `src/Simplex/Chat/Library/Subscriber.hs` — `memberCanComment` helper +
  composition; `runDeliveryJobOperation` per-recipient version gate.
- `src/Simplex/Chat/Store/Messages.hs` — `getGroupNavInfo_` signature
  and predicate completion.
- `src/Simplex/Chat/Library/Commands.hs` — comment near `APINewPublicGroup`
  channel-creation default override.
- `tests/ChatTests/Groups.hs` — 10 new tests in `describe "channel
  comments"`.
- `tests/JSONTests.hs` — `testChannelCommentRoundtripJSON` cases.

iOS (Slices 3–5):

- `apps/ios/SimpleXChat/ChatTypes.swift` — `ChannelMsgInfo`,
  `ChatInfo.group` third associated value, `ChatItem` new fields,
  `isChannelPost`, `channelMsgInfo()`.
- `apps/ios/Shared/Model/AppAPITypes.swift` — new ChatCommand cases,
  `parent` param.
- `apps/ios/Shared/Model/SimpleXAPI.swift` — `apiSendComment`,
  `apiSetCommentsDisabled`, extended `apiGetChat`.
- `apps/ios/Shared/Model/ChatModel.swift` — `getCIItemsModel` branch,
  audited gating sites, `SecondaryItemsModelFilter.groupChannelMsgContext`,
  `loadSecondaryChat` branch.
- `apps/ios/Shared/Views/Chat/ChatView.swift` — `commentsParent` +
  hidden `NavigationLink`.
- `apps/ios/Shared/Views/Chat/ChatItem/<channel post item>.swift` —
  Comments button + context-menu items.
- `apps/ios/Shared/Views/Chat/Group/ChannelMsgChatToolbar.swift` (new).
- `apps/ios/Shared/Views/Chat/ComposeMessage/ComposeView.swift` —
  composer gating banner.
- `apps/ios/spec/client/comments.md` (new), `chat-view.md`, `api.md`,
  `state.md`, `compose.md`, `impact.md`.
- `apps/ios/product/views/comments.md` (new), `concepts.md`,
  `glossary.md`, `rules.md`, `gaps.md`.

## 10. Verification

Backend (run by Claude in this environment):

1. After Slice 1, `cabal build --ghc-options=-O0` and
   `cabal test simplex-chat-test --test-options='-m "channel comments"'`
   must continue to pass.
2. After Slice 2, run the full `cabal test simplex-chat-test` plus
   `JSONTests.hs` / `ProtocolTests.hs`.

iOS (run in Xcode):

1. After Slice 3, build the iOS app target.
2. After Slice 4, manual simulator runs covering golden path and the
   four edge cases listed in Slice 4.
3. Run iOS test suite via `xcodebuild test`.
4. After Slice 5, two consecutive clean adversarial self-review passes
   per `apps/ios/CODE.md`.
