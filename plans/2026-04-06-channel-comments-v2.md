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

**Invariant.** `forward = Just False` is semantically equivalent to
`forward = Nothing` on the wire (the serializer omits both). Construction
sites SHOULD normalize to `Nothing`; treating `Just False` as "forwarded"
would be a bug. The smart constructor `mcForward` always sets `Just True`;
no in-tree caller produces `Just False`.

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

**Invariant.** A non-owner moderator cannot mutate content via `XMsgUpdate`;
their content field is silently discarded by the receive-path arm ordering.
The owner-content branch (`maybe True (\m -> memberRole' m == GROwner) m_`,
Subscriber.hs:2089) is the only path that reaches `updateCI`; the moderator
branch (`isJust prefs_ && memberRole' m'' >= GRModerator`, line 2090) calls
`applyMsgPrefs` and returns directly. A moderator's `XMsgUpdate` carrying
both `content` and `prefs` applies prefs only. Verified at
Subscriber.hs:2087-2092.

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

The asymmetry reflects a difference in how each platform consumes
`ChatInfo`, not a temporary compromise. Haskell call chains pass
`Maybe ChannelMsgInfo` along function signatures explicitly because every
caller knows whether it is in a comments context: pagination
(`getGroupChat`, `getChatItemIDs`), filtering (`prohibitedGroupContent`,
`commentsClosed`), persistence (`createNewChatItem_`,
`sendGroupContentMessages_`), and receive-path resolution
(`newGroupContentMessage`) are all entered with the parent context already
materialized at the call site. The parameter flows along the call graph.

SwiftUI views compose hierarchically: `cInfo` flows through view bodies and
`@Environment` propagation, with no caller chain to thread a separate
parameter through. `ChatModel.swift` inspects `cInfo.groupChatScope()`
inside `getCIItemsModel` and at four gating sites
(`ChatModel.swift:657, 679, 717, 788`) to decide whether the item belongs
to the main chat or a secondary scope. The "this Chat is a comments thread
for parent X" predicate must be observable from `cInfo` alone for nested
view gating, composer disabling, and secondary-IM routing to work
uniformly. Embedding `channelMsgInfo` as a third associated value beside
`groupChatScope: GroupChatScopeInfo?` mirrors the existing pattern.

**Cross-platform JSON consequence.** The Haskell `Chat 'CTGroup` returned
from `APIGetChat` JSON-encodes via the existing 2-parameter `GroupChat`
constructor; the wire shape is unchanged. iOS reconstructs the embedded
`channelMsgInfo` locally: when iOS calls `apiGetChat(parent: parentId)`, it
already has the parent post in memory (the parent lives in the main
channel's `ItemsModel`), so it composes the local
`ChatInfo.group(_, _, ChannelMsgInfo(channelMsgItem: parent, channelMsgSharedId: sharedId))`
on receipt. No new JSON field is needed.

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

**Decision: defer in v2.** As built, comment items are admitted to history
via `includeInHistory` (Store/Messages.hs:631-635), and
`getGroupHistoryItems` (Store/Messages.hs:3825-3846) runs
`ORDER BY item_ts DESC, chat_item_id DESC LIMIT N` and then `reverse`, so
playback is **ASC by `item_ts`** with `chat_item_id` ASC as tiebreaker.
Comment items have `item_ts >= parent.item_ts` (set at receive to the
relay's `brokerTs`, which is monotone in normal operation) and
`chat_item_id` strictly greater than the parent's (rows are inserted in
arrival order). **Within the cap-N window, parents always precede their
own comments in the playback stream.** The receiver's
`resolveCommentParent` (Subscriber.hs:1989-1991) therefore resolves
in-window comments correctly.

The actual failure mode is narrower:

- **Out-of-window comments.** Comments whose parent fell out of the cap-N
  window are dropped at `resolveCommentParent` (the parent isn't in the
  joiner's local DB). Those comments had no place to render anyway —
  their parent isn't in the joiner's chat — so the visible inconsistency
  is only `commentsTotal > delivered`, not "messages out of order".
- **Live count vs. delivered count.** A parent post arrives carrying the
  live `commentsTotal`, which may exceed the comments actually delivered
  to the joiner (because heavy-comment windows displace earlier comments
  from the cap-N).

Both are user-visible inconsistencies, not data-loss bugs. v2 documents
them under §6 (Forward compatibility & threat model) and §8 (Out of
scope). The v1 sizing approach — replace flat cap N with **post window N
+ per-parent cap M**, both tunable constants — would ensure parents and
recent comments are admitted together. A future PR can also optionally
re-sort the relay's history send by parent dependency to handle the rare
case of broker-ts reordering across SMP queues. v2 explicitly excludes
the helper `getChannelMsgCommentsForHistory` and the
`getGroupHistoryItems` extension.

**7. Default channel preference at creation site.**

**Decision: ratify.** The override `comments = CommentsGroupPreference
{enable = FEOn, closeAfter = Nothing}` is set in `APINewPublicGroup`
(Commands.hs:2483-2485) — the only owner-side channel-creation path that
sets `useRelays = True`. Verified by grep: the only other source-tree
`useRelays = True` caller is `APIPrepareGroup` (Commands.hs:2011-2014),
which is the **subscriber-side join** path; subscribers inherit the
published prefs of the channel they join. There is no "import group
converted to channel" path in the current tree, no business-channel path,
and the `createPreparedGroup` / `createNewGroup` store helpers do not
synthesize prefs themselves.

**Post-creation immutability of `useRelays`.** Verified at
`Store/Groups.hs:2244-2275`: `updateGroupProfile` updates `display_name`,
`full_name`, `short_descr`, `description`, `image`, `group_type`,
`group_link`, `preferences`, `member_admission`, and `updated_at`. It does
NOT update the `groups.use_relays` column. Therefore `APIUpdateGroupProfile`
cannot flip a regular group into a channel after creation, and there is no
hidden override site introduced by profile edits.

**Slice 1 follow-up.** Add a one-line verification at implementation time:
grep `useRelays = True` and `useRelays = BoolDef True` across
`src/Simplex/Chat/`. The only matches must be the two known sites
(`APINewPublicGroup` at Commands.hs:~2484, the conditional pre-channel-link
flow at Commands.hs:~2011, and the row-construction sites at
`Store/Groups.hs:380, 390` and `Store/Shared.hs:685`, all of which are
seeded by the two API paths above). Any new match is a new override site
that must apply the comments-pref default.

**8. Forward-compatibility for parent-bearing `XMsgNew` — acceptable
degradation, NO gate.**

**Decision: do not add a relay-side version gate for comment-bearing
`XMsgNew` or prefs-bearing `XMsgUpdate`.** The earlier "verified gap"
framing inverted the threat: there is no privacy or content boundary
that a degraded comment rendering would violate.

In a channel, all posts and all comments are visible to every
subscriber. There is no privacy or content boundary that "comment
renders as regular main-channel post on an older client" violates —
the comment was already destined for that subscriber's eyes either
way. An older parser successfully parses the `MsgContainer.parent`
field (it is optional in the wire shape) but lacks the UI to surface
the parent-thread context, so the message renders inline in the main
channel chat. This is **degraded rendering, not a content leak**.

This is the same shape as prefs-bearing `XMsgUpdate`, which v2
already classifies as acceptable degradation: older parsers ignore
`prefs` via `omittedField`, fail to enforce `commentsDisabled`
locally, but cannot exfiltrate anything they were not already
authorized to see.

Older clients also have no path to construct comment-bearing replies:
they lack the UI, the `parent` field is unknown to their composer,
and they would only be able to send a regular `XMsgNew` — which
lands in the main channel for all clients regardless of version.
Promotion to `commenter` is also a non-issue on the receive side: the
default `channelSubscriberRole` was `GRObserver` pre-`commentsVersion`,
and the `"commenter"` text encoding parses as `GRUnknown "commenter"`
on old clients, which sits below `GRObserver` in the `Ord
GroupMemberRole` derivation. A promoted-to-commenter old client
therefore still cannot send to the main channel. No exposure path
exists.

Consequently §3 contains no relay-forwarding gate, no
`min_recipient_version` schema, no task/job tagging, and no
per-recipient filtering on `getGroupMembersByCursor`. The existing
recipient-version filters for `groupKnockingVersion` and
`contentReportsVersion` (Internal.hs:1601-1627, Subscriber.hs:3593,
3631) protect different invariants — receivers without protocol
support for those features would malfunction or miss governance
events — and do not generalize to comments where the degradation is
purely cosmetic.

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
  "items after this one" for navigation. **Must filter on the same scope
  as the open chat.** When the open chat is a comments thread (the
  function is called from `getGroupChatAround_` at line 1773, which
  receives `parentChatItemId_` from the as-built work), the predicate
  must be `parent_chat_item_id = ?` with the parent id; when the open
  chat is the main channel, the predicate must be
  `parent_chat_item_id IS NULL`. As-built has neither predicate, so
  navigation counts in either scope include items from the other scope.

  **Fix.** New signature:

  ```haskell
  getGroupNavInfo_
    :: DB.Connection
    -> User
    -> GroupInfo
    -> Maybe GroupChatScopeInfo
    -> Maybe ChatItemId          -- NEW: parent chat item id
    -> CChatItem 'CTGroup
    -> IO NavigationInfo
  ```

  Predicate matrix (added to both `getAfterUnreadCount` and
  `getAfterTotalCount` subqueries; the two arguments are mutually
  exclusive at the call site):

  - `parentChatItemId_ = Just pId`:
    `AND parent_chat_item_id = ?`  -- bound to `pId`
  - `parentChatItemId_ = Nothing`, `scopeInfo = Nothing`:
    `AND parent_chat_item_id IS NULL AND group_scope_tag IS NULL AND group_scope_group_member_id IS NULL`
  - `parentChatItemId_ = Nothing`, `scopeInfo = Just GCSIMemberSupport {groupMember_}`:
    `AND parent_chat_item_id IS NULL AND group_scope_tag = ? AND group_scope_group_member_id IS NOT DISTINCT FROM ?`

  Caller `getGroupChatAround_` (Store/Messages.hs:1756-1777) already has
  `parentChatItemId_` and `scopeInfo` in scope from the as-built work;
  threading the parameter through is a one-line change at the call site.
  The signature change touches no other caller.

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

### 3.3 Reactions on comments

Reactions are routed by `(SharedMsgId, Maybe MemberId, Maybe MsgScope)`
in `XMsgReact` (Protocol.hs ChatMsgEvent line ~436) and persisted via
`groupMsgReaction` (Subscriber.hs:1891). Comments use **no scope** (their
parent reference lives in the message container, not in `MsgScope`), so a
reaction on a comment carries `scope = Nothing` and resolves to the
comment row by `(SharedMsgId, MemberId)` lookup just like a reaction on a
main-channel post. No code change is needed: the existing reaction path
flows comment reactions through the relay and updates
`chat_item_reactions` unchanged. The deletion-cascade behavior is also
unchanged because `chat_item_reactions` is keyed on `chat_item_id`, which
is hard-deleted by the parent-comment FK cascade.

Test: `testChannelCommentReact` per §5 covers the happy path.

### 3.4 No other backend changes

Specifically NOT in scope of v2:

- **Per-recipient version gate for comment-bearing `XMsgNew` or
  prefs-bearing `XMsgUpdate`** — intentionally absent (decision 8);
  pre-`commentsVersion` recipients render comments inline as regular
  posts, which is acceptable degradation in a channel context where
  all content is public to subscribers.
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
        guard let sharedId = parent.meta.itemSharedMsgId else {
            logger.error("loadSecondaryChat: parent has no shared msg id")
            return
        }
        // calls apiGetChat with parentItemId: parent.id
    ```
    The returned `Chat` has wire `chatInfo` of the form
    `.group(groupInfo, scope: nil, channelMsgInfo: nil)` because the
    Haskell side does not embed `ChannelMsgInfo` into the wire. iOS
    rewrites it locally to
    `.group(groupInfo, scope: nil, channelMsgInfo: ChannelMsgInfo(channelMsgItem: parent, channelMsgSharedId: sharedId))`
    before storing the `Chat` in `secondaryIM`. The guard handles the
    defensive case where a malformed parent post has no `itemSharedMsgId`;
    channel posts always carry one by construction, but the guard is
    cheap and prevents a crash on bad data.

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
    the existing legacy `NavigationLink(isActive:)` pattern.

    **Deprecation warning posture.** Accept the `NavigationLink(isActive:)`
    deprecation warning. The codebase already calls
    `NavigationLink(isActive:)` from `NavStackCompat`,
    `UserSupportChatNavLink`, and `GroupReportsChatNavLink` with the same
    warning; following the existing pattern means inheriting the same
    warning. A project-wide migration to `NavigationStack` /
    value-based navigation is a separate, out-of-scope PR. Do not invent
    a new suppression mechanism; `@available(iOS, deprecated:)` would
    annotate an API as deprecated, not silence call-site warnings, and
    the codebase does not use any other suppression here.

17. `openComments: (ChatItem) -> Void` closure plumbed from `ChatView`
    down through the chat-items list and into each item view. Tapping
    the comments button sets `commentsParent = parent` only after a
    defensive guard:
    ```swift
    let openComments: (ChatItem) -> Void = { parent in
        guard parent.meta.itemSharedMsgId != nil else {
            logger.error("openComments: parent has no shared msg id")
            return
        }
        commentsParent = parent
    }
    ```
    The hidden link's `destination` view also unwraps via `guard let
    sharedId = parent.meta.itemSharedMsgId else { return EmptyView() }`
    when constructing the local `ChannelMsgInfo`, so a force-unwrap
    cannot crash on a malformed parent. Channel posts always carry an
    `itemSharedMsgId` by construction; the guards are defensive against
    bad data only.

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
   `quote` set on the same `MsgContainer`. **Sub-test
   `testChannelCommentQuoteCrossThreadRejected`:** alice posts P1 and
   P2; bob comments under P1; cath calls `APISendComment groupId P2`
   with `quotedItemId = bobCommentId`. Assert: rejected with
   `"quoted item does not belong to the same comment section"` matching
   the error string emitted at Commands.hs:639-642 by the
   `quotedItemInCommentSection` check.
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
   from §3.1. The mechanism is the join handshake, not history-replay
   ordering: `XGrpMemRole` is persisted as a `CIRcvGroupEvent` whose
   `ciMsgContent` is `Nothing`, so `mcTag_` is `Nothing` and
   `includeInHistory` (Store/Messages.hs:632-635) evaluates to `False`.
   Role-change events are NOT replayed by `getGroupHistoryItems`. What
   IS replayed is bob's old comment (which has a real `MsgContent`).
   Frank learns bob's current role at handshake time via `XGrpMemNew`.

   Concrete sequencing:
   1. Create channel `team`; bob is `GRCommenter` (default subscriber
      role).
   2. Bob comments under alice's post; cath, dan, eve all receive the
      comment with `bob.memberRole = GRCommenter` recorded in their
      local member tables.
   3. Alice demotes bob to `GRObserver` via `/mr team bob observer`;
      cath, dan, eve receive the `XGrpMemRole` event and update their
      local copy of bob's role.
   4. New subscriber frank joins. During the join handshake, alice (or
      the relay on alice's behalf) introduces bob to frank via
      `XGrpMemNew` carrying bob's CURRENT role at the time of the
      introduction — `GRObserver`, because frank joined after the
      demotion at step 3. Frank's local member record for bob is
      therefore created with `memberRole = GRObserver`.
   5. `getGroupHistoryItems` replays bob's earlier comment (made when
      bob was `GRCommenter`) to frank. The role-change event from
      step 3 is NOT in the replay stream because role-change items
      have `includeInHistory = False`. By the time frank's
      `newGroupContentMessage` processes the replayed comment,
      frank's local copy of bob's role is `GRObserver` (set in
      step 4).
   6. Assert: frank's `memberCanComment` rejects the comment with
      `messageError "member is not allowed to comment"`; no chat item
      is created in frank's chat under alice's post.

   **Fallback.** If the test harness cannot reliably orchestrate the
   handshake-then-replay sequencing (e.g. the join handshake does not
   complete before history items are forwarded in the test broker),
   substitute a unit-level test that constructs a `GroupMember` at
   `GRObserver` and calls `memberCanComment` directly, asserting it
   returns `Nothing` and emits the expected `messageError`. The
   defense-in-depth invariant is on the helper, not on the harness
   behavior.

10. `testChannelCommentReact` — bob comments under alice's post; cath
    reacts to bob's comment with a thumbs-up (`/_react`); alice and
    dan see the reaction count increment on bob's comment via the
    relay; the reaction's wire scope is `Nothing` (verified in the
    captured `XMsgReact` event).

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
  legacy parser successfully parses the optional `parent` field but
  lacks the UI to surface it as a comment-thread context, so the
  message renders inline in the main channel chat. No privacy or
  content boundary is crossed: in a channel, all posts and all comments
  are visible to every subscriber by design — the comment was destined
  for that subscriber's eyes either way. Older clients also cannot
  construct comment-bearing replies: they have no parent-aware
  composer, no UI, and the default `channelSubscriberRole` was
  `GRObserver` pre-`commentsVersion`. A subscriber promoted to
  `commenter` parses the role as `GRUnknown "commenter"` on old
  clients, which sits below `GRObserver` in `Ord GroupMemberRole` and
  therefore still cannot send. Acceptable degradation; not gated
  (decision 8).
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
- **History replay limitation.** As built (decision 6), in-window
  parents always precede their own comments in the joiner's playback
  stream because `getGroupHistoryItems` orders ASC by `item_ts` with
  `chat_item_id` as tiebreaker, and comments have monotonically greater
  values for both. The narrow failure mode is **out-of-window comments**:
  a comment whose parent fell out of the cap-N replay window is dropped
  on `resolveCommentParent` because the parent is not in the joiner's
  local DB. The user-visible inconsistency is `commentsTotal > delivered`
  on the parent post; the parent's chat is otherwise self-consistent.
  Documented in `apps/ios/product/rules.md` and `apps/ios/product/gaps.md`.
  Future PR per the v1 §15 M/N approach (post-window N + per-parent cap
  M).

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
   **The relay is the trust boundary chosen by the channel owner.** A
   malicious relay re-broadcasting stale prefs is in the same threat
   class as a malicious relay dropping or reordering messages —
   defended by relay choice, not by the protocol. Channel governance
   has no replay protection today; this is consistent with the existing
   model. Documented in `apps/ios/product/rules.md`.
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

## 7. Slices

Each slice ends with a build + test invocation that should pass. Slices
are ordered by dependency: backend gaps → backend tests → iOS API/state
→ iOS UI → docs → adversarial review.

### Slice 1 — Backend gap closure

1. Add `memberCanComment` helper in `Subscriber.hs` and compose it in
   `newGroupContentMessage` per §3.1.
2. Thread `parentChatItemId_` into `getGroupNavInfo_` and apply the
   appropriate predicate inside both subqueries per §3.2.
3. Verify channel-creation override at `APINewPublicGroup` is the only
   `useRelays = True` owner-side path in the tree (one-line grep
   check added as a comment near Commands.hs:2484; `APIPrepareGroup`
   subscriber join is the expected second match — see decision 7).

Build: `cabal build --ghc-options=-O0`.
Test: `cabal test simplex-chat-test --test-options='-m "channel comments"'`
must continue to pass (existing 8 tests).

### Slice 2 — Remaining Haskell tests

Add the 10 tests from §5 (numbered 1–10 plus the cross-thread sub-test
folded into #2) to `tests/ChatTests/Groups.hs`'s `describe "channel
comments"` block. Test #7 (`testChannelCommentRoundtripJSON`) lives in
`tests/JSONTests.hs` instead. Extend
`testChannelCommentMainChatExclusion` (already landed) to cover
`getGroupNavInfo_` scope correctness per §3.2.

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
  `include_in_history` and orders ASC by `item_ts` (so in-window parents
  precede their comments), but a flat cap N can displace parents and
  drop their out-of-window comments at `resolveCommentParent`. The v1
  §15 approach replaces flat cap N with **post-window N + per-parent
  cap M**, ensuring parents and their recent comments are admitted
  together. A future PR could optionally also re-sort the relay's
  history send by parent dependency to defend against rare broker-ts
  reordering across SMP queues. Future PR will add
  `getChannelMsgCommentsForHistory` and extend `getGroupHistoryItems`.
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
  composition into `newGroupContentMessage` (§3.1).
- `src/Simplex/Chat/Store/Messages.hs` — `getGroupNavInfo_` signature
  and predicate completion (§3.2).
- `src/Simplex/Chat/Library/Commands.hs` — comment near `APINewPublicGroup`
  channel-creation default override (verification only; no behavioral
  change).
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
