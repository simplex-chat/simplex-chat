# Plan v2: Comments on channel messages

## 1. Context

Channels (PR #6382 chat-relays MVP) are groups where one node relays signed
posts from owners to many subscribers. Channel **comments** add Telegram-style
threads under each post: a `comments(N)` affordance opens a flat thread with
its own composer, where subscribers can comment and quote both the parent post
and other comments.

This plan is v2 for the `f/channel-comments` branch (HEAD `912657058`). v1
was largely implemented on the backend with deliberate design deviations;
v2 audits the as-built surface, ratifies/rejects each deviation, lists the
small remaining backend gaps, and slices the unstarted iOS work against the
as-built Haskell shape. Kotlin Multiplatform remains deferred. v2 is
self-contained: it does not require reading v1.

## 2. As-built summary

Diff vs `master`: ~21 backend / ~3 test files; `apps/ios/` untouched.

- **Protocol & wire.** `MsgContainer` merged into one record (`quote`,
  `parent`, `forward` as independent optional fields); smart constructors
  `mcSimple/mcQuote/mcComment/mcForward` over `mcEmpty`. `commentsVersion =
  currentChatVersion = VersionChat 18`. `XMsgUpdate` gains optional
  `prefs :: Maybe MsgPrefs` with `commentsDisabled :: Bool`.
- **Local types.** `ChannelMsgInfo {channelMsgItem, channelMsgSharedId}` in
  Messages.hs (NOT Protocol.hs). `CIMeta` gains `parentChatItemId`,
  `commentsTotal`, `commentsDisabled`. `ChatInfo.GroupChat` / `ChatRef`
  unchanged; `Maybe ChannelMsgInfo` is param-threaded; `APIGetChat` carries
  `parentItemId` as a separate record field.
- **Roles & prefs.** `GRCommenter` between `GRObserver` and `GRAuthor`
  (text `"commenter"`); `CommentsGroupPreference {enable, closeAfter}`
  fully wired; `channelSubscriberRole = GRCommenter`. Channel default
  `comments = {FEOn, Nothing}` set inside `APINewPublicGroup` — the only
  owner-side `useRelays = True` caller.
- **API & receive.** `APISendComment`, `APISetCommentsDisabled`; CLI
  `/_comment`, `/_comments_disabled`, `parent=` on `/_get chat`. Send /
  filter paths thread `Maybe ChannelMsgInfo`; `newGroupContentMessage`
  resolves `mc.parent` via `getChannelMsgInfoBySharedMsgId` and drops on
  missing/forged parents; `memberCanSend` Nothing-arm corrected from
  `> GRObserver` to `>= GRAuthor`.
- **Store.** Migration `M20260407_channel_comments` adds three
  `chat_items` columns + two indexes + role-reset down step (SQLite +
  Postgres). Adds `getChannelMsgInfo[BySharedMsgId]`,
  `adjustChannelMsgCommentCount` (`MAX(0,…)`),
  `setChannelMsgCommentsDisabled`, `quotedItemInCommentSection`.
  Pagination + default-scope SELECTs filter `parent_chat_item_id`;
  comment-count decrement on every delete / moderate path, guarded on
  `isNothing wasDeleted`.
- **Tests landed.** 8 of v1's 16 in `Groups.hs` `describe "channel
  comments"`; `ProtocolTests` updated for merged `MsgContainer` and
  `XMsgUpdate.prefs`.

## 3. Decisions

Each decision: verdict, rationale, invariants future commits must preserve.

**1. `forward :: Maybe Bool` in `MsgContainer`.** Ratify. Serializer omits
both `Nothing` and `Just False`; parser collapses absent / `Bool False`
into `Nothing`, so round-trip is byte-identical for legacy messages;
reverting to `Bool` would touch ~50 sites for zero behavior change.
*Invariants.* `Just False ≡ Nothing` on the wire; construction normalizes
to `Nothing`; `mcForward` always yields `Just True`; no in-tree caller may
produce `Just False`.

**2. `MsgPrefs` on `XMsgUpdate` instead of a dedicated
`XGrpCommentsDisabled` event.** Ratify. An optional sub-object is
forward-compat (older clients ignore via `omittedField`); a new event tag
would be refused entirely.
*Invariants (Subscriber.hs:2087-2092).* The owner-content branch (line
2089) is the only path that reaches `updateCI` (which itself calls
`applyMsgPrefs`); the moderator branch (line 2090) applies prefs only and
returns, so a moderator `XMsgUpdate` with content+prefs applies prefs
only; `applyMsgPrefs` is reachable only in the `CIChannelRcv` arm, so
non-channel `XMsgUpdate.prefs` is a silent no-op; `CEvtChatItemNotChanged`
fires only when no prefs were applied.

**3. `APISendComment` as a separate command.** Ratify. Its validation
surface (`useRelays'` assertion, parent resolution via
`getChannelMsgInfo`, `quotedItemInCommentSection`, `assertCommentsOpen`,
extended `allowedRole` mapping `(Nothing, Just _) -> Just GRCommenter`)
would balloon `APISendMessages` branching.
*Invariants.* Comment send must run every check above; comment-bearing
content never enters `APISendMessages`.

**4. Comments-thread carrier — param-threaded in Haskell, embedded in
`ChatInfo.group` in iOS (option b).** Ratify. Haskell threads `Maybe
ChannelMsgInfo` as a parameter through call chains that already know
context per call site; iOS embeds it on `ChatInfo.group`'s third
associated value because SwiftUI views need the predicate readable from
`cInfo` alone (no caller chain to thread a parameter through).
*Invariants.* Wire shape unchanged — `APIGetChat` JSON has no
`channelMsgInfo` field; iOS reconstructs the embedded value locally from
the in-memory parent on receipt. `ChatRef.channelMsg_` is NOT added.

**5. Receive-side `memberCanComment` defense-in-depth.** Add. The send-side
`allowedRole` check runs on the sender's instance; a modified subscriber
can bypass it locally and rely on a cooperating relay; receivers must
re-check `memberRole >= GRCommenter` before persisting.
*Invariants.* Helper composed into `newGroupContentMessage` immediately
after `resolveCommentParent` returns `Just _`; one composition site;
send-side `allowedRole = Just GRCommenter` (Commands.hs:4221) preserved
unchanged.

**6. History replay for new joiners.** Defer. `getGroupHistoryItems` orders
ASC by `(item_ts, chat_item_id)`, and `includeInHistory` admits comment
items, so in-window parents always precede their comments in a joiner's
replay. The narrow failure is out-of-window comments dropped at
`resolveCommentParent` — visible as `commentsTotal > delivered`, not data
loss. Future PR adds post-window N + per-parent cap M per v1 §15.
*Invariants (future PR must preserve).* `includeInHistory` admits comment
items; ASC `(item_ts, chat_item_id)` ordering; v2 explicitly does NOT add
`getChannelMsgCommentsForHistory` or the `getGroupHistoryItems` extension.

**7. Default channel preference at creation site.** Ratify. The override
`comments = {FEOn, Nothing}` is set in `APINewPublicGroup` — the only
owner-side `useRelays = True` caller. `APIPrepareGroup` is subscriber-side
join (inherits prefs); `updateGroupProfile` does NOT touch
`groups.use_relays`, so a regular group cannot be flipped to a channel
after creation.
*Invariants.* `useRelays` is immutable post-creation; any new owner-side
`useRelays = True` caller must seed the comments default. Implementation
grep check: `useRelays = True` and `useRelays = BoolDef True` across
`src/Simplex/Chat/` must match only `APINewPublicGroup` at
Commands.hs:~2484, the pre-channel-link conditional at Commands.hs:~2011,
and row-construction at `Store/Groups.hs:380, 390` and
`Store/Shared.hs:685`. Any new match is a new override site.

**8. No version gate for comment-bearing `XMsgNew` / prefs-bearing
`XMsgUpdate`.** Reject. In a channel, all content is visible to every
subscriber by design; an older client rendering a comment inline crosses
no privacy boundary. Older clients cannot construct comment-bearing
replies (no UI, no parent-aware composer; `"commenter"` parses as
`GRUnknown "commenter"`, which sits below `GRObserver` in derived
`Ord GroupMemberRole`).
*Invariants.* §4 contains no per-recipient version gate, no
`min_recipient_version`, no task / job tagging, no filtering on
`getGroupMembersByCursor`. Existing `groupKnockingVersion` /
`contentReportsVersion` filters protect different invariants and do not
generalize.

## 4. Remaining backend work

### 4.1 Receive-side `memberCanComment` guard

Per decision 5: add helper next to `memberCanSend` in `Subscriber.hs` —
`memberCanComment :: Maybe GroupMember -> CM (Maybe a) -> CM (Maybe a)`,
returning `Nothing` with `messageError "member is not allowed to comment"`
when below `GRCommenter`; `Nothing` member (owner self) passes through.
Compose in `newGroupContentMessage` (Subscriber.hs:1948-1973) on the
branch where `resolveCommentParent` returns `Just _`. Test §6 #9.

### 4.2 Default-scope SELECT predicate audit completion

`getGroupNavInfo_` (Store/Messages.hs:1875-1912) has no scope predicate;
counts in either scope include items from the other. Extend signature
with `Maybe ChatItemId parentChatItemId_`; both `getAfterUnreadCount` and
`getAfterTotalCount` subqueries gain:

- `Just pId`: `AND parent_chat_item_id = ?` bound to `pId`.
- `Nothing` + `scopeInfo = Nothing`: `AND parent_chat_item_id IS NULL AND
  group_scope_tag IS NULL AND group_scope_group_member_id IS NULL`.
- `Nothing` + `GCSIMemberSupport`: `AND parent_chat_item_id IS NULL AND
  group_scope_tag = ? AND group_scope_group_member_id IS NOT DISTINCT FROM ?`.

Sole caller `getGroupChatAround_` (Store/Messages.hs:1756-1777) already
has both arguments in scope. All other `chat_items` SELECTs re-audited
against "every group-context default-scope query that is not per-parent
must filter `parent_chat_item_id IS NULL`" — clear. Test: extend
`testChannelCommentMainChatExclusion` with a `CPAround` assertion.

### 4.3 Reactions on comments

Reactions are routed by `(SharedMsgId, Maybe MemberId, Maybe MsgScope)`;
comments carry their parent in `MsgContainer`, not `MsgScope`, so comment
reactions use `scope = Nothing` and resolve identically to main-channel
reactions. No code change. Test: §6 #10.

### 4.4 Not in scope

Per-recipient version gate (decision 8); `getChannelMsgCommentsForHistory`,
per-parent cap M, `getGroupHistoryItems` extension (decision 6);
`XGrpCommentsDisabled` event tag (decision 2); 3-parameter
`ChatInfo.GroupChat`, `ChatRef.channelMsg_` (decision 4); subscriber
profile dissemination, per-comment notifications, separate
`DeliveryWorkerScope`.

## 5. Remaining iOS work

Per `apps/ios/CODE.md`'s three-layer Change Protocol (product → spec →
source). Constraints below derive from decisions 3, 4, and `MsgPrefs` on
`XMsgUpdate`.

### 5.1 API types

`SimpleXChat/ChatTypes.swift`:

1. `ChannelMsgInfo {channelMsgItem: ChatItem, channelMsgSharedId: String}`,
   `Decodable, Hashable`.
2. Extend `ChatInfo.group` (line 1376) with a `ChannelMsgInfo?` third arg;
   ~25 existing pattern matches gain `_`; third arg decodes to `nil` via
   `decodeIfPresent` (no wire field), populated locally on receipt.
3. `GroupChatScope` (line 1905) — UNCHANGED. No `.channelMsg` case.
4. `ChatItem` gains `parentChatItemId: Int64? = nil`, `commentsTotal: Int
   = 0`, `commentsDisabled: Bool = false`, decoded via `decodeIfPresent`
   mirroring Haskell `omittedField`.
5. `ChatItem.isChannelPost: Bool` — true iff `chatDir == .channelRcv`.
   Subscriber view only; owner-side composes locally as
   `groupInfo.useRelays && chatDir == .groupSnd`.
6. `ChatInfo.channelMsgInfo() -> ChannelMsgInfo?` helper.

`Shared/Model/AppAPITypes.swift`:

7. `.apiSendComment(groupId, parentItemId, liveMessage, ttl, composedMessages)`
   → `/_comment #<id> <parentId> [live=on] [ttl=<n>] (json|text) <body>`
   (Commands.hs:4814).
8. `.apiSetCommentsDisabled(groupId, parentItemId, disabled)` →
   `/_comments_disabled #<id> <parentId> (on|off)` (Commands.hs:4815).
9. `.apiGetChat` extended with `parentItemId: Int64?` → `parent=<id>`;
   default `nil` keeps existing call sites unchanged.

`Shared/Model/SimpleXAPI.swift`:

10. `apiSendComment(...) async throws -> [ChatItem]` and
    `apiSetCommentsDisabled(...) async throws -> ChatItem` (returns
    updated parent); extend existing `apiGetChat` (~line 539).

### 5.2 State model

`Shared/Model/ChatModel.swift`:

11. Extend `getCIItemsModel` (line 691) with a `cInfo.channelMsgInfo()`
    branch checked after `cInfo.groupChatScope()` (mutually exclusive at
    runtime). Inbound routing compares `ci.parentChatItemId == parent.id`
    (NOT `channelMsgItem.id`) because inbound items lack
    `channelMsgInfo` on the wire.
12. Add `SecondaryItemsModelFilter.groupChannelMsgContext(parent: ChatItem)`
    (existing local sum at line 75).
13. Four gating sites at lines 657, 679, 717, 788 currently gate on
    `cInfo.groupChatScope() == nil`. Add `&& cInfo.channelMsgInfo() == nil`
    so comments behave like member-support (no main-chat preview /
    unread / chat-list reorder).
14. `ItemsModel.loadSecondaryChat(...)` gains a `.groupChannelMsgContext`
    branch: call `apiGetChat(parentItemId: parent.id)`, then locally
    rewrite the returned `Chat.chatInfo` to inject `ChannelMsgInfo(parent,
    sharedId)` before storing in `secondaryIM`. Guard on
    `itemSharedMsgId != nil`.

### 5.3 View layer

15. Comments button on the channel post bubble (locate via `.channelRcv`
    under `Shared/Views/Chat/ChatItem/`). Render when `parent.isChannelPost
    || (groupInfo.useRelays && memberRole >= .owner && chatDir ==
    .groupSnd)`; body `bubble.left` plus `comments` or `comments N`;
    disabled when `parent.commentsDisabled ||
    !fullGroupPreferences.comments.on || commentsClosedLocally(parent)`.
16. Hidden `NavigationLink` in `ChatView.swift` driven by
    `@State private var commentsParent: ChatItem?`. Follows legacy
    `NavigationLink(isActive:)` pattern (`NavStackCompat`,
    `UserSupportChatNavLink`, `GroupReportsChatNavLink`); deprecation
    warning accepted; `NavigationStack` migration OOS.
17. `openComments: (ChatItem) -> Void` closure plumbed through the items
    list. Sets `commentsParent = parent` after a guard on
    `parent.meta.itemSharedMsgId != nil`; hidden link's `destination`
    also `guard let`s on local `ChannelMsgInfo` construction.
18. New `Shared/Views/Chat/Group/ChannelMsgChatToolbar.swift`, mirror of
    `MemberSupportChatToolbar.swift`: "Comments on:" with 1-line parent
    preview; wired via `ChatView`'s toolbar selector keyed on
    `chatInfo.channelMsgInfo() != nil`.
19. Composer gating in `ComposeView.swift`: when `chatInfo.channelMsgInfo()
    != nil`, disable composer with "Comments are closed" banner whenever
    `parent.commentsDisabled`, group `comments` pref is off, or post age
    exceeds `closeAfter`.
20. Owner controls in channel post context menu: "Disable comments" /
    "Enable comments" gated to `memberRole >= .moderator`, calling
    `apiSetCommentsDisabled`; reconciles via standard `upsertChatItem`.

### 5.4 Three-layer documentation

New: `apps/ios/spec/client/comments.md`, `apps/ios/product/views/comments.md`.
Update: `spec/client/chat-view.md`, `spec/api.md`, `spec/state.md`,
`spec/impact.md`, `spec/client/compose.md`; `product/concepts.md`,
`product/glossary.md`, `product/rules.md`, `product/gaps.md`
(history-replay limitation from §7). Adversarial self-review: repeat
until two consecutive clean passes.

## 6. Remaining tests

In `tests/ChatTests/Groups.hs`'s existing `describe "channel comments"`,
except #7 (in `tests/JSONTests.hs`).

1. `testChannelCommentRcvFromAnotherSubscriber` — cath comments, bob
   receives via relay; cath appears via the existing unknown-member path.
2. `testChannelCommentQuoteAnotherComment` — dan's comment quotes bob's
   earlier comment AND shares the same parent (both `parent` and `quote`
   set). Sub-test `…CrossThreadRejected`: cross-parent quote rejected
   with `"quoted item does not belong to the same comment section"`
   (Commands.hs:639-642 via `quotedItemInCommentSection`).
3. `testChannelCommentModerationDelete` — owner deletes a subscriber's
   comment; `commentsTotal` decrements exactly once.
4. `testChannelCommentClosingWindow` — `comments.closeAfter = 1`; old
   post stops accepting comments after the window.
5. `testChannelCommentPrefOff` — `comments.enable = FEOff`; rejected by
   both send-side preflight and receive-side `prohibitedGroupContent`.
6. `testChannelCommentOwnerSentAsGroupNoLeak` — owner comment with
   `sentAsGroup = True`; subscribers do not see owner's member identity.
7. `testChannelCommentRoundtripJSON` — merged `MsgContainer` with
   `parent`-only / `quote`-only / both / neither; `XMsgUpdate` with and
   without `prefs`.
8. `testChannelCommentDisabledViaPrefs` — owner toggles via
   `APISetCommentsDisabled`; subscribers' local `commentsDisabled` flips
   via `XMsgUpdate.prefs`; follow-up non-disabling content edit does NOT
   silently reset it (Subscriber.hs:2126 invariant).
9. `testChannelCommentMemberCanCommentReceiveGuard` — defense-in-depth
   (§4.1) via handshake-then-replay: demote a commenter; a new joiner
   replays the old comment with the post-demotion role recorded locally;
   `memberCanComment` rejects. Fallback if harness cannot orchestrate:
   unit-level test on the helper with a `GRObserver` member.
10. `testChannelCommentReact` — bob comments, cath reacts; wire `scope =
    Nothing`; relay propagates the count.

Extend already-landed `testChannelCommentMainChatExclusion` with a
`CPAround` assertion covering §4.2 `getGroupNavInfo_`.

## 7. Forward compatibility & threat model

### 7.1 Forward compatibility

- **`MsgContainer` wire identity.** Merged record's JSON is identical to
  the pre-merge shape; `forward` is omitted unless `Just True`.
- **Optional fields on the wire.** `MsgContainer.parent` and
  `XMsgUpdate.prefs` are ignored by older parsers via `omittedField`;
  comments render inline as main-channel posts; `commentsDisabled` is
  enforced by the relay's send-side preflight instead of locally. No
  privacy boundary crossed (channel content is visible to every
  subscriber by design).
- **`GRUnknown "commenter"` ordering invariant.** `"commenter"` parses as
  `GRUnknown` on old clients, which sits below `GRObserver` in derived
  `Ord GroupMemberRole`; a promoted-but-old subscriber still cannot send.
- **`CIMeta` new fields.** `parentChatItemId`, `commentsTotal`,
  `commentsDisabled` all carry `omittedField` defaults so older
  remote-connection clients do not fall back to `CInfoInvalidJSON`
  (`docs/CONTRIBUTING.md`).
- **History replay limitation** (decision 6) — out-of-window comments
  dropped at `resolveCommentParent`; symptom `commentsTotal > delivered`.
  Documented in `apps/ios/product/gaps.md`.

### 7.2 Threat model

- **Parent-id forgery by malicious relay** —
  `getChannelMsgInfoBySharedMsgId` returns not-found, message dropped
  (Subscriber.hs:1989-1991).
- **`memberCanComment` defense-in-depth** — closes the gap where a
  modified subscriber bypasses send-side `allowedRole` locally (§4.1).
- **Cascade FK and count drift** — `parent_chat_item_id ON DELETE CASCADE`
  removes child comments with their parent;
  `adjustChannelMsgCommentCount` uses `MAX(0, comments_total + ?)`; bulk
  paths (`decrementMemberCommentCounts_`, inlined `GROUP BY` in
  `deleteGroupMember` / `deleteGroupExpiredCIs`) decrement BEFORE the
  bulk delete, keeping the count a safe upper bound.
- **`sentAsGroup` leak on owner comments** — preserved through the
  comment send path; test §6 #6.
- **Merged forward+parent injection** — `memberCanComment` (§4.1) runs
  after `memberCanSend`, so a member below `GRCommenter` cannot deliver a
  forwarded comment regardless of the forward flag.

## 8. Slices

Each slice closes a numbered task set defined elsewhere.

### Slice 1 — Backend gap closure

§4.1, §4.2, §4.3, decision-7 grep check.
Test: `cabal build --ghc-options=-O0` and `cabal test simplex-chat-test
--test-options='-m "channel comments"'`. Done: 8 landed tests still pass.

### Slice 2 — Remaining Haskell tests

§6 tests 1–10 plus the extension to landed
`testChannelCommentMainChatExclusion` (§4.2). Test #7 in `JSONTests.hs`.
Test: `cabal test simplex-chat-test --test-options='-m "channel comments"'`
plus `'-m "JSON"'` and `'-m "Protocol"'`. Done: all pass.

### Slice 3 — iOS API types and state

§5.1 items 1–10 and §5.2 items 11–14. Build iOS target in Xcode.

### Slice 4 — iOS UI

§5.3 items 15–20. Manual simulator: alice creates a channel, bob joins
and comments and quotes another comment; navigate Channel → Comments →
back, verify `comments(N)` increments; alice disables / re-enables;
`closeAfter = 60` window-close path. Test: `xcodebuild test`.

### Slice 5 — iOS documentation

§5.4. Two consecutive clean adversarial self-review passes per
`apps/ios/CODE.md`.

### Slice 6 — Adversarial review and full-suite test pass

Full `cabal test simplex-chat-test`, `JSONTests.hs`, `ProtocolTests.hs`;
re-read every changed backend file end-to-end; two consecutive clean
adversarial review passes.

## 9. Out of scope (deferred)

- History replay with post-window N + per-parent cap M (decision 6);
  future PR per v1 §15, optionally re-sorting the relay's history send by
  parent dependency to defend against rare broker-ts reordering across
  SMP queues.
- Kotlin Multiplatform (Android/Desktop) port — separate follow-up.
- Subscriber profile dissemination — comment authors unknown to a viewing
  subscriber appear via the existing "unknown member" path.
- Per-comment notifications.
- Comment count indicator on the channel chat list.
- "List of comment threads" view for owners (no `MemberSupportView` analog).
- Separate `DeliveryWorkerScope` for comments — share the channel post
  worker for batching.

## 10. Critical files

Backend:

- `src/Simplex/Chat/Library/Subscriber.hs`
- `src/Simplex/Chat/Store/Messages.hs`
- `src/Simplex/Chat/Library/Commands.hs`
- `tests/ChatTests/Groups.hs`
- `tests/JSONTests.hs`

iOS:

- `apps/ios/SimpleXChat/ChatTypes.swift`
- `apps/ios/Shared/Model/AppAPITypes.swift`
- `apps/ios/Shared/Model/SimpleXAPI.swift`
- `apps/ios/Shared/Model/ChatModel.swift`
- `apps/ios/Shared/Views/Chat/ChatView.swift`
- `apps/ios/Shared/Views/Chat/ChatItem/<channel post item>.swift`
- `apps/ios/Shared/Views/Chat/Group/ChannelMsgChatToolbar.swift` (new)
- `apps/ios/Shared/Views/Chat/ComposeMessage/ComposeView.swift`
- `apps/ios/spec/client/comments.md` (new), `chat-view.md`, `api.md`,
  `state.md`, `compose.md`, `impact.md`
- `apps/ios/product/views/comments.md` (new), `concepts.md`,
  `glossary.md`, `rules.md`, `gaps.md`
