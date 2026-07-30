# Performance audit of the Android/desktop Kotlin code — impact-ranked findings

## Goal
Deliver a performance audit of the shared multiplatform Kotlin used by the
Android and desktop apps (`apps/multiplatform/common`). The deliverable is a
list of concrete findings, **categorized by impact with the most severe at
the top**, plus a remediation plan for the top findings. Each finding is
grounded in the repository: it names the `file:line`, the cost mechanism
(what the code does per invocation), and the hot path (how often that
invocation runs). This revision investigates further than a loose prior audit
and organizes the results into ranked severity tiers.

The goal is the audit itself and a plan to act on it. Specific remediation
techniques belong to the plan, not to this brief; where this brief names a fix
it is illustrative, not a commitment.

## Severity model
Severity is **analytical, not profiled**. Impact is estimated as
`per-invocation cost × invocation frequency`. Findings on the per-message and
per-scroll-frame paths, and a crash-risk regression, rank highest; findings
gated behind rare user actions rank lowest. This ranking is a hypothesis to be
confirmed by profiling (see success criteria) before large remediation effort
is committed in the ordered plan.

## Deliverable: ranked findings

### Tier A — Critical (crash risk, or dominant per-message / per-frame cost)

- **F1 — Unguarded index into the reversed item list can crash
  (regression vs iOS).** `getPrevHiddenMember`
  (`common/.../model/ChatModel.kt:1163-1177`) loops `for (i in range)` and reads
  `reversedChatItems[i].chatDir` at `:1167` with no bounds check. It is called
  from `MemberNameAndRole` at `common/.../views/chat/ChatView.kt:2028`, passing a
  freshly read `reversedChatItems.value`. `range` comes from a `MutableStateFlow`
  populated at merge time (`ChatItemsMerger.kt`); `mergedItems` is a
  `derivedStateOf` (`ChatView.kt:1795`). If `chatItems` shrinks (trim, delete,
  chat switch) between the merge computation and this read, `range` can exceed
  `reversedChatItems.lastIndex` → `IndexOutOfBoundsException`. The Swift port
  guards the identical loop (`apps/ios/Shared/Model/ChatModel.swift:1222-1230`
  logs and skips when `i >= items.count`); the Kotlin port does not. Hot path:
  every grouped `GroupRcv` item that shows a member name, on every recomposition.

- **F2 — `mergedItems` derived state rebuilds the whole merged list on every
  message and every reveal.** `ChatView.kt:1795-1799` wraps
  `MergedItems.create(chatItems.value.asReversed(), unreadCount, revealedItems, chatState)`
  in `derivedStateOf`. `MergedItems.create` (`ChatItemsMerger.kt:17-108`) is a
  full O(n) pass over all loaded items building `ArrayList`, an
  `indexInParentItems` map, split ranges, and per-group `MutableStateFlow`
  allocations. It recomputes whenever `chatItems`, `unreadCount`, or
  `revealedItems` change — i.e. on every new/edited/deleted message and every
  reveal toggle. This is the dominant per-update cost feeding the `LazyColumn`.
  (`reversedChatItems` at `ChatView.kt:1800` is a cheap O(1) view, not a copy.)

- **F3 — Chats have no id→index map; every mutation does an O(n) scan.** Chats
  are a flat `SnapshotStateList`; lookups are linear `firstOrNull`/`indexOfFirst`:
  `getChatIndex` (`ChatModel.kt:379`, the hot one, keyed on `id + remoteHostId`),
  `hasChat` (`:377`), `getChat` (`:344`, `:378`), `getContactChat` (`:345`),
  `getGroupChat` (`:346`). `getChatIndex` runs on essentially every mutation:
  `addChatItem` (`:533`), `upsertChatItem` (`:615`), `removeChatItem` (`:677`),
  `markChatItemsRead` (`:836`) — once per received/sent/updated message. Group
  members already have an O(1) index map (`getGroupMember`, `:357-364`); chats do
  not. Note: item scans are keyed by `id` only (`:584`, `:639`, `:662`, `:1123`);
  the remote-host dimension is carried at the chat level, not the item level.

### Tier B — High (per-message list copies, per-row composition cost)

- **F4 — Un-remembered O(n) scans inside composable bodies, per item, per
  recomposition.** Three item renderers each do
  `chatItems.value.asReversed()` + `indexOfFirst` + a `while` loop with no
  `remember`: `EventItemView` (`item/ChatItemView.kt:640-643` → `getConnectedMemberNames`,
  `ChatModel.kt:1122-1123`), `mergedFeatures` (`item/CIChatFeatureView.kt:74-91`),
  and `MergedMarkedDeletedText` (`item/MarkedDeletedItemView.kt:43-67`). The
  `asReversed()` view is cheap; the O(n) `getChatItemIndexOrNull` scan and loop
  are not, and they rerun on every recomposition of each matching item.

- **F5 — `filteredChats` copies and filters the whole chat list on every
  ChatList recomposition.** `views/chatlist/ChatListView.kt:943` calls
  `filteredChats(..., allChats.value.toList(), ...)`; the impl
  (`ChatListView.kt:1443-1471`) copies the entire list via `.toList()`, then
  `.filter`s it, and when searching does per-chat `anyNameContains`/`lowercase().contains`.
  A comment at `:937-939` records that a `derivedStateOf` memoization was removed
  because it threw `IndexOutOfBoundsException`, so this now recomputes
  unconditionally, including during scroll (ChatList recomposes on scroll-direction
  and keyboard state).

- **F6 — List-mutation helpers rebuild a full copy of the backing list on every
  mutation.** These helpers allocate a fresh `SnapshotStateList` and copy every
  element on each call: `add`/`add(index)` (`ChatModel.kt:3475-3481`), `addAll`
  variants (`:3486-3492`), `removeAll` (`:3494-3496`), `removeAllAndNotify`
  (`:3500-3516`, copy **and** full scan), `removeAt` (`:3518-3524`), `replaceAll`
  (`:3526-3528`), plus `addToChatItems` (`:500-506`), `removeLastChatItems`
  (`:508-520`), and `reorderChat` (`:406-413`, rebuilds `newChats` on every sent
  item on desktop). Each runs on the per-message path. **Correction from prior
  revision:** `clear` (`:3530-3532`) and `clearAndNotify` (`:3536-3540`) are **not**
  in this set — both assign a fresh empty `SnapshotStateList()`, which is O(1), not
  a copy; they must not be remediated as copy-on-write.

- **F7 — Chat-list avatars and previews decode base64 images during composition
  without `remember`.** `ProfileImage` decodes at
  `views/helpers/ChatInfoImage.kt:111` (`base64ToBitmap(image)`, synchronous
  `async=false` branch, no `remember`), reached from every row via
  `views/chatlist/ChatPreviewView.kt:400`. Link/chat previews decode inline at
  `ChatPreviewView.kt:320` and `:357`. These re-decode on every recomposition of
  the row (scroll, unread-count change, selection, draft). By contrast the chat
  view memoizes the same call (`item/CIImageView.kt:45`
  `remember(image){ base64ToBitmap(image) }`); the list call sites do not.
  `ProfileImage` also has an `async=true`/IO path (`ChatInfoImage.kt:103-109`)
  that the list does not use. (Per-decode cost is F13.)

### Tier C — Medium (per-row composition cost, throttled full rebuilds, per-frame flows)

- **F8 — `MarkdownText` rebuilds its `AnnotatedString` on every recomposition.**
  `views/chat/item/TextItemView.kt:200-211` (plain branch) and `:224-351`
  (formatted branch, a full loop over `formattedText.withIndex()` with per-segment
  `withStyle`/`withAnnotation`) are not memoized. Called once per visible chat-list
  row for the last message and draft (`ChatPreviewView.kt:233`, `:277`). Markdown
  is not re-parsed (the parse is a serialized core field, `ChatModel.kt:3106`), but
  the annotated-string build is redone every pass.

- **F9 — Per-item date formatting and object allocation inside the
  `itemsIndexed` body.** `ChatView.kt:2416`/`:2420` call `getItemSeparation`
  (`:3737-3754`), which calls `getTimestampDateText` twice (`:3752`) and allocates
  a fresh `ItemSeparation` per item per pass; the body also allocates an
  `ItemContext` per item at `:2423`. Runs for every non-banner item on every list
  recomposition.

- **F10 — `getTimestampText` recomputed per chat-list row, per recomposition.**
  `ChatPreviewView.kt:412` calls it un-remembered; the impl
  (`ChatModel.kt:3682-3711`) allocates `TimeZone.currentSystemDefault()`,
  `Clock.System.now()`, `Locale.getDefault().country`, and a `DateTimeFormatter`
  on each call. A precomputed `meta.timestampText` exists (`ChatModel.kt:3113`) but
  the list uses the live recompute instead.

- **F11 — The throttled `popCollectedChats` rebuilds the entire chat list.**
  `PopChatCollector` (`ChatModel.kt:785-831`) throttles at 2 s
  (`throttleLatest(2000)`), then on the main thread rebuilds the list. The
  `withContext(Dispatchers.Main)` wrapper opens at `ChatModel.kt:795` and the
  `chats.replaceAll(popCollectedChats())` call is at `:796`. `popCollectedChats`
  (`:815-830`) does an O(n) `getChat` per collected entry (`:819`), a sort (`:826`),
  and an O(n) `filter` (`:827`), then `replaceAll` copies the whole list. Fed by
  `throttlePopChat` from `addChat` (`:403`), `reorderChat` (`:412`), and
  `addChatItem` (`:571`) — i.e. per message, coalesced by the throttle.

- **F12 — Scroll `snapshotFlow` collectors fire per frame and do date
  formatting / visible-item loops.** `ChatView.kt:2905-2921` collects
  `layoutInfo.visibleItemsInfo` (emits per scroll frame, loops over visible items);
  `ChatView.kt:2937-2945` collects `firstVisibleItemScrollOffset` (emits per scroll
  pixel) and each emission runs `setDateVisibility` with a double `getTimestampDateText`.

### Tier D — Low (narrow, rare, or platform-specific minor cost)

- **F13 — base64 image decode cost, split by platform.**
  - **Android (`platform/Images.android.kt:26-42`)** decodes **twice**: a bounds
    pass with `inJustDecodeBounds=true` (`:32-33`) followed by a full
    `BitmapFactory.decodeByteArray` (`:37`), and there is **no cache**. This is the
    material cost, made worse by F7's un-memoized call sites.
  - **Desktop (`platform/Images.desktop.kt:30-55`)** is materially cheaper: it
    reads dimensions from metadata via `reader.getWidth(0)`/`getHeight(0)`
    (`:40-41`) and then performs a **single** full `reader.read(0)` (`:46`) — there
    is no second full decode. It also already has an LRU `base64BitmapCache`
    (`:24`, checked `:31`, populated `:49`) that Android lacks. **Correction from
    prior revision:** the "decode twice" premise applies to Android only.

- **F14 — Group-member index maintenance does redundant work and full item
  copies.** `populateGroupMembersIndexes` (`ChatModel.kt:348-355`) writes
  `groupMembersIndexes.value = emptyMap()` (`:349`), then immediately `.toMutableMap()`
  of that empty map (`:350`), rebuilds via `forEachIndexed` (`:351-353`), and
  reassigns (`:354`) — the `:349` write is wasted. `upsertGroupMember` copies the
  entire `chatItems` list via `.map` on every matching member update
  (`ChatModel.kt:941-954`) plus a `groupMembers.toMutableList()` copy (`:956`).

- **F15 — Minor chat-list and settings allocations.** `nextChatSelected`
  (`ChatListView.kt:1002`) keys a `remember { derivedStateOf { … } }` on the whole
  freshly built `chats` list, so it invalidates per row on every recomposition;
  a per-visible-item `snapshotFlow { highlighted.value }` collector is allocated in
  `ChatItemViewShortHand` (`ChatView.kt:1941-1948`, has `distinctUntilChanged`);
  and two settings screens construct `Regex` inside functions
  (`NetworkAndServers.kt:659`, `OperatorView.kt:871`) rather than at module level
  (contrast the correctly hoisted `ChatListView.kt:1047`).

## Success criteria
Qualitative (deliverable quality):
- Every finding names an exact `file:line`, its cost mechanism, and its hot path,
  and is verifiable against the checked-out source.
- Findings are grouped into severity tiers, most severe first, with the ranking
  rationale (`cost × frequency`) stated.
- The F6 scope excludes `clear`/`clearAndNotify`; F13 is split per platform with
  the desktop description corrected.

Quantitative (to confirm the ranking and any remediation, via profiling, not
assumed):
- Capture a baseline with Android Studio / Compose layout-inspection recomposition
  counts and a method trace before changing anything, on named scenarios:
  (a) receiving messages into an open large chat (~5k items), (b) scrolling that
  chat, (c) scrolling a chat list of ~500 chats, (d) a group with ~500 members.
- For any finding acted on, show a measurable reduction on its scenario
  (recomposition count and/or frame time / allocation) versus the baseline, with
  no behavioural regression.

## Edge cases and constraints
- **List-identity contract (`ChatModel.kt:369-372`).** The `chatItems` list must
  not be mutated in place except by index; add/remove must go through the notify
  helpers so `ChatView`'s `LazyColumn` and `chatState` stay correct. Any F2/F3/F6
  remediation must preserve this contract and the `chatState.itemsRemoved`/`clear`
  notifications.
- **IOOBE regression (F1).** The fix must guard the index like iOS
  (`ChatModel.swift:1222-1230`) and cover the case where the list shrinks between
  merge and access; do not merely reduce the window.
- **Stale-data / shrinking-list guard.** F5's removed memoization was dropped for
  an `IOOBE`; any re-introduction must handle concurrent list mutation.
- **Reversed message list.** Items are rendered newest-first via an `asReversed()`
  view; indexing and range math operate in reversed coordinates — remediation must
  keep that convention.
- **Remote-host keying.** Chats are keyed by `id + remoteHostId`; any chat index
  map must key on both, or multi-remote-host state will collide.
- **Dispatcher precision.** Model mutations run on `Dispatchers.Main`
  (`ChatModel.kt:795`); heavy work must move to `Default`/`IO` without moving state
  writes off Main.
- **Platform divergence.** Android and desktop have separate `Images` actuals with
  different cost and caching; each fix must be applied to the correct platform
  (F13).

## Out of scope
- The iOS/Swift app and the Haskell core (referenced only as the correctness
  baseline for F1 and for where parsing already happens).
- Functional changes to chat behaviour, protocol, or persistence.
- Rewriting the storage model beyond what a finding requires.
- Committing a fixed remediation order before profiling confirms the analytical
  ranking.
