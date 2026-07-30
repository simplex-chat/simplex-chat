# Brief: Performance review report for the multiplatform Kotlin clients

## Goal
Produce one human-readable Markdown report that documents the performance
problems found in the SimpleX Chat Android/Desktop Kotlin code
(`apps/multiplatform/`) and states a concrete fix for each. The report is the
deliverable. It changes no application code.

## Request and prior feedback
The request has two parts:
1. Review the Kotlin client code and determine whether performance issues exist.
2. Write the result as a structured, human-readable report: each finding is a
   section in the exact form `## Issue N: <title>`, followed by a prose
   description, then a `### Fix` subsection.

Prior feedback: an earlier attempt deleted the findings and produced content
that did not follow the `## Issue N` / `### Fix` structure. This revision
restores every grounded finding and fixes the structure. Each issue below is
verified against the checked-out worktree with a real `file:line`.

## Deliverable
A single Markdown file, `apps/multiplatform/PERFORMANCE_REPORT.md`, containing:
- A short **Brief** (scope: `apps/multiplatform/` Kotlin; method: static read of
  the checked-out worktree; how severity is used).
- A **Table of contents** linking to every issue, ordered by severity.
- One `## Issue N: <title>` section per finding, each with a prose description
  and a `### Fix` subsection. This structure is mandatory.
- A closing **Verified-good patterns / not defects** section recording the
  places already doing the right thing, so the report is balanced.

## Audience and voice
SimpleX maintainers who read Kotlin and Compose. Do not explain language basics.
Every claim points at a concrete `file:line` under `apps/multiplatform/`.
Plain and formal engineering prose. No hedging, no marketing adjectives, no
emoji, no AI tells. Each issue states: what the code does, the `file:line`
evidence (short excerpt where it clarifies), why it costs time or memory (name
the complexity or "runs on the UI thread"), when it triggers (scroll, incoming
message batch, recomposition, startup, tap), and a severity label.

Severity labels:
- **High**: blocks the UI thread, or scales O(n^2) on a common path.
- **Medium**: per-event or per-recomposition waste on a hot path.
- **Low**: bounded, startup-only, or already partly mitigated.

## Corrections folded in from verification
Three claims from the earlier review were inaccurate and are corrected here so
the report does not repeat them:
- `Base64AsyncImage` is at `platform/Images.kt:36` and **is used** (one caller,
  `views/helpers/ChatInfoImage.kt:104`). It is not "unused". Frame it as an
  existing off-thread decode pattern that the media item views do not reuse.
- `CIImageView.kt:45`, `CIVideoView.kt:42`, and `ImageFullScreenView.kt:157`
  **do** wrap `base64ToBitmap` in `remember`, so they do not re-decode on every
  recomposition. The real cost is the first synchronous decode on the composing
  thread. Do not claim "no remember" for these three.
- `getChatItemIndexOrNull` at `ChatItemView.kt:887` is inside an `onClick`
  handler (tap-time), not render-time. The render-time linear scan is
  `EventItemView` (`ChatItemView.kt:640-643`). Do not claim 887 is per-frame.

## Issues to cover (grounded and verified)
Order in the report by severity, then group related items. Each entry below
maps to one `## Issue N` / `### Fix` section.

### High severity
- **Full image decoded on the composition thread in the fullscreen gallery.**
  `ImageGalleryProvider.getMedia` calls `runBlocking { getLoadedImage(item.file) }`
  (`views/chat/ChatView.kt:3599`), and `ImageFullScreenView.kt:97` calls
  `provider.getMedia(index)` directly in the composable `Content` body (also in a
  `LaunchedEffect` at `:49`). This decrypts and decodes a full-resolution image
  synchronously, blocking the UI thread on open and page change.
  Fix: move the load off the main thread — decode in `produceState` /
  `LaunchedEffect` on `Dispatchers.IO`, mirroring `Base64AsyncImage`
  (`platform/Images.kt:36`); never call the blocking provider from composition.

- **O(n) list copy per chat-item insert, O(n^2) per incoming batch.**
  `addToChatItems` rebuilds a fresh `SnapshotStateList` and `addAll`s the whole
  list on every insert (`model/ChatModel.kt:500-506`); `addChatItem` first scans
  with `none { it.id == cItem.id }` (`:584`) then calls that O(n) copy.
  `CR.NewChatItems` runs `r.chatItems.forEach { addChatItem(...) }`
  (`model/SimpleXAPI.kt:2899-2900`), so a batch of m items costs O(m*n).
  Triggered on every incoming message and batch.
  Fix: mutate the existing `SnapshotStateList` in place (`add`/`add(index, …)`)
  instead of reallocating; maintain an id->index map for the dedupe, mirroring
  `groupMembersIndexes` (`ChatModel.kt:139`) and `mapItemsToIds` in
  `ChatItemsLoader.kt`.

- **O(n) `indexOfFirst` per upsert/update, O(m*n) per status/reaction batch.**
  `upsertChatItem` (`ChatModel.kt:639`) and `updateChatItem` (`:662`) locate the
  item with `indexOfFirst { it.id == cItem.id }`; a status/reaction batch runs
  `r.chatItems.forEach { upsertChatItem(...) }` (`SimpleXAPI.kt:2930-2935`).
  Fix: back `chatItems` with an id->index map so upsert/update are O(1) lookups.

### Medium severity
- **Linear chat lookup per incoming item.** `getChatIndex`, `getChat`, and
  `hasChat` are linear scans (`ChatModel.kt:377-379`) called on the add/upsert
  paths (e.g. `:533`, `:615`, `:677`).
  Fix: maintain an id->index map for `chats`, invalidated on reorder/replace.

- **Chat list re-filters and re-copies the whole list every recomposition.**
  `ChatList` calls `filteredChats(..., allChats.value.toList(), ...)` inline in
  composition (`views/chatlist/ChatListView.kt:943`; definition `:1443-1471`),
  copying then filtering all chats each pass.
  Fix: memoize with `remember`/`derivedStateOf` keyed on the inputs
  (search text, active filter, chats) so it recomputes only when they change.

- **Per-row `remember` keyed on the freshly allocated filtered list.**
  `remember(chat.id, chats)` (`ChatListView.kt:1002`) is keyed on `chats`, the
  new filtered list, so every visible row's `derivedStateOf` is invalidated each
  pass.
  Fix: key the row `remember` on stable values only (`chat.id` and the neighbour
  id), not the list identity.

- **Whole-list reassignment invalidates all readers.** `replaceAll`
  (`ChatModel.kt:3526-3528`) and `reorderChat` (`:406-413`, two full allocations
  per reorder) discard and rebuild the `SnapshotStateList`.
  Fix: mutate in place (`move`/`add`/`removeAt`) so only affected rows recompose.

- **`upsertGroupMember` maps the whole `chatItems` list per member update.**
  It builds a full `map` copy, does an O(n) structural `!=` comparison, then
  `replaceAll` rebuilds the list (`ChatModel.kt:941-955`), on every member update.
  Fix: update only the affected items in place; skip the full map + compare +
  rebuild.

- **Unstable `selectChatItem` lambda makes `ChatItemView` non-skippable.**
  A new closure is allocated inline per item at `views/chat/ChatView.kt:1950`.
  Fix: hoist/`remember` the lambda so the parameter is stable and Compose can
  skip unchanged items.

- **`EventItemView` reverses and scans `chatItems` in composition.**
  `chatsCtx.chatItems.value.asReversed()` then `mergedGroupEventText(...)` scans
  it and computes text unmemoized (`ChatItemView.kt:640-643`, `625-638`).
  Fix: memoize the merged event text with `remember` keyed on the relevant ids.

- **Chat-list rows read model-wide state in the row body.**
  `ChatListNavLinkView` reads `chatModel.chatRunning.value` and
  `chatModel.deletedChats.value` in every row (`ChatListNavLinkView.kt:42`), so a
  single change recomposes all rows.
  Fix: derive a per-chat boolean outside the row, or scope the read so only the
  affected row recomposes.

- **Toolbar rebuilds a list of `@Composable` lambdas each recomposition.**
  `ChatListToolbar` allocates `arrayListOf<@Composable RowScope.() -> Unit>()`
  and re-adds lambdas each pass (`ChatListView.kt:484-624`).
  Fix: extract stable composables or `remember` the button set.

- **Chat-list previews decode base64 in the composable body with no `remember`.**
  `ChatPreviewView.kt:320` and `:357` call `base64ToBitmap(...)` inline, so the
  preview re-decodes on every recomposition while scrolling.
  Fix: wrap in `remember(image)` or use `Base64AsyncImage`.

- **Android `base64ToBitmap` has no cache, double-decodes, no `inSampleSize`.**
  `platform/Images.android.kt:26-42` runs `decodeByteArray` twice (bounds then
  full) at full resolution and caches nothing, unlike the desktop
  `base64BitmapCache` (`Images.desktop.kt:24-49`).
  Fix: add a bounded cache like desktop and set `inSampleSize` for the target size.

- **Android `getLoadedImage` has no cache and re-reads the file each call.**
  `views/helpers/Utils.android.kt:172-194` calls `readBytes()` and decodes on
  every call (`clearImageCaches()` at `:169` is a no-op), unlike the desktop
  `loadedImageCache` (`Utils.desktop.kt:128-146`).
  Fix: add a bounded cache mirroring desktop and wire `clearImageCaches()` to it.

### Low severity
- **Media preview decode is synchronous on the composing thread (first pass).**
  `CIImageView.kt:45`, `CIVideoView.kt:42`, and `ImageFullScreenView.kt:157`
  wrap `base64ToBitmap` in `remember` (so no re-decode on recomposition), but the
  first decode runs on the composing thread. `Base64AsyncImage`
  (`platform/Images.kt:36`, used once at `ChatInfoImage.kt:104`) already decodes
  off-thread on `Dispatchers.IO`.
  Fix: reuse the off-thread decode pattern for these media previews.

- **`getChatItemIndexOrNull` is a linear scan.** `ChatModel.kt:1122-1125`
  (`indexOfFirst`). The per-recomposition caller is `EventItemView`
  (`ChatItemView.kt:640-643`); the call at `ChatItemView.kt:887` is a tap-time
  delete handler, not render-time.
  Fix: resolve index via the id->index map introduced for `chatItems`.

- **Pagination dedupe scans `splits` per item.** The loader runs
  `splits.value.indexOf(it.id)` and `splits.contains(...)` inside per-item passes
  and `removeAll { newIds.contains(it.id) }`
  (`views/chat/ChatItemsLoader.kt:204, 246, 284, 318, 336, 354-356, 384, 410`),
  giving O(items*splits) where `splits` is a `List`. Note `newIds` is already a
  Set, so credit that.
  Fix: index `splits` by id (Set/Map) for O(1) membership within these loops.

- **Full undecoded file bytes retained per visible media item, Coil
  `Size.ORIGINAL`.** `CIImageView.kt` (image data path) and
  `CIImageView.android.kt` request original size, holding full bytes per item.
  Fix: request a bounded target size rather than original for list/preview use.

- **Desktop `resizeImageToStrSize` recomputes `hasAlpha()` each iteration.**
  The resize loop calls `compressImageStr` per iteration
  (`platform/Images.desktop.kt:57-68`), and `compressImageStr` calls
  `bitmap.hasAlpha()` (`:105-106`), a full pixel scan (`:143-155`), every time.
  Fix: compute `usePng`/`hasAlpha` once before the loop and pass it in, as
  `resizeImageToDataSize` already does.

- **`Thread.sleep(10)` busy-wait on the UI thread in `ModalManager`.**
  `runAtomically` spins with `Thread.sleep(10)` until a CAS succeeds
  (`views/helpers/ModalView.kt:241-247`).
  Fix: replace the spin-wait with a coroutine `Mutex` or a suspend guard.

- **`runBlocking { progressJob?.cancelAndJoin() }` in audio recorder `stop()`.**
  `platform/RecAndPlay.android.kt:82-84` and `RecAndPlay.desktop.kt:80` block the
  caller; the player path in the same files uses non-blocking `cancel()`.
  Fix: use non-blocking `cancel()` (or a suspend `stop()`), matching the player.

- **`Thread.sleep(50)` file-existence polling at desktop startup.**
  `SingleInstance.kt:50-53` polls with `Thread.sleep(50)` for up to 1s.
  Fix: acceptable as startup/background, but a `WatchService` or shorter bounded
  wait removes the poll. Mark Low.

## Verified-good patterns (must appear in the closing section)
- `groupMembersIndexes` id->index map (`ChatModel.kt:139`) — the pattern the
  other collections should adopt.
- `Base64AsyncImage` off-thread decode (`platform/Images.kt:36`).
- Desktop `base64BitmapCache` (`Images.desktop.kt:24-49`) and `loadedImageCache`
  (`Utils.desktop.kt:128-146`).
- `newIds` Set-based membership in the pagination loader (`ChatItemsLoader.kt`).
- `PopChatCollector.throttlePopChat` throttling the chat-pop hot path
  (`ChatModel.kt`).
- Stable `LazyColumn` keys in `ChatList` (`ChatListView.kt:1001`).

## Success criteria
- `apps/multiplatform/PERFORMANCE_REPORT.md` exists and follows the structure:
  Brief, TOC, one `## Issue N: <title>` / description / `### Fix` per finding,
  then the verified-good section.
- Every issue above is covered, each with a real `file:line`, a stated cost, a
  trigger, a severity, and a concrete fix grounded in this repo.
- Issues are ordered by severity and grouped by theme in the TOC.
- The three corrections above are honoured; no corrected claim is restated in its
  wrong form.
- Prose is plain and free of AI tells; no application code is modified.

## Edge cases and constraints
- Do not assert unverified claims. Findings that could not be confirmed at the
  call site (WebRTC signaling decode in `CallView.*`, the `rememberSaveable`
  Saver decode in `ComposeView.kt`) are excluded, not stated as defects.
- Distinguish Android-only, desktop-only, and shared issues; several image/cache
  problems exist on Android but are already solved on desktop.
- Keep the report descriptive: it recommends fixes but edits no code, and does
  not overstate — note where a hot path is already throttled or optimized.
- Length follows coverage; do not pad. One tight section per issue.
