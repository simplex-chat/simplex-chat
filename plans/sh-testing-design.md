# Brief: Performance review report for the multiplatform Kotlin clients

## Goal
Produce one human-readable Markdown report that documents the performance
problems found in the SimpleX Chat Android/Desktop Kotlin code
(`apps/multiplatform/`) and states a concrete fix for each. The report is the
deliverable; it changes no application code.

## Origin
The request has two parts:
1. Review the Kotlin client code and determine whether performance issues exist.
2. Write the result as a structured, human-readable report — styled like the
   project's own docs (table of contents, plain technical prose), with a BRIEF
   at the top and one section per issue in the form `## Issue N: <title>`,
   followed by a description and a `### Fix` subsection.

The review has already been carried out against the checked-out worktree; the
findings below are grounded in specific files and line numbers. This brief and
the plan define how those findings become the report.

## Deliverable
A single Markdown file, `apps/multiplatform/PERFORMANCE_REPORT.md`, containing:
- A short **Brief** (scope, what was reviewed, method, how severities are used).
- A **Table of contents** linking to every issue.
- One `## Issue N: <title>` section per finding, each with a description and a
  `### Fix`.
- A closing **Not defects / verified-good patterns** section that records the
  places already doing the right thing (stable `LazyColumn` keys, off-main JSON
  decode, existing caches and index maps), so the report is balanced and does
  not imply the whole codebase is slow.

## Audience
SimpleX maintainers familiar with the codebase. Assume they read Kotlin and
Compose; do not explain language basics. Every claim must point at a concrete
`file:line` in `apps/multiplatform/`.

## Content and voice
- Structure exactly as requested: `## Issue N: ...` → description → `### Fix`.
- Each issue states: what the code does, the file:line evidence (with a short
  excerpt where it clarifies), why it costs time or memory (name the complexity,
  e.g. O(n) per add / O(n^2) per batch, or "runs on the UI thread"), when it is
  triggered (scroll, incoming message batch, recomposition), and its severity.
- The fix is specific and grounded in this repo — prefer pointing at patterns the
  codebase already uses (`groupMembersIndexes` id->index map at
  `ChatModel.kt:139`; the async `Base64AsyncImage` at `Images.kt:36`; the desktop
  `loadedImageCache` / `base64BitmapCache`; the `mapItemsToIds` Set in
  `ChatItemsLoader.kt`).
- Plain and formal. No LLM/AI tells: no "As an AI", no hedging ("it seems",
  "might possibly"), no marketing adjectives, no emoji, no per-section filler.
  Write as an engineer filing findings.
- Severity labels: High (blocks the UI thread or scales O(n^2) on common paths),
  Medium (per-event or per-recomposition waste on hot paths), Low (bounded or
  startup-only).

## Issues to cover (grounded findings)
Data model / collections (`model/ChatModel.kt`, `views/chat/ChatItemsLoader.kt`):
- `addToChatItems` allocates a new `SnapshotStateList` and copies the whole list
  on every single insert — O(n) per add, O(n^2) for the per-item batch loop in
  `CR.NewChatItems` (`ChatModel.kt:500-506`; `SimpleXAPI.kt:2899-2928`).
- `addChatItem` dedupes with `none { it.id == ... }` — O(n) scan per add before
  the O(n) copy (`ChatModel.kt:582-590`).
- `upsertChatItem` / `updateChatItem` locate items with
  `indexOfFirst { it.id == ... }` — O(n) per update, O(m*n) per status/reaction
  batch (`ChatModel.kt:638-639, 661-662`).
- Chat lookup by id (`getChatIndex` / `getChat` / `hasChat`) is a linear scan
  called per incoming item (`ChatModel.kt:377-379`, used at 460, 533, 615, 677 …).
- `getChatItemIndexOrNull` linear scan is called from render-time composables
  (`ChatModel.kt:1122-1145`; callers `ChatItemView.kt:887`, `ChatView.kt:3333`,
  `CIChatFeatureView.kt:79`, `MarkedDeletedItemView.kt:46`).
- `upsertGroupMember` maps the entire `chatItems` list and does an O(n) equality
  check on every member update (`ChatModel.kt:941-955`).
- Pagination duplicate-removal uses `List.indexOf`/`contains` on `splits` inside
  per-item `removeAll` predicates — O(items*splits)
  (`ChatItemsLoader.kt:196-, 341-364`).

Compose recomposition (`views/chatlist/*`, `views/chat/*`):
- `ChatList` re-runs `filteredChats(... allChats.value.toList() ...)` inline on
  every recomposition — full copy + full filter, not memoized
  (`ChatListView.kt:936-943`, `1443-1471`).
- The per-row `remember(chat.id, chats)` is keyed on the freshly allocated
  filtered list, so it is invalidated for every visible row every pass
  (`ChatListView.kt:1001-1006`).
- Whole-list reassignment: `replaceAll` and `reorderChat` throw away and rebuild
  the `SnapshotStateList`, invalidating all readers (`ChatModel.kt:3526-3528`,
  `406-413`).
- Unstable `selectChatItem` lambda allocated per item makes `ChatItemView`
  non-skippable (`ChatView.kt:1950`).
- `EventItemView` reads `chatsCtx.chatItems.value` and reverses/scans the list in
  composition, recomputing text unmemoized (`ChatItemView.kt:605-643`).
- Chat-list rows read model-wide `deletedChats`/`chatRunning` in the row body, so
  one deletion recomposes every row (`ChatListNavLinkView.kt:42`).
- Toolbar rebuilds a mutable list of `@Composable` lambdas each recomposition
  (`ChatListView.kt:483-568`).

Main-thread blocking (`views/chat/ChatView.kt`, helpers, platform):
- `runBlocking { getLoadedImage(...) }` decrypts + decodes a full image on the
  composition thread in the fullscreen gallery — High
  (`ChatView.kt:3599`; `getMedia` at `ImageFullScreenView.kt:49, 97`).
- `Thread.sleep(10)` busy-wait in `ModalManager.runAtomically` on the UI thread
  (`ModalView.kt:241-247`).
- `runBlocking { progressJob?.cancelAndJoin() }` in audio recorder `stop()`
  (`RecAndPlay.android.kt:82-84`, `RecAndPlay.desktop.kt:80`).
- `Thread.sleep(50)` file-existence polling at desktop startup — Low
  (`SingleInstance.kt:50-53`).

Image / media decoding and memory:
- Chat-list previews call `base64ToBitmap` in the composable body with no
  `remember`, re-decoding on every recomposition (`ChatPreviewView.kt:320, 357`).
- Media items decode the base64 preview synchronously during composition
  (`CIImageView.kt:45`, `CIVideoView.kt:42`, `ImageFullScreenView.kt:157`) while
  the async `Base64AsyncImage` (`Images.kt:36`) exists and is unused.
- Android `base64ToBitmap` has no cache and decodes the byte array twice at full
  resolution, never setting `inSampleSize` (`Images.android.kt:26-42`), unlike the
  desktop cache (`Images.desktop.kt:24-50`).
- Android `getLoadedImage` has no cache and re-reads the whole file with
  `readBytes()` on every call (`Utils.android.kt:172-194`), unlike the desktop
  `loadedImageCache` (`Utils.desktop.kt:128-146`).
- Full undecoded file bytes are retained per visible media item and passed to
  Coil with `Size.ORIGINAL` (`CIImageView.kt:167-213`, `CIImageView.android.kt:30-34`).
- Desktop `resizeImageToStrSize` recomputes `hasAlpha()` (full pixel scan) on each
  shrink iteration (`Images.desktop.kt:57-68, 105-115, 143-155`).

## Success criteria
- The report file exists at the path above and follows the requested structure
  (Brief, TOC, `## Issue N` / description / `### Fix`).
- Every issue above is covered, each with a real `file:line`, a stated cost, a
  trigger, a severity, and a concrete fix.
- Issues are ordered by severity/impact and grouped by theme in the TOC.
- The "verified-good patterns" section is present.
- The prose is plain and free of LLM/AI tells; no application code is modified.

## Edge cases and constraints
- Do not assert unverified claims. Findings the review could not confirm at the
  call site (WebRTC signaling decode in `CallView.*:786/290`, the `rememberSaveable`
  Saver decode in `ComposeView.kt:235`) are excluded, not stated as defects.
- Distinguish Android-only from desktop-only from shared issues; several problems
  are platform-specific (the caches exist on desktop but not Android).
- Keep the report descriptive: it recommends fixes but must not edit code, and
  must not overstate — note where a hot path is already throttled (e.g.
  `PopChatCollector`) or already optimized (Set-based dedupe, stable keys).
- Length follows coverage; do not pad. One tight section per issue.
