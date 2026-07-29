# Performance audit — SimpleX Chat Kotlin (Android / desktop, `apps/multiplatform`)

## Goal
Answer the request "check the Kotlin code (android/desktop), determine if there are performance issues" and turn the answer into an actionable remediation plan. The audit covered the Compose Multiplatform module under `apps/multiplatform/common` across five areas: message-list rendering, chat-list rendering, coroutine/main-thread usage, image/video/file media, and the in-memory state model. The PR head diff itself only touches a build script (`scripts/simplex-chat-reproduce-builds-android.sh`); this work is a codebase-wide performance review, not a review of that diff.

## Verdict
Yes — there are real performance issues. The codebase is broadly well-engineered (extensive, correct use of `remember` / `derivedStateOf`; JSON decoding of the high-volume receive loop is correctly off-main on `Dispatchers.IO`; the merge algorithm `MergedItems.create` is a single O(n) pass). But concrete, verified problems exist in three classes, roughly in order of user impact:

1. **Per-frame recomposition cost on the hot scroll paths** (message list and chat list) — un-memoized work that re-runs on every recomposition of every visible row.
2. **Per-event O(n) work in the state model** that scales with the number of loaded items and with group activity, executed on the main thread and often duplicated across primary/secondary chat contexts.
3. **Media memory and lifecycle** — full-resolution bytes retained per visible image, inline video (ExoPlayer) instances that are stopped but not released while a chat stays open, and an Android base64→bitmap path with no cache and no downsampling.

A smaller fourth class is **synchronous main-thread blocking** (`runBlocking` in the image-gallery provider and the recorder-stop path; a `Thread.sleep` spin-lock in modal navigation).

## User-visible behaviour to improve
- **Scrolling the message list** in long or media-heavy chats: dropped frames from re-building each text bubble's `AnnotatedString`, un-memoized per-item geometry, and lack of LazyColumn item recycling by content type.
- **Scrolling / updating the chat list** with many chats: each recomposition copies and re-filters the entire chat list on the UI thread, and per-row memoization is defeated.
- **Opening the image gallery** and swiping between images: the provider decrypts/decodes on the calling thread via `runBlocking`.
- **Active groups and busy chats**: each incoming message, member update, or file-progress tick rebuilds whole lists and does linear scans, causing rising CPU and jank as more items load.
- **Memory pressure** while scrolling image/video-heavy chats, risking `OutOfMemory` and background eviction.

## Success criteria
- Hot-path composables (`MarkdownText`, per-item geometry, chat-list preview thumbnails) do no unbounded or repeated allocation/parsing per recomposition; expensive results are keyed with `remember` on their real inputs.
- The message `LazyColumn` provides a `contentType` so heterogeneous item types are recycled.
- Chat-list filtering is computed once per relevant input change, off the recomposition critical path, without reintroducing the `IndexOutOfBoundsException` that caused `derivedStateOf` to be removed there.
- Inline list video players are released (not merely stopped) when their item leaves composition; full undecoded image bytes are not retained alongside the decoded bitmap on the scroll path; the Android base64→bitmap path downsamples and/or caches like the desktop path already does.
- No disk read / decryption / decode runs under `runBlocking` on a UI-driving thread; the modal spin-lock does not block the main thread.
- Per-event state updates avoid whole-list rebuilds and repeated linear scans where a single pass or index/map lookup suffices; behaviour (recomposition correctness, unread counters, ordering) is unchanged.
- No regression in message ordering, unread counts, merge/reveal behaviour, search results, or media correctness. Changes are verified with the existing test module (`commonTest`) plus manual profiling of scroll and receive paths on Android and desktop.

## Constraints, edge cases, and risks
- **Intentional patterns must not be broken blindly.** `filteredChats` is deliberately *not* wrapped in `derivedStateOf` (comment at `ChatListView.kt:937-939` records an `IndexOutOfBoundsException`); any caching must use a safe mechanism. The `chatItems` mutation helpers deliberately allocate a new `SnapshotStateList` and reassign the `MutableState` (`ChatModel.kt:500-506`, `3490-3540`); this likely exists to force reliable recomposition, so any change to in-place mutation must preserve observer notification and be validated.
- **Dual-context amplification.** Most receive handlers mutate both `chatModel.chatsContext` and `secondaryChatsContext`; optimizations must apply to both and preserve the secondary (support/reports) views.
- **Correctness-sensitive counters.** `ActiveChatState` unread/split bookkeeping is subtle; the per-event scans there are bounded by deletion size and are lower priority — touch only with tests.
- **Platform split.** Android and desktop have separate `Images`/`VideoPlayer`/`Utils` implementations; the desktop base64 cache already exists, so parity work is Android-side.
- Changes should be incremental and independently verifiable, prioritized by user-visible impact (rendering first), so each can be profiled and reverted in isolation.
