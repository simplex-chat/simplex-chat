# Product brief: Kotlin (Android/Desktop) performance audit and remediation

## Goal
Review the Compose Multiplatform Kotlin code under `apps/multiplatform` (shared `commonMain`, plus `androidMain`/`desktopMain`) and remove the concrete performance problems it contains. The two costs that matter to users are (1) UI jank — dropped frames while scrolling the chat list and an open chat, and (2) CPU/battery/memory spent per received or sent message. This brief records what the audit found and defines the target state; the plan lists the ordered fixes.

## Who is affected and when
- Users in large groups or long chats (hundreds–thousands of loaded items), where per-message work is O(n) and repeats.
- Users scrolling the chat list with many chats, or a chat with mixed content (text, images, video, voice, calls, group events).
- Android users specifically for image previews (Android lacks caches that Desktop already has).
- All users on every sent/received message (JSON encode/decode and terminal-log overhead).

## What the audit found (grounded in the code)

### A. Message-list state and merging — highest impact
- `ChatModel.chatItems` and `ChatModel.chats` are `mutableStateOf(SnapshotStateList<…>)`; nearly every mutator (`addToChatItems`, `add/addAll/replaceAll/removeAt/removeAll`, `ChatModel.kt:500-506,3475-3540`) allocates a **new** `SnapshotStateList`, copies every element, and reassigns `.value`. Each message is O(n) copy; a burst of n messages is O(n²), and each reassignment invalidates every reader of the state, not just the changed row.
- No id→index map for `chats` or `chatItems`; inserts/updates do linear `indexOfFirst`/`none` scans on every event (`ChatModel.kt:379,584,639,662`). Group members already have such a map (`groupMembersIndexes`), proving the pattern is available.
- `MergedItems.create` (`ChatItemsMerger.kt:17-108`, driven by the `derivedStateOf` at `ChatView.kt:1795`) rebuilds the entire grouped/split model with three fresh allocations on **every** new message, reveal/collapse, or unread-count change. `itemSplits.contains(item.id)` (`ChatItemsMerger.kt:39`) is a linear `List` scan per item.
- Pagination replaces the whole backing list on `Dispatchers.Main` (`ChatItemsLoader.kt:97,116,138,161`), forcing a full merge rebuild per "load more", and uses a `SnapshotStateList` as a scratch buffer (snapshot-record overhead for throwaway work, `ChatItemsLoader.kt:54`).
- `upsertGroupMember` maps + element-wise `!=` compares + `replaceAll`s the whole item list on any member/connection-stat change, on Main (`ChatModel.kt:941-955`).

### B. Compose recomposition in list rows
- Neither LazyColumn sets `contentType`: chat list (`ChatListView.kt:1001`) and message list (`ChatView.kt:2381`). Keys are stable, but heterogeneous row layouts share one reuse pool, defeating slot reuse while scrolling.
- Timestamp/date strings are recomputed per item per recomposition and each call allocates a `DateTimeFormatter` (`getTimestampText`/`getTimestampDateText`, `ChatModel.kt:3671-3711`; read via `get()` at `ChatModel.kt:3602,3113`; used at `CIMetaView.kt:125,192`, `ChatView.kt:3750-3752`, `ChatPreviewView.kt:412`). `getItemSeparation` formats dates ~4×/item.
- `MarkdownText` rebuilds its `AnnotatedString` on every recomposition with no `remember` (`TextItemView.kt:200-351`); `reserveSpaceForMeta` concatenates with `+=` and re-reads prefs each recomposition (`CIMetaView.kt:130-195`).
- `ChatItemView` receives ~35 freshly-allocated lambdas per pass (`ChatView.kt:1950`), so it is unstable and cannot be skipped.
- Chat list filtering runs on every recomposition and copies the whole list first (`allChats.value.toList()` + `filteredChats`, `ChatListView.kt:943,1443-1471`); a per-item `derivedStateOf` is remembered on a new list instance so it is recreated every recomposition (`ChatListView.kt:1002-1004`); several `.filter{}` passes are unremembered (`ChatListView.kt:582-585,1186-1197`). `EventItemView` scans the full item list per event item (`ChatItemView.kt:605-641`).

### C. Media/images (Android-specific gaps vs Desktop)
- Android `base64ToBitmap` has no cache; Desktop has a 200-entry cache (`Images.android.kt:26-42` vs `Images.desktop.kt:24-31`) — Android re-decodes previews on every scroll-in.
- Chat-list previews decode base64 on every recomposition with no `remember` on Android's uncached path (`ChatPreviewView.kt:320,357`).
- Android `getLoadedImage` has no cache and re-reads (and re-decrypts) the file from disk on every scroll-back (`Utils.android.kt:172-194` vs Desktop's `loadedImageCache`).
- Android decodes previews at full resolution — measures bounds then discards them instead of using `inSampleSize` (`Images.android.kt:32-37`); the correct downsampling helper already exists (`Utils.android.kt:197-210`).
- First base64 decode of each image/video item runs synchronously on the composition thread (`CIImageView.kt:45`, `CIVideoView.kt:42`) although an off-thread `Base64AsyncImage` helper exists but is unused. `getMedia()` uses `runBlocking { getLoadedImage(...) }` (`ChatView.kt:3599`).

### D. JSON and threading
- Every response and event is parsed twice: `APISerializer.deserialize` decodes to a `JsonElement` DOM then re-decodes that tree to `CR` (`SimpleXAPI.kt:6426-6441`); the fallback re-encodes the element to a string (a third pass).
- The shared `json` has `prettyPrint = true` (`SimpleXAPI.kt:6355`) and is used to **encode outgoing commands** (`SimpleXAPI.kt:3998,4008,…`), so every sent message is pretty-printed before the core re-parses it; a `prettyPrint = false` `jsonShort` already exists but is not used there.
- `terminalItems.value += item` copies the whole list on every message even when the terminal view is not open (`ChatModel.kt:1228-1232`, driven from `SimpleXAPI.kt:2792,842,854`).
- The receive loop acquires a wake-lock and launches a new release coroutine per received message (`SimpleXAPI.kt:705-710`); `getUserChatData` runs the potentially large `updateChats` merge on `Dispatchers.Main` (`SimpleXAPI.kt:676-679`). Receive/decode itself is correctly on `Dispatchers.IO` (good).

### E. Tooling gap
- The Compose compiler plugin is applied but no stability configuration or metrics/reports output is enabled (`common/build.gradle.kts`), so recomposition regressions and unstable model classes are invisible.

## User-visible behaviour after remediation
- Smooth scrolling in the chat list and in open chats, including chats with mixed media and large groups; no perceptible per-frame stutter attributable to re-decoding images, reformatting timestamps, or rebuilding annotated text.
- Lower CPU/battery use per received and sent message; incoming message bursts in large groups do not degrade quadratically.
- Android image-preview scrolling matches Desktop responsiveness (cached decode, downsampled previews, no repeated disk reads).
- Identical rendered output and behaviour — this is an internal optimization, not a feature or UX change.

## Success criteria
- No behavioural or visual change: existing rendering, ordering, reveal/collapse, read-marking, pagination, and message send/receive all behave exactly as before; existing tests still pass.
- Per-incoming-message work in an open chat is no longer O(n) in the number of loaded items for the common append case (id lookups O(1); merge updated incrementally or measurably cheaper).
- Both message-list and chat-list LazyColumns declare `contentType`; scroll recomposition counts drop (verified with Compose compiler metrics / layout inspector).
- Timestamp/date strings and annotated message text are computed once per item (cached/`remember`ed), not per recomposition.
- On Android, base64 previews and loaded images are cached and downsampled; scrolling a chat back and forth does not re-decode or re-read from disk.
- Outgoing command JSON is not pretty-printed; responses are parsed in a single pass; terminal-log growth does not copy the whole list per message when the terminal is closed.
- Compose compiler metrics/report generation is available for regression tracking.

## Edge cases and risks to preserve
- `derivedStateOf` was previously removed from chat-list filtering due to an `IndexOutOfBoundsException` (comment at `ChatListView.kt:937-939`); any re-introduction of memoization must not reintroduce index desync between `chats` and per-item `index`.
- Snapshot semantics: switching a `SnapshotStateList`-reassignment pattern to in-place mutation must keep Compose observing the right granularity — under-invalidation (stale UI) is as harmful as over-invalidation. Reveal/collapse, unread markers, splits, and "scroll to item" rely on current invalidation behaviour.
- `contentType` values must be coarse enough to enable reuse yet not collapse incompatible layouts (e.g. banner vs message vs date separator).
- Serialization changes must not alter the exact command strings/whitespace the core expects, nor drop `ignoreUnknownKeys`/coercion behaviour; the double-parse exists to inspect the response shape before decoding — the single-pass replacement must preserve error/unknown handling (`CR.Response`/`CR.Invalid`).
- Image caches must be memory-bounded (LRU) and invalidated on the existing `clearImageCaches()` hook and on file change/deletion.
- Off-loading merge/model work off `Dispatchers.Main` must preserve ordering guarantees relative to concurrent events.
- Platform parity: Android and Desktop paths differ; fixes must not regress the platform that is already correct.
