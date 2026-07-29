# Performance hardening of the Kotlin (Android/desktop) client

## Goal
Reduce jank, latency, and memory pressure in the SimpleX Chat multiplatform Kotlin client (`apps/multiplatform`, shared `commonMain` plus `androidMain`/`desktopMain`). The work is a targeted performance pass driven by a code audit of the message model, Compose UI, media pipeline, and the API/JSON receive/send path. Behaviour must be preserved exactly; only cost is removed.

## Why now
The audit found that the two most-exercised paths — receiving/updating a message in an open chat, and scrolling the message/chat lists — are super-linear in the number of loaded items, and that several per-frame costs (annotated-string building, base64 image decoding, date formatting) run uncached on the composition (main) thread. These compound in large groups and long chats, exactly where users notice slowness.

## Scope — the concrete issues to address

### A. Message/chat state is O(N) per event and copies whole lists (model)
`ChatModel.kt` stores `chats` and `chatItems` as flat `SnapshotStateList` with no id→index map (unlike `groupMembers`, which has `groupMembersIndexes`).
- Every locate-by-id is a linear scan: `getChatIndex` (`:379`), and `indexOfFirst { it.id == cItem.id }` in `upsertChatItem` (`:639`), `updateChatItem` (`:662`), `addChatItem` dedup `none { it.id == … }` (`:584`). `Chat.id`/`ChatInfo.id` are computed string getters, so each comparison allocates.
- Every single-element mutation rebuilds the entire list: `addToChatItems` (`:500-506`), `removeLastChatItems` (`:508`), `removeAllAndNotify` (`:3500`), `replaceAll` (`:3526`), and the `SnapshotStateList` `add`/`addAll`/`removeAt`/`removeAll` helpers (`:3475-3528`). A single incoming message into an N-item chat is O(N) copy + O(N) locate; a burst of M is ~O(N·M).
- `upsertGroupMember` (`:941-955`) maps over the entire loaded message list per member update.
- `addTerminalItem` (`:1228-1233`) does an O(n) `subList`+concat copy on every received message and command, even when no terminal view is open.

### B. Compose UI does uncached per-item work (rendering)
- `MarkdownText` (`TextItemView.kt:124`) rebuilds the full `AnnotatedString` via `buildAnnotatedString` on every recomposition (`:200-211`, `:224-351`), for every visible message and every chat-list preview row. Largest per-item cost.
- `MergedItems.create` (`ChatItemsMerger.kt:17`) is a full O(n) pass over all loaded items inside a `derivedStateOf` (`ChatView.kt:1795`); it re-runs on `revealedItems`/`unreadCount` changes (expand/collapse, mark-read while scrolling).
- `filteredChats` (`ChatListView.kt:943`, `:1443`) filters the whole chat list and copies it (`allChats.value.toList()`) on every `ChatList` recomposition, including scroll-direction and keyboard-state changes, not only data/search changes. The per-row `nextChatSelected` `remember(chat.id, chats)` (`:1002`) is keyed on the new list instance, so it re-allocates a `derivedStateOf` per row each pass.
- `getTimestampDateText` (`ChatModel.kt:3671`) builds `DateTimeFormatter` instances per call; called twice per item via `getItemSeparation` (`ChatView.kt:2416`, `:2420`).
- The message `LazyColumn` (`ChatView.kt:2381`) supplies stable keys but no `contentType`, so heterogeneous rows (text/image/video/voice/event/banner) cannot reuse slots on scroll.
- `ChatItemView` receives ~40 unstable inline lambdas (`ChatView.kt:1950`, defined `:531-798`), preventing recomposition skipping of visible items on any `ChatView`-level state change.
- `EventItemView` (`ChatItemView.kt:640`) scans the whole item list per group-event item via `getConnectedMemberNames`.

### C. Media decoding is uncached and on the main thread on Android
- Android `base64ToBitmap` (`Images.android.kt:26`) has no cache (desktop has `base64BitmapCache` LRU 200), decodes twice (bounds + full), and never applies `inSampleSize`, so previews decode at full resolution. It is called inside `remember(image)` on the composition thread in `CIImageView.kt:45`, `CIVideoView.kt:42`, and `ChatPreviewView.kt:320`.
- `ProfileImage` default path (`ChatInfoImage.kt:111`, `async=false`, 52 call sites) calls `base64ToBitmap` in the composable body with no `remember`, decoding the avatar on every recomposition on the main thread.
- Android `getLoadedImage` (`Utils.android.kt:172`) has no cache (desktop has `loadedImageCache`); the returned full-file `ByteArray` is then re-decoded by Coil at `Size.ORIGINAL` (`CIImageView.android.kt:30`) regardless of display size.
- `decryptedUris` (`ChatModel.kt:4408`) is an unbounded map with no eviction.

### D. API/JSON send/receive overhead
- The shared `json` instance has `prettyPrint = true` (`SimpleXAPI.kt:6354`) and is used to encode outgoing commands, including the send-message hot path (`ApiSendMessages` `:3997`, reactions `:4017`, updates `:4008`). A non-pretty `jsonShort` (`:6370`) already exists but is unused here.
- `APISerializer.deserialize` (`:6426`) parses every response twice: `decodeJsonElement()` to a DOM tree, then `decodeFromJsonElement` to the typed value — heaviest for the large payloads the code itself flags (`apiChats`, `apiChat`, `apiListMembers`).
- The receive loop launches a throwaway 30s-delay coroutine per message to release a wake lock (`:705`).

## User-visible behaviour after the change
- Smoother scrolling in long chats, large groups, and the chat list, with fewer dropped frames when images/avatars are on screen.
- Lower latency and less UI stutter when receiving bursts of messages, delivery receipts, edits, and reactions in an open chat.
- Lower memory use and fewer full-resolution bitmap allocations on Android; parity with desktop's existing caches.
- Faster send of messages and faster handling of large `apiChats`/member-list responses.
- No change to displayed content, ordering, unread counts, merging/reveal behaviour, selection, or link/secret/mention handling.

## Success criteria
- Receiving/updating a message in an open chat of N items is O(1)/O(log N) amortized for the locate and no longer copies the whole `chatItems` list per event; message-burst handling no longer scales ~O(N²).
- Chat-list and chat-item lookups by id use an index/map rather than a linear scan with string-getter comparisons.
- The per-item `AnnotatedString`, merged-items structure, date text, and Android base64/avatar decodes are computed off the main thread and/or cached so they do not re-run on unrelated recompositions.
- Outgoing commands are serialized without pretty-print; response deserialization does not double-parse the payload for the common typed cases.
- All existing unit tests pass, including `ChatItemsMergerTest`; behaviour (merging, unread bookkeeping in `ActiveChatState`, reveal/collapse, selection, scrolling-to-item, mark-read) is unchanged.

## Non-goals
- No feature or protocol changes; no change to the Haskell core or FFI signatures.
- No visual redesign; no change to what is rendered.
- The `messagesChannel` rendezvous/`trySend` drop behaviour (`SimpleXAPI.kt:524`, `:714`) is a correctness observation, not part of this performance pass, and is left as-is unless a fix is trivially safe.
- iOS/Swift client is out of scope.

## Key risks / edge cases
- The list-copy pattern may be intentional to force `SnapshotStateList` identity change and trigger recomposition; any switch to in-place mutation must still notify Compose correctly and keep `mergedItems`/`derivedStateOf` observers firing.
- An id→index map must be kept consistent across every add/remove/reorder/replace path (primary and secondary chat contexts, remote hosts) or lookups will desync.
- Memoizing `AnnotatedString` must key on every input that affects output (text, formattedText, mentions, theme colours, live-typing/secret/highlight state) to avoid stale rendering.
- Adding `contentType` must not change key stability or scroll-position restoration.
- Bitmap caches on Android must be memory-bounded (LRU) and must not retain `Bitmap`s past the image's lifetime; avatar/preview caches keyed on the base64 string must handle profile-image updates.
- Switching outgoing encode to `jsonShort` must preserve `encodeDefaults`/`explicitNulls` semantics so the core receives identical command JSON (only whitespace differs).
