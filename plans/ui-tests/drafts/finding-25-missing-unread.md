# Finding 25: a received message is missing from a chat that opens while it arrives, and "unread above" shows 1

Status: root cause found by reading the code. Nothing was run and no code was changed.
Code base: /home/user/wt-uitest at 7a27abe9f (base master cfcce1b43). The branch changes to the files involved only add UI keys
(`git diff cfcce1b43` on ChatModel.kt, ChatView.kt and ChatListNavLinkView.kt). ChatItemsLoader.kt, ChatItemsMerger.kt and
Store/Messages.hs are the same as on master.

## Conclusion

The item is never in `chatsContext.chatItems`. It is not a display or merge problem. Opening a chat is a non-atomic "read a snapshot,
then publish it" sequence:

1. The core reads the chat (items + stats) in one DB transaction (`APIGetChat` -> `withFastStore` -> `getDirectChatInitial_`).
2. Later, the app publishes that snapshot on Main: it overwrites the chat-list stats, then `replaceAll`s `chatItems`, and only then
   sets `chatModel.chatId`.

A `NewChatItems` event for that chat that is committed after step 1 and handled on Main before `chatId` is set:

- is **not added** to `chatItems`, because `chatItemBelongsToScope` compares it with the old `chatId` (null or another chat);
- **is counted** in the chat-list `unreadCount` (+1);
- is never fetched again: the snapshot says nothing follows its last item (`navInfo.afterTotal == 0`, so there are no splits and no
  `After` pagination), and later events only append newer items.

The item stays out of the chat until it is reopened. The item after it (5) arrives once `chatId` is set and is appended normally.
That is why the chat shows 1, 2, 3, 5.

"Unread above = 1" and the chat-list badge of 1 both come from the same counter. It counts item 4 (still `RcvNew` in the DB), but
item 4 is not in the list, so nothing can ever mark it read.

## Code path (file:line)

Opening the chat:
- `ChatListNavLinkView.kt:68`: the row click runs `scope.launch { directChatAction(..) }`, which calls `openDirectChat` -> `openChat`
  (`:220-243`) -> `apiLoadMessages(.., ChatPagination.Initial(INITIAL_COUNT))`.
- `ChatItemsLoader.kt:35`: `apiGetChat(..)` -> `SimpleXAPI.kt:1205` `sendCmd` -> `:974` `withContext(Dispatchers.IO)` (FFI call).
- Core: `Commands.hs:702` `withFastStore (getDirectChat ..)`. `Store/Messages.hs:1356-1364` `getDirectChatInitial_` reads
  `unreadCount` and the items around the pivot in **one transaction**. `getContactNavInfo_` (`:1413`) returns `afterTotal`/`afterUnread`
  relative to the snapshot's last item, so it is 0/0 when the snapshot ends at the newest committed item.
- `ChatItemsLoader.kt:64-91` (`Initial`), in two separate `withContext(Dispatchers.Main)` blocks:
  - Block 1, `:68-77`, line `:75`: `updateChatStats(.., chat.chatStats ..)` **overwrites** the chat-list `unreadCount` with the
    snapshot value.
  - Block 2, `:79-90`: `:80` `chatsCtx.chatItems.replaceAll(chat.chatItems)` (snapshot only), `:81` `chatModel.chatId.value = chat.id`,
    `:82` `splits = newSplits` (empty because `afterTotal == 0`, `:65`), `:84` `unreadAfterItemId` = the snapshot's last item, and
    `:87-89` `unreadTotal`/`unreadAfter`/`unreadAfterNewestLoaded` from the snapshot.

The event that arrives while the chat opens:
- `SimpleXAPI.kt:3047` `is CR.NewChatItems -> withBGApi { .. withContext(Main) { chatsContext.addChatItem(..) } }` (`:3053`).
- `ChatModel.kt:620`: the chat-list `unreadCount + 1`. This happens whether or not the chat is open.
- `ChatModel.kt:641-651`: the item is added to `chatItems` only `if (chatItemBelongsToScope(cInfo, cItem))`. `:657` defines that as
  `chatId.value == cInfo.id ..`. While the chat is opening, `chatId` is still the previous value, so the item is dropped. The
  "already in the list" dedup at `:643` shows the code expects only the opposite race, where the item is in the snapshot and the
  event comes later.

Nothing brings the item back afterwards:
- `ChatView.kt:2512-2526` `loadLastItems` / `lastItemsLoaded`, and `PreloadItems` (`:2759+`), only fetch beyond a split. `splits` is
  empty (`ChatItemsLoader.kt:65`).
- `ChatItemsMerger.kt` (`MergedItems.create`) only presents `chatItems`. Received text items have `mergeCategory == null`, so they are
  always `Single` and cannot be hidden by grouping. The merger is not involved.

Why the unread-above button says 1:
- `ChatView.kt:263-266`: `unreadCount` = the chat-list `chatStats.unreadCount` (which includes item 4).
- `ChatView.kt:2641-2646`: `bottomUnreadCount = unreadCount - oldestVisible.unreadBefore`. `:2711-2712`:
  `topUnreadCount = unreadCount - bottomUnreadCount`, which equals `oldestVisible.unreadBefore`.
- `ChatItemsMerger.kt:30,42,44`: `unreadBefore` starts at `unreadCount - unreadAfterNewestLoaded` (or `unreadCount - unreadAfter` at
  the `unreadAfterItemId`) and goes down by 1 for each `RcvNew` item **present in the list**. Item 4 is counted but not present, so
  after 1, 2, 3 and 5 are marked read, every item has `unreadBefore = 1`, and the button shows 1.
- Marking items read (`ChatView.kt:767-785` -> `ChatModel.kt:900-918`, `:909`) subtracts only the items that were found and marked
  in `chatItems`. The badge never goes below the 1 that item 4 represents, and item 4 is never shown, so it is never marked.

### Unread arithmetic for the observed run (1, 2, 3, 5 shown, badge 1, unread-above 1)
- Items 1-3 are committed before the snapshot. Block 1 sets the chat-list count to 3, whatever was counted before.
- Item 4's event runs **between block 1 and block 2**: count 4, item dropped.
- Item 5's event runs after block 2: count 5, item appended (`addToChatItems` -> `chatState.itemAdded`).
- 1, 2, 3 and 5 become visible and are marked read: 5 - 4 = **1**. The badge and the unread-above button both show 1. The DB really
  has 1 unread item (4).
- If item 4's event lands **before block 1**, block 1's overwrite (`:75`) erases its +1. The item is still missing, but the badge and
  the button show 0 while the DB still has 1 unread item. That is the same bug with the counter error going the other way.

So the unread counting is not a separate bug. The count is correct for the DB and wrong for what the list holds.

## Timing: why it happens and why slow runners hit it more

1. **The test opens the chat while messages are still being delivered.** `bob sends` (`PeerLines.kt:24-27`) waits only for the CLI
   echo of the command, not for delivery to alice. `ChatListScreen.open` (`Screens.kt:83-91`) calls `quiesce()`
   (`Helpers.kt:20-24`). That waits for "no API call in flight and no app event for 1 s". Before delivery starts there is no event at
   all, so if alice's last connection event is more than 1 s old, `quiesce` returns at once and the click lands while bob's messages
   are in transit. Delivery is one message at a time (the SMP server delivers the next message after the ACK), so a 5-message burst
   spans the open. The affected tests all have `X sends ..` followed directly by `open`:
   `TextSelectionDesktopTest.kt:18-19,35-36`, `ReceivedMessagesTest.kt:99-100` ("before"), `MessagingTest.kt:27-28`
   ("hello! how are you?").
2. **The race window.** It runs from the core's snapshot read to block 2 at `ChatItemsLoader.kt:79`. In the UI test it is much wider
   than in the app. The test composition runs on `StandardTestDispatcher()` (`UiTest.kt:39`) and moves forward only when the test
   thread calls `waitForIdle`/`advanceTimeBy` (one `poll` iteration, at least 16 ms plus a sleep). `Dispatchers.Main` is the real Swing
   EDT (`kotlinx-coroutines-swing`, common/build.gradle.kts:135), where events are handled freely. The continuation after `apiGetChat`
   and the return from block 1 both wait for the next pump, so the EDT can run any number of `addChatItem` calls before block 1 and
   between block 1 and block 2. On slow runners, delivery and pumping are both slower, and the window lines up with the delivery burst
   more often.
3. **The real app is affected too** (inference: not reproduced in the app, but the code path is the same). The window from the core
   read to block 2 always exists: FFI return, JSON decode on IO, then dispatch back to the click scope and to Main. It is shorter, and
   blocks 1 and 2 may run back to back. So the app mostly shows the "badge 0, item missing" variant. The same race also affects:
   - `Around` with `openAroundItemId` (`ChatItemsLoader.kt:135-164`; `chatId` is set at `:159`);
   - `Initial` reloads of a chat that is already open. `ChatView.kt:2617` (`scrollToTopUnread`) and `apiFindMessages`
     (`ChatListNavLinkView.kt:255-260`) are examples. There an event is added to the old list and then erased by `replaceAll` at `:80`.
   - Status, edit and reaction events (`upsertChatItem`/`updateChatItem`, `ChatModel.kt:670+`) that arrive during the window are lost
     the same way for items in the snapshot.

## Minimal reproduction

Deterministic in the desktop UI harness (no app change):
1. `connect(bob)`. Wait until alice is quiet (`quiesce()`).
2. `click(Ui.ChatList.Row(bobChatId))` once. Its `waitForIdle` runs the click coroutine up to `withContext(IO)` in `sendCmd`, and the
   core reads the snapshot with no message.
3. **Without pumping the test dispatcher** (use only `Thread.sleep`, no `poll`/`waitForIdle`): `bob types "@alice before"`, then
   sleep-loop until `chatModel.chats.value.first { it.id == bobChatId }.chatItems.last().content.text == "before"`. That model is
   updated on the EDT. `chatModel.chatId.value` is still null at that point.
4. Resume with `poll`. Expected result: `chatModel.chatsContext.chatItems.value.none { it.content.text == "before" }`, and the item is
   missing from the opened chat for good. This variant has badge 0, because the event landed before block 1.
   For the badge-1 / unread-above-1 variant, let the test thread pump exactly until `chatModel.chats` shows the snapshot's stats
   (block 1 done) but `chatId` is still null, and deliver the message then.

In the app (manual, needs a temporary debug delay): put `delay(3000)` after `apiGetChat` in `apiLoadMessages`
(`ChatItemsLoader.kt:35`), open a contact's chat, and send a message from the contact within those 3 s. The message shows in the
chat-list preview but not in the opened chat until it is reopened.

## Fix ideas (not implemented)

App fix (the real bug). Make the published snapshot include everything committed after it was read:
- **A (recommended). Detect events that arrive during the load, then reload or fill the gap.**
  - Keep a per-`ChatsContext` "events for chat X" sequence counter, bumped in `addChatItem`/`upsertChatItem`/`updateChatItem` for any
    chat id, whether or not it is in scope.
  - Read it before `apiGetChat` and again in a single combined Main block (blocks 1 and 2 merged, so stats, items and `chatId` change
    atomically).
  - If it changed, run one follow-up `ChatPagination.After(lastLoadedId)` load (the merge path at `ChatItemsLoader.kt:111-134` already
    dedups and inserts after the anchor). Or rerun `Initial` once.
  - This also covers lost status and edit updates if the follow-up is an `Initial` reload.
- **B. Buffer events for the chat being opened.** Set a "pending open chat id" before `apiGetChat`. While it is set, `addChatItem`
  buffers items for that chat. In the combined Main block, append buffered items not in the snapshot, call `chatState.itemAdded` for
  each, and do not overwrite the chat-list stats with the older snapshot count. This needs no extra API call, but it handles only new
  items, not updates.
- With either option, merge the two Main blocks in the `Initial` branch. Today block 1's overwrite at `:75` can also erase increments
  from events that the snapshot does not contain, which gives the badge-0 variant.

Test-side (separate from the app fix, and it would hide the bug if done alone): before `chatList.open(..)` after `X sends ..`, wait
for delivery, for example by polling until the chat-list preview text is the last sent message and `unreadCount == N`. `quiesce()`
does not guarantee delivery. Keep one dedicated regression test for the race, using the deterministic recipe above, once the app fix
lands.
