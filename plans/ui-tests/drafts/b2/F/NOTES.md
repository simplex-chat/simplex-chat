# b2/F: chat-list contact, notes and pending-connection menus (catalogue 01 §3, rows 84–89)

Line numbers: catalogue rows refer to `01-onboarding-chatlist-connect-profiles.md`. App code is cited at master `d72f68e6d` (`~/wt-uitest-ref`), as the catalogue does. Haskell (`tests/ChatTests/*.hs`, `View.hs`, `Store/…`) is cited from `/home/user/wt-uitest`. `CLNLV` = `C/views/chatlist/ChatListNavLinkView.kt`.

## Files

| File | Tests |
|---|---|
| `main/ChatListMenusPendingTest.kt` (shared) | `contactMenuMarkRead`, `contactMenuMarkUnread`, `contactMenuRightClickOpens`, `notesMenuMarkUnreadAndClear`, `pendingInvitationRowIconAndName`, `pendingConnectionMenuSetName`, `pendingConnectionClickOpensInfo`, `pendingConnectionMenuDeleteInitiated` |
| `desktop/ChatListMenusPendingTest.kt` (desktop only) | `contactMenuForNotReadyContactOnlyDelete` |
| `keys.diff` | 3 new row-menu keys in `CLNLV` (see below) |
| `screens.kt` | `UiScope.openRowMenu(name)`, `CliPeer.createOneTimeLink()` |

The shared file is a drop-in replacement for the worktree's `main/ChatListMenusPendingTest.kt`. It keeps the existing `groupMenuMuteModesAllMentionsNone` unchanged.

`main/ChatListMenuTest.kt` in the worktree still has `contactMenuMarkReadAndUnread`. That name is not in the catalogue, and `contactMenuMarkRead` plus `contactMenuMarkUnread` now cover it. The other three tests in that file (`contactMenuFavoriteUnfavorite`, `contactMenuMuteUnmute`, `contactMenuClearChat`) are catalogue names, but the class map puts them in `ChatListMenusPendingTest`. I did not touch them because that is outside my fence. The coordinator should move them and delete the old test.

## Keys added (`keys.diff`, applies cleanly to `/home/user/wt-uitest` with `git apply --check`)

All keys use the existing `Ui.ChatList.RowMenu(chatId, action, rhId)` scheme. Ui.kt is unchanged. The keys are commonMain only, because the menus are shared code and the platform `ChatListNavLinkLayout` actuals only host them.

| Key | Composable | Used by |
|---|---|---|
| `RowMenu(id, "delete")` | `DeleteContactAction` (contact "Delete") | `contactMenuRightClickOpens`, `contactMenuForNotReadyContactOnlyDelete` |
| `RowMenu(id, "delete")` | `ContactConnectionMenuItems` "Delete" (pending connection) | `pendingConnectionMenuDeleteInitiated` |
| `RowMenu(id, "clear")` | `ClearNoteFolderAction` (notes "Clear"), the same action name as `ClearChatAction` | `notesMenuMarkUnreadAndClear` |

These keys were already there and are reused: `markRead`, `markUnread`, `favorite`, `ntfs.<mode>`, `clear` (contacts and groups) and `setName` (pending connection). `DeleteGroupAction`, `JoinGroupAction`, `ArchiveAllReportsItemAction` and `TagListAction` still have no keys. b2/D adds `tagList`. My tests avoid depending on it and check the text "Add to list" instead. The hunks do not overlap with b2/D's `CLNLV` hunk at line ~405.

## Helpers (`screens.kt`)

- `UiScope.openRowMenu(name): String` waits for API calls to finish, settles and right-clicks the row (a long press on Android). It returns the chat id so the test can check which `RowMenu` keys exist before it clicks one. Its first lines duplicate `ChatListScreen.rowMenu`. `rowMenu` could call it, but Screens.kt is additions-only for this batch.
- `CliPeer.createOneTimeLink()` runs `/c` and returns the first `https://` or `simplex:/` link it prints (`viewConnReqInvitation`, View.hs:1066-1072). It parses the same way as `connectPeers`.

Menus are left open, or closed only by clicking an item. The desktop `escape()` goes through `simplexWindowState.backstack`, and a `DropdownMenu` popup does not register there. A later click outside the popup would then wait for the popup to close (`waitPopupsClosedUnlessInside`).

## Per test

### `contactMenuMarkRead` (01:84)
- **Steps:** bob sends one message, and alice's row shows the badge "1". Then row menu → "Mark read" (key `markRead`, offered because `showMarkRead` = `unreadCount > 0 || unreadChat`, `CLNLV:39-41,289-290`).
- **Asserts:** the badge "1" disappears from the row and `chatStats.unreadCount == 0` (`markChatRead`, `CLNLV:643-675`, `apiChatRead`).
- **Haskell:** `testMarkReadDirect` (Direct.hs:374).
- **Platform:** shared. On Android, `rightClick` is a long press (05 carryover R1, `ChatListNavLinkView.android.kt:26-27`).

### `contactMenuMarkUnread` (01:84)
- **Steps:** after `connect` and `quiesce`, the row has no unread messages, so the menu offers "Mark unread" (`markUnread`, `CLNLV:291-292`).
- **Asserts:**
  - `chatStats.unreadChat` becomes true (`markChatUnread` :677-696, `/_unread chat @<id> on`, Haskell `testAddContact` Direct.hs:206).
  - The row gets one more empty text node. That node is the count-less unread badge: `UnreadBadge(text = "")` when `n == 0 && unreadChat` (`ChatPreviewView.kt:464,496`).
  - Reopening the menu offers `markRead` and not `markUnread`. Clicking it clears the mark (`markChatRead` :660-675).
- **Uncertainty:** the badge check counts empty `Text` semantics nodes relative to before. Compose's `BasicText` sets `text = AnnotatedString("")` for an empty string, so the badge should add one empty entry. If that proves flaky, add a key on `UnreadBadge` instead. That needs the chat id threaded into `ChatPreviewView`, which is an app change outside this row's scope.

### `contactMenuRightClickOpens` (01:84)
- **Steps:** right-click on bob's row.
- **Asserts:**
  - The menu items exist by key: `markUnread`, `favorite`, `ntfs.None` (contact mute cycles All↔None, `nextMode(false)`), `clear` and `delete`.
  - The labels "Mark unread", "Favorite", "Mute", "Add to list", "Clear" and "Delete" are visible (strings.xml 851, 858, 855, 863, 847, 848).
- **Code:** `ContactMenuItems`, `CLNLV:278-300`; `.onRightClick`, `D/views/chatlist/ChatListNavLinkView.desktop.kt:40`.
- **Platform:** shared. On Android this exercises the long press of the same handler (05 carryover R1).

### `contactMenuForNotReadyContactOnlyDelete` (01:85), desktop only
- **Steps:**
  1. bob creates a 1-time link (`/c`).
  2. The link is typed into the chat-list search (`ChatListView.kt:837-844` starts the plan).
  3. The plan alert ("Open new chat", key `Alert.Confirm(PrivacySensitive)`, `showPrepareContactAlert`, `ConnectPlan.kt:95,700-747`) is confirmed, which creates a prepared contact with no connection (`createPreparedContact`, `Store/Direct.hs:407-413`).
- **Asserts:**
  - In the model, `nextConnectPrepared && activeConn == null`.
  - The row menu has `delete` and none of `markRead`/`markUnread`/`favorite`/`unfavorite`/`ntfs.*`/`clear`, and no "Add to list" (`CLNLV:288` skips them without `activeConn`).
  - "Delete" opens "Confirm contact deletion?" (`deleteContactDialog` → `deleteNotReadyContact`, `C/views/chat/ChatInfoView.kt:231-246,421-426`, strings.xml 722). The alert's buttons have no keys, so the test stops at the title.
- **Why desktop only:** typing a link into the search field needs `performTextInput` without a read-back. The app replaces the link with its description at once, so `typeInto`'s poll would fail. This is the same limitation and the same private `enterLinkInSearch` as b1/C's `desktop/ChatListToolbarSearchTest.kt`. A shared `enterText(key, text)` in both UiScopes would let it move to `main/`. On Android, the 05 R6 path (paste link through the connect screen) is the adaptation.

### `notesMenuMarkUnreadAndClear` (01:87)
- **Steps:**
  1. alice sends "keep in mind" to Private notes, then goes back to the chat list (`backToChatList`; on desktop the notes chat stays open on the right).
  2. Row menu "Mark unread". It is offered because own notes are read.
  3. Row menu "Clear" (new key `clear` on `ClearNoteFolderAction`, `CLNLV:449-458`).
  4. The alert "Clear private notes?" / "All messages will be deleted - this cannot be undone!" (`clearNoteFolderDialog`, `ChatInfoView.kt:508-516`; strings 842, 844) is confirmed with `Alert.Confirm(Shared)`.
- **Asserts:**
  - `unreadChat` is set.
  - After the clear, the notes chat has no items, the unread mark is reset (`ChatsContext.clearChat` replaces the stats with `ChatStats()`, `C/model/ChatModel.kt:803-816`), and the row preview no longer shows the note.
- **Haskell:** `testNotes` (Local.hs:48 `/_unread chat *1 on`; :164-165 `/clear *` → "notes: all messages are removed").
- **Platform:** shared. On desktop, marking the open chat unread is not undone, because `markUnreadChatAsRead` runs only when `chatId` changes (`ChatView.kt` snapshotFlow on `chatModel.chatId`).

### `pendingInvitationRowIconAndName` (01:88)
- **Steps:** `keepOneTimeLink` creates a link and keeps it ("Keep unused invitation?" → keep).
- **Asserts:**
  - The row reads "invited to connect" / "you shared one-time link" (`PendingContactConnection.displayName`/`description`, `ChatModel.kt:3242-3266`; strings 95, 99).
  - The row has the icon description "profile image placeholder" (`ContactConnectionView.kt:26` → `ProfileImage`, `ChatInfoImage.kt:92`, strings 889).
  - The model's `initiated == true`, which is what selects `ic_add_link`. The icon itself cannot be told apart from `ic_link` by semantics, as the catalogue says.

### `pendingConnectionMenuSetName` (01:88)
- **Steps:**
  1. Before the menu, "You invited a contact" is not shown.
  2. Row menu "Set contact name" (`setName`, `CLNLV:582-592`).
- **Asserts:**
  - The menu opens "You invited a contact" (`ContactConnectionInfoView.kt:110-115`, strings 875) with the name field (`Ui.Info.Alias`, `LocalAliasEditor`, :128).
  - Typing "friend" saves the alias (`apiSetConnectionAlias`). After Back, the row reads "friend" (`displayName` returns `localAlias`, `ChatModel.kt:3243`).
- **Note:** the screen from the menu focuses the name field (`focusAlias = true`, `CLNLV:590`). Focus is not asserted.

### `pendingConnectionClickOpensInfo` (01:88)
- **Steps:** click the pending row. Its click sets `chatModel.chatId` (`CLNLV:150-152`), and `ChatView` shows `ContactConnectionInfoView` in a modal (`ChatView.kt:824-828`).
- **Asserts:** `chatId` is the row's id, "You invited a contact" is shown, the "One-time invitation link" section is shown (strings 950; shown because `connLinkInv` is set and the connection was initiated, `ContactConnectionInfoView.kt:131-133`), and the name field exists.

### `pendingConnectionMenuDeleteInitiated` (01:89)
- **Steps:**
  1. Row menu "Delete" (new key `delete`).
  2. Alert "Delete pending connection?" with "invited to connect\n\nThe contact you shared this link with will NOT be able to connect!". It is plain text (`parseHtml = false`), so the test matches a substring (`deleteContactConnectionAlert`, `CLNLV:775-797`; strings 877-878).
  3. Confirm with `Alert.Confirm(Shared)`. The row disappears.
  4. The CLI repeats `/_connect plan 1 <link>` until the result is "error: connection authorization failed…" rather than "invitation link: ok to connect" (View.hs:2241). The agent deletes the queue on the server later, in a background thread.
  5. `/c <link>` then prints "error: connection authorization failed - …" (View.hs:2922-2924).
- **Haskell:** `testShortLinkDeletedInvitation` (Profiles.hs:4268) for the deletion. `testPlanShortLinkInvitation` (Profiles.hs:3895-3932) for `/_connect plan` and `/c` failing with AUTH on a link the server no longer serves.
- **Uncertainty:**
  - The poll body sends a CLI command on every iteration. Each iteration takes one server round trip, so the loop is bounded by `CONNECT_TIMEOUT_MS` and not by a fixed count.
  - simplexmq `Agent.hs:1013-1015` (the background deletion) was not re-checked. I took it from the catalogue.

## Uncertainties (top)

1. The `contactMenuMarkUnread` badge check counts empty text nodes. See above.
2. The desktop-only placement of `contactMenuForNotReadyContactOnlyDelete` comes from the harness, not from the app. It runs on Android once both scopes have `enterText`.
3. `pendingConnectionMenuDeleteInitiated` assumes that the GUI link shown in `Ui.Link.Text` (returned by `keepOneTimeLink`) is accepted verbatim by the CLI's `/_connect plan 1` and `/c`. The existing `OneTimeLinkTest` flows rely on the same assumption.
4. `contactMenuRightClickOpens` checks generic labels ("Clear", "Delete") with `seesText`. The keys are the real assertion. The labels can only give a false positive, and only if the same text is visible elsewhere.

## Catalogue corrections

1. **01:84, Verification, the `contactMenuMarkUnread` clause.**
   - **Problem:** with no unread messages, "Mark unread" sets the chat's unread mark, which shows as an empty badge with no count. The menu then offers "Mark read" instead.
   - **Replace** `` `contactMenuMarkUnread`: "Mark unread" puts the unread badge back on the row (`ChatListNavLinkView.kt:288-292`) `` **with:** `` `contactMenuMarkUnread`: "Mark unread" (offered while the row has no unread messages, `ChatListNavLinkView.kt:39-41,288-292`) sets the chat's unread mark (`markChatUnread`, :677-696; `/_unread chat @<id> on`), shown as an unread badge with no count (`ChatPreviewView.kt:464,496`); the menu then offers "Mark read" in its place, which clears the mark (`markChatRead`, :643-675) ``
2. **01:85, Notes cell (currently `–`).**
   - **Problem:** the cell does not say that entering the link needs a desktop-only harness action, or which dialog "Delete" opens.
   - **Replace** `–` **with:** `The link is typed into the chat-list search (ChatListView.kt:837-844); only the desktop harness can enter it (the field replaces the link with its description at once), so the test is in desktopTest and Android adapts it through "Paste link / Scan" (05 R6). "Delete" opens "Confirm contact deletion?" (C/views/chat/ChatInfoView.kt:231-246,421-426)`
3. **01:87, Verification.**
   - **Problem:** the cell says only "Notes cleared" and gives no strings or checks.
   - **Replace** `Notes cleared. Haskell: \`Local.hs testNotes\`` **with:** `With one note sent the menu offers "Mark unread" and "Clear" (ChatListNavLinkView.kt:355-362); "Mark unread" sets the unread mark; "Clear" shows "Clear private notes?" / "All messages will be deleted - this cannot be undone!" (C/views/chat/ChatInfoView.kt:508-516), and its "Clear" empties the notes and resets the chat stats, the unread mark included (C/model/ChatModel.kt:803-816). Haskell: testNotes (Local.hs:48 "/_unread chat *1 on"; :164-165 "/clear *" → "notes: all messages are removed")`
4. **01:88, Verification, append.**
   - **Problem:** the cell does not say how the two entry points differ.
   - **Append:** `; the screen opened from "Set contact name" focuses the name field (focusAlias = true, ChatListNavLinkView.kt:590), a click opens it unfocused (ChatView.kt:827)`
5. **01:89, Verification, Haskell citation.**
   - **Problem:** `testShortLinkDeletedInvitation` checks `/_connect contact @2` on a prepared contact. It does not check `/_connect plan` or `/c`.
   - **Replace** `Haskell: \`testShortLinkDeletedInvitation\` (H/Profiles.hs:4268)` **with:** `Haskell: testShortLinkDeletedInvitation (H/Profiles.hs:4268) for deletion; testPlanShortLinkInvitation (H/Profiles.hs:3895-3932) for "/_connect plan" and "/c" printing "error: connection authorization failed" on a link the server no longer serves (View.hs:2922-2924); before the deletion reaches the server the plan prints "invitation link: ok to connect" (View.hs:2241)`

## Rows skipped

None. All 9 names in `tests.txt` are drafted. Row 84's other names (`contactMenuFavoriteUnfavorite`, `contactMenuMuteUnmute`, `contactMenuClearChat`) were not assigned to me. They already exist in the worktree's `ChatListMenuTest.kt`.
