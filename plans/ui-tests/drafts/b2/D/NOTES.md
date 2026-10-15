# Chat list preset edge cases and user lists (catalogue 01, section 3, rows 69-74): notes

15 tests drafted in 2 files (13 shared, 2 desktop-only), 1 row test skipped. Nothing has been compiled or run.

Paths:
- `C/` = `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/`
- `CLV` = `C/views/chatlist/ChatListView.kt`, `TLV` = `C/views/chatlist/TagListView.kt`, `CLNLV` = `C/views/chatlist/ChatListNavLinkView.kt`
- `D/TLV` = `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/views/chatlist/TagListView.desktop.kt`, `A/TLV` = the androidMain `TagListView.android.kt`
- `S` = `apps/multiplatform/common/src/commonMain/moko-resources/base/strings.xml`
- `src/` = `src/Simplex/Chat/` (Haskell core)
- App line numbers are from `/home/user/wt-uitest` as it is now (`eda94a505` plus uncommitted harness changes), before `keys.diff` is applied. Catalogue lines are from `plans/ui-tests/catalogue/01-onboarding-chatlist-connect-profiles.md`.

## Files

| File | Contents |
|---|---|
| `main/ChatListFiltersListsTest.kt` (shared) | A drop-in replacement for the repo file: the 8 existing tests unchanged, plus `presetFiltersCollapsedMenuWhenMany`, `presetFilterAllResets`, `presetTagsHiddenWhenOnlyOneKind`, `createListFromAddList`, `createListDuplicateNameError`, `addChatToListFromContextMenu`, `changeChatListFromContextMenu`, `removeChatFromList`, `filterByUserList`, `emptyListShowsNoChatsInList`, `listUnreadDot`, `deleteListConfirm`, `listsArePerUser` |
| `desktop/ChatListFiltersListsTest.kt` (desktop only) | `createListWithEmoji`, `editListNameAndEmoji` |
| `screens.kt` | New harness file (`UserListsScreen`, `ListEditorScreen`, `UiScope.lists()`); nothing in it repeats an existing helper |
| `keys.diff` | App-side keys. `git apply --check` passes on `/home/user/wt-uitest` as it is now |

## Keys added (`keys.diff`)

| Key | Attached at | Why |
|---|---|---|
| `Ui.ChatList.PresetMenuAll` | the "All" `ItemAction` in `CollapsedTagsFilterView`, CLV:1506 | The item has no key and is present only when a filter or search text is active (CLV:1505) |
| `Ui.ChatList.AddList` | the "Add list" row (no lists, CLV:1398) and the "+" icon (lists exist, CLV:1406) | Both open the editor (CLV:1390-1395). Only one is composed at a time |
| `Ui.ChatList.UserTag(tagId)` | the user-list chip row, CLV:1322-1339 | Click toggles the filter, right-click/long-press opens the menu |
| `Ui.ChatList.UserTagMenu(tagId, "edit" / "delete")` | `EditTagAction` TLV:319-337, `DeleteTagAction` TLV:306-316 | "Change order" (TLV:340-352) gets no key, because the reorder test is skipped |
| `Ui.ChatList.RowMenu(chatId, "tagList")` | `TagListAction`, CLNLV:400-418 | Same action pattern as the other row menu items. Its label is "Add to list" or "Change list" |
| `Ui.TagList.Tag(tagId)` | the list row in `TagListView`, TLV:111-137 | Tapping it assigns, moves or removes (TLV:115-135) |
| `Ui.TagEditor.Name` | `TagListNameTextField`, TLV:368-377 | |
| `Ui.TagEditor.Save` | the save `SectionItemView`, TLV:264 | Its label is "Save list" or "Add to list" (TLV:266) |
| `Ui.TagEditor.Duplicate` | the error footer row, TLV:272, **only while the error shows** (`showErrorMessage`, TLV:270) | The message is always in the tree and only its colour changes (TLV:279,287). So the gated key is the only way to read it by semantics, the same pattern as `Onboarding.ProfileName.part("invalid")` |
| `Ui.TagEditor.Emoji` | desktop `SingleEmojiInput` TextField, D/TLV:44-60; Android picker button `Box`, A/TLV:25-36 | Both platforms get the key. Only desktop tests type into it |
| `Ui.Alert.Confirm(Shared)` (existing key) | "Confirm" `SectionItemView` in `deleteTagDialog`, TLV:483-493 | A buttons-column alert has no keyed buttons. "Confirm" is the confirm action, so the existing key fits |

New imports: `chat.simplex.common.ui.Ui` and `chat.simplex.common.ui.ui` in TLV, D/TLV and A/TLV (CLV and CLNLV already have them).

Not added: a key on the check mark in the list sheet (TLV:151-153). The icon has no description and sits inside the row's `clickable`, which merges descendants, so a `testTag` on it would not show in the merged tree that `exists()` uses. The tests check the state that draws it (`chatInfo.chatTags`, TLV:52,109) instead. The screenshot shows the icon.

## Helpers (`screens.kt`)

- `UserListsScreen` (from `UiScope.lists()`):
  - `tag(name)` polls `chatModel.userTags`.
  - `chip(name)`, `chipTexts(name)`, `activeTagId()`, `tagsOf(chat)` (from `chatInfo.chatTags`), `editor()`.
  - `addList()` clicks `AddList`. `create(name)` = `addList` + `typeName` + `save`.
  - `menu(name, action)` waits for in-flight API calls, like `rowMenu`, then right-clicks the chip and clicks the item.
  - `filter(name)` clicks the chip and polls `ActiveFilter.UserTag`.
  - `assign(chat, list)` = row menu `tagList`, tap the list, poll `chatTags == [id]` and the sheet closed.
- `ListEditorScreen`:
  - `shown()`, `typeName`, `replaceName`, `nameText`, `saveLabel`.
  - `emojiText()` reads the desktop field's editable text, or the Android button's text.
  - `save()` clicks and polls the editor closed.
  - `saveIgnored()` clicks, `settle()` + `quiesce()`, then checks the editor is still open.

Why `saveIgnored`: when it is disabled, `SectionItemView` drops its `clickable` without setting the `Disabled` semantics (`C/views/helpers/Section.kt:212-219`), so `isEnabled()` reads true. "Disabled" is therefore tested by behaviour: a click does nothing.

All helpers use only methods that both `UiScope` implementations have (`click`, `rightClick`, `typeInto`, `replaceText`, `exists`, `poll`, `settle`, `textsOf`, `textOf`, `editableText`).

## Tests

### Shared (`main/ChatListFiltersListsTest.kt`)

**presetFiltersCollapsedMenuWhenMany** (catalogue :69)
- Setup: bob contact, team invitation (`hostGroup`), notes.
- With 3 kinds (Contacts, Groups, Notes) there are 3 chips and no menu, because ≤3 means expanded (CLV:1300-1304).
- Favouriting bob makes 4 kinds. The chips collapse into the menu, whose icon reads "All" (`chat_list_all`, S:539; CLV:1494-1499).
- The menu lists Favorites/Contacts/Groups/Notes (S:531-533,536) and has no "All" item, because no filter or search is active (CLV:1505).
- Choosing Groups makes it the active filter, the menu icon's description becomes "Groups" (CLV:1486-1493), and only team is listed.
- An invitation group counts as Groups (`presetTagMatchesChat`, CLV:1600-1603).

**presetFilterAllResets** (:69)
- Same collapsed setup. Searching "bob" leaves only bob (3 letters, so no name row, `MIN_NAME_LENGTH = 5`, CLV:1147).
- Menu → "All" clears the search text and shows every chat (CLV:1509-1513).
- `presetFilter(CONTACTS)` leaves only bob. Menu → "All" clears the filter (icon back to "All") and shows every chat.

**presetTagsHiddenWhenOnlyOneKind** (:69)
- Fresh alice: `presetTags` has only NOTES, and the tags row is rendered (`AddList` exists), but there are no preset chips and no menu, because the presets need at least two collapsible kinds (CLV:1300).
- After `connect(bob)`, the Contacts and Notes chips appear and there is still no menu.

**createListFromAddList** (:70)
- "Add list" (S:540) opens an empty editor with the placeholder "List name..." (S:866) and the button "Save list" (S:865).
- After saving "work", the chip shows "work", and "Add list" becomes the "+" icon (described "Add list", CLV:1404-1408). The created tag has no emoji.

**createListDuplicateNameError** (:70)
- With "work" existing, typing "work" shows the duplicate error (gated key, text `duplicated_list_error` S:867, `isDuplicateEmojiOrName` TLV:187-194).
- Save does nothing (TLV:259-264); `userTags` still holds only "work".
- "work2" clears the error, and saving creates the second list.

**addChatToListFromContextMenu** (:71)
- With no lists, bob's row menu "Add to list" (S:863) opens the editor, whose button reads "Add to list" (TLV:266; CLNLV:410-411).
- Saving creates "work" and tags bob (TLV:196-225 → `setTag` TLV:402-440).
- cath's "Add to list" then opens the list sheet (CLNLV:412-413). Tapping "work" tags cath and closes the sheet (TLV:129-133).

**changeChatListFromContextMenu** (:71)
- Two lists, bob in work. The row menu reads "Change list" (S:864) and the sheet lists both lists.
- Tapping family replaces bob's tags with `[family]` (`apiSetChatTags` with one id, TLV:404-411; core replaces the tags, `src/Library/Commands.hs:766-770`), and the sheet closes.

**removeChatFromList** (:71)
- Bob in work. "Change list" → tapping the checked "work" sends `tagId = null` (TLV:130), which leaves bob with no tags and closes the sheet.
- The row menu reads "Add to list" again. Its sheet is closed with `escape()`, and the list itself remains.

**filterByUserList** (:72)
- Bob in work, cath not. Tapping the chip shows only bob (`filtered` UserTag branch, CLV:1584).
- Tapping it again clears the filter (CLV:1327-1328).

**emptyListShowsNoChatsInList** (:72)
- An empty "work" list filtered shows "No chats in list work." (S:505, CLV:1250) and no rows.

**listUnreadDot** (:72)
- Bob in work. The chip reads "work" with no dot.
- After bob's message the chip text is "work ●" (CLV:1355; `unreadTags` raised in `ChatModel.kt:590-601` → `:1037-1051`).
- Opening bob's chat marks it read. Back on the list, the dot is gone.
- CLI: `bob sends "@alice hi"` (same form as the existing `unreadFilterToggle`).

**deleteListConfirm** (:73)
- Bob in work. Chip menu "Delete" (S:868) shows "Delete list?" / "All chats will be removed from the list work, and the list deleted" (S:869-870, TLV:478-481).
- "Confirm" (S:787) removes the tag from `userTags` and the chip, keeps bob in the list, and clears bob's tags (TLV:442-468).

**listsArePerUser** (:74)
- alice creates "work". Adding the profile alice2 makes it active, with no lists and "Add list" shown (`getUserChatData` reloads `userTags`, `C/model/SimpleXAPI.kt:775-776`; core `getUserChatTags` filters by `user_id`, `src/Store/Shared.hs:919-928`).
- alice2 creates "home". Switching back to alice shows only "work".

### Desktop only (`desktop/ChatListFiltersListsTest.kt`)

Why desktop only: the emoji input is a text field on desktop (D/TLV:33-72). On Android it is a button that opens `EmojiPickerView`, an `AndroidView` (A/TLV:25-36,54-80) that Compose semantics cannot drive.

**createListWithEmoji** (:70)
- Typing "🚀" (passes `isShortEmoji`, `C/views/chat/item/EmojiItemView.kt:58-61`) and "work" creates a tag whose emoji is "🚀".
- That tag comes from the core's `CRChatTags` reply to `/_create tag` (`src/Library/Commands.hs:763-765`), so the emoji did go through the API. This replaces the catalogue's Log check.
- The chip texts include "🚀" (desktop `ReactionIcon` is a `Text`, `ChatItemView.desktop.kt:24-30`) and "work".

**editListNameAndEmoji** (:73)
- Chip menu "Edit" (S:871) opens the editor with "🚀" and "work" and the button "Save list". Save does nothing while unchanged (TLV:260).
- Changing to "🏠"/"home" and saving updates the model tag (TLV:231-238) and the chip texts, and "work" is gone.

## Platform notes

- Chip menus: `rightClick` = desktop `onRightClick` (CLV:1338) or Android long press (`onLongClick`, CLV:1333). Both are the same `showMenu`.
- On Android the tags row scrolls horizontally (`TagsRow`, `ChatListView.android.kt:33-43`). `click` scrolls nodes into view, and `exists` does not need them on screen (it is not a lazy list).
- With `OneHandUI` (default on) the list sheet puts "Create list" at the top instead of the bottom (TLV:95-99,164-168). The tests do not use that row.
- `listUnreadDot` uses `backToChatList()` (Android closes the chat; desktop keeps the list visible).

## Uncertainties

1. **`saveIgnored` relies on a click doing nothing.** If the `clickable`-less row ever passes the click on to a parent, the check could pass for the wrong reason. Making the button truly disabled would need `Disabled` semantics in `SectionItemView`, an app change outside key scope; I did not make it.
2. **`removeChatFromList` closes the list sheet with `escape()`.** This assumes `ModalManager.start` modals are on the desktop window back stack, as `ProfilesScreen.close()` already assumes.
3. **The chip-text assertions assume `textsOf` returns full AnnotatedString texts** ("work ●" is one span-styled string, CLV:1356-1376). The chip has two Text nodes (an invisible sizing copy and the visible one), so each string appears more than once. The tests use `in`, not equality.
4. **`createListDuplicateNameError` assumes a gated `testTag` drops the node from the tree.** When `showErrorMessage` turns false, `Modifier.ui(null)` should remove it (the row then has no semantics of its own). If a semantics node lingers without the tag, the `!exists` checks still hold.
5. **`presetTagsHiddenWhenOnlyOneKind` assumes a fresh harness user has only the notes folder.** If the harness ever adds a contact card ("Ask SimpleX Team"), it is not counted (`isContactCard`, CLV:1594), and the poll's precondition reports it.
6. **`TagListEditor` with a chat calls `close()` twice** (TLV:204 and again via `setTag`, TLV:214-217). `toRemove` is a set, so the second call is harmless when the editor was the only start modal. If another start modal were open it would close that one too. This is a latent app quirk, not covered.

## Catalogue corrections

- **:69 (Collapsed presets)**
  - Problem: the threshold, the "All" item and the single-kind rule are incomplete.
  - Replace the Verification text with: `With at least two kinds that can collapse (every preset except Reports, CLV:1300,1629-1632), the presets are separate chips while kinds + Reports + user lists ≤ 3 (CLV:1301-1304) and otherwise a menu whose icon reads "All" or the selected preset (CLV:1486-1499); the menu lists the collapsible kinds that have chats and an "All" item only while a filter or search text is active (CLV:1505), which clears both (CLV:1509-1513); with fewer than two collapsible kinds no presets are shown`.
  - Code refs: `ChatListView.kt:1300-1311,1469-1527`.
- **:70 (Create list)**
  - Problem: emoji input is platform-specific, "disabled" is not observable, and the Log check is replaced.
  - Blockers column: `Emoji input is a single-emoji text field on desktop (TagListView.desktop.kt:33-72) and an EmojiPickerView AndroidView on Android (TagListView.android.kt:25-36,54-80), so createListWithEmoji is desktop-only; "Save list" when disabled drops its clickable without Disabled semantics (Section.kt:212-219), so the tests check that a click does nothing`.
  - In the Verification text, replace `the Log's /_create tag carries "emoji": "<emoji>"` with `the tag returned by /_create tag (CRChatTags, src/Simplex/Chat/Library/Commands.hs:763-765) carries the emoji`.
- **:71 (Assign)**
  - Problem: the code ref is stale, and the scope of the menu item is missing.
  - Code refs: `ChatListNavLinkView.kt:400-418,304,345`.
  - Add to Verification: `The item exists only for contacts with an active connection (ChatListNavLinkView.kt:296-305) and for joined groups (:335-345), not for notes, requests, pending connections or group invitations; the check mark follows chatInfo.chatTags (TagListView.kt:52,109,151-153)`.
- **:72 (Filter by list)**
  - Problem: the code refs are stale.
  - Replace with `ChatListView.kt:1313-1388,1244-1250,1581-1586`.
  - Add: `tapping the active chip again clears the filter (:1327-1328); the dot comes from unreadTags (:1355; ChatModel.kt:1037-1051)`.
- **:73 (Edit, delete, reorder)**
  - Problem: the entry path is desktop-only, the emoji edit is desktop-only, and the reorder can't be driven.
  - Entry path: `Right-click (desktop) or long-press (Android) chip → "Edit" / "Delete" / "Change order"` (ChatListView.kt:1333,1338).
  - Blockers column: `Emoji editing is desktop-only (see :70), so editListNameAndEmoji is desktop-only; reordering is detectDragGesturesAfterLongPress (DragAndDrop.kt:144-146) with no semantics action, so reorderListsDrag can't be driven without coordinates`.
- **:74 (Per-user lists)**
  - Problem: the core-side reference is missing.
  - Add `src/Simplex/Chat/Store/Shared.hs:919-928` (tags selected by `user_id`).
  - Verification: `Lists differ per profile: a new profile starts with none and "Add list"; switching back restores the first profile's lists`.

## Skipped

- **reorderListsDrag** (:73). Reordering exists only as drag-and-drop: `Modifier.dragContainer` → `detectDragGesturesAfterLongPress` (`C/views/helpers/DragAndDrop.kt:144-146`, used at TLV:67-75). It exposes no semantics action (no custom accessibility action, no move-up/down control), so it cannot be driven without pointer coordinates, which the rules forbid. The "Change order" menu item (TLV:340-352) could be checked to open the sheet in reorder mode (drag handles at TLV:154-156, no "Create list" row at TLV:95,164), but that would not test the row's claim that the order changes. Enabling the test would need an app-side accessibility action (for example `customActions` "Move up"/"Move down" on each row calling the same `reorderTags`), which is a feature change and out of scope.
