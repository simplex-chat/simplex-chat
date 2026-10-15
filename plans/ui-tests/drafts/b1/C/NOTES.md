# Chat list search and filters (catalogue 01, section 3): notes

17 tests in 3 classes. None of them has been compiled or run.

Paths:
- `C/` means `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/`.
- `CLV` means `C/views/chatlist/ChatListView.kt`.
- `S` means `apps/multiplatform/common/src/commonMain/moko-resources/base/strings.xml`.
- `H/` means `tests/ChatTests/`, and `src/` means `src/Simplex/Chat/`.
- App line numbers are from `/home/user/wt-uitest` (`eda94a505` plus its uncommitted harness changes), before `keys.diff` is applied.
- Catalogue line numbers are from `plans/ui-tests/catalogue/01-onboarding-chatlist-connect-profiles.md`. The catalogue's own code references (from `~/wt-uitest-ref`, `d72f68e6d`) sit about 4 lines lower in CLV than the ones given here.

## Files

| File | Tests |
|---|---|
| `main/ChatListToolbarSearchTest.kt` (shared) | `newChatButtonOpensNewChatSheet`, `searchFiltersByAlias`, `searchMatchesPendingConnectionAlias`, `searchNoChatsFound`, `searchClearsWhenFilterChanges`, `searchNameShowsConnectToNameRow` |
| `desktop/ChatListToolbarSearchTest.kt` (desktop only) | `searchPasteLinkTriggersConnectPlan`, `searchPasteKnownContactLinkFiltersToContact`, `searchPasteKnownGroupLinkFiltersToGroup` |
| `main/ChatListFiltersListsTest.kt` (shared) | `unreadFilterToggle`, `unreadFilterEmptyState`, `presetFilterFavorites`, `presetFilterContacts`, `presetFilterGroups`, `presetFilterBusinesses`, `presetFilterNotes`, `presetFilterReports` |
| `screens.kt` | New top-level helpers for `harness/` (listed below). Nothing in it repeats an existing helper. |
| `keys.diff` | App-side keys. `git apply --check` passes on `/home/user/wt-uitest` as it is now. |

The class names come from the class map: `ChatListToolbarSearchTest` starts at `chatListTitleAndProfileButton` (toolbar and search rows), and `ChatListFiltersListsTest` starts at `unreadFilterToggle`. The repo already has `main/ChatListToolbarSearchTest.kt` with `searchFiltersByDisplayName`. The six shared tests here are meant to be added to that class, not to replace it.

## Keys added (`keys.diff`)

All of them are in `Ui.ChatList`. Every composable involved is in commonMain, so desktop and Android both get the keys.

| Key | Attached at | Why |
|---|---|---|
| `NewChatFab` | FAB, `CLV:489` (it used `NewChat` before) | The toolbar button (`CLV:593`) and the FAB had the same key, so a test could not tell which one was on screen. `NewChat` stays on the toolbar button, which is the default (`OneHandUI` defaults to true, `C/model/SimpleXAPI.kt:276`), so `App.newChat()` keeps working. |
| `UnreadFilter` | `ToggleFilterEnabledButton` `IconButton`, `CLV:753` | The icon has no description. |
| `NoUnreadChats` | the clickable "No unread chats" row, `CLV:1250` | Tapping it clears the filter. |
| `ConnectByName` | `ConnectByNameRow`, `CLV:1213` | The NewChatSheet search uses this row too (`NewChatSheet.kt:314,405`), so it gets the same key there. Only one of the two is composed at a time. |
| `PresetTag(kind.name)` | `ExpandedTagFilterView` row, `CLV:1424` | |
| `PresetMenu` | `CollapsedTagsFilterView` box, `CLV:1476` | |
| `PresetMenuItem(kind.name)` | `ItemPresetFilterAction` (`uiKey =`), `CLV:1529` | |
| `RowMenu(id, "setName")` | "Set contact name" in `ContactConnectionMenuItems`, `ChatListNavLinkView.kt:600-610` | This is the only path that names a pending connection without opening a chat. |

`PresetTag` and `PresetMenuItem` take the `PresetTagKind` name as a `String`, so `Ui.kt` does not need to import the views package.

## Helpers added (`screens.kt`)

- `presetFilter(kind, label)` waits for `quiesce()`. If the chip for `kind` exists, it checks the chip's label and clicks it. Otherwise it opens the collapsed menu and clicks the item, after checking that item's label. In both cases it then polls `chatModel.activeChatTagFilter` for the new filter.
- `backToChatList()` calls `escape()` only when the search field is missing, which is the case on Android while a chat covers the list. On desktop a chat cannot be closed (there is no back button or BackHandler for `ChatView` on desktop, `ChatView.kt:1226-1228,1446`), so this does nothing there.
- `keepOneTimeLink(app)` creates the link through the New chat sheet, closes it with `escape()`, picks "Keep" in "Keep unused invitation?" (`NewChatView.kt:73-101`, `Ui.Alert.Dismiss(Shared)`), closes the sheet and returns the link. The pending connection stays in the list (`NewChatView.kt:819`).
- `namePendingConnection(alias)` uses the row menu "Set contact name", which opens `ContactConnectionInfoView` in the center modal. It types the alias into `Ui.Info.Alias`, waits for the debounced save (`ChatInfoView.kt:866-877`, `ContactConnectionInfoView.kt:193-199`) and closes the modal.
- `joinLink(link)` runs `apiConnectPlan` and then `apiConnect`. It is API setup, the same way `Helpers.connect` uses `apiAddContact`. The chat appears through the app's own event handlers (`ContactConnected`, `SimpleXAPI.kt:2927`; `GroupLinkConnecting` :3162; `BusinessLinkConnecting` :3175, all of which reach `updateChat(addMissing)`, `ChatModel.kt:495-501`).
- `businessChat(biz)` creates a business address on `biz`, makes alice join it, and waits for the business chat in the model.
- `CliPeer.createAddress(business)`: `/ad`, the link line, then `/auto_accept on` or `/auto_accept on business`.
- `CliPeer.createGroupLink(group)`: `/g`, then `/create link #g`, and returns the link line.

CLI lines come from `src/View.hs`: `connReqContact_` :1139-1150 (the short link sits on the third line and the "old clients" line only appears with full links), `viewAddressSettings` :1188-1198 (`auto_accept on` and `auto_accept on, business`), `groupLink_` :1200-1212, `viewAcceptingBusinessRequest` :1261, and member joined :1368. Haskell uses them in `H/Utils.hs:609-619` and `:640-649`, `H/Profiles.hs:1650-1651` and `H/Profiles.hs:1953-1967` (`testBusinessAddress`).

## A fact the filter tests are built around

`filteredChats` always keeps the open chat (`CLV:1558,1561`, `chat.id == chatModel.chatId.value`). The only exception is a list filtered by a pasted link (`CLV:1552-1554`). Desktop has no way to close a chat. On Android the list is not visible while a chat is open. So every test sets its chats up without opening any of them: contacts come from `connect`, groups from `hostGroup` (an invitation row already counts as a group, `CLV:1594-1597`), and aliases come from the pending-connection menu. The one exception is `presetFilterReports`, which needs alice to own the group: `ownGroup` opens it, and the test makes that group the expected row.

## Per test

### `newChatButtonOpensNewChatSheet` (catalogue :61)
- **Asserts:** with the default `OneHandUI = true`, the toolbar button exists and the FAB does not. The button's description is "Start new chat" (`S:795`), and clicking it opens the sheet (`chatModel.newChatSheetVisible`, "New chat" `S:945`, `Ui.NewChat.CreateLink`). `escape()` closes the sheet. With `OneHandUI = false`, the FAB is there and the toolbar button is not; the FAB opens the sheet too.
- **Code:** `CLV:105-130` (`showNewChatSheet`), the FAB at :262-267 and :482-505, the toolbar button at :585-605 (only in the `else` branch of `if (stopped)`, :545,564), and the titles at `NewChatSheet.kt:79,360`.
- **Platforms:** shared. The pref is set directly with `appPrefs.oneHandUI.set(false)`, which is setup in the same style as `TestPrefs`; prefs are restored for each test (`UiTestApp.resetForTest`, `PrefStores.restore()`).
- **Not covered:** the "chat stopped, neither button" clause. Stopping the chat needs Settings › Database keys that this batch does not have.

### `searchFiltersByAlias` (:62)
- **Setup:** a one-time link is kept as a pending connection, named "friend", and then bob connects through it. The contact inherits the alias: `createDirectContact` copies the connection's `localAlias`, `src/Store/Direct.hs:454-457`, and Haskell checks the same thing in `testSetConnectionAlias`, `H/Profiles.hs:3247-3263`. The pending row is removed when the contact connects (`SimpleXAPI.kt:2931-2935`).
- **Asserts:** searching "frie" shows `friend` and hides "Private notes". "frix" hides `friend`. bob's display name contains neither string, so only the alias can match (`Contact.anyNameContains`, `ChatModel.kt:1921-1924`).
- **Platforms:** shared. No chat is opened.

### `searchMatchesPendingConnectionAlias` (:62)
- **Asserts:** the pending connection named "dentist" is found by "dent", and the notes row is hidden. Pending connections are matched only by `localAlias` (`CLV:1566`).
- **Platforms:** shared. No peer is needed, although the row's Needs say P.

### `searchNoChatsFound` (:62)
- **Asserts:** "qq" (too short to be a name candidate, `CLV:1145,1164`) hides bob and the notes row, and "No chats found" appears (`S:508`, `CLV:1118`, `NoChatsView` :1268-1270).

### `searchClearsWhenFilterChanges` (:62)
- **Asserts:** with "bo" typed into the search, clicking the "Contacts" chip clears the field (`LaunchedEffect(activeFilter)`, `CLV:1136-1138`).
- **Note:** the unread toggle is not composed while the search has text (`CLV:819-827`). That is why the filter is changed through a preset chip, which stays visible during a text search (`TagsOrConnectByName`, :1197-1208).

### `searchNameShowsConnectToNameRow` (:64)
- **Asserts:** "bobby" shows "Connect to bobby.testing", "@bob.simplex" shows "Connect to @bob.simplex", and "bob" shows no row (`S:17`, `nameSearchCandidate` `CLV:1156-1167`, `MIN_NAME_LENGTH = 5` at :1145, and the `.testing` suffix at :1141).
- **Code:** the candidate is set before the debounced local search runs (`CLV:850-869`). With `PRMNever`, the core searches only the local store (`src/Library/Commands.hs:4502-4503`), and the UI shows no alert when that search finds nothing (`SimpleXAPI.kt:1652-1660`). Both lookups miss, so the row stays.
- **Platforms:** shared. On desktop the row is shown next to the tags; on Android it replaces them (`CLV:1197-1208`).

### `searchPasteLinkTriggersConnectPlan` (:63), desktop only
- **Setup:** bob's address is created on the CLI and is unknown to alice.
- **Asserts:**
  - The search field becomes disabled and shows "SimpleX contact address (via …" (`CLV:808,842-849`; `Format.SimplexLink.simplexLinkText`, `ChatModel.kt:5157-5160`; strings at `S:109,114`).
  - The open-chat alert shows the name "bob" and "Open new chat" (`ConnectPlan.kt:155-167,700-747`; `AlertManager.kt:291-430`, keyed `PrivacySensitive`; `S:18`).
  - "Cancel" clears the field and makes it editable again (cleanup at `CLV:891-903`, then the flow's else branch at :864-875).
- **Why desktop only:** see "Link entry" below.

### `searchPasteKnownContactLinkFiltersToContact` (:63), desktop only
- **Setup:** alice joins bob's auto-accepting address. Pasting the address afterwards resolves to `ContactAddressPlan.Known` (core: `contactRequestPlan`, `src/Library/Commands.hs:4724-4747`, via the request hash; Haskell `H/Profiles.hs:1702-1704`).
- **Asserts:** the app filters to that contact (`ConnectPlan.kt:216-229`, `filterKnownContact`), so bob is listed, the notes row is hidden, and no alert appears.

### `searchPasteKnownGroupLinkFiltersToGroup` (:63), desktop only
- **Setup:** alice joins bob's group link and the test waits until her membership is active. Pasting the link afterwards resolves to `GroupLinkPlan.Known` (`groupPlan`, `src/Library/Commands.hs:4767-4775`).
- **Asserts:** `filterKnownGroup` (`ConnectPlan.kt:322-335`) leaves only "team", the field shows "SimpleX group link (via …" (`S:111`), and no alert appears.

### `unreadFilterToggle` (:66)
- **Setup:** cath is muted first. Then cath and bob each send a message, and the test waits until both rows show "1" with the filter off.
- **Asserts:** with the filter on, only bob is listed. Muted cath and the notes row are hidden (`Chat.unreadTag`, `ChatModel.kt:1460-1464`). Turning the filter off brings them back.
- **Not covered:** the "Mark unread" and mentions-only cases.

### `unreadFilterEmptyState` (:66)
- **Asserts:** with nothing unread, turning the filter on shows "No unread chats" (`S:506`, `CLV:1248-1262`) and no rows. Tapping that text clears the filter.

### `presetFilterFavorites` / `Contacts` / `Groups` / `Businesses` / `Notes` / `Reports` (:67)

Each test has a chat of another kind that has to disappear. The matching rules are in `presetTagMatchesChat`, `CLV:1583-1610`, and the labels in `S:531-537`.

| Test | Shown | Hidden | Tag layout |
|---|---|---|---|
| Favorites | bob (starred through the row menu) | cath, notes | 3 collapsible kinds, so expanded chips |
| Contacts | bob | team (group invitation from bob), notes | 3, so expanded |
| Groups | team | biz (business chat), bob, notes | 4 collapsible kinds, so the collapsed menu (`CLV:1297-1307`); this also exercises `PresetMenu` |
| Businesses | biz | notes | 2, so expanded |
| Notes | Private notes | bob | 2, so expanded |
| Reports | team | bob, cath, notes | Reports chip next to the collapsed menu (Reports cannot be collapsed, :1623-1626) |

**Business chat:** alice becomes the customer of `biz`'s business address, so her chat has `businessChat.chatType == Business` and counts as a Business. The owner's side would count as a Customer chat, which goes under Contacts (:1588).

**Reports setup** follows `testGroupMemberReports` (`H/Groups.hs:8038-8090`):
1. alice owns "team" through `ownGroup`, with bob and cath as members.
2. The test waits for "new member cath is connected" and "member bob is connected".
3. cath sends a message, and bob runs `/report #team content bad joke`. The parser is at `src/Library/Commands.hs:6182`, the lookup by text at :794-797 and `Store/Messages.hs:3146-3164`, and the echo is `#team (support) > cath bad joke`.
4. alice's report counter goes up when the report arrives (`SimpleXAPI.kt:3030-3033`, `ChatModel.kt:1081-1103`).
5. `ownGroup` leaves "team" open. `backToChatList()` handles Android. On desktop "team" stays open, and it is the expected row anyway.

**Platforms:** all shared.

## Platform notes and uncertainties

1. **Link entry (the 3 paste tests).**
   - The search field has no paste code of its own. The app reacts to any text change (`CLV:838-849`), and a real paste goes through Compose's text field.
   - In Compose 1.8.2, `CoreTextField` and `TextFieldSelectionManager` read `LocalClipboard`. I checked the strings in `foundation-desktop-1.8.2.jar`. `TestRoot` provides only `LocalClipboardManager` (`desktopTest/.../TestRoot.kt`), so a Ctrl+V from `TestClipboard` cannot reach this field. The plan (line 459) says `TestRoot` should provide both.
   - So the tests type the link text. They cannot use `typeInto`, because it polls for the typed text and the app at once replaces the link with its description (`CLV:844`). That makes the poll time out.
   - The tests therefore call `performTextInput` on the desktop `ui` (private `enterLinkInSearch`) and live in `desktop/`.
   - To share them with Android, both `UiScope`s need an `enterText(key, text)` that does not check the field afterwards. That is a harness change outside this folder's fence.
2. **Short links on a local server.** The tests assume that the link printed by the CLI for the local test SMP server parses as a SimplexLink in the app's markdown parser (`strConnectTarget`, `NewChatView.kt:842-856`). If it does not, the field never turns read-only and the three paste tests fail at their first poll.
3. **`joinLink` uses plain `APIConnect`.** The GUI's own flow for links with profile data prepares a chat first (`showPrepareContactAlert`). The CLI's `/c` (the same command) works for contact addresses, group links and business addresses in Haskell. That holds for full links. I have not confirmed it for short links in a GUI-originated `APIConnect`.
4. **Business chat name.** The tests expect alice's business chat to be named after the business profile ("biz"). On biz's side the member name may get a `_1` suffix; the regex accepts both.
5. **`presetFilterReports`.** It assumes the report from bob (a member) reaches alice (the owner) as a new item with `isActiveReport`. Haskell shows the same delivery to the owner.
6. **Android preset chips** sit in a horizontally scrolling `Row` (`ChatListView.android.kt:33-43`). On a narrow screen, three chips plus "Add list" may run past the right edge. The Android `click` may need a horizontal scroll-into-view.
7. **`newChatButtonOpensNewChatSheet`** writes the pref from the test thread. `SharedPreference.set` updates the Compose state that `CLV:251,529` reads.
8. **`App.newChat()`** now finds only the toolbar button. A future test that turns off `OneHandUI` and then calls `App.newChat()` must click `NewChatFab` instead.

## Catalogue corrections

1. **`01-…md:63` (Paste link).**
   - Verification cell. Problem: "Field shows the short link text" is wrong. The field shows the link's description, `simplexLinkText` (`ChatListView.kt:838-845` in ref, `ChatModel.kt:5157-5160`). Replacement:
     > Field shows the link's description, e.g. "SimpleX contact address (via <host>)" or "SimpleX group link (via <host>)", and becomes read-only (disabled). A known contact/group filters the list to that chat with no alert; an unknown link with profile data shows the open-chat alert (profile name, "Open new chat", "Cancel"), and "Cancel" clears the field (§8)
   - Needs cell. Problem: `CB` does not apply. Paste into this field is Compose's own text-field paste, which reads `LocalClipboard` in Compose 1.8.2, and `TestRoot` provides only `LocalClipboardManager`. The tests type the link instead. Replacement: `P`.
   - Notes cell. Replacement:
     > The app reacts to any text change (`ChatListView.kt:836-845`), so the tests enter the link text directly; desktop-only until the harness has a text entry without the echo check of `typeInto`
2. **`01-…md:62` (Text search), Verification cell.** Problem: incomplete. It does not say that the unread toggle is hidden while text is entered, or that the open chat is always listed. Replacement:
   > Filters on `anyNameContains` and the pending-connection alias (a pending connection matches only its alias, `ChatListView.kt:1562`). Empty result shows "No chats found". Changing filter clears the text (`ChatListView.kt:1132`); while text is entered the unread toggle is hidden (:819-825), so the filter is changed with a preset or list chip. The open chat is always listed whatever the search (:1554,1557). Haskell: `testFilterSearch` (ChatList.hs)
3. **`01-…md:67` (Preset tags), Verification cell.** Problem: the condition for showing presets is wrong. They show only when at least two collapsible kinds have chats (`collapsiblePresetTags.size > 1`, :1293). The ">3" threshold counts collapsible kinds, Reports and user lists together. "Groups" also excludes channels. Replace the first sentence with:
   > Presets show only when at least two kinds other than Reports have chats (`ChatListView.kt:1293`); they are chips when those kinds plus Reports plus user lists number at most 3, otherwise a menu (:1294-1303), with Reports always a chip (:1619-1622)

   Replace the "Groups" clause with:
   > "Groups" a group, not a business chat or channel
4. **`01-…md:61` (New chat), Verification cell.** Problem: the reference ":545,571" points at the `if (stopped)` line and an empty line. The toolbar button is added only in the `else` branch. Replacement for the parenthetical:
   > (the FAB needs `chatRunning == true`, :262, and the bar button is added only in the `else` branch of `if (stopped)`, :545,560,569,585-605)

## Rows skipped

None of the 17 assigned names is skipped. Parts of rows that are not covered:
- `newChatButtonOpensNewChatSheet`: the "chat stopped" clause (no Database/stop-chat keys in this batch).
- `unreadFilterToggle`: the "Mark unread" and mentions-only cases.

`presetFilterChannels` (:68, Needs RELAY) and `searchKnownNameFiltersChat` (:65, NAMES) were not in this batch.
