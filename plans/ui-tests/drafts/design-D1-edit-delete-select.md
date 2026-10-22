# Design D1: edit / history, delete / reveal, multi-select (catalogue 02 §I, §J, §K, §P history)

Status: design only (2026-10-20). Nothing is implemented. All file:line references were read in `~/wt-uitest` (branch `nd/desktop-ui-tests`). Paths are relative to `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/` unless they start with `apps/`, `src/`, `tests/` or `plans/`. Aliases: **CIV** `views/chat/item/ChatItemView.kt`, **CV** `views/chat/ChatView.kt`, **CMP** `views/chat/ComposeView.kt`, **SEL** `views/chat/SelectableChatItemToolbars.kt`, **INFO** `views/chat/ChatItemInfoView.kt`, **MDV** `views/chat/item/MarkedDeletedItemView.kt`, **API** `model/SimpleXAPI.kt`, **PTF** `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/platform/PlatformTextField.desktop.kt`, **Screens** `apps/multiplatform/common/src/uiTestShared/kotlin/chat/simplex/app/ui/harness/Screens.kt`.

English strings are in `apps/multiplatform/common/src/commonMain/moko-resources/base/strings.xml` (the brief's `resources/MR/base/strings.xml` does not exist).

## 0. Summary

29 catalogue rows (I 12, J 8, K 9) plus P `info_history_listsVersions` collapse into **10 tests** (8 shared, 2 desktop-only), plus 2 optional changes that make existing tests stronger. Already done (`status.py`): `edit_menu_updatesPeer`, `edit_received_updatesBubble`, `delete_ownForEveryone_peerSeesMarkedDeleted`, `delete_received_onlyForMe`.

| # | Test (catalogue name) | Class / file | Rows covered | Peers | Pri |
|---|---|---|---|---|---|
| T1 | `edit_menu_updatesPeer` (existing, extended) | `chat/EditMessageTest` (moved from `MessagingTest`, decision Q1) | I `edit_menu_updatesPeer`, `edit_unchanged_noUpdate`, `edit_cancel_clearsCompose`, `edit_metaShowsEditedIcon` (model only), P `info_history_listsVersions` | bob | P1 |
| T2 | `edit_notOffered_forReceivedOrForwarded` (renamed from `…ForReceivedVoiceForwarded`) | `chat/EditMessageTest` | I rules row (voice part moved to §E) | bob | P1 |
| T3 | `edit_upArrow_editsLastMessage` | `desktop/EditMessageDesktopTest` | I keyboard row | bob | P1 |
| T4 | `edit_ownFile_previewWithoutCancel` | `desktop/EditMessageDesktopTest` | I own-caption row (file variant) | bob, XFTP | P2 |
| T5 | `edit_group_membersSeeEdited` | `chat/EditMessageTest` | I group row | bob, cath | P2 |
| T6 | `delete_forEveryone_fullDeletion_peerSeesDeleted` | `chat/DeleteMessageTest` | J `delete_ownForMe_localOnly`, `delete_forEveryone_fullDeletion_peerSeesDeleted`; K full-delete text of the selection alert | bob | P1 |
| T7 | `markedDeleted_received_revealHide` | `chat/DeleteMessageTest` | J `markedDeleted_received_revealHide`, `markedDeleted_consecutive_collapsedCount`, `delete_received_fullDelete_removed`; K `select_mergedEvents_selectsWholeRange` (on a deleted-items range) | bob | P1 |
| T8 | `select_toggle_updatesCounter` | `chat/MultiSelectTest` (new) | K `select_enterMode_showsCounterToolbar`, `select_toggle_updatesCounter`, `select_close_exitsMode` | bob | P1 |
| T9 | `selectDelete_forEveryone_peerSeesBoth` | `chat/MultiSelectTest` | K `selectDelete_forEveryone_peerSeesBoth`, `selectDelete_mixed_onlyForMe` | bob | P1 |
| T10 | `selectForward_multiple_peerReceives` | `chat/MultiSelectTest` | K `selectForward_multiple_peerReceives`, `select_itemDeletedByPeer_recheck` | bob, cath | P1 |

Dropped: `edit_quotedReply_keepsQuote`, `edit_imageCaption_updates`, `edit_ownImage_previewWithoutCancel`, `blockedByAdmin_items_showBlocked`, `select_limits_forward20_delete200`, plus the voice branch of the edit rule and the edit-window branch (section 5).

App changes are keys only, and **Ui.kt needs no new entries**: 6 call sites reuse `Ui.Chat.ItemMenu`, `Ui.Chat.Selection.part(...)` and `Ui.Alert.Option` (section 3).

Class placement: the class map (`00-legend.md:150-152`) sends S rows to `EditMessageCompatTest` / `DeleteMessageCompatTest`. The stable-peer run is not wired (legend line 42), and the guide §5 forbids the `Compat` name unless the test really runs against the stable peer. Existing S tests (`edit_menu_updatesPeer`, `delete_ownForEveryone_peerSeesMarkedDeleted`) therefore live in plain classes, and so do these.

## 1. Decisions to confirm (with recommendation)

- **Q1. Extend `edit_menu_updatesPeer` in place or move it.** T1 starts with the existing test's steps (send "hello", Edit, "hey 👋"), so guide §5 says extend it, not add a new test. Options: (a) move it from `MessagingTest.kt:35-44` into `EditMessageTest` (its class-map class) and extend it there; (b) extend it in `MessagingTest`. **Recommend (a)**: it changes one existing test, so it needs your OK (memory: no scope expansion without permission).
- **Q2. How alice allows full deletion in T7.** (a) A `ViaApi` precondition helper, `allowFullDeletionViaApi("bob")` (`apiSetContactPrefs`, as `HiddenProfilesTest.newGroupViaApi` does); (b) through the Contact preferences UI, which needs keys in `views/chat/ContactPreferences.kt` (owned by catalogue 01 §14). **Recommend (a)**: the preference UI is not what T7 tests, and guide §6 allows API setup for preconditions outside the feature when the helper name says so. Alternative: split that step into its own test (+1 reset).
- **Q3. Selection-toolbar keys.** (a) `Ui.Chat.Selection.part("delete")` / `.part("forward")`, no Ui.kt edit (same pattern as `Ui.Chat.Item(id).part("quote")` at `views/chat/item/FramedItemView.kt:297`); (b) new named `Ui.Chat.SelectionDelete` / `SelectionForward`. **Recommend (a).**
- **Q4. Two controls found by content description instead of a key**: the edit bar's (x), `described("Cancel")` (`views/chat/ContextItemView.kt:148-151`), and the selection bar's (x), `described("Back")` (`DefaultTopAppBar.kt:140-146` via SEL:31). Both are unique on screen in the states used, so no app change is needed. Fallback if either turns out ambiguous: a `modifier` parameter on `NavigationButtonClose`, which is a shared helper, so that would be a larger diff. **Recommend described().**
- **Q5. T3 and T4 are desktop-only.** The Up arrow exists only on desktop (PTF:141). T4 attaches the file by pasting its path (desktop route, as `FilesDesktopTest.paste_filePathText_attachesFile`), not through the FD dialog the catalogue names. **Recommend yes.** A shared T4 needs FD support on both platforms.
- **Q6. T5 (group edit) is P2.** It costs 2 peers plus group setup. It catches a group-only regression (`type = cInfo.chatType` in CMP:812-818). Keep or drop?
- **Q7. Optional changes that make existing tests stronger** (section 4.11). Each one changes an existing test, so each needs your OK.

## 2. Row-by-row verdict (code verified)

### §I Edit and history (catalogue lines 138-144)

| Row / test | Code (verified, corrected) | Verdict |
|---|---|---|
| `edit_menu_updatesPeer` | Edit item CIV:468-473 (catalogue says 464-469); `updateMessage` CMP:808-827, `UpdatedMessage(updateMsgContent(...))` CMP:818; edit-bar cancel → `clearState()` CMP:1265-1267 | **keep, extend (T1)** |
| `edit_unchanged_noUpdate` | `apiUpdateChatItem` accepts `ChatItemNotChanged` (API:1336); core `viewItemNotChanged` prints only on the sender (`src/Simplex/Chat/View.hs:917-920`) | **merge into T1** (bob's next line proves nothing was sent) |
| `edit_cancel_clearsCompose` | CMP:1265-1267; the edit bar's (x) has content description "Cancel" (`ContextItemView.kt:148-151`) | **merge into T1** |
| `edit_metaShowsEditedIcon` | CIMetaView.kt:83-84 → `StatusIconText` :198-199, `Icon(icon, null, …)`: **no content description**, so the icon is not observable (guide §2: don't name a test after an unobservable visual) | **merge into T1** as a model check (`meta.itemEdited`); the visible proof is the 2-version history |
| `edit_upArrow_editsLastMessage` | PTF:141-142 (catalogue says :138): Up only when the field is empty; `editPrevMessage` CMP:1187-1192 (catalogue says :1185): only with no context item and no preview, `findLast { it.meta.editable }` | **keep (T3, desktop)**, both branches |
| `edit_received_updatesBubble` | done (`EditMessageTest.kt:9-19`) | done; optional strengthening (4.11) |
| `edit_quotedReply_keepsQuote` | The quote is stored by the core; the app sends only `msgContent` (CMP:818) and upserts what comes back (API:3090) | **drop**: no app-side mutation breaks only this; covered by `testDirectMessageUpdate` (`tests/ChatTests/Direct.hs:704-714`) |
| `edit_imageCaption_updates` | Same receive path as text, `chatItemUpdateNotify` (API:3090-3091) | **drop**: duplicates `edit_received_updatesBubble` and adds XFTP cost |
| `edit_ownImage_previewWithoutCancel` | `MediaPreview` `cancelEnabled = !editing && !inProgress` CMP:1210-1212 | **drop for now** (needs image attach on both platforms; the file variant T4 covers the same rule and `updateMsgContent`) |
| `edit_ownFile_previewWithoutCancel` | `FilePreview` CMP:1220-1225, `ComposeFileView.kt:37-46` (button only if `cancelEnabled`); `updateMsgContent` MCFile CMP:779; preview from `chatItemPreview` CMP:275 | **keep (T4, desktop paste route)** |
| `edit_notOffered_forReceivedVoiceForwarded` | CIV:468 `meta.editable && msgContent !is MCVoice && !live`; core `editable = deletable && isNothing itemForwarded`, `deletable` only for `CISndMsgContent` younger than `nominalDay` (`src/Simplex/Chat/Messages.hs:538-550`) | **keep (T2) as `edit_notOffered_forReceivedOrForwarded`**; voice needs REC → move to §E; the 1-day window can't be reached in a UI test |
| `edit_group_membersSeeEdited` | same CMP path with `type`/`scope` (CMP:812-818); CLI from `testGroupMessageUpdate` (`tests/ChatTests/Groups.hs:1307-1313`) | **keep (T5, P2)** |

### §J Delete / reveal / blocked (lines 149-151)

| Row / test | Code | Verdict |
|---|---|---|
| `delete_ownForMe_localOnly` | alert CIV:1399-1433 (catalogue says 1390-1424); "Delete for me" → `cidmInternal` CIV:1418 | **merge into T6** |
| `delete_ownForEveryone_peerSeesMarkedDeleted` | done (`MessagingTest.kt:46-55`) | done; T6/T9 check the alert's mark-deleted text and the sent-side "marked deleted" display |
| `delete_forEveryone_fullDeletion_peerSeesDeleted` | question text CIV:346-352 (`!sent \|\| fullDeleteAllowed \|\| Local`); `fullDeleteAllowed` CIV:125; local removal CV:3417-3421 | **keep (T6)** |
| `delete_received_onlyForMe` | done (`DeleteMessageTest.kt:9-22`) | done |
| `markedDeleted_received_revealHide` | render branch CIV:702-704 → `MarkedDeletedItemDropdownMenu` CIV:561-573 (Reveal :563-565); revealed → `MsgContentItemDropdownMenu` Hide CIV:484-486; Reveal/Hide actions CIV:958-980 (catalogue: 950-972); header "marked deleted" when revealed `FramedItemView.kt:286-287` | **keep (T7)** |
| `delete_received_fullDelete_removed` | API:3131-3135 remove vs upsert | **merge into T7** |
| `blockedByAdmin_items_showBlocked` | MDV:73-74 / :107-108 | **drop**: the single-item label is already asserted by `GroupLinksTest.memberInfo_blockForAllNotifiesMembers` (`groups/GroupLinksTest.kt:91-93`); the count branch could be one more step in that test (owner 03 §14) |
| `markedDeleted_consecutive_collapsedCount` | MDV:48-81 (count :69-78, catalogue says 68-110); grouping `ChatItemsMerger.kt:46-48`, the newest item is rendered (CV:2402) | **merge into T7** (direct chat, no group needed) |

### §K Multi-select (lines 156-159)

| Row / test | Code | Verdict |
|---|---|---|
| `select_enterMode_showsCounterToolbar` / `select_toggle_updatesCounter` / `select_close_exitsMode` | counter SEL:25-51 (text :34-38, close :31, key `Ui.Chat.Selection` :48 → only `.part("title")`); toggle overlay CV:2238-2243; range logic CV:3345-3381 (catalogue says 3342); toolbar swap CV:1172-1178 | **merge into T8**. Correction: "the compose field is back" can't be asserted: in selection mode SEL:82 composes a hidden measuring `ComposeView` that also carries `Ui.Chat.Message`/`Ui.Chat.Send`; assert the buttons toolbar (`Selection.part("delete")`) is gone instead |
| `selectDelete_forEveryone_peerSeesBoth` / `selectDelete_mixed_onlyForMe` | delete button SEL:97-104; `rDeleteForEveryoneEnabled` SEL:159 (catalogue says 157); question CV:330-335; dialog CIV:1435-1460 (`if (forAll)` :1451); mode CV:3399 | **merge into T9** |
| `selectForward_multiple_peerReceives` | CV:363-389 (catalogue says 361-384), `forwardContent` CV:3681-3687 (chronological order) | **keep (T10)** |
| `select_itemDeletedByPeer_recheck` | `LaunchedEffect(chatInfo, chatItems.value, selected)` SEL:127-129; forward rule SEL:163 (catalogue says 161) | **merge into T10** |
| `select_mergedEvents_selectsWholeRange` | CV:3353-3366 (range via `getPrevShownChatItem`, the same for every merge category) | **merge into T7** using the collapsed "2 messages marked deleted" range (no group setup); the group-events variant stays with 03 §16 if wanted |
| `select_limits_forward20_delete200` | SEL:145-146 | **drop**: needs 201 clicks (no "select all" in the main chat: `selectAll` is passed only in secondary contexts) and 300 messages; low value for the cost |

### §P (line 207)

`info_history_listsVersions`: INFO:319-343 (list :329-330, newest first, "(current)" on the first one INFO:105-110; catalogue says 103-108). An unedited item has one version (`src/Simplex/Chat/Library/Commands.hs` `APIGetChatItemInfo`: `if null versions then maybeToList $ mkItemVersion ci`). One tab only for a direct sent item (`numTabs` INFO:467-478), so the history is shown directly. **Merge into T1.**

## 3. App keys (no Ui.kt change)

| Key | Where (exact) | Change | Used by |
|---|---|---|---|
| `Ui.Chat.ItemMenu(cItem.id, "info")` | CIV:861-867, `ItemInfoAction` body | add `uiKey = Ui.Chat.ItemMenu(cItem.id, "info")` to its `ItemAction(...)` call (signature unchanged; all 6 callers get it) | T1 |
| `Ui.Chat.ItemMenu(cItem.id, "reveal")` | CIV:959 `RevealItemAction` + call CIV:564 | new param `uiKey: UiKey? = null` **before** `reveal`; pass it to `ItemAction(..., uiKey = uiKey, onClick = …)`; at :564 `RevealItemAction(revealed, showMenu, Ui.Chat.ItemMenu(cItem.id, "reveal"), reveal)`. Other callers (:512, :665) unchanged (not acted on) | T7 |
| `Ui.Chat.ItemMenu(cItem.id, "hide")` | CIV:971 `HideItemAction` + call CIV:485 | same pattern; at :485 `HideItemAction(revealed, showMenu, Ui.Chat.ItemMenu(cItem.id, "hide"), reveal)`; :510, :663 unchanged | T7 |
| `Ui.Alert.Option(Ui.AlertSource.Shared, "forMe")` / `"forEveryone"` | CIV:1446-1449 and :1452-1455 (`deleteMessagesAlertDialog` TextButtons) | add `Modifier.ui(...)` as the second argument, exactly as the single-item dialog does at CIV:1421/:1428 | T6, T9 |
| `Ui.Chat.Selection.part("delete")` | SEL:97 `IconButton({…}, enabled = …)` | `IconButton({…}, Modifier.ui(Ui.Chat.Selection.part("delete")), enabled = …)` | T6, T8, T9 |
| `Ui.Chat.Selection.part("forward")` | SEL:115 | `IconButton({ forwardItems() }, Modifier.ui(Ui.Chat.Selection.part("forward")), enabled = …)` | T10 |

Reused unchanged: `Ui.Chat.Item(id)`, `Ui.Chat.ItemMenu(id, "edit"/"delete"/"select"/"forward")`, `Ui.Chat.Message`, `Ui.Chat.Send`, `Ui.Chat.Bar.part("title")`, `Ui.Chat.Selection.part("title")`, `Ui.Share.Row`, `Ui.Alert.Option(Shared, "forMe"/"forEveryone")` (CIV:1421, 1428). Controls found by description: `described("Cancel")` (edit bar), `described("Back")` (selection close). The Reveal/Hide keys use a new `uiKey` parameter on private functions in one file, so there is no cross-module caller risk; both platforms still need building (guide §4).

## 4. Tests

Common constants (test-file private):
```kotlin
private val FOR_ME = Ui.Alert.Option(Ui.AlertSource.Shared, "forMe")
private val FOR_EVERYONE = Ui.Alert.Option(Ui.AlertSource.Shared, "forEveryone")
private val SEL_TITLE = Ui.Chat.Selection.part("title")
private val SEL_DELETE = Ui.Chat.Selection.part("delete")
private val SEL_FORWARD = Ui.Chat.Selection.part("forward")
private const val MARK_ONE = "Message will be marked for deletion. The recipient(s) will be able to reveal this message."   // strings.xml:445
private const val FULL_ONE = "Message will be deleted - this cannot be undone!"                                          // :443
private const val MARK_MANY = "Messages will be marked for deletion. The recipient(s) will be able to reveal these messages." // :446
private const val FULL_MANY = "Messages will be deleted - this cannot be undone!"                                        // :444
```

### 4.0 New screen helpers (add to `Screens.kt`, class `ChatScreen` at :207, plus a new `ItemInfoScreen`)

```kotlin
// ChatScreen
fun select(text: String) { menu(text, "select"); s.waitFor(Ui.Chat.Selection.part("title")) }
fun toggle(id: Long) = s.click(Ui.Chat.Item(id))                         // selection overlay CV:2238-2243 covers the item
fun selectionTitle(): String? = Ui.Chat.Selection.part("title").let { k -> if (s.exists(k)) s.textsOf(k).firstOrNull() else null }
fun forwardSelectionTo(chatName: String) {
  s.click(Ui.Chat.Selection.part("forward"))
  ChatListScreen(s).shareTo(chatName)
  s.poll(what = "the send button to be enabled for forwarding") { s.isEnabled(Ui.Chat.Send) }
  s.click(Ui.Chat.Send)
}
fun itemInfo(text: String): ItemInfoScreen {
  menu(text, "info")
  s.poll(what = "the item info with `Sent message` and `History`") { s.seesText("Sent message") && s.seesText("History") }
  return ItemInfoScreen(s)
}

class ItemInfoScreen(private val s: UiScope) {
  fun history(): List<String> = s.texts().dropWhile { it != "History" }.drop(1)
  fun close() {
    s.hideKeyboard(); s.escape()
    s.poll(what = "the item info to close") { !ModalManager.end.hasModalsOpen() }
  }
}
```
`history()` relies on the info pane being later in the semantics tree than the chat. On desktop `EndPartOfScreen` follows `CenterPartOfScreen` (`App.kt:443-452`), so chat bubbles with the same text come before "History". The "History" heading (INFO:328) appears only in the info pane. Android: the modal is drawn after the chat; check on the first run.

Desktop-only helper (private in `EditMessageDesktopTest`, mirrors desktop `UiTest.pressEnter` at `apps/multiplatform/common/src/desktopTest/kotlin/chat/simplex/app/ui/harness/UiTest.kt:293-298`):
```kotlin
private fun UiScope.pressUp(key: UiKey) {
  val node = ui.onNode(matcher(key))
  ui.runOnUiThread { node.performKeyInput { pressKey(Key.DirectionUp) } }
  ui.waitForIdle()
}
```

Precondition helper (decision Q2):
```kotlin
private fun allowFullDeletionViaApi(contactName: String) {
  val ct = (chatModel.chats.value.first { it.chatInfo.chatViewName == contactName }.chatInfo as ChatInfo.Direct).contact
  val prefs = contactFeaturesAllowedToPrefs(contactUserPrefsToFeaturesAllowed(ct.mergedPreferences).copy(fullDelete = ContactFeatureAllowed.Always))  // API:6373, 6398, 6324
  runBlocking { chatController.apiSetContactPrefs(null, ct.contactId, prefs) } ?: error("apiSetContactPrefs($contactName) failed")  // API:2021
}
```
(The model is not updated. T7 tests received deletions, which the core decides.)

### 4.1 T1 `edit_menu_updatesPeer` (extended)

- **Scenario**: alice opens Edit on her message and cancels; opens it again and submits it unchanged; then edits it. bob sees exactly one edit. The item info shows both versions, newest first and marked "(current)".
- **Fails if**:
  - the edit isn't sent with the new text: CMP:818 `updateMsgContent(oldMsgContent)` → `oldMsgContent`;
  - Cancel leaves the edit: delete `clearState()` at CMP:1266;
  - the field isn't prefilled: CMP:128 `else -> editingItem.content.text` → `""`;
  - the history is wrong: INFO:329-330 show only `versions.first()`, or `current = i == 0` → `i == versions.lastIndex`.
- **Setup**: `app("alice")`, `cli("bob")`, `connect(bob)` (`Helpers.kt:26`).
- **CLI** (from `testDirectMessageUpdate`, Direct.hs:672-674, and `View.hs:3079-3080`): `bob expectsMessage "alice> hello"`, then **as the very next line** `bob expectsMessage "alice> [edited] hey 👋"`.
- **Steps / assertions**:
```kotlin
val chat = alice.chatList.open("bob")
chat.send("hello"); bob expectsMessage "alice> hello"
val id = chat.itemId("hello")
chat.itemInfo("hello").run {
  val h = history()
  check(h.count { it == "(current)" } == 1 && h.indexOfFirst { it.startsWith("hello") } in 0 until h.indexOf("(current)") && h.none { "hey" in it }) { "unedited history: $h" }
  close()
}
waitFor(Ui.Chat.Message); assertFalse("edit bar before Edit", exists(described("Cancel")))
chat.menu("hello", "edit")
poll(what = "the edit bar with `hello` in the message field") { editableText(Ui.Chat.Message) == "hello" && exists(described("Cancel")) }
hideKeyboard(); click(described("Cancel"))
poll(what = "the edit bar gone and the field empty") { !exists(described("Cancel")) && editableText(Ui.Chat.Message).isNullOrEmpty() }
chat.menu("hello", "edit")
poll(what = "`hello` in the message field") { editableText(Ui.Chat.Message) == "hello" }
chat.replaceAndSend("hello")
chat.menu("hello", "edit")
chat.replaceAndSend("hey 👋")
bob expectsMessage "alice> [edited] hey 👋"
poll(what = "the same bubble showing `hey 👋`, not `hello`, edited in the model") {
  chat.item(id)?.let { it.content.text == "hey 👋" && it.meta.itemEdited } == true &&
    textsOf(Ui.Chat.Item(id)).let { t -> t.any { "hey 👋" in it } && t.none { "hello" in it } }
}
chat.itemInfo("hey 👋").run {
  val h = history()
  val newest = h.indexOfFirst { it.startsWith("hey 👋") }; val cur = h.indexOf("(current)"); val old = h.indexOfFirst { it.startsWith("hello") }
  check(newest in 0 until cur && cur < old && h.count { it == "(current)" } == 1) { "edited history: $h" }
  close()
}
```
  - Opposite first: no edit bar before Edit; one version before the edit; "hello" in the bubble before.
  - Negative control: the unchanged submit and the cancel produce no peer line (`expectsMessage` reads the next line).
- **Strings**: "Sent message" (strings.xml:421), "History" (:423), "(current)" (:2016), "Cancel" (:801).
- **Keys**: new `ItemMenu(id,"info")`; existing `ItemMenu(id,"edit")` (CIV:469).
- **Platform**: Android long press via `menu`; `hideKeyboard()` before tapping (x) and before `escape()` in `close()` (guide §3). The info opens in the end pane (desktop) or full screen (Android).
- **Flakiness**: `itemInfo` loads asynchronously (CV:716-760), and the poll waits for it. `escape()` closes the modal through its BackHandler (same as `GroupLinksTest.kt:83-86`).

### 4.2 T2 `edit_notOffered_forReceivedOrForwarded`

- **Scenario**: Edit appears in the menu of alice's own message, but not on bob's message or on a message alice forwarded.
- **Fails if**: CIV:468 drops `cItem.meta.editable &&` (Edit shows on received items), or replaces it with `cItem.chatDir.sent` (Edit shows on the forwarded item; only the forwarded branch catches this).
- **Setup / CLI**: `connect(bob)`; `bob sends "@alice theirs"`; alice sends "mine" → `bob expectsMessage "alice> mine"`.
- **Steps**:
```kotlin
val chat = alice.chatList.open("bob")
val received = chat.itemId("theirs")
chat.send("mine"); bob expectsMessage "alice> mine"
val mine = chat.itemId("mine")
chat.forwardTo("theirs", "bob")                                     // Screens:257
var fwd = 0L
poll(what = "alice's forwarded copy of `theirs`") {
  fwd = chatModel.chatsContext.chatItems.value.lastOrNull { it.content.text == "theirs" && it.meta.itemForwarded != null }?.id ?: 0L
  fwd != 0L && exists(Ui.Chat.Item(fwd))
}
assertTrue(editOffered(mine)); assertFalse(editOffered(received)); assertFalse(editOffered(fwd))

private fun UiScope.editOffered(id: Long): Boolean {
  rightClick(Ui.Chat.Item(id)); waitFor(Ui.Chat.ItemMenu(id, "select"))
  val offered = exists(Ui.Chat.ItemMenu(id, "edit"))
  click(Ui.Chat.ItemMenu(id, "select")); waitFor(SEL_TITLE); escape()
  poll(what = "selection mode to end") { !exists(SEL_TITLE) }
  return offered
}
```
  - Absence follows presence: "select" is waited for before reading "edit".
  - "select" closes the menu the way `FilesTest.kt:80-84` does (there is no harness close-menu action).
  - bob's "-> forwarded" lines are left unconsumed; they are not errors.
- **Platform**: the share list must list the open chat (bob). If it doesn't, forward to "Private notes" (a forwarded note is not editable either, `Messages.hs:539`).
- **Catalogue**: rename the row (section 6). The voice branch ("no Edit on own voice", CIV:468 `!is MCVoice`) belongs in §E's voice test as a menu check.

### 4.3 T3 `edit_upArrow_editsLastMessage` (desktop)

- **File**: `apps/multiplatform/common/src/desktopTest/kotlin/chat/simplex/app/ui/desktop/EditMessageDesktopTest.kt`, class `EditMessageDesktopTest : UiTest()`.
- **Scenario**: with an empty field, Up edits alice's last own message, even when bob's message is newer. With text in the field, Up does nothing.
- **Fails if**: PTF:141 drops `&& cs.message.text.isEmpty()` (Up with text enters edit), or CMP:1190 `findLast { it.meta.editable }` → `lastOrNull()` (picks bob's message), or PTF:142 `onUpArrow()` is removed.
- **CLI**: `bob expectsMessage "alice> first"`, `"alice> second"`, `bob sends "@alice from bob"`, finally `bob expectsMessage "alice> [edited] second!"`.
- **Steps**:
```kotlin
val chat = alice.chatList.open("bob")
chat.send("first"); bob expectsMessage "alice> first"
chat.send("second"); bob expectsMessage "alice> second"
bob sends "@alice from bob"; chat.itemId("from bob")
check(editableText(Ui.Chat.Message).isNullOrEmpty() && !exists(described("Cancel")))
pressUp(Ui.Chat.Message)
poll(what = "the edit bar with alice's last message `second`") { editableText(Ui.Chat.Message) == "second" && exists(described("Cancel")) }
chat.replaceAndSend("second!")
bob expectsMessage "alice> [edited] second!"
typeInto(Ui.Chat.Message, "x")
pressUp(Ui.Chat.Message)
assertTrue("Up with text in the field", editableText(Ui.Chat.Message) == "x" && !exists(described("Cancel")))
```
  - The key handler is synchronous (PTF:141-143). `waitForIdle` in `pressUp` makes the immediate check valid, with no sleep.
  - Focus: the field holds focus after `send`/`typeInto`. If `performKeyInput` needs explicit focus, `click(Ui.Chat.Message)` first.
- **Flakiness**: none expected. The second branch's opposite (Up works) is proven by the first branch.

### 4.4 T4 `edit_ownFile_previewWithoutCancel` (desktop, P2)

- **Scenario**: alice sends a file with the caption "doc". While she edits it, the compose area shows the file preview without the cancel button. bob gets the edited caption, and the file is not sent again.
- **Fails if**: CMP:1224 `cancelEnabled = !composeState.value.editing && …` → `!composeState.value.inProgress` (Cancel shown while editing), or CMP:779 `MsgContent.MCFile(msgText)` → `MsgContent.MCText(msgText)` (the core rejects the type change, so bob gets no edit), or CMP:275 `FilePreview` → `NoPreview`.
- **Setup**: as `FilesDesktopTest.kt:13-32`: a temp `report.txt` with a fixed 31-character text.
- **CLI** (`tests/ChatTests/Files.hs:91-92` pattern, `View.hs:2443-2444`):
  - `bob.eventuallyMessage("alice> doc", CONNECT_TIMEOUT_MS)`;
  - `bob expectsMessage "alice> sends file report.txt (31 bytes / 31 bytes)"`;
  - `bob expects "use /fr 1 [<dir>/ | <path>] to receive it"`;
  - later `bob expectsMessage "alice> [edited] new doc"`, then sentinel `bob expectsMessage "alice> ping"`.
- **Steps**:
```kotlin
alice.chatList.open("bob"); waitFor(Ui.Chat.Message)
enterText(Ui.Chat.Message, source.absolutePath)
poll(what = "the report.txt preview with `Cancel file preview`") { seesDescription("Cancel file preview") && seesText("report.txt") && editableText(Ui.Chat.Message).isNullOrEmpty() }
typeInto(Ui.Chat.Message, "doc"); click(Ui.Chat.Send)
// CLI lines above
val id = fileItemId("report.txt")   // as FilesTest: msgContent is MCFile && file.fileName == "report.txt"
poll(what = "the sent preview cleared, one `report.txt` (the item)") { !seesDescription("Cancel file preview") && texts().count { it == "report.txt" } == 1 }
rightClick(Ui.Chat.Item(id)); click(Ui.Chat.ItemMenu(id, "edit"))
poll(what = "editing `doc` with a report.txt preview and no `Cancel file preview`") {
  editableText(Ui.Chat.Message) == "doc" && texts().count { it == "report.txt" } == 2 && exists(described("Cancel")) && !seesDescription("Cancel file preview")
}
ChatScreen(this).replaceAndSend("new doc")
bob expectsMessage "alice> [edited] new doc"
ChatScreen(this).send("ping"); bob expectsMessage "alice> ping"
```
  - Opposite first: "Cancel file preview" is present before sending.
  - Negative control: no second "sends file" line before "ping".
- **Strings**: "Cancel file preview" (strings.xml:604).
- **Risk**: the count of "report.txt" texts (item plus preview). Check on the first run that the file item renders the name as exactly one text node.

### 4.5 T5 `edit_group_membersSeeEdited` (P2)

- **Fails if**: CMP:813 `type = cInfo.chatType` → `ChatType.Direct` (direct edit still passes, group edit fails).
- **Setup**: `connect(bob); connect(cath); val chat = ownGroup(alice, "team", bob, cath)` (Screens:377). Then `bob.eventuallyMatching(Regex("#team: (new )?member cath is connected"), CONNECT_TIMEOUT_MS)` and the same for cath/bob (as `GroupLinksTest.kt:52-53`).
- **CLI** (Groups.hs:1300-1313):
  - after `chat.send("hello")`: `bob.eventuallyMessage("#team alice> hello", CONNECT_TIMEOUT_MS)` and the same for cath;
  - after `chat.menu("hello","edit"); chat.replaceAndSend("hey 👋")`: `bob expectsMessage "#team alice> [edited] hey 👋"` and `cath expectsMessage "#team alice> [edited] hey 👋"`.
- **Flakiness**: open flake 15.4.1 (a CLI member's join not completed) applies to `ownGroup`.

### 4.6 T6 `delete_forEveryone_fullDeletion_peerSeesDeleted`

- **Scenario**:
  1. Under default preferences, alice's Delete warns that the message "will be marked for deletion". "Delete for me" removes it only locally.
  2. bob allows full deletion. Now the warning says it "cannot be undone", and "For everyone" removes the message on both sides.
  3. The selection alert follows the same rule for two messages.
- **Fails if**:
  - CIV:347 drops `fullDeleteAllowed ||` (wrong single warning);
  - CV:332 drops `fullDeleteAllowed ||` (wrong multi warning);
  - CIV:1418 `cidmInternal` → `cidmBroadcast` (bob gets "[marked deleted] one" instead of "alice> two");
  - CV:3418-3420 always upserts (alice keeps "two").
- **CLI**:
  - `bob expectsMessage "alice> one"`;
  - `bob sends "/set delete @alice always"`, `bob expects "you updated preferences for alice:"`, `bob expects "Full deletion: enabled for contact (you allow: always, contact allows: no)"` (mirror of `testAllowFullDeletionContact`, `tests/ChatTests/Profiles.hs:3420-3422`);
  - `bob expectsMessage "alice> two"` (sentinel for the delete-for-me);
  - `bob expectsMessage "alice> [deleted] two"` (Profiles.hs:3428; `View.hs:3082-3084`);
  - `"alice> three"`, `"alice> four"`, then `"alice> [deleted] three"`, `"alice> [deleted] four"`.
- **Steps**:
```kotlin
val chat = alice.chatList.open("bob")
chat.send("one"); bob expectsMessage "alice> one"; val one = chat.itemId("one")
chat.menu("one", "delete")
poll(what = "`Delete message?` saying it will be marked for deletion, with both buttons") { seesText("Delete message?") && seesText(MARK_ONE) && exists(FOR_ME) && exists(FOR_EVERYONE) }
click(FOR_ME)
poll(what = "`one` removed from alice's chat") { chat.item(one) == null && !exists(Ui.Chat.Item(one)) }
bob sends "/set delete @alice always"; bob expects "you updated preferences for alice:"
bob expects "Full deletion: enabled for contact (you allow: always, contact allows: no)"
poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's full-deletion consent reaching alice") { alice.chatList.chat("bob").chatInfo.featureEnabled(ChatFeature.FullDelete) }
chat.send("two"); bob expectsMessage "alice> two"; val two = chat.itemId("two")
chat.menu("two", "delete")
poll(what = "`Delete message?` saying it cannot be undone") { seesText(FULL_ONE) && !seesText(MARK_ONE) && exists(FOR_EVERYONE) }
click(FOR_EVERYONE); bob expectsMessage "alice> [deleted] two"
poll(what = "`two` removed, not marked deleted") { chat.item(two) == null && !exists(Ui.Chat.Item(two)) }
chat.send("three"); bob expectsMessage "alice> three"; chat.send("four"); bob expectsMessage "alice> four"
val three = chat.itemId("three"); val four = chat.itemId("four")
chat.select("three"); chat.toggle(four); poll(what = "Selected 2") { chat.selectionTitle() == "Selected 2" }
click(SEL_DELETE)
poll(what = "`Delete 2 messages?` saying they cannot be undone, with For everyone") { seesText("Delete 2 messages?") && seesText(FULL_MANY) && exists(FOR_EVERYONE) }
click(FOR_EVERYONE)
bob expectsMessage "alice> [deleted] three"; bob expectsMessage "alice> [deleted] four"
poll(what = "both removed and selection ended") { chat.item(three) == null && chat.item(four) == null && !exists(SEL_TITLE) }
```
  - The `featureEnabled` poll is a wait for a precondition (catalogue: the CLI prints before the change reaches alice), not an assertion.
  - The mark-deleted multi text (opposite branch of CV:332) is asserted in T9.
- **Strings**: "Delete message?" (:441), "Delete %d messages?" (:442), warnings :443-446, "Delete for me"/"For everyone" (:453-454).
- **Keys**: new multi-dialog options and `SEL_DELETE`.
- **Platform**: none beyond `menu` (long press on Android).

### 4.7 T7 `markedDeleted_received_revealHide`

- **Scenario**:
  1. bob deletes one of his messages for everyone: alice sees "marked deleted".
  2. After a second consecutive deletion, the two collapse into "2 messages marked deleted".
  3. Reveal shows both texts; Hide collapses them again; Select on the collapsed row selects both.
  4. Once alice allows full deletion, bob's next delete removes his message from her chat.
- **Fails if**:
  - MDV:69 `if (total <= 1)` → `if (true)` (no count);
  - CIV:563-565 Reveal removed;
  - CIV:484-486 Hide removed;
  - CV:3353-3366 range branch removed (Select gives "Selected 1");
  - API:3131-3135 `if (toChatItem == null) removeChatItem(...) else upsert` → always upsert (full-deleted "three" stays).
- **CLI**:
  - `bob sends "@alice keep"`, `"@alice one"`, `"@alice two"`;
  - `bob sends "\\@alice one"` (echoed: not a message command, `src/Simplex/Chat/Terminal/Input.hs:64,90-100`), `bob expects "message marked deleted"` (`View.hs:941` + `chatItemDeletedText` :635-647);
  - the same for "two";
  - after the API helper: `bob expects "alice updated preferences for you:"`, `bob expects "Full deletion: enabled for you (you allow: default (no), contact allows: always)"` (Profiles.hs:3423-3424);
  - `bob sends "@alice three"`, `bob sends "\\@alice three"`, `bob expects "message deleted"`.
  - The core matches the text by prefix, newest first (`Store/Messages.hs:2741-2745`); "keep", "one", "two" and "three" are prefix-distinct.
- **Steps**:
```kotlin
bob sends "@alice keep"; bob sends "@alice one"; bob sends "@alice two"
val chat = alice.chatList.open("bob")
val keep = chat.itemId("keep"); val one = chat.itemId("one"); val two = chat.itemId("two")
check(textsOf(Ui.Chat.Item(one)).let { t -> t.any { "one" in it } && "marked deleted" !in t })
bob sends "\\@alice one"; bob expects "message marked deleted"
poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`one` shown as `marked deleted` without its text") { textsOf(Ui.Chat.Item(one)).let { t -> "marked deleted" in t && t.none { "one" in it } } }
bob sends "\\@alice two"; bob expects "message marked deleted"
poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`2 messages marked deleted` on one row") { "2 messages marked deleted" in textsOf(Ui.Chat.Item(two)) && !exists(Ui.Chat.Item(one)) }
check(textsOf(Ui.Chat.Item(keep)).any { "keep" in it })
chat.menu("two", "reveal")
poll(what = "both texts revealed") { textsOf(Ui.Chat.Item(one)).any { "one" in it } && textsOf(Ui.Chat.Item(two)).any { "two" in it } }
chat.menu("two", "hide")
poll(what = "collapsed again") { "2 messages marked deleted" in textsOf(Ui.Chat.Item(two)) && !exists(Ui.Chat.Item(one)) }
chat.menu("two", "select")
poll(what = "the collapsed range selected as 2") { chat.selectionTitle() == "Selected 2" }
escape(); poll(what = "selection mode to end") { !exists(SEL_TITLE) }
allowFullDeletionViaApi("bob")
bob expects "alice updated preferences for you:"
bob expects "Full deletion: enabled for you (you allow: default (no), contact allows: always)"
bob sends "@alice three"; val three = chat.itemId("three")
bob sends "\\@alice three"; bob expects "message deleted"
poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`three` removed from alice's chat") { chat.item(three) == null && !exists(Ui.Chat.Item(three)) }
check(exists(Ui.Chat.Item(keep)) && "2 messages marked deleted" in textsOf(Ui.Chat.Item(two)))
```
  - Opposite first: text before deletion; single label before the count; collapsed before Reveal; "three" visible before its deletion.
  - Negative control: "keep" is unaffected throughout.
- **Strings**: "marked deleted" (:51), "%d messages marked deleted" (:52), "Selected %d" (:521).
- **Keys**: `ItemMenu(id,"reveal")` (CIV:564), `ItemMenu(id,"hide")` (CIV:485).
- **Platform**: `escape()` exits selection on both (counter-toolbar BackHandler SEL:27; used by `FilesTest.kt:83`).
- **Flakiness**: the merged row's lazy-list key is the newest item (CV:2401), so "two" keeps its node when it collapses.

### 4.8 T8 `select_toggle_updatesCounter`

- **Scenario**: "Select" puts the chat into selection mode with "Selected 1". Clicking items toggles them (sent and received). Unselecting all shows "Nothing selected" with Delete disabled. (x) leaves selection mode with all messages kept.
- **Fails if**: CV:2241 `select = !checked` → `select = true` (no unselect); SEL:34-38 count text; SEL:31 `NavigationButtonClose(onButtonClicked = null)` (close disabled); SEL:145 `count == 0 ||` removed (Delete enabled with nothing selected).
- **Setup / CLI**: `connect(bob)`; `bob sends "@alice one"`, `"@alice two"`; alice sends "mine" → `bob expectsMessage "alice> mine"`.
- **Steps**:
```kotlin
val chat = alice.chatList.open("bob")
chat.send("mine"); bob expectsMessage "alice> mine"
val one = chat.itemId("one"); val two = chat.itemId("two"); val mine = chat.itemId("mine")
waitFor(Ui.Chat.Bar.part("title")); assertFalse(exists(SEL_TITLE)); assertFalse(exists(SEL_DELETE))
chat.menu("one", "select")
poll(what = "`Selected 1` instead of bob's chat bar, Delete enabled") { chat.selectionTitle() == "Selected 1" && !exists(Ui.Chat.Bar.part("title")) && isEnabled(SEL_DELETE) }
chat.toggle(two);  poll(what = "Selected 2") { chat.selectionTitle() == "Selected 2" }
chat.toggle(mine); poll(what = "Selected 3") { chat.selectionTitle() == "Selected 3" }
chat.toggle(one);  poll(what = "Selected 2 after unselecting `one`") { chat.selectionTitle() == "Selected 2" }
chat.toggle(two);  poll(what = "Selected 1") { chat.selectionTitle() == "Selected 1" }
chat.toggle(mine); poll(what = "`Nothing selected` with Delete disabled") { chat.selectionTitle() == "Nothing selected" && exists(SEL_DELETE) && !isEnabled(SEL_DELETE) }
click(described("Back"))
poll(what = "selection ended with bob's chat bar back and all three messages kept") {
  !exists(SEL_TITLE) && !exists(SEL_DELETE) && exists(Ui.Chat.Bar.part("title")) && listOf(one, two, mine).all { chat.item(it) != null && exists(Ui.Chat.Item(it)) }
}
```
- **Strings**: "Selected %d" (:521), "Nothing selected" (:520), "Back" (:800, close icon description).
- **Platform**:
  - Android one-hand UI puts the counter toolbar at the bottom (CV:1177); keys don't care.
  - Overlap: Android-only `BackKeyTest.back_exitsSelectionMode` enters selection and reaches "Selected 2" too, then tests Back. T8 tests the (x) and the toggles, so it is not a duplicate. Note it in the PR.
- **Flakiness**: received items slide by `selectionOffset` (CV:2026), which `waitStable` in `click` handles. Do not use `exists(Ui.Chat.Message)` as "compose back" (hidden composer SEL:82).

### 4.9 T9 `selectDelete_forEveryone_peerSeesBoth`

- **Scenario**:
  1. With one of alice's and one of bob's messages selected, Delete warns that they "cannot be undone" and offers only "Delete for me". Both disappear locally; bob sees nothing.
  2. With two of alice's messages selected, Delete warns they "will be marked for deletion" and offers "For everyone". bob sees both marked deleted, and alice sees them collapsed.
- **Fails if**: SEL:159 drops `ci.meta.deletable &&` (For everyone offered on a mixed selection); CIV:1451 `if (forAll)` → `if (true)`; CV:332-335 branches swapped; CV:3399 `if (forAll) cidmBroadcast` → `cidmInternal`.
- **CLI** (`testDirectMessageDeleteMultiple`, Direct.hs:883-886):
  - `bob expectsMessage "alice> hello"`;
  - `bob sends "@alice hey"`;
  - `bob expectsMessage "alice> hi"`, `"alice> bye"`;
  - then, as the next lines, `bob expectsMessage "alice> [marked deleted] hi"` and `bob expectsMessage "alice> [marked deleted] bye"`.
- **Steps**:
```kotlin
val chat = alice.chatList.open("bob")
chat.send("hello"); bob expectsMessage "alice> hello"
bob sends "@alice hey"
chat.send("hi"); bob expectsMessage "alice> hi"; chat.send("bye"); bob expectsMessage "alice> bye"
val hello = chat.itemId("hello"); val hey = chat.itemId("hey"); val hi = chat.itemId("hi"); val bye = chat.itemId("bye")
chat.select("hello"); chat.toggle(hey); poll(what = "Selected 2") { chat.selectionTitle() == "Selected 2" }
click(SEL_DELETE)
poll(what = "`Delete 2 messages?` saying they cannot be undone, with Delete for me") { seesText("Delete 2 messages?") && seesText(FULL_MANY) && exists(FOR_ME) }
assertFalse("For everyone offered for a selection with bob's message", exists(FOR_EVERYONE))
click(FOR_ME)
poll(what = "`hello` and `hey` removed, selection ended, `hi` kept") {
  chat.item(hello) == null && chat.item(hey) == null && !exists(Ui.Chat.Item(hello)) && !exists(Ui.Chat.Item(hey)) && !exists(SEL_TITLE) && exists(Ui.Chat.Item(hi))
}
chat.select("hi"); chat.toggle(bye); poll(what = "Selected 2") { chat.selectionTitle() == "Selected 2" }
click(SEL_DELETE)
poll(what = "`Delete 2 messages?` saying they will be marked for deletion, with For everyone") { seesText(MARK_MANY) && exists(FOR_EVERYONE) }
click(FOR_EVERYONE)
bob expectsMessage "alice> [marked deleted] hi"
bob expectsMessage "alice> [marked deleted] bye"
poll(what = "alice's two messages collapsed into `2 messages marked deleted`") { "2 messages marked deleted" in textsOf(Ui.Chat.Item(bye)) && !exists(Ui.Chat.Item(hi)) }
```
  - The first deletion's "nothing to bob" is proven by bob's next lines being the marked-deleted ones.
  - Both branches of the For everyone button and of the CV:332 text are covered (the full-delete text in T6).
- **Keys**: `SEL_DELETE` and the multi-dialog options.

### 4.10 T10 `selectForward_multiple_peerReceives`

- **Scenario**:
  1. alice selects her "hi" and bob's "hey" and "bye"; Forward is enabled.
  2. bob deletes "bye" for everyone. It stays selected, but Forward turns off.
  3. Unselecting "bye" turns Forward on again.
  4. Forwarding to cath delivers "hi" and "hey" only.
- **Fails if**: SEL:127 drops `chatItems.value` from the `LaunchedEffect` keys (Forward stays enabled after bob's delete); SEL:163 drops `ci.meta.itemDeleted == null`; CV:368 `itemIds.toList()` → `listOf(itemIds.first())` (only one forwarded).
- **CLI** (`testForwardContactToContactMulti`, `tests/ChatTests/Forward.hs`):
  - `bob expectsMessage "alice> hi"`, `bob sends "@alice hey"`, `"@alice bye"`;
  - `bob sends "\\@alice bye"`, `bob expects "message marked deleted"`;
  - `cath.eventuallyMessage("alice> -> forwarded", CONNECT_TIMEOUT_MS)`, `cath expects "      hi"`, `cath expectsMessage "alice> -> forwarded"`, `cath expects "      hey"`;
  - sentinel `cath expectsMessage "alice> ping"`.
- **Steps**:
```kotlin
connect(bob); connect(cath)
val chat = alice.chatList.open("bob")
chat.send("hi"); bob expectsMessage "alice> hi"
bob sends "@alice hey"; bob sends "@alice bye"
val hi = chat.itemId("hi"); val hey = chat.itemId("hey"); val bye = chat.itemId("bye")
chat.select("hi"); chat.toggle(hey); chat.toggle(bye)
poll(what = "Selected 3 with Forward enabled") { chat.selectionTitle() == "Selected 3" && isEnabled(SEL_FORWARD) }
bob sends "\\@alice bye"; bob expects "message marked deleted"
poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`bye` marked deleted, still selected, Forward disabled") {
  chat.item(bye)?.meta?.itemDeleted != null && chat.selectionTitle() == "Selected 3" && exists(SEL_FORWARD) && !isEnabled(SEL_FORWARD)
}
chat.toggle(bye)
poll(what = "Selected 2 with Forward enabled again") { chat.selectionTitle() == "Selected 2" && isEnabled(SEL_FORWARD) }
chat.forwardSelectionTo("cath")
// cath lines above
chat.send("ping"); cath expectsMessage "alice> ping"
```
  - Order: `forwardContent` takes items in chat order (CV:3684), so "hi" comes before "hey".
  - After Send, alice's open chat is cath's (`forwardContent` closes the source chat, CV:3682), so `chat.send` goes to cath.

### 4.11 Optional changes that make existing tests stronger (each needs your OK, Q7)

- `EditMessageTest.edit_received_updatesBubble` (`EditMessageTest.kt:9-19`): assert "hello" is in the bubble before bob's edit and absent after (today it only checks "hey" is present).
- `GroupLinksTest.memberInfo_blockForAllNotifiesMembers` (03 §14 owner): after "after block", bob sends one more message, and the single row reads "2 messages blocked by admin" (MDV:73-74). This replaces the dropped `blockedByAdmin_items_showBlocked`.

## 5. Dropped rows and branches (reasons)

| Row / branch | Reason |
|---|---|
| `edit_quotedReply_keepsQuote` | The quote is kept by the core; the app sends only the content (CMP:818) and upserts the result. No realistic app mutation fails only this; covered by Haskell `testDirectMessageUpdate`. |
| `edit_imageCaption_updates` | Same app receive path as `edit_received_updatesBubble` (API:3090); +XFTP cost. |
| `edit_ownImage_previewWithoutCancel` | Image attach on both platforms needs FD support; the cancel-while-editing rule and `updateMsgContent` are covered by the T4 file variant. Revisit when §F has an image-attach helper. |
| Voice branch of the "no Edit" rule | Needs REC; belongs in §E (one menu check in the voice test). |
| Edit window (24 h) branch | `deletable'` uses `nominalDay` at item load (`Messages.hs:544-550`); unreachable without clock control. |
| `blockedByAdmin_items_showBlocked` | Single label already asserted by `GroupLinksTest.memberInfo_blockForAllNotifiesMembers`; count branch → 4.11. |
| `select_limits_forward20_delete200` | 300 messages plus 201 individual clicks (no select-all in the main chat); the rule is two comparisons (SEL:145-146). Better as a unit test of `recheckItems`, outside this suite. |
| Merged rows | `edit_unchanged_noUpdate`, `edit_cancel_clearsCompose`, `edit_metaShowsEditedIcon`, `info_history_listsVersions` → T1; `delete_ownForMe_localOnly` → T6; `markedDeleted_consecutive_collapsedCount`, `delete_received_fullDelete_removed`, `select_mergedEvents_selectsWholeRange` → T7; `select_enterMode_showsCounterToolbar`, `select_close_exitsMode` → T8; `selectDelete_mixed_onlyForMe` → T9; `select_itemDeletedByPeer_recheck` → T10. |

## 6. Catalogue corrections (02)

- I line 138: CIV:464-469 → CIV:468-473; CMP:806 → CMP:808-827; CMP:1263-1265 → CMP:1265-1267. `edit_metaShowsEditedIcon`: the icon has no content description (CIMetaView.kt:84, :198-199), so it can't be asserted visually; it is merged into `edit_menu_updatesPeer` (model `meta.itemEdited` plus the 2-version history).
- I line 139: PTF:138 → PTF:141-142; CMP:1185 → CMP:1187-1192 (only with no context item and no preview; picks `findLast { meta.editable }`, i.e. alice's last own message, not the last item); desktop-only.
- I line 142: CMP:1210, 1222 → CMP:1212, 1224; the file variant runs desktop-only through path paste.
- I line 143: rename to `edit_notOffered_forReceivedOrForwarded`; CIV:464 → CIV:468; the voice part moves to §E; note that `editable` also expires after 24 h (`Messages.hs:538-550`, untestable).
- J line 149: CIV:869-915 → CIV:873-920; CIV:1390-1424 → CIV:1399-1433; `delete_ownForMe_localOnly` merged into `delete_forEveryone_fullDeletion_peerSeesDeleted`.
- J line 150: CIV:503-519 → CIV:507-523; CIV:950-972 → CIV:958-980. A marked-deleted item renders through CIV:702-704, so its Reveal is CIV:563-565; Hide on a revealed item is CIV:484-486. `delete_received_fullDelete_removed` is merged into `markedDeleted_received_revealHide`.
- J line 151: MDV:68-110 → MDV:48-111 (count logic :69-78). `blockedByAdmin_items_showBlocked` is a duplicate of `GroupLinksTest.memberInfo_blockForAllNotifiesMembers` for the single case. `markedDeleted_consecutive_collapsedCount` is merged and works in a direct chat (no group needed).
- K line 156: CV:3342 → CV:3345-3381; CV:298 → CV:300; CV:1170-1176 → CV:1172-1178. "The compose field is back" is not observable: SEL:82 composes a hidden `ComposeView` (with `Ui.Chat.Message`) in selection mode; check the buttons toolbar instead. The three tests merge into `select_toggle_updatesCounter`. The close (x) is `described("Back")` (SEL:31 → `DefaultTopAppBar.kt:140-146`).
- K line 157: CV:327-347 → CV:325-349; SEL:95-101 → SEL:97-104; SEL:157 → SEL:159; CIV:1437-1448 → CIV:1435-1460. Merged into one test.
- K line 158: CV:361-384 → CV:363-389.
- K line 159: SEL:144-145 → SEL:145-146; SEL:161 → SEL:163; SEL:125-127 → SEL:127-129. `select_itemDeletedByPeer_recheck` → merged into `selectForward_multiple_peerReceives`. `select_mergedEvents_selectsWholeRange` → merged into `markedDeleted_received_revealHide` (deleted-items range, same CV:3353-3366 logic). `select_limits_forward20_delete200` dropped.
- P line 207: ChatItemInfoView.kt:103-108 → :105-110; merged into `edit_menu_updatesPeer`.
- Header line 5 says the selection-toolbar keys come "in K2". Concretely they are `Ui.Chat.Selection.part("delete")` / `.part("forward")` (SEL:97, :115); selection circles need no key (clicking the item toggles, CV:2238-2243).

## 7. Suspected app issues (report only, plan §15.5 candidates)

1. **Select-all button labelled "Back"**: `SelectAllButton` uses `stringResource(MR.strings.back)` as its icon description (SEL:57). Screen readers announce "Back" for "select all". In report/support selection it also sits next to the real (x) "Back", making `described("Back")` ambiguous there.
2. **Selection toolbar buttons and the edited icon have no accessible labels**: Delete/Moderate/Forward icons pass `null` (SEL:98-121), and so does the edited mark (CIMetaView.kt:198-199). A minor accessibility gap (the catalogue already plans K2 keys for the toolbar).
3. **A live hidden composer in selection mode**: SEL:82 composes a full `ComposeView` with `Chat.sampleData` behind the toolbar, only to measure its height. It duplicates the `chat.compose`/`chat.send` nodes. On desktop it may be reachable by keyboard focus (Tab) while the visible toolbar covers it. Unverified; worth a manual check before reporting.
4. No functional bug found in the edit/delete/select logic read for this design.

## 8. Cost and order

- P1 (8 tests): T1, T2, T6, T7, T8, T9, T10 (T10 needs 2 peers), T3.
- P2: T4 (XFTP), T5 (group).
- Expected run time: about 1 reset plus 1-2 contact connections each; T5 adds group setup.
- Implement in this order: keys (section 3) → Screens helpers (4.0) → T8/T9 (validate the selection keys) → T1/T2 → T6/T7 → T10 → desktop T3/T4 → T5.
- One build at the end (memory: batch builds); compile both platforms because of the `uiKey` parameter.
