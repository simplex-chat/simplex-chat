# Design D2: forwarding (02 §L) and draft persistence (02 §S)

Design only: no test code, keys or catalogue edits are made here. Rules: `apps/multiplatform/common/src/uiTestShared/README.md` (the guide), plan §0.5.

- Paths are relative to `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/` unless they start with `apps/`, `tests/` or `src/`. **CMP** = `views/chat/ComposeView.kt`, **CV** = `views/chat/ChatView.kt`, **CIV** = `views/chat/item/ChatItemView.kt`, **SEL** = `views/chat/SelectableChatItemToolbars.kt`, **SLV** = `views/chatlist/ShareListView.kt`, **SLN** = `views/chatlist/ShareListNavLinkView.kt`, **FIV** = `views/chat/item/FramedItemView.kt`, **CTX** = `views/chat/ContextItemView.kt`. Harness: `H/` = `apps/multiplatform/common/src/uiTestShared/kotlin/chat/simplex/app/ui/harness/`.
- Line numbers are from the worktree `~/wt-uitest` at HEAD `8927c1892`, where the tests will be written. The catalogue cites master `d72f68e6d`, so its numbers differ (corrections at the end).
- UI strings are from `apps/multiplatform/common/src/commonMain/moko-resources/base/strings.xml` (the brief's `resources/MR/base` path doesn't exist). "…" is U+2026.
- CLI lines were checked against `src/Simplex/Chat/View.hs` and `tests/ChatTests/Forward.hs`.

## 1. Result

There are 11 new tests (10 shared, 1 desktop-only). The 2 existing tests are kept as they are. The 25 catalogue rows (17 in §L, 8 in §S) map as follows: 22 are covered, 1 row is added (share-list search), 1 is deferred (REC) and 1 is dropped (`forward_groupToGroup`). `forward_notOffered_forDeletedOrLive` is split: the deleted-item half is covered and the live half is dropped.

| # | Test (class) | Catalogue names covered | Needs | Peers |
|---|---|---|---|---|
| — | `forward_contactToContact_peerSeesForwarded` (ForwardTest, exists) | same | P ×2 | bob, cath |
| — | `forward_toNotes_showsSavedFrom` (ForwardTest, exists) | same | P | bob |
| T1 | `forward_acrossChatTypes_peersReceiveAndSourceOpens` (ForwardTest, **new name**) | `forward_contactToGroup`, `forward_groupToContact`, `forward_fromNotes_toContact`, `forwarded_goToSource_opensOriginalChat` | P ×2 | bob, cath |
| T2 | `forward_withComment_sendsCommentQuoted` (ForwardTest) | + `forward_cancelContext`, + §S `draft_forwardIntoChatWithDraft_restoresDraftAfterSend` | P ×2 | bob, cath |
| T3 | `forwarded_received_showsHeader` (ForwardTest) | same | P ×2 | bob, cath |
| T4 | `forwardPlan_filesNotAccepted_alertOptions` (ForwardTest) | + `forward_itemsDeletedAfterSelection_alert` (overlaps K `selectForward_multiple_peerReceives`) | P ×2, XFTP | bob, cath |
| T5 | `forwardPlan_downloadButton_receivesFiles` (ForwardTest) | + `forwardPlan_nothingToForward`, `forward_file_peerReceivesFile`, deleted half of `forward_notOffered_forDeletedOrLive` | P ×2, XFTP | bob, cath |
| T6 | `forward_destinationProhibitsContent_alert` (ForwardTest) | same | P, XFTP | bob |
| T7 | `forward_searchDestinations_filtersList` (ForwardTest, **new row**) | — | P | bob |
| T8 | `draft_onlyLastChatKept` (DraftTest) | + `draft_text_restoredAfterChatSwitch`, + "draft cleared after send" (not in the catalogue yet) | P ×2 | bob, cath |
| T9 | `draft_withQuote_restored` (DraftTest) | + `draft_editingState_restored` | P | bob |
| T10 | `draft_clearedWhenSendDisabled` (DraftTest) | same, + switch-order regression | P ×2 | bob, cath |
| T11 | `draft_withAttachment_restored` (desktop `DraftDesktopTest`) | same | P, XFTP | bob |

Merging was the rule throughout (guide §5: each test costs a reset plus peer connections). Where a merge crosses classes (T2 takes a §S row) or needs a new name (T1), it is a decision for you, listed in §7.

## 2. Row triage (every §L and §S row)

| Row (catalogue line) | Code verified (worktree) | Decision | Reason |
|---|---|---|---|
| L164 `forward_contactToContact_peerSeesForwarded` | CV:640-643, SLN:31-44, CMP:729-757 | keep (exists) | Already passing. Note: it opens bob's chat right after bob sends, which is the §15.4.1b flake pattern |
| L164 `forward_withComment_sendsCommentQuoted` | CMP:845-855 (the comment quotes the last forwarded item, `quoted = message.id` at :850) | keep → T2 | |
| L164 `forward_cancelContext` | CMP:1268-1270, CTX:148-151 | merge → T2 | Same setup; T2's comment lines prove nothing was sent by the cancelled attempt |
| L165 `forward_contactToGroup` | CMP:732-738 | merge → T1 | contact→contact can't catch a from/to type mix-up in CMP:732-738; a cross-type test can |
| L165 `forward_groupToContact` | same | merge → T1 | |
| L165 `forward_groupToGroup` | same | **drop** | The app path is identical to T1's legs (type and id are passed through); the only difference is in the core (`testForwardGroupToGroup`). It would cost a second group and a third peer |
| L166 `forward_toNotes_showsSavedFrom` | FIV:319-351 | keep (exists) | |
| L166 `forward_fromNotes_toContact` | CMP:736-738 with `ChatType.Local` | merge → T1 | bob is already connected there |
| L167 `forwarded_goToSource_opensOriginalChat` | CIV:286-305 (arrow), ChatModel.kt:4220-4226 | merge → T1 | On its own it would be a superset of the existing contact→contact test (guide §5, no prefixes). Correction: the arrow is always composed when the item has a source; hovering (desktop) or tapping the bubble (Android, CIV:334-336) changes only how it looks |
| L167 `forwarded_received_showsHeader` | FIV:348-350, ChatModel.kt:4231-4233 | keep → T3 | This is where "the original sender is not leaked" is checked on the GUI |
| L168 `forward_file_peerReceivesFile` | CIV:474-482, ChatModel.kt:4643-4647; core `forwardContent` (`src/Simplex/Chat/Library/Commands.hs:1180-1200`) | merge → T5 | Downloading through the plan alert's "Download" button avoids becoming a prefix of `FilesTest.rcvFile_menuDownload_receives` |
| L169 `forwardPlan_filesNotAccepted_alertOptions` | CV:363-386, 3698-3756 | keep → T4 | |
| L169 `forwardPlan_nothingToForward` | CV:3737-3752 | merge → T5 | T5's first step |
| L169 `forwardPlan_downloadButton_receivesFiles` | CV:3004-3032 | keep → T5 | |
| L170 `forward_notOffered_forDeletedOrLive` | CIV:474-477 | split: deleted half → T5; **live half dropped** | Live messages are owned by §C (`LiveMessageTest`), and starting one has no key or helper. Suggest §C checks that the item menu has no "forward" during a live message |
| L170 `forward_itemsDeletedAfterSelection_alert` | CMP:749-754; core `forwardMsgContent` skips deleted items (`Commands.hs:5124`) | merge → T4 | T4 already has a multi-item forward to cath |
| L171 `forward_destinationProhibitsContent_alert` | SLV:29-58, SLN:46-63, 75-80 | keep → T6 | |
| S237 `draft_text_restoredAfterChatSwitch` | CMP:1421-1433 | merge → T8 | Its steps are contained in T8 |
| S237 `draft_withQuote_restored` | CMP:1427-1429 (the whole `ComposeState` is saved) | keep → T9 | |
| S237 `draft_editingState_restored` | same | merge → T9 | One peer, same switch pattern |
| S237 `draft_onlyLastChatKept` | CMP:1427-1435 (one slot, ChatModel.kt:255-257) | keep → T8 | |
| S237 `draft_clearedWhenSendDisabled` | CMP:1443-1448, ChatModel.kt:1711, 1778 | keep → T10 | Adds the switch-order regression that the comment at CMP:1441-1442 guards against |
| S237 `draft_forwardIntoChatWithDraft_restoresDraftAfterSend` | CV:150-160 (the draft is hidden while forwarding), CMP:997-999 | merge → T2 | Same peers and forward flow |
| S238 `draft_withAttachment_restored` | CMP:1427-1429; `D/platform/PlatformTextField.desktop.kt:99-107` (a pasted path becomes an attachment) | keep → T11, desktop-only | Needs is **P, XFTP**, not CB: the path is entered as text, as in `FilesDesktopTest.paste_filePathText_attachesFile` |
| S239 `draft_voiceRecording_finishedOnSwitch` | CMP:1422-1426 | **defer** | Needs the REC seam, which is not built (plan §0.5.11, 7.2/11.2). Keep the row |
| (new) draft across restart | ChatModel.kt:255-257: `draft`/`draftChatId` are plain `mutableStateOf`; nothing writes them to prefs or the database | **no test, no row** | The app does not keep drafts across a restart. The harness `restart()` restarts only the core, so `ChatModel.draft` survives it (H/CommonReset.kt runs only at reset) and a "persists" test would pass falsely. Harness finding in §6 |

## 3. App changes (keys only)

Every key below is on an element a test clicks or types into.

| Key | Ui.kt | Where (worktree) | Used by |
|---|---|---|---|
| `Ui.Chat.Item(id).part("goTo")` | none (uses `part`, like `.part("quote")`) | CIV:273-279, the `IconButton` in `GoToItemInnerButton`: add `.ui(Ui.Chat.Item(cItem.id).part("goTo"))` to the modifier chain at :275 | T1 clicks it. T3 checks it is absent, which is allowed because T1 acts on it |
| `Ui.Chat.Selection.part("forward")` | none | SEL:115: `IconButton({ forwardItems() }, Modifier.ui(Ui.Chat.Selection.part("forward")), enabled = …)` | T4, T5 |
| `Ui.Alert.Option(Ui.AlertSource.Shared, "forward")` | none | CV:3041-3048 `ForwardButton`: `modifier = modifier.ui(…)` | T4 |
| `Ui.Alert.Option(Ui.AlertSource.Shared, "download")` | none | CV:3014-3016 `DownloadFilesButton`: `modifier = modifier.ui(…)` | T5 (click), T4 (presence) |
| `Ui.Share.Bar` → `Ui.Share.Bar.part("search")` | add `val Bar = UiKey("share.bar")` to `object Share` (Ui.kt:165-167) | SLV:142 `DefaultAppBar(…, uiKey = Ui.Share.Bar)`. The search field then gets the key at `views/helpers/DefaultTopAppBar.kt:87` | T7 |

No new key is needed for these: the forwarding/reply/edit bar's (x), `described("Cancel")` (CTX:148-151, content description `cancel_verb`); the share-list search button, `described("Search")` (SLV:167-169, only composed with 8 or more chats); OK in `showAlertMsg`, `Ui.Alert.Confirm(Shared)` (`views/helpers/AlertManager.kt:251`). The existing keys used are `Ui.Chat.ItemMenu(id, "forward"|"select"|"reply"|"edit"|"download")`, `Ui.Share.Row`, `Ui.Chat.Send`, `Ui.Chat.Message` and `Ui.Chat.Selection.part("title")` (SEL:48 → DefaultTopAppBar.kt:42).

`described("Cancel")` risk: on desktop the chat list is visible next to the chat. No other clickable with the exact description "Cancel" was found in the chat view or the chat list. If one appears, add `Ui.Chat.Message.part("contextCancel")` at CTX:148.

## 4. Harness additions (no refactor of existing helpers)

H/Screens.kt, `ChatScreen` (stateless, :207, so one instance serves whichever chat is open):
```kotlin
fun itemId(what: String, match: (ChatItem) -> Boolean): Long {          // same body as FilesTest.kt:99-107
  var id: Long? = null
  s.poll(timeoutMs = CONNECT_TIMEOUT_MS, what = what) {
    id = chatModel.chatsContext.chatItems.value.lastOrNull(match)?.id; id != null && s.exists(Ui.Chat.Item(id!!))
  }
  return id!!
}
fun forwardItemTo(id: Long, chatName: String) {
  s.rightClick(Ui.Chat.Item(id)); s.click(Ui.Chat.ItemMenu(id, "forward"))
  ChatListScreen(s).shareTo(chatName)
  s.poll(what = "the send button to be enabled for forwarding") { s.isEnabled(Ui.Chat.Send) }
  s.click(Ui.Chat.Send)
}
fun select(vararg ids: Long) {
  s.rightClick(Ui.Chat.Item(ids[0])); s.click(Ui.Chat.ItemMenu(ids[0], "select"))
  ids.drop(1).forEach { s.click(Ui.Chat.Item(it)) }                    // the selection overlay, CV:2238-2242
  s.poll(what = "`Selected ${ids.size}`") { "Selected ${ids.size}" in s.textsOf(Ui.Chat.Selection.part("title")) }
}
fun forwardSelected() = s.click(Ui.Chat.Selection.part("forward"))
fun sendComposed() = enterWhenSendable()                               // the existing private fun, made reachable
```
`ChatListScreen` (H/Screens.kt):
```kotlin
fun shareSearch(text: String) { s.click(described("Search")); s.typeInto(Ui.Share.Bar.part("search"), text) }
fun shareShown(name: String): Boolean = s.exists(Ui.Share.Row(chatId(name)))
```
H/Helpers.kt:
```kotlin
fun CliPeer.sendsFileTo(contact: String, path: String, fileId: Int, name: String) {   // FilesTest.kt:93-97 with the contact as a parameter
  this sends "/f @$contact $path"; this expects "use /fc $fileId to cancel sending"
  eventually("completed uploading file $fileId ($name) for $contact", CONNECT_TIMEOUT_MS)
}
fun CliPeer.receivesFile(fileId: Int, name: String, from: String) {
  val dir = java.io.File(createFile(".fr-probe", "")).parent          // a host-side path on both platforms (Android: bridge `file`)
  this sends "/fr $fileId $dir/"
  eventually("completed receiving file $fileId ($name) from $from", CONNECT_TIMEOUT_MS)
}
fun newGroupsViaApi(vararg names: String) = names.forEach {           // precondition only, as at ChatListToolbarSearchTest.kt:42-45
  runBlocking { chatController.apiNewGroup(null, incognito = false, GroupProfile(it, "", null)) } ?: error("apiNewGroup($it) failed")
}
```
H/ChatListRowsScreens.kt (next to `switchAwayFromChat`, :7):
```kotlin
fun UiScope.visitNotesAndBack(app: App, name: String): ChatScreen {
  backToChatList(); app.chatList.open("Private notes")
  poll(what = "the notes folder with an empty field and no context bar") {
    chatModel.chatId.value == app.chatList.chatId("Private notes") && editableText(Ui.Chat.Message).isNullOrEmpty() && !seesDescription("Context icon")
  }
  backToChatList(); return app.chatList.open(name)
}
```

## 5. Tests

Shared conventions:
- "row ready" = `poll(what = "<text> on <name>'s row") { "<text>" in alice.chatList.rowTexts("<name>") }` before opening a chat a peer just wrote to. This avoids the §15.4.1b flake.
- "bar" = the compose context bar, detected with `seesDescription("Context icon")` (CTX:122, `icon_descr_context`). It is absent when there is no context item.

### T1 `forward_acrossChatTypes_peersReceiveAndSourceOpens` (ForwardTest)
- **Scenario.** alice forwards bob's message into her group, forwards a group message to bob and follows its arrow back to the group, then forwards a note to bob.
- **Fails if** forwarding between different chat types breaks, or the source arrow opens the wrong chat. Mutations:
  - CMP:736 `fromChatType = fromChatInfo.chatType` → `chat.chatInfo.chatType`. The contact→group leg fails, while the existing contact→contact test stays green.
  - CIV:300 `openChat(…, chatType, apiId, msgId)` → `openChat(…, cInfo.chatType, cInfo.apiId, cItem.id)`. alice stays in bob's chat.
  - ChatModel.kt:4224 `ChatType.Group` → `ChatType.Direct`.
  - FIV:325 `forwarded.groupType != null` → `true` (a two-row "forwarded from" header for an ordinary group).
- **Setup:** `connect(bob)`, `connect(cath)`, `ownGroup(alice, "team", cath)` (H/Screens.kt:377), `backToChatList()`.
- **Steps and assertions:**
```kotlin
bob sends "@alice hey"; row ready("bob", "hey")
val chat = alice.chatList.open("bob"); chat.forwardTo("hey", "team")                     // Screens.kt:257
cath.eventuallyMessage("#team alice> -> forwarded", CONNECT_TIMEOUT_MS); cath expects "      hey"
cath sends "#team hello"
val teamId = alice.chatList.chatId("team"); val bobId = alice.chatList.chatId("bob")
poll(what = "team open after the forward") { chatModel.chatId.value == teamId }
val original = chat.itemId("hello")
chat.forwardTo("hello", "bob")
bob.eventuallyMessage("alice> -> forwarded", CONNECT_TIMEOUT_MS); bob expects "      hello"
val fwd = chat.itemId("hello")                                                            // now the item in bob's chat
poll(what = "`forwarded` header without a source name") {
  chatModel.chatId.value == bobId && textsOf(Ui.Chat.Item(fwd)).let { t -> "forwarded" in t && t.none { "forwarded from" in it || it == "team" } }
}
click(Ui.Chat.Item(fwd).part("goTo"))
poll(what = "team's chat opened at cath's `hello`") { chatModel.chatId.value == teamId && exists(Ui.Chat.Item(original)) }
backToChatList()
val notes = alice.chatList.open("Private notes"); notes.send("memo"); notes.forwardTo("memo", "bob")
bob.expectsMessage("alice> memo", CONNECT_TIMEOUT_MS)                                       // the next line, so no "-> forwarded" before it
```
- **CLI:** `CIFFUnknown -> "-> forwarded"` (View.hs:1026). The group form is from `testForwardContactToGroup` (Forward.hs:205-227). A forward from notes prints a plain item (`testForwardNotesToContact`, Forward.hs:334-345, `alice <# "@cath hi"`, `cath <# "alice> hi"`).
- **Platform:**
  - Android: `forwardTo` closes the chat, since `forwardItem` sets `chatId = null` (CV:641), and the share list replaces the chat list (`App.kt:376-384`). After the send, alice is in the destination chat, so the test doesn't call `backToChatList()` between legs except before opening notes.
  - The arrow is composed on both platforms, so it is tapped directly.
- **Flake risks:**
  - cath's `#team hello` must be in alice's open team chat. `itemId` polls the model and the node.
  - The click on the arrow happens while bob's chat may still be loading. `click` waits for the node.

### T2 `forward_withComment_sendsCommentQuoted` (ForwardTest)
- **Scenario.** alice starts forwarding bob's "hey" to cath and cancels it. She writes a draft in cath's chat and leaves. Then she forwards "hey" again with a comment. cath gets the forward and the comment quoting it, and alice's draft comes back in cath's compose field.
- **Fails if:**
  - cancel no longer removes the bar: CMP:1268-1270 lambda → `{}`;
  - the comment is not sent or not quoted: CMP:845-855 removed, or `quoted = message.id` → `null` at :850;
  - the draft is lost after the forward: CMP:998-999 removed;
  - the draft is merged into the forward: CV:154, drop `(sharedContent !is SharedContent.Forward || …)`. The field then holds "unsent" next to the bar.
- **Steps:**
```kotlin
connect(bob); connect(cath); bob sends "@alice hey"; row ready("bob", "hey")
val chat = alice.chatList.open("bob"); val cathId = alice.chatList.chatId("cath")
chat.menu("hey", "forward"); alice.chatList.shareTo("cath")
poll(what = "cath's chat with the forwarding bar") { chatModel.chatId.value == cathId && seesDescription("Context icon") && isEnabled(Ui.Chat.Send) }
click(described("Cancel"))
poll(what = "the bar gone, nothing forwarded") { exists(Ui.Chat.Message) && !seesDescription("Context icon") && chatModel.chatsContext.chatItems.value.none { it.content.text == "hey" } }
typeInto(Ui.Chat.Message, "unsent")
backToChatList(); alice.chatList.open("bob")
poll(what = "cath's row showing the draft") { alice.chatList.rowTexts("cath").any { it.endsWith(" unsent") } }
chat.menu("hey", "forward"); alice.chatList.shareTo("cath")
poll(what = "the bar with an empty field, the draft hidden") { chatModel.chatId.value == cathId && seesDescription("Context icon") && editableText(Ui.Chat.Message).isNullOrEmpty() }
typeInto(Ui.Chat.Message, "see this"); poll(what = "send enabled") { isEnabled(Ui.Chat.Send) }; pressEnter(Ui.Chat.Message)
cath.expectsMessage("alice> -> forwarded", CONNECT_TIMEOUT_MS); cath expects "      hey"
cath expectsMessage "alice> >> hey"; cath expects "      see this"
poll(what = "the draft back after the forward") { editableText(Ui.Chat.Message) == "unsent" && !seesDescription("Context icon") }
```
- `chat.send()` can't be used for the comment. Its poll for an empty field (Screens.kt:287) can miss the moment between clearing and the draft restore.
- `>>`: `directQuote` prints `>>` when the item and the quote have the same direction (View.hs:1017-1019). On cath's side both are received from alice. The catalogue line agrees.
- **Negative control:** cath's first line read is the second forward, and its comment follows. If the cancelled attempt had sent anything, a second `-> forwarded` would come where `>> hey` is expected.
- **Platform:**
  - After entering cath's chat through the share list, ComposeView is newly composed with an empty state (CV:150-160); `KeyChangeEffect` doesn't run on first composition. Verify on the first desktop run that the field really is empty.
  - Android: no `hideKeyboard()` is needed before `pressEnter`.

### T3 `forwarded_received_showsHeader` (ForwardTest)
- **Scenario.** bob forwards to alice a message he received from cath. alice sees a "forwarded" header and nothing that names cath. bob's ordinary message has no header.
- **Fails if:**
  - the header is removed or renamed: FIV:348-350 deleted, or ChatModel.kt:4233 → `saved_description`;
  - a received forward shows a two-row "forwarded from" header: FIV:327 `else -> false` → `true`;
  - a received forward gets a source arrow: ChatModel.kt:4223 changed to give `Unknown` a target.
- **Steps:**
```kotlin
connect(bob); connectPeers(bob, cath)                                    // Helpers.kt:34
bob sends "@alice plain"
cath sends "@bob secret"; bob.eventuallyMessage("cath> secret", CONNECT_TIMEOUT_MS)
bob types "@alice <- @cath secret"                                       // not `sends`: the echo differs (plan 0.5 line 835)
bob expectsMessage "@alice <- @cath"; bob expects "      secret"           // View.hs:1028, Forward.hs:414-417
row ready("bob", "secret")
val chat = alice.chatList.open("bob"); val plain = chat.itemId("plain"); val fwd = chat.itemId("secret")
check("forwarded" !in textsOf(Ui.Chat.Item(plain))) { "an ordinary message shows a forwarded header" }
poll(what = "the `forwarded` header with no source name") { textsOf(Ui.Chat.Item(fwd)).let { t -> "forwarded" in t && t.none { "forwarded from" in it || "cath" in it } } }
assertFalse("a received forward offers a go-to-source arrow", exists(Ui.Chat.Item(fwd).part("goTo")))
```
- The recipient's item has `CIFFUnknown` (`forwardCIFF`, Commands.hs:1174-1178), so its `chatName` is "" and no arrow is composed.
- **Platform:** none.

### T4 `forwardPlan_filesNotAccepted_alertOptions` (ForwardTest)
- **Scenario.** alice selects four of bob's messages, one of them a file she hasn't downloaded. The plan alert offers Forward, Download and Cancel, and she chooses Forward. She picks cath. bob deletes one of the messages before she sends. cath gets the other two in order, and alice sees "1 messages not forwarded".
- **Fails if:**
  - the plan check is skipped: CV:378 condition → `false`, so no alert;
  - the forward is built from the wrong ids: CV:381/3043 pass `chatItemIds` instead of `forwardPlan.chatItemIds`, or CV:3684 drops the filter;
  - the deleted-after-selection alert is gone: CMP:749-754 removed;
  - the order changes: CV:3684 `.reversed()`.
- **Steps:**
```kotlin
connect(bob); connect(cath)
bob sends "@alice one"; bob.sendsFileTo("alice", bob.createFile("notes.md", NOTES), 1, "notes.md")
bob sends "@alice two"; bob sends "@alice three"; row ready("bob", "three")
val chat = alice.chatList.open("bob")
val file = chat.itemId("the item of notes.md") { it.file?.fileName == "notes.md" }
chat.select(chat.itemId("one"), file, chat.itemId("two"), chat.itemId("three"))
chat.forwardSelected()
poll(what = "the plan alert for 3 messages and 1 file") {
  seesText("Forward 3 message(s)?") && seesText("1 file(s) were not downloaded.", substring = true) &&
    seesText("Forward messages without files?", substring = true) && exists(Ui.Alert.Option(Ui.AlertSource.Shared, "forward")) &&
    exists(Ui.Alert.Option(Ui.AlertSource.Shared, "download")) && seesText("Cancel")
}
click(Ui.Alert.Option(Ui.AlertSource.Shared, "forward"))
poll(what = "the share list for several messages") { seesText("Forward messages…") }
alice.chatList.shareTo("cath")
poll(what = "cath's chat with `Forwarding 3 messages`") { chatModel.chatId.value == alice.chatList.chatId("cath") && seesText("Forwarding 3 messages") && isEnabled(Ui.Chat.Send) }
bob sends "\\@alice three"; bob.eventually("message marked deleted")      // as ChatListRowsTest.previewDeletedMessage
poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's deletion processed by alice") { "marked deleted" in alice.chatList.rowTexts("bob") }
click(Ui.Chat.Send)
cath.expectsMessage("alice> -> forwarded", CONNECT_TIMEOUT_MS); cath expects "      one"
cath expectsMessage "alice> -> forwarded"; cath expects "      two"
poll(what = "the deleted-after-selection alert") { seesText("1 messages not forwarded") && seesText("Messages were deleted after you selected them.") }
click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
chat.send("end"); cath expectsMessage "alice> end"                          // sentinel: no file, no `three`
```
- Strings are from strings.xml:521, 522, 524, 526, 530, 531, 525, 571, 615 and 801.
- The file without text is left out of the plan (`hasContent`, `MCFile t -> t /= ""`, Commands.hs:1085-1094), so the count is 3, not 4. `three` is dropped at send time because it is deleted (`forwardMsgContent`, Commands.hs:5124).
- **Overlap:** this is a superset of K `selectForward_multiple_peerReceives` (`testForwardContactToContactMulti`). Suggest the K design keeps only the parts T4 doesn't cover, or drops it.
- **Platform:**
  - Selecting more items taps the overlay at CV:2238-2242, which lies above the item node. If `tapPointInNode` lands on the file item's own download button (`Ui.Chat.ItemFile`), the overlay still receives the tap because it is drawn on top. Verify on Android.
  - The rows of the chat list behind the chat are composed on Android (BackKeyTest `back_chatToChatList` relies on this), so `rowTexts` works while cath's chat is open.
- **Flake risk:** the deletion must reach alice before Send. The row poll handles this.

### T5 `forwardPlan_downloadButton_receivesFiles` (ForwardTest)
- **Scenario.**
  - Forward is offered for a live text message but not for a message bob deleted, nor for a file alice hasn't downloaded.
  - Forwarding the file alone says "Nothing to forward!" and offers Download.
  - Download receives the file without opening the share list. The file's menu now offers Forward, and cath receives the forwarded file through XFTP.
- **Fails if:**
  - Forward is offered for deleted items: CIV:474 `cItem.meta.itemDeleted == null` removed;
  - Forward is offered for files that aren't loaded: CIV:475 removed;
  - Download doesn't call `receiveFiles`: CV:3020-3026 removed;
  - Download also opens the share list: `forwardContent` added to the Download button;
  - the file isn't copied into the forward: core regression, caught by cath's lines.
- **Steps:**
```kotlin
connect(bob); connect(cath)
bob sends "@alice kept"; bob sends "@alice gone"; bob sends "\\@alice gone"; bob.eventually("message marked deleted")
bob.sendsFileTo("alice", bob.createFile("notes.md", NOTES), 1, "notes.md"); row ready("bob", "notes.md")
val chat = alice.chatList.open("bob"); val bobId = alice.chatList.chatId("bob")
val kept = chat.itemId("kept"); val gone = chat.itemId("gone")
val file = chat.itemId("the item of notes.md") { it.file?.fileName == "notes.md" }
menuCheck(kept) { exists(Ui.Chat.ItemMenu(kept, "forward")) }
menuCheck(gone) { !exists(Ui.Chat.ItemMenu(gone, "forward")) }
rightClick(Ui.Chat.Item(file)); waitFor(Ui.Chat.ItemMenu(file, "select"))
check(exists(Ui.Chat.ItemMenu(file, "download")) && !exists(Ui.Chat.ItemMenu(file, "forward"))) { "menu of the offered file" }
click(Ui.Chat.ItemMenu(file, "select")); chat.forwardSelected()
poll(what = "`Nothing to forward!` with Download and no Forward") {
  seesText("Nothing to forward!") && seesText("1 file(s) were not downloaded.") && !seesText("Forward messages without files?", substring = true) &&
    exists(Ui.Alert.Option(Ui.AlertSource.Shared, "download")) && !exists(Ui.Alert.Option(Ui.AlertSource.Shared, "forward"))
}
click(Ui.Alert.Option(Ui.AlertSource.Shared, "download"))
poll(what = "the alert closed, no share list") { !seesText("Nothing to forward!") && !seesText("Forward message…") && chatModel.chatId.value == bobId }
poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "notes.md received") { chat.item(file)?.file?.fileStatus is CIFileStatus.RcvComplete }
escape(); poll(what = "selection mode to end") { !exists(Ui.Chat.Selection.part("title")) }
chat.forwardItemTo(file, "cath")                                          // the menu now has "forward": the opposite branch of CIV:475
cath.expectsMessage("alice> -> forwarded", CONNECT_TIMEOUT_MS); cath expects "      notes_1.md"
cath expectsMessage "alice> sends file notes_1.md (${NOTES.length} bytes / ${NOTES.length} bytes)"
cath expects "use /fr 1 [<dir>/ | <path>] to receive it"
cath.receivesFile(1, "notes_1.md", "alice")
```
- `menuCheck(id, cond)` is a private test helper: `rightClick`, `waitFor(ItemMenu(id, "select"))`, `check(cond)`, click "select", `escape()`, then poll until selection mode ends (the pattern of FilesTest.kt:78-84).
- **Forwarded file name.** The core copies the file into alice's files folder under a unique name (`uniqueCombine`, Commands.hs:1189). `mediaFilePrefix` is `Nothing` for `MCFile`, so the name is `notes_1.md`. For a forward of a file with no text, the message line shows the item's file name (View.hs:795), as in `testForwardFileContactToContact` (Forward.hs:599-604).
- **Size.** The size is the content size (`getFileContentsSize`, Commands.hs:5147), not the encrypted size.
- **Platform:**
  - `escape()` ends selection mode on both platforms (FilesTest, BackKeyTest `back_exitsSelectionMode`).
  - `receivesFile` saves into the peer's host directory. On Android this goes through the bridge `file` command, so no bytes are compared on the device. alice's received bytes are already compared by FilesTest.
- **Flake risk:** XFTP upload and download time. All file waits use `CONNECT_TIMEOUT_MS`.

### T6 `forward_destinationProhibitsContent_alert` (ForwardTest)
- **Scenario.**
  - With files on in bob's group "team", alice can forward bob's image there; she cancels.
  - After bob turns files off, picking "team" shows "Cannot send message", and the group chat doesn't open.
  - The same image can still go to Private notes, and nothing ever reached the group.
- **Fails if:**
  - the check ignores the group setting: SLN:48 → `isMediaOrFileAttachment`, so step A shows the alert;
  - the check is lost: SLN:48 → `false`, or SLN:54-58 always opens the chat;
  - the alert opens the chat anyway: SLN:55 followed by `groupChatAction`.
- **Steps:**
```kotlin
connect(bob); hostGroup(bob, "team", "alice"); joinGroupFromList(alice, bob, "team")        // Helpers.kt:42, 69
bob sends "/img @alice $FIXTURE_JPG"; bob expectsMessage "/f @alice $FIXTURE_JPG"
bob expects "use /fc 1 to cancel sending"; bob.eventually("completed uploading file 1 (test.jpg) for alice", CONNECT_TIMEOUT_MS)   // FilesTest.kt:55-58 (there file 2)
backToChatList(); val chat = alice.chatList.open("bob"); val teamId = alice.chatList.chatId("team")
val img = chat.itemId("bob's image") { it.content.msgContent is MsgContent.MCImage }
poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the image received (Forward needs a loaded file)") { chat.item(img)?.file?.fileStatus is CIFileStatus.RcvComplete }
rightClick(Ui.Chat.Item(img)); click(Ui.Chat.ItemMenu(img, "forward")); alice.chatList.shareTo("team")
poll(what = "team open with the bar while files are on") { chatModel.chatId.value == teamId && seesDescription("Context icon") && isEnabled(Ui.Chat.Send) }
click(described("Cancel")); poll(what = "the bar gone") { exists(Ui.Chat.Message) && !seesDescription("Context icon") }
bob sends "/set files #team off"; bob expects "updated group preferences:"; bob expects "Files and media: off"      // Files.hs:1248-1251 (there alice is the owner)
chat.itemId("`Files and media: off` in team") { it.content.text.startsWith("Files and media: off") }             // ChatModel.kt:4311, 4361-4368
backToChatList(); alice.chatList.open("bob")
rightClick(Ui.Chat.Item(img)); click(Ui.Chat.ItemMenu(img, "forward")); alice.chatList.shareTo("team")
poll(what = "`Cannot send message`") { seesText("Cannot send message") && seesText("Selected chat preferences prohibit this message.") && chatModel.chatId.value == null }
click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
poll(what = "still in the share list") { !seesText("Cannot send message") && seesText("Forward message…") && chatModel.chatId.value == null }
alice.chatList.shareTo("Private notes")
poll(what = "notes with the bar") { chatModel.chatId.value == alice.chatList.chatId("Private notes") && seesDescription("Context icon") }
click(Ui.Chat.Send)
val saved = chat.itemId("the image saved in notes") { it.content.msgContent is MsgContent.MCImage }
poll(what = "`saved from` / `bob` on the saved image") { textsOf(Ui.Chat.Item(saved)).let { t -> t.any { "saved from" in it } && "bob" in t } }
backToChatList(); alice.chatList.open("team").send("after")
bob expectsMessage "#team alice> after"                                   // the next line: nothing reached #team before it
```
- Strings: strings.xml:574-575 and 570.
- The alert is `showForwardProhibitedByPrefAlert` (SLN:75-80), and its OK carries `Ui.Alert.Confirm(Shared)`.
- **Platform:** `joinGroupFromList` leaves alice in team's chat, so the test calls `backToChatList()` before opening bob's chat.
- **Flake risk:** "Forward" is offered only once the image is loaded (CIV:475). The test polls for that first.

### T7 `forward_searchDestinations_filtersList` (ForwardTest, new catalogue row)
- **Scenario.** With eight chats, alice opens the forward list, taps Search and types "bob". Only bob and the group "bobcats" remain. The group "jobs" (which contains "ob"), the group "team" and Private notes disappear. Picking "bobcats" opens it with the forwarding bar.
- **Fails if:**
  - the search text is ignored: SLV:201 passes `""`;
  - the button disappears: the SLV:166 threshold is raised (the test has exactly 8 chats);
  - groups or contacts are excluded from matching: `filteredChats` (`views/chatlist/ChatListView.kt:1600-1608`).
- **Steps:**
```kotlin
app("alice"); newGroupsViaApi("bobcats", "jobs", "team", "club", "news", "work")
val alice = restart()                                                       // loads the groups into the model
val bob = cli("bob"); connect(bob)
bob sends "@alice hey"; row ready("bob", "hey")
check(chatModel.chats.value.size == 8) { "precondition: 8 chats for the search button (SLV:166)" }
alice.chatList.open("bob").menu("hey", "forward")
poll(what = "the share list") { seesText("Forward message…") }
for (n in listOf("bob", "bobcats", "jobs", "team", "Private notes")) scrollTo(Ui.Share.Row(alice.chatList.chatId(n)))   // all listed first
alice.chatList.shareSearch("bob")
poll(what = "only bob and bobcats listed for `bob`") {
  alice.chatList.shareShown("bob") && alice.chatList.shareShown("bobcats") &&
    !alice.chatList.shareShown("jobs") && !alice.chatList.shareShown("team") && !alice.chatList.shareShown("Private notes")
}
alice.chatList.shareTo("bobcats")
poll(what = "bobcats opened with the bar") { chatModel.chatId.value == alice.chatList.chatId("bobcats") && seesDescription("Context icon") }
```
- The open-chat exemption in `filteredChats` (:1600) doesn't apply, because `chatId` is null while sharing.
- The empty groups are listed because the creator's membership is active (ChatModel.kt:2530) and sending is allowed.
- **Catalogue row to add (§L):** `| Destinations | Search the destination list (shown with 8 or more chats) | "Forward" → Search icon → type | SLV:166-170, 198-202; ChatListView.kt:1584-1612 | forward_searchDestinations_filtersList | … as above … | P | groups created via the API + restart, as ChatListToolbarSearchTest |`.
- **Platform:** on Android the share list fills the screen and on desktop it replaces the chat list (`App.kt:376-384`). Either way there is one "Search" button.

### T8 `draft_onlyLastChatKept` (DraftTest)
- **Scenario.**
  - alice types in bob's chat, then in cath's chat, then opens notes. Only cath's row shows a draft.
  - bob's chat opens with an empty field, and cath's draft comes back.
  - Sending it delivers it, and cath's row then shows the sent message, not the draft.
- **Fails if:**
  - the draft isn't saved on switch: CMP:1427-1430 removed;
  - it isn't restored: CMP:1432-1433 removed;
  - one chat's draft shows in another chat: CMP:1432 condition reduced to `chatModel.draft.value != null`;
  - a sent draft stays in the model and on the row: CMP:836-838 removed. Visible because the row reads the draft whenever `draftChatId == chat.id` (ChatPreviewView.kt:231), even while the chat is open.
- **Steps:**
```kotlin
connect(bob); connect(cath); val bobId = alice.chatList.chatId("bob"); val cathId = alice.chatList.chatId("cath")
alice.chatList.open("bob"); typeInto(Ui.Chat.Message, "for bob")
backToChatList(); alice.chatList.open("cath")
poll(what = "cath's field empty: bob's draft not carried over") { chatModel.chatId.value == cathId && editableText(Ui.Chat.Message).isNullOrEmpty() }
typeInto(Ui.Chat.Message, "for cath")
switchAwayFromChat(alice)                                                  // ChatListRowsScreens.kt:7
poll(what = "a draft on cath's row only") {
  alice.chatList.rowTexts("cath").any { it.endsWith(" for cath") } && alice.chatList.rowTexts("bob").none { it.endsWith(" for bob") }
}
alice.chatList.open("bob")
poll(what = "bob's field empty: only the last draft is kept") { chatModel.chatId.value == bobId && editableText(Ui.Chat.Message).isNullOrEmpty() }
backToChatList(); val chat = alice.chatList.open("cath")
poll(what = "cath's draft restored") { editableText(Ui.Chat.Message) == "for cath" }
chat.sendComposed()
cath.eventuallyMessage("alice> for cath", CONNECT_TIMEOUT_MS)
poll(what = "cath's row with the sent text, no draft") { alice.chatList.rowTexts("cath").let { "for cath" in it && it.none { t -> t != "for cath" && t.endsWith(" for cath") } } }
```
- The draft row form (an icon placeholder, then " text") is the one `previewShowsDraft` uses (ChatListRowsTest.kt:50-53). That test (catalogue 01) checks only the positive row; T8 adds the negative control and the restore.
- **Platform:** on Android every switch goes through the list (`backToChatList`). The final row check reads composed but covered rows while cath's chat is open; no Back is pressed, so `KeyChangeEffect` cannot hide the CMP:837 mutation.

### T9 `draft_withQuote_restored` (DraftTest)
- **Scenario.** A reply in progress and an edit in progress each survive a visit to another chat, and each is sent as a reply or an edit.
- **Fails if** the saved draft drops its context item, e.g. CMP:1428 → `composeState.value.copy(contextItem = ComposeContextItem.NoContextItem)`. bob would then get plain `alice> answer` and a new `alice> hello world` instead of a quote and `[edited]`.
- **Steps:**
```kotlin
connect(bob); bob sends "@alice question"; row ready("bob", "question")
val chat = alice.chatList.open("bob"); chat.send("hello"); bob expectsMessage "alice> hello"
check(!seesDescription("Context icon")) { "a context bar before Reply" }
chat.menu("question", "reply"); typeInto(Ui.Chat.Message, "answer")
visitNotesAndBack(alice, "bob")
poll(what = "the reply draft restored") { editableText(Ui.Chat.Message) == "answer" && seesDescription("Context icon") }
chat.sendComposed()
bob expectsMessage "alice> > question"; bob expects "      answer"         // as MessagingTest.reply_menu_sendQuoted_peerSeesQuote
chat.menu("hello", "edit"); poll(what = "the edit text") { editableText(Ui.Chat.Message) == "hello" }
replaceText(Ui.Chat.Message, "hello world")
visitNotesAndBack(alice, "bob")
poll(what = "the edit draft restored") { editableText(Ui.Chat.Message) == "hello world" && seesDescription("Context icon") }
chat.sendComposed()
bob expectsMessage "alice> [edited] hello world"
```
- The quote direction is `>`, because bob's received item quotes his own sent message (View.hs:1019).
- `visitNotesAndBack` also checks that notes shows neither the field text nor a bar (no leak).
- **Platform:** none beyond `backToChatList`.

### T10 `draft_clearedWhenSendDisabled` (DraftTest)
- **Scenario.**
  - bob deletes alice while she is typing in his chat. Her text is cleared and isn't kept as a draft.
  - A draft in cath's chat survives a visit to bob's chat, where sending is impossible.
- **Fails if:**
  - the compose isn't cleared: CMP:1446 removed;
  - the draft isn't dropped: CMP:1445 removed. Leaving bob's chat then saves "for bob" to his row;
  - the effect at CMP:1443-1448 is moved above the `KeyChangeEffect` at CMP:1395. `clearState()` then empties the compose before it is saved, and CMP:1435 drops cath's draft. This is the regression the comment at CMP:1441-1442 describes; it is caught on desktop only (see Platform).
- **Steps:**
```kotlin
connect(bob); connect(cath); val bobId = alice.chatList.chatId("bob"); val cathId = alice.chatList.chatId("cath")
alice.chatList.open("bob"); typeInto(Ui.Chat.Message, "for bob")
check(editableText(Ui.Chat.Message) == "for bob")
bob sends "/d alice"; bob expects "alice: contact is deleted"              // View.hs:235, Direct.hs:585-586
poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the field cleared once sending is impossible") {
  !alice.chatList.chat("bob").chatInfo.sendMsgEnabled && exists(Ui.Chat.Message) && editableText(Ui.Chat.Message).isNullOrEmpty()
}
backToChatList(); alice.chatList.open("cath")
poll(what = "no draft on bob's row") { chatModel.chatId.value == cathId && alice.chatList.rowTexts("bob").none { it.endsWith(" for bob") } }
typeInto(Ui.Chat.Message, "for cath")
backToChatList(); alice.chatList.open("bob"); poll(what = "bob's chat open") { chatModel.chatId.value == bobId }
backToChatList(); alice.chatList.open("cath")
poll(what = "cath's draft survives the visit to bob's chat") { editableText(Ui.Chat.Message) == "for cath" }
```
- **Overlap:** catalogue 01 `contactDeletedByPeerShowsDeleted` (not done) runs the same `/d alice` and checks the "contact deleted" field. That test should not also check the draft.
- **Platform:** desktop reuses one ComposeView across chat switches (comment at CMP:1458), so the effect order matters there. On Android each chat composes a fresh ComposeView, the order mutation doesn't apply, and the test still passes. Name the platform in the PR.

### T11 `draft_withAttachment_restored` (desktop `chat.simplex.app.ui.desktop.DraftDesktopTest`)
- **Scenario.** alice attaches a file by pasting its path in bob's chat, switches to notes and back. The file preview is restored, and sending it delivers the file.
- **Fails if** the saved draft loses its preview, e.g. CMP:1428 → `composeState.value.copy(preview = ComposePreview.NoPreview)`, or the attached file is deleted on switch (a `deleteUnusedFiles()` added to CMP:1421-1431).
- **Steps** (pattern of `FilesDesktopTest`):
```kotlin
connect(bob); val source = File(createTempDirectory("uitest-draft").toFile(), "report.txt").apply { writeText(REPORT) }
alice.chatList.open("bob"); waitFor(Ui.Chat.Message); check(!seesDescription("Cancel file preview"))
enterText(Ui.Chat.Message, source.absolutePath)
poll(what = "the preview of report.txt") { seesDescription("Cancel file preview") && seesText("report.txt") }
alice.chatList.open("Private notes")
poll(what = "notes without the preview") { chatModel.chatId.value == alice.chatList.chatId("Private notes") && !seesDescription("Cancel file preview") }
alice.chatList.open("bob")
poll(what = "the preview restored") { seesDescription("Cancel file preview") && seesText("report.txt") }
click(Ui.Chat.Send)
bob.eventuallyMessage("alice> sends file report.txt (${REPORT.length} bytes / ${REPORT.length} bytes)", CONNECT_TIMEOUT_MS)
bob expects "use /fr 1 [<dir>/ | <path>] to receive it"
```
- Strings: "Cancel file preview" (strings.xml:604).
- Do not use `!seesText("report.txt")` in notes. bob's row shows the file draft as `report.txt` (ChatPreviewView.kt:105-106), so only the content description is specific to the compose area.
- **Why desktop-only:** pasting a path into the field is desktop code (`D/platform/PlatformTextField.desktop.kt:99-107`). Android has no equivalent without the file picker (FD).

## 6. Findings

Suspected app bugs. They are reported only, never fixed in a test PR. Each needs a confirming run before it goes into §15.5.
1. **A draft is lost when forwarding into its chat and then leaving without sending.**
   - CV:154 hides the draft while forwarding.
   - If alice cancels the bar (CMP:1268-1270 clears only the context item) and leaves, the empty compose takes the `else` branch: `clearPrevDraft(prevChatId)` at CMP:1434-1436 deletes the draft.
   - If she leaves with the bar still open, CMP:1421-1431 saves the forwarding state over her typed draft.
   - Expected: the draft returns, as it does after a successful forward (CMP:997-999).
   - Once confirmed, keep a test that exposes it: `draft_cancelForwardIntoChatWithDraft_keepsDraft` with `@KnownAppIssue`. T2 avoids the path by cancelling before the draft exists.
2. **The forward list checks only voice for direct contacts** (SLN:31-44), but files and SimpleX links for groups (SLN:47-50). A contact whose Files preference is off (bots: CMP:1355 hides attach then) is not marked or blocked, so the user gets whatever the core returns instead of "Cannot send message". Not verified against the core.
3. **Cosmetic:** a draft in a chat whose sending becomes impossible while another chat is open stays on that chat's row until the chat is opened. CMP:1443-1448 runs only for the open chat. Not verified.

Harness finding (for §15.4): `restart()` restarts only the core (desktop UiTest.kt:93-103). In-memory model state such as `ChatModel.draft` survives it, but a real relaunch loses it. A "persists across restart" check of model-only state would pass falsely. Add this to the guide's Persistence paragraph.

## 7. Decisions for you

1. Merges across catalogue names (T1, T2, T4, T5, T8, T9). Each removes names from the catalogue and the Count/class map is regenerated. The alternative is separate tests at about one reset plus 1–2 peer connections each. Recommended: merge.
2. Two placement questions:
   - T2 takes the §S row `draft_forwardIntoChatWithDraft_restoresDraftAfterSend` into ForwardTest. Alternative: keep it in DraftTest as its own test (bob and cath again).
   - T1 needs a new name (`forward_acrossChatTypes_peersReceiveAndSourceOpens`). Alternative: keep `forward_contactToGroup` as the name.
3. T4 makes K `selectForward_multiple_peerReceives` redundant. Should the K design drop it?
4. `forward_groupToGroup` dropped, the live half of `forward_notOffered_forDeletedOrLive` dropped, and `draft_voiceRecording_finishedOnSwitch` deferred until the REC seam exists.

## 8. Catalogue corrections (02, lines at master `d72f68e6d` → what to write)

- L164: "CMP:727-755" → "forwardItem CMP:729-757 (worktree)". Comment quoting "CMP:843-848" → "CMP:845-855". Cancel "CMP:1266-1268" → "CMP:1268-1270, the (x) is `described("Cancel")`, ContextItemView.kt:148-151".
- L167: replace "Hover → arrow button" with "the arrow (CIV:296-303) is composed whenever the item has a source; hover (desktop) or tapping the bubble (Android, CIV:334-336) only changes how it looks".
- L169: "CV:3695-3753, 3002" → "CV:3698-3756 (alert), 3004-3051 (Download / Forward buttons)". With an undownloaded file without text, the title counts only the forwardable items ("Forward 3 message(s)?" for 4 selected, core `hasContent`, Commands.hs:1085-1094). "Nothing to forward!" shows "Cancel" and "Download" and no "Forward".
- L170: "CIV:470-478" → "CIV:474-482". "CMP:747-752" → "CMP:749-754". The alert title is "%1$s messages not forwarded" ("1 messages not forwarded").
- L171: "strings.xml:573-574" → "574-575".
- S237: "CMP:1381-1467" → "CMP:1395-1481". "996-1010" → "997-1012". "C/model/ChatModel.kt:1696" → ":1711". "CMP:1429-1434" → "CMP:1443-1448". "CMP:995-996" → "CMP:997-999". "CMP:1407-1422" → "CMP:1421-1435". Add: drafts are held in memory only (ChatModel.kt:255-257) and do not survive an app restart.
- S238: Needs "P, CB" → "P, XFTP". Trigger: "enter the file's absolute path into the compose field (PlatformTextField.desktop.kt:99-107)", not Ctrl+V. The test is desktop-only.
- S239: note "blocked: REC seam not built (plan §0.5.11)".
- §L: add the T7 row (§5 T7). Add "draft cleared after send" to the S237 Feature cell.
