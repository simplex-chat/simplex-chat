package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.ChatScreen
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitRowText
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.connectPeers
import chat.simplex.app.ui.harness.described
import chat.simplex.app.ui.harness.hostGroup
import chat.simplex.app.ui.harness.joinGroupFromList
import chat.simplex.app.ui.harness.newGroupsViaApi
import chat.simplex.app.ui.harness.ownGroup
import chat.simplex.app.ui.harness.receivesFile
import chat.simplex.app.ui.harness.sendsFileTo
import chat.simplex.common.model.CIFileStatus
import chat.simplex.common.model.CIForwardedFrom
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.MsgContent
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertFalse
import org.junit.Test

class ForwardTest : UiTest() {
  @Test fun forward_contactToContact_peerSeesForwarded() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    bob sends "@alice hey"
    awaitRowText(alice, "bob", "hey")
    alice.chatList.open("bob").forwardTo("hey", "cath")
    cath.eventuallyMessage("alice> -> forwarded", CONNECT_TIMEOUT_MS)
    cath expects "      hey"
  }

  @Test fun forward_toNotes_showsSavedFrom() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice hey"
    awaitRowText(alice, "bob", "hey")
    val chat = alice.chatList.open("bob")
    chat.forwardTo("hey", "Private notes")
    val notes = alice.chatList.chatId("Private notes")
    poll(what = "the note saved from bob") {
      chatModel.chatId.value == notes && chatModel.chatsContext.chatItems.value.any { (it.meta.itemForwarded as? CIForwardedFrom.Contact)?.chatName == "bob" }
    }
    val id = chat.itemId("hey")
    poll(what = "the `saved from` / `bob` header") { textsOf(Ui.Chat.Item(id)).let { t -> t.any { "saved from" in it } && "bob" in t } }
  }

  @Test fun forward_acrossChatTypes_peersReceiveAndSourceOpens() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    ownGroup(alice, "team", cath)
    backToChatList()
    bob sends "@alice hey"
    awaitRowText(alice, "bob", "hey")
    val chat = alice.chatList.open("bob")
    chat.forwardTo("hey", "team")
    cath.eventuallyMessage("#team alice> -> forwarded", CONNECT_TIMEOUT_MS)
    cath expects "      hey"
    cath sends "#team hello"
    val teamId = alice.chatList.chatId("team"); val bobId = alice.chatList.chatId("bob")
    poll(what = "team open after the forward") { chatModel.chatId.value == teamId }
    val original = chat.itemId("hello")
    chat.forwardTo("hello", "bob")
    bob.eventuallyMessage("alice> -> forwarded", CONNECT_TIMEOUT_MS)
    bob expects "      hello"
    val fwd = chat.itemId("hello")
    poll(what = "the `forwarded` header without the source name") {
      chatModel.chatId.value == bobId && textsOf(Ui.Chat.Item(fwd)).let { t -> "forwarded" in t && t.none { "forwarded from" in it || it == "team" } }
    }
    click(Ui.Chat.Item(fwd).part("goTo"))
    poll(what = "team's chat opened at cath's `hello`") { chatModel.chatId.value == teamId && exists(Ui.Chat.Item(original)) }
    backToChatList()
    val notesId = alice.chatList.chatId("Private notes")
    val notes = alice.chatList.open("Private notes")
    poll(what = "the notes folder open") { chatModel.chatId.value == notesId }
    notes.send("memo")
    notes.forwardTo("memo", "bob")
    bob.expectsMessage("alice> memo", CONNECT_TIMEOUT_MS)
  }

  @Test fun forward_withComment_sendsCommentQuoted() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    bob sends "@alice hey"
    awaitRowText(alice, "bob", "hey")
    val chat = alice.chatList.open("bob")
    val cathId = alice.chatList.chatId("cath")
    chat.menu("hey", "forward")
    alice.chatList.shareTo("cath")
    poll(what = "cath's chat with the forwarding bar") { chatModel.chatId.value == cathId && seesDescription("Context icon") && isEnabled(Ui.Chat.Send) }
    click(described("Cancel"))
    poll(what = "the forwarding bar gone and nothing forwarded") {
      exists(Ui.Chat.Message) && !seesDescription("Context icon") && chatModel.chatsContext.chatItems.value.none { it.content.text == "hey" }
    }
    typeInto(Ui.Chat.Message, "unsent")
    backToChatList()
    alice.chatList.open("bob")
    poll(what = "cath's row showing the draft") { alice.chatList.rowTexts("cath").any { it.endsWith(" unsent") } }
    chat.menu("hey", "forward")
    alice.chatList.shareTo("cath")
    poll(what = "the forwarding bar with an empty field, the draft hidden") {
      chatModel.chatId.value == cathId && seesDescription("Context icon") && editableText(Ui.Chat.Message).isNullOrEmpty()
    }
    typeInto(Ui.Chat.Message, "see this")
    poll(what = "the send button to be enabled") { isEnabled(Ui.Chat.Send) }
    pressEnter(Ui.Chat.Message)
    cath.expectsMessage("alice> -> forwarded", CONNECT_TIMEOUT_MS)
    cath expects "      hey"
    cath expectsMessage "alice> >> hey"
    cath expects "      see this"
    poll(what = "the draft back after the forward") { editableText(Ui.Chat.Message) == "unsent" && !seesDescription("Context icon") }
  }

  @Test fun forwarded_received_showsHeader() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connectPeers(bob, cath)
    bob sends "@alice plain"
    cath sends "@bob secret"
    bob.eventuallyMessage("cath> secret", CONNECT_TIMEOUT_MS)
    bob types "@alice <- @cath secret"
    bob expectsMessage "@alice <- @cath"
    bob expects "      secret"
    awaitRowText(alice, "bob", "secret")
    val chat = alice.chatList.open("bob")
    val plain = chat.itemId("plain")
    val fwd = chat.itemId("secret")
    check(textsOf(Ui.Chat.Item(plain)).let { t -> t.any { "plain" in it } && "forwarded" !in t }) { "an ordinary message shows a forwarded header" }
    poll(what = "the `forwarded` header without the source name") {
      textsOf(Ui.Chat.Item(fwd)).let { t -> "forwarded" in t && t.none { "forwarded from" in it || "cath" in it } }
    }
    assertFalse("a received forward offers a go-to-source arrow", exists(Ui.Chat.Item(fwd).part("goTo")))
  }

  @Test fun forwardPlan_filesNotAccepted_alertOptions() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    bob sends "@alice one"
    bob.sendsFileTo("alice", bob.createFile("notes.md", NOTES), 1, "notes.md")
    bob sends "@alice two"
    bob sends "@alice three"
    awaitRowText(alice, "bob", "three")
    val chat = alice.chatList.open("bob")
    val file = chat.itemId("the item of notes.md") { it.file?.fileName == "notes.md" }
    val two = chat.itemId("two"); val three = chat.itemId("three")
    check(chat.item(file)?.file?.fileStatus is CIFileStatus.RcvInvitation) { "notes.md was received before the forward: ${chat.item(file)?.file?.fileStatus}" }
    chat.select("one")
    chat.toggle(file); chat.toggle(two); chat.toggle(three)
    poll(what = "`Selected 4`") { chat.selectionTitle() == "Selected 4" }
    chat.forwardSelected()
    poll(what = "the plan alert for 3 messages and 1 file with Forward, Download and Cancel") {
      seesText("Forward 3 message(s)?") && seesText("1 file(s) were not downloaded.", substring = true) &&
        seesText("Forward messages without files?", substring = true) && exists(FORWARD) && exists(DOWNLOAD) && seesText("Cancel")
    }
    click(FORWARD)
    poll(what = "the destination list for several messages") { seesText("Forward messages…") }
    alice.chatList.shareTo("cath")
    val cathId = alice.chatList.chatId("cath")
    poll(what = "cath's chat with `Forwarding 3 messages`") { chatModel.chatId.value == cathId && seesText("Forwarding 3 messages") && isEnabled(Ui.Chat.Send) }
    bob sends "\\@alice three"
    bob.eventually("message marked deleted")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's deletion processed by alice") { "marked deleted" in alice.chatList.rowTexts("bob") }
    click(Ui.Chat.Send)
    cath.expectsMessage("alice> -> forwarded", CONNECT_TIMEOUT_MS)
    cath expects "      one"
    cath expectsMessage "alice> -> forwarded"
    cath expects "      two"
    poll(what = "the deleted-after-selection alert") { seesText("1 messages not forwarded") && seesText("Messages were deleted after you selected them.") }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    chat.send("end")
    cath expectsMessage "alice> end"
  }

  @Test fun forwardPlan_downloadButton_receivesFiles() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    bob sends "@alice kept"
    bob sends "@alice gone"
    bob sends "\\@alice gone"
    bob.eventually("message marked deleted")
    bob.sendsFileTo("alice", bob.createFile("notes.md", NOTES), 1, "notes.md")
    awaitRowText(alice, "bob", "notes.md")
    val chat = alice.chatList.open("bob")
    val bobId = alice.chatList.chatId("bob")
    val kept = chat.itemId("kept"); val gone = chat.itemId("gone")
    val file = chat.itemId("the item of notes.md") { it.file?.fileName == "notes.md" }
    menuCheck(chat, kept, "the menu of a message has no Forward") { exists(Ui.Chat.ItemMenu(kept, "forward")) }
    menuCheck(chat, gone, "the menu of a deleted message offers Forward") { !exists(Ui.Chat.ItemMenu(gone, "forward")) }
    rightClick(Ui.Chat.Item(file))
    waitFor(Ui.Chat.ItemMenu(file, "select"))
    check(exists(Ui.Chat.ItemMenu(file, "download")) && !exists(Ui.Chat.ItemMenu(file, "forward"))) { "the menu of a file not downloaded has no Download or offers Forward" }
    click(Ui.Chat.ItemMenu(file, "select"))
    poll(what = "`Selected 1`") { chat.selectionTitle() == "Selected 1" }
    chat.forwardSelected()
    poll(what = "`Nothing to forward!` with Download and no Forward") {
      seesText("Nothing to forward!") && seesText("1 file(s) were not downloaded.") && !seesText("Forward messages without files?", substring = true) &&
        exists(DOWNLOAD) && !exists(FORWARD)
    }
    click(DOWNLOAD)
    poll(what = "the alert closed and no destination list") { !seesText("Nothing to forward!") && !seesText("Forward message…") && chatModel.chatId.value == bobId }
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "notes.md received") { chat.item(file)?.file?.fileStatus is CIFileStatus.RcvComplete }
    escape()
    poll(what = "selection mode to end") { chat.selectionTitle() == null }
    chat.forwardItemTo(file, "cath")
    cath.expectsMessage("alice> -> forwarded", CONNECT_TIMEOUT_MS)
    cath expects "      notes_1.md"
    cath expectsMessage "alice> sends file notes_1.md (${NOTES.length} bytes / ${NOTES.length} bytes)"
    cath expects "use /fr 1 [<dir>/ | <path>] to receive it"
    cath.receivesFile(1, "notes_1.md", "alice")
  }

  @Test fun forward_destinationProhibitsContent_alert() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    hostGroup(bob, "team", "alice")
    joinGroupFromList(alice, bob, "team")
    bob sends "/img @alice $FIXTURE_JPG"
    bob expectsMessage "/f @alice $FIXTURE_JPG"
    bob expects "use /fc 1 to cancel sending"
    bob.eventually("completed uploading file 1 (test.jpg) for alice", CONNECT_TIMEOUT_MS)
    backToChatList()
    val chat = alice.chatList.open("bob")
    val teamId = alice.chatList.chatId("team")
    val img = chat.itemId("bob's image") { it.content.msgContent is MsgContent.MCImage }
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the image received") { chat.item(img)?.file?.fileStatus is CIFileStatus.RcvComplete }
    rightClick(Ui.Chat.Item(img))
    click(Ui.Chat.ItemMenu(img, "forward"))
    alice.chatList.shareTo("team")
    poll(what = "team open with the forwarding bar while files are allowed") { chatModel.chatId.value == teamId && seesDescription("Context icon") && isEnabled(Ui.Chat.Send) }
    click(described("Cancel"))
    poll(what = "the forwarding bar gone") { exists(Ui.Chat.Message) && !seesDescription("Context icon") }
    bob sends "/set files #team off"
    bob expects "updated group preferences:"
    bob expects "Files and media: off"
    chat.itemId("`Files and media: off` in team") { it.content.text.startsWith("Files and media: off") }
    backToChatList()
    alice.chatList.open("bob")
    rightClick(Ui.Chat.Item(img))
    click(Ui.Chat.ItemMenu(img, "forward"))
    alice.chatList.shareTo("team")
    poll(what = "`Cannot send message` and no chat opened") {
      seesText("Cannot send message") && seesText("Selected chat preferences prohibit this message.") && chatModel.chatId.value == null
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "still in the destination list") { !seesText("Cannot send message") && seesText("Forward message…") && chatModel.chatId.value == null }
    alice.chatList.shareTo("Private notes")
    val notesId = alice.chatList.chatId("Private notes")
    poll(what = "notes with the forwarding bar") { chatModel.chatId.value == notesId && seesDescription("Context icon") && isEnabled(Ui.Chat.Send) }
    click(Ui.Chat.Send)
    val saved = chat.itemId("the image saved in notes") { it.content.msgContent is MsgContent.MCImage }
    poll(what = "the `saved from` / `bob` header on the saved image") { textsOf(Ui.Chat.Item(saved)).let { t -> t.any { "saved from" in it } && "bob" in t } }
    backToChatList()
    val team = alice.chatList.open("team")
    poll(what = "team open") { chatModel.chatId.value == teamId }
    team.send("after")
    bob expectsMessage "#team alice> after"
  }

  @Test fun forward_searchDestinations_filtersList() = uiTest {
    app("alice").chatList.row("Private notes")
    val groups = listOf("bobcats", "jobs", "team", "club", "news", "work").take(SEARCH_MIN_CHATS - 1 - chatModel.chats.value.size)
    check(groups.size >= 3) { "too many chats before the test: ${chatModel.chats.value.map { it.id }}" }
    newGroupsViaApi(*groups.toTypedArray())
    val alice = restart()
    poll(what = "the ${groups.size} groups loaded after the restart") { chatModel.chats.value.count { it.chatInfo is ChatInfo.Group } == groups.size }
    val bob = cli("bob")
    connect(bob)
    bob sends "@alice hey"
    awaitRowText(alice, "bob", "hey")
    check(chatModel.chats.value.size == SEARCH_MIN_CHATS) { "$SEARCH_MIN_CHATS chats are needed for the search button, found ${chatModel.chats.value.map { it.id }}" }
    alice.chatList.open("bob").menu("hey", "forward")
    poll(what = "the destination list") { seesText("Forward message…") }
    for (name in listOf("bob", "bobcats", "jobs", "team", "Private notes")) scrollTo(Ui.Share.Row(alice.chatList.chatId(name)))
    alice.chatList.shareSearch("bob")
    poll(what = "only bob and bobcats listed for `bob`") {
      alice.chatList.shareShown("bob") && alice.chatList.shareShown("bobcats") &&
        !alice.chatList.shareShown("jobs") && !alice.chatList.shareShown("team") && !alice.chatList.shareShown("Private notes")
    }
    alice.chatList.shareTo("bobcats")
    val bobcatsId = alice.chatList.chatId("bobcats")
    poll(what = "bobcats opened with the forwarding bar") { chatModel.chatId.value == bobcatsId && seesDescription("Context icon") }
  }

  private fun UiScope.menuCheck(chat: ChatScreen, id: Long, failure: String, cond: () -> Boolean) {
    rightClick(Ui.Chat.Item(id))
    waitFor(Ui.Chat.ItemMenu(id, "select"))
    check(cond()) { failure }
    click(Ui.Chat.ItemMenu(id, "select"))
    poll(what = "`Selected 1`") { chat.selectionTitle() == "Selected 1" }
    escape()
    poll(what = "selection mode to end") { chat.selectionTitle() == null }
  }

  private companion object {
    const val NOTES = "Shopping list from bob: apples and pears"
    const val SEARCH_MIN_CHATS = 8
    const val FIXTURE_JPG = "tests/fixtures/test.jpg"
    val FORWARD = Ui.Alert.Option(Ui.AlertSource.Shared, "forward")
    val DOWNLOAD = Ui.Alert.Option(Ui.AlertSource.Shared, "download")
  }
}
