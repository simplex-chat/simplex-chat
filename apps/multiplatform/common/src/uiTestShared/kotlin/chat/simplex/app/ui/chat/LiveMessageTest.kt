package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.CliPeer
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.described
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertFalse
import org.junit.Test

class LiveMessageTest : UiTest() {
  @Test fun liveMessage_menuStart_alertConfirm_peerSeesStarted() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "/show on"
    bob expects "ok"
    val chat = alice.chatList.open("bob")
    typeInto(Ui.Chat.Message, "hello")
    rightClick(Ui.Chat.Send)
    click(Ui.Chat.SendLive)
    confirmLiveMessageAlert()
    bob.expectsLiveStarted("hello")
    val id = chat.itemId("hello")
    poll(what = "alice's `hello` marked LIVE") { "LIVE" in textsOf(Ui.Chat.Item(id)) }
    typeInto(Ui.Chat.Message, " there,")
    bob.expectsMessage("alice> [LIVE] hello there,", CONNECT_TIMEOUT_MS)
    poll(what = "alice's bubble updated in place to `hello there,`, still LIVE") {
      chat.item(id)?.content?.text == "hello there," && textsOf(Ui.Chat.Item(id)).let { t -> "LIVE" in t && t.any { "hello there," in it } }
    }
    typeInto(Ui.Chat.Message, " bob")
    click(Ui.Chat.Send)
    bob.eventuallyMessage("alice> [LIVE ended] hello there, bob", CONNECT_TIMEOUT_MS)
    poll(what = "the field cleared and alice's bubble showing `hello there, bob` without LIVE") {
      editableText(Ui.Chat.Message).isNullOrEmpty() &&
        chat.item(id)?.let { it.content.text == "hello there, bob" && !it.meta.isLive } == true && "LIVE" !in textsOf(Ui.Chat.Item(id))
    }
    chat.send("bye")
    bob expectsMessage "alice> bye"
  }

  @KnownAppIssue("15.5 item 13", "[LIVE started] use /show", intermittent = true)
  @Test fun liveMessage_boltButton_emptyStart() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.open("bob")
    click(Ui.Chat.Live)
    confirmLiveMessageAlert()
    poll(what = "an empty live message: `Cancel live message` in place of the live button and a placeholder item") {
      exists(CANCEL) && !exists(Ui.Chat.Live) && chatModel.chatsContext.chatItems.value.any { it.isLiveDummy }
    }
    click(CANCEL)
    poll(what = "the live button back and the placeholder item gone") {
      exists(Ui.Chat.Live) && !exists(CANCEL) && chatModel.chatsContext.chatItems.value.none { it.isLiveDummy }
    }
    click(Ui.Chat.Live)
    poll(what = "a second empty live message") { exists(CANCEL) }
    assertFalse("the `Live message!` alert was shown again", seesText("Live message!"))
    typeInto(Ui.Chat.Message, "hi!")
    bob.expectsLiveStarted("hi!")
    click(Ui.Chat.Send)
    bob.expectsMessage("alice> [LIVE ended] hi!", CONNECT_TIMEOUT_MS)
  }

  @Test fun liveMessage_received_showsLiveBadge() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    val aliceId = bob.contactId("alice")
    bob sends "/_send @$aliceId live=on text hello"
    val id = chat.itemId("hello")
    poll(what = "bob's `hello` marked LIVE") { "LIVE" in textsOf(Ui.Chat.Item(id)) && chat.item(id)?.meta?.isLive == true }
    bob sends "/last_item_id @alice"
    val bobItemId = bob.eventuallyMatching(Regex("\\d+")).value
    bob sends "/_update item @$aliceId $bobItemId live=on text hello there"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the same bubble showing `hello there`, still LIVE") {
      chat.item(id)?.content?.text == "hello there" && textsOf(Ui.Chat.Item(id)).let { t -> "LIVE" in t && t.any { "hello there" in it } }
    }
    bob sends "/_update item @$aliceId $bobItemId text hello there"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the bubble without LIVE once bob ends the live message") {
      chat.item(id)?.meta?.isLive == false && textsOf(Ui.Chat.Item(id)).let { t -> "LIVE" !in t && t.any { "hello there" in it } }
    }
    chat.receivedItemInfo("hello there").run {
      val h = history()
      val newest = h.indexOfFirst { it.startsWith("hello there") }
      val current = h.indexOf("(current)")
      val first = h.indexOfFirst { it.startsWith("hello") && !it.startsWith("hello there") }
      check(newest in 0 until current && current < first && h.count { it == "(current)" } == 1) { "live message history: $h" }
      close()
    }
  }

  @Test fun liveMessage_chatSwitch_sendsToOriginalChat() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    typeInto(Ui.Chat.Message, "hello")
    rightClick(Ui.Chat.Send)
    click(Ui.Chat.SendLive)
    confirmLiveMessageAlert()
    bob.expectsLiveStarted("hello")
    val id = chat.itemId("hello")
    poll(what = "alice's `hello` marked LIVE") { "LIVE" in textsOf(Ui.Chat.Item(id)) }
    backToChatList()
    val notes = alice.chatList.open("Private notes")
    bob.expectsMessage("alice> [LIVE ended] hello", CONNECT_TIMEOUT_MS)
    poll(what = "the notes with an empty message field") { exists(Ui.Chat.Message) && editableText(Ui.Chat.Message).isNullOrEmpty() }
    assertFalse("the live message was saved in the notes", "hello" in notes.itemTexts())
    backToChatList()
    alice.chatList.open("bob")
    poll(what = "bob's chat with `hello` no longer live and an empty message field") {
      chat.item(id)?.let { it.content.text == "hello" && !it.meta.isLive } == true && "LIVE" !in textsOf(Ui.Chat.Item(id)) &&
        editableText(Ui.Chat.Message).isNullOrEmpty()
    }
    chat.send("bye")
    bob expectsMessage "alice> bye"
  }

  private fun UiScope.confirmLiveMessageAlert() {
    poll(what = "the `Live message!` alert with `Send`") { seesText("Live message!") && "Send" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
  }

  private fun CliPeer.expectsLiveStarted(text: String) {
    val line = nextLine(CONNECT_TIMEOUT_MS)
    val started = Regex("""^(\d{2}:\d{2} )?alice> \[LIVE started] use /show \[on/off/\d+] """ + Regex.escape(text) + "$")
    check(started.matches(line)) { "$name: expected alice's live message `$text` to start, got `$line`; next: ${pending()}" }
  }

  private companion object {
    val CANCEL = described("Cancel live message")
  }
}
