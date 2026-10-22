package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.App
import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.ChatScreen
import chat.simplex.app.ui.harness.CliPeer
import chat.simplex.app.ui.harness.PeerFlags
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.connectPeers
import chat.simplex.app.ui.harness.hostGroup
import chat.simplex.app.ui.harness.joinGroupFromList
import chat.simplex.app.ui.harness.quiesce
import chat.simplex.common.model.CIDirection
import chat.simplex.common.model.CIStatus
import chat.simplex.common.model.ChatItem
import chat.simplex.common.model.MsgReceiptStatus
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Assume.assumeTrue
import chat.simplex.common.platform.appPlatform
import chat.simplex.app.ui.harness.KnownAppIssue
import org.junit.Test

private const val FILLERS = 30
private const val TIMER_S = 10

class ReceivedMessagesTest : UiTest() {
  @Test fun reply_received_showsQuote_clickScrollsToOriginal() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.send("hello! how are you?")
    bob expectsMessage "alice> hello! how are you?"
    val original = chat.itemId("hello! how are you?")
    for (i in 1..FILLERS) bob sends "@alice filler $i"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's $FILLERS filler messages in alice's chat") { chat.itemTexts().contains("filler $FILLERS") }
    bob types ">@alice (hello) all good - you?"
    bob expectsMessage "@alice > hello! how are you?"
    bob expects "      all good - you?"
    val reply = chat.itemId("all good - you?")
    poll(what = "bob's reply bubble quoting `you` / `hello! how are you?` above `all good - you?`") {
      textsOf(Ui.Chat.Item(reply)).let { t -> "you" in t && t.any { "hello! how are you?" in it } && t.any { "all good - you?" in it } }
    }
    assumeTrue("Android: the chat does not reach bob's reply before the quote can be clicked (plan 15.4.1c, open)", !appPlatform.isAndroid)
    hideKeyboard()
    assertFalse("the quoted message is out of view before the quote is clicked", exists(Ui.Chat.Item(original)))
    click(Ui.Chat.Item(reply).part("quote"))
    poll(what = "the quoted message scrolled into view") { exists(Ui.Chat.Item(original)) }
  }

  @Test fun groupRcv_consecutiveMessages_avatarOnce() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    val chat = teamWithCath(alice, bob, cath)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "cath's connection event in alice's group chat") {
      chatModel.chatsContext.chatItems.value.any { it.isFrom("cath") }
    }
    quiesce()
    bob sends "#team welcome"
    val welcome = chat.itemId("welcome")
    cath sends "#team one"
    val one = chat.itemId("one")
    cath sends "#team two"
    val two = chat.itemId("two")
    poll(what = "name and avatar of bob on `welcome` and of cath on `one`, none on `two`") {
      shows(welcome, "bob") && shows(one, "cath") &&
        !exists(Ui.Chat.Item(two).part("member")) && !exists(Ui.Chat.Item(two).part("avatar"))
    }
  }

  @Test fun status_direct_sentThenDelivered() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "alice's contact with bob ready") { alice.chatList.chat("bob").chatInfo.ready }
    bob.stop()
    chat.send("hi")
    val id = chat.itemId("hi")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`hi` sent while bob is offline") { chat.item(id)?.meta?.itemStatus is CIStatus.SndSent }
    bob.start()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`hi` delivered once bob is back") {
      (chat.item(id)?.meta?.itemStatus as? CIStatus.SndRcvd)?.msgRcptStatus == MsgReceiptStatus.Ok
    }
  }

  @Test fun timed_chatPref_receivedMessagesDisappear() = uiTest {
    val alice = app("alice"); val bob = cli("bob", PeerFlags(markRead = true))
    connect(bob)
    bob sends "@alice before"
    val chat = alice.chatList.open("bob")
    val before = chat.itemId("before")
    bob sends "/set disappear @alice yes $TIMER_S"
    bob expects "you updated preferences for alice:"
    bob.eventuallyMatching(Regex("Disappearing messages: enabled .*$TIMER_S sec.*"))
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's $TIMER_S s timer to reach alice") { alice.chatList.chat("bob").chatInfo.timedMessagesTTL == TIMER_S }
    chat.send("hi")
    bob expectsMessage "alice> hi"
    bob sends "@alice hey"
    val hi = chat.itemId("hi")
    val hey = chat.itemId("hey")
    bob sends "/tail @alice 1"
    bob expectsMessage "@alice hey"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`hi` and `hey` to disappear from alice's chat $TIMER_S s after they were read") {
      chat.item(hi) == null && chat.item(hey) == null && !exists(Ui.Chat.Item(hi)) && !exists(Ui.Chat.Item(hey))
    }
    assertTrue("the message sent before the timer was set stays", exists(Ui.Chat.Item(before)))
    bob sends "/tail @alice 1"
    val last = bob.nextLine()
    assertFalse("bob's chat still ends with a timed message: `$last`", last.endsWith("@alice hey") || last.endsWith("alice> hi"))
  }

  @Test fun mention_picker_select_insertsAndSends() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    val chat = teamWithCath(alice, bob, cath)
    typeInto(Ui.Chat.Message, "@")
    click(Ui.Chat.MentionMember(memberId("bob")))
    poll(what = "`@bob ` in the message field") { editableText(Ui.Chat.Message) == "@bob " }
    chat.send("hello")
    bob.eventuallyMessage("#team alice!> @bob hello", CONNECT_TIMEOUT_MS)
    cath.eventuallyMessage("#team alice> @bob hello", CONNECT_TIMEOUT_MS)
  }

  private fun UiScope.teamWithCath(alice: App, bob: CliPeer, cath: CliPeer): ChatScreen {
    connect(bob)
    connectPeers(bob, cath)
    hostGroup(bob, "team", "alice", "cath")
    joinGroupFromList(alice, bob, "team")
    cath sends "/j team"
    cath.eventually("#team: you joined the group")
    bob.eventually("#team: cath joined the group")
    cath.eventually("#team: member alice is connected", CONNECT_TIMEOUT_MS)
    backToChatList()
    return alice.chatList.open("team")
  }

  private fun UiScope.shows(itemId: Long, name: String): Boolean =
    exists(Ui.Chat.Item(itemId).part("avatar")) && exists(Ui.Chat.Item(itemId).part("member")) &&
      name in textsOf(Ui.Chat.Item(itemId).part("member"))

  private fun UiScope.memberId(name: String): Long {
    var id: Long? = null
    poll(what = "member `$name` in the mention picker") {
      id = chatModel.groupMembers.value.firstOrNull { it.memberProfile.displayName == name }?.groupMemberId
      id != null && exists(Ui.Chat.MentionMember(id!!))
    }
    return id!!
  }

  private fun ChatItem.isFrom(name: String) = (chatDir as? CIDirection.GroupRcv)?.groupMember?.memberProfile?.displayName == name

  @KnownAppIssue("15.5 item 25", "all five of bob's messages in the open chat", intermittent = true)
  @Test fun openChat_whileMessagesArrive_showsAll() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val id = alice.chatList.chatId("bob")
    val texts = (1..5).map { "burst $it" }
    texts.forEach { bob types "@alice $it" }
    click(Ui.ChatList.Row(id))
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "all five of bob's messages in the open chat") {
      chatModel.chatId.value == id && chatModel.chatsContext.chatItems.value.map { it.content.text }.containsAll(texts)
    }
  }

}
