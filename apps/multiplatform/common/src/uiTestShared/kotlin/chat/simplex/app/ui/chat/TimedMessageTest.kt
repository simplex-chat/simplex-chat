package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.PeerFlags
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitGoneFromPeer
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.openTimedMenu
import chat.simplex.app.ui.harness.receivedTtlSeconds
import chat.simplex.common.model.ChatFeature
import chat.simplex.common.model.shortTimeText
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class TimedMessageTest : UiTest() {
  @Test fun timed_send30s_bothSidesDelete() = uiTest {
    val alice = app("alice"); val bob = cli("bob", PeerFlags(markRead = true))
    connect(bob)
    val chat = alice.chatList.open("bob")
    typeInto(Ui.Chat.Message, "hello!")
    assertFalse("the disappearing message menu is open before the send button is right-clicked", exists(Ui.Chat.SendTimed))
    openTimedMenu()
    assertTrue("`1 minute` and `5 minutes` in the disappearing message menu", seesText("1 minute") && seesText("5 minutes"))
    click(Ui.Chat.SendTimed.option(30))
    bob.expectsMessage("alice> hello!", CONNECT_TIMEOUT_MS)
    val id = chat.itemId("hello!")
    poll(what = "alice's `hello!` with a 30 s timer and `${shortTimeText(30)}` in its meta") {
      chat.item(id)?.meta?.itemTimed?.ttl == 30 && shortTimeText(30) in textsOf(Ui.Chat.Item(id))
    }
    val ttl = bob.receivedTtlSeconds("alice", "hello!")
    assertTrue("bob's `hello!` is deleted $ttl s after it was sent, expected about 30 s", ttl in 28L..40L)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`hello!` to disappear from alice's chat 30 s after it was sent") {
      chat.item(id) == null && !exists(Ui.Chat.Item(id))
    }
    awaitGoneFromPeer(bob, "alice", "alice> hello!")
  }

  @Test fun timed_menuHidden_whenFeatureOff() = uiTest {
    val alice = app("alice"); val bob = cli("bob", PeerFlags(markRead = true))
    connect(bob)
    val chat = alice.chatList.open("bob")
    typeInto(Ui.Chat.Message, "allowed")
    openTimedMenu()
    click(Ui.Chat.SendTimed.option(30))
    bob.expectsMessage("alice> allowed", CONNECT_TIMEOUT_MS)
    chat.itemId("allowed")
    bob sends "/set disappear @alice no"
    bob expects "you updated preferences for alice:"
    bob.eventuallyMatching(Regex("Disappearing messages: off \\(you allow: no.*"))
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's preference change to turn disappearing messages off for alice") {
      !alice.chatList.chat("bob").chatInfo.featureEnabled(ChatFeature.TimedMessages)
    }
    typeInto(Ui.Chat.Message, "not allowed")
    rightClick(Ui.Chat.Send)
    poll(what = "the send menu with `Send live message` and without `Disappearing message`") {
      exists(Ui.Chat.SendLive) && !exists(Ui.Chat.SendTimed)
    }
  }

  @Test fun timed_meta_showsTimerAndTtl() = uiTest {
    val alice = app("alice"); val bob = cli("bob", PeerFlags(markRead = true))
    connect(bob)
    val chat = alice.chatList.open("bob")
    bob sends "/set disappear @alice yes $CHAT_TTL"
    bob expects "you updated preferences for alice:"
    bob.eventuallyMatching(Regex("Disappearing messages: enabled .*"))
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's $CHAT_TTL s timer to reach alice") { alice.chatList.chat("bob").chatInfo.timedMessagesTTL == CHAT_TTL }
    chat.send("chat timer")
    bob.expectsMessage("alice> chat timer", CONNECT_TIMEOUT_MS)
    typeInto(Ui.Chat.Message, "own timer")
    openTimedMenu()
    click(Ui.Chat.SendTimed.option(30))
    bob.expectsMessage("alice> own timer", CONNECT_TIMEOUT_MS)
    val chatTimer = chat.itemId("chat timer")
    val ownTimer = chat.itemId("own timer")
    poll(what = "the chat timer item without a TTL in its meta and the own timer item with `${shortTimeText(30)}`") {
      chat.item(chatTimer)?.meta?.itemTimed?.ttl == CHAT_TTL && chat.item(ownTimer)?.meta?.itemTimed?.ttl == 30 &&
        textsOf(Ui.Chat.Item(chatTimer)).none { it == shortTimeText(CHAT_TTL) || it == shortTimeText(30) } &&
        shortTimeText(30) in textsOf(Ui.Chat.Item(ownTimer))
    }
    val chatTtl = bob.receivedTtlSeconds("alice", "chat timer")
    assertTrue("bob's `chat timer` is deleted $chatTtl s after it was sent, expected about $CHAT_TTL s", chatTtl in (CHAT_TTL - 2L)..(CHAT_TTL + 10L))
    val ownTtl = bob.receivedTtlSeconds("alice", "own timer")
    assertTrue("bob's `own timer` is deleted $ownTtl s after it was sent, expected about 30 s", ownTtl in 28L..40L)
  }

  private companion object {
    const val CHAT_TTL = 120
  }
}
