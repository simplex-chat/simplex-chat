package chat.simplex.app.ui.desktop

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.PeerFlags
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.openTimedMenu
import chat.simplex.app.ui.harness.receivedTtlSeconds
import chat.simplex.common.model.shortTimeText
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertTrue
import org.junit.Test

class TimedMessageDesktopTest : UiTest() {
  @Test fun timed_customTime_sendsWithChosenTtl() = uiTest {
    val alice = app("alice"); val bob = cli("bob", PeerFlags(markRead = true))
    connect(bob)
    val chat = alice.chatList.open("bob")
    typeInto(Ui.Chat.Message, "custom")
    openTimedMenu()
    click(Ui.Chat.SendTimed.option("custom"))
    poll(what = "the `Delete after` picker at the default 5 minutes with `Send`") {
      seesText("Delete after") && exists(Ui.TimePicker.Confirm) && "5" in textsOf(Ui.TimePicker.Value)
    }
    click(Ui.TimePicker.Value)
    click(Ui.TimePicker.Value.option(CUSTOM_TTL))
    poll(what = "2 (minutes) picked") { "2" in textsOf(Ui.TimePicker.Value) }
    check(storedSetting(PREF) != "$CUSTOM_TTL") { "the custom time is stored as $CUSTOM_TTL before it is sent" }
    click(Ui.TimePicker.Confirm)
    bob.expectsMessage("alice> custom", CONNECT_TIMEOUT_MS)
    val id = chat.itemId("custom")
    poll(what = "alice's `custom` with a $CUSTOM_TTL s timer and `${shortTimeText(CUSTOM_TTL)}` in its meta") {
      chat.item(id)?.meta?.itemTimed?.ttl == CUSTOM_TTL && shortTimeText(CUSTOM_TTL) in textsOf(Ui.Chat.Item(id))
    }
    val ttl = bob.receivedTtlSeconds("alice", "custom")
    assertTrue("bob's `custom` is deleted $ttl s after it was sent, expected about $CUSTOM_TTL s", ttl in (CUSTOM_TTL - 2L)..(CUSTOM_TTL + 10L))
    poll(what = "the custom time stored as $CUSTOM_TTL s") { storedSetting(PREF) == "$CUSTOM_TTL" }
  }

  private companion object {
    const val CUSTOM_TTL = 120
    const val PREF = "CustomDisappearingMessageTime"
  }
}
