package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.PeerFlags
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.createChannel
import chat.simplex.app.ui.harness.joinPreparedChannel
import chat.simplex.app.ui.harness.openPreparedChannelFromSearch
import chat.simplex.common.model.CIDirection
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test

class ChannelSubscriberTest : UiTest() {
  @Test fun subscriber_receivesBroadcast() = uiTest {
    val alice = app("alice"); val relay = cli("relay1", PeerFlags(relay = true)); val bob = cli("bob")
    val link = bob.createChannel("team", relay)
    alice.chatList.row("Private notes")
    val chat = openPreparedChannelFromSearch(link, "team")
    joinPreparedChannel("team", bob, relay)
    check(chatModel.chatsContext.chatItems.value.none { it.content.text == "hi" }) { "team shows `hi` before the owner posts it" }
    bob sends "#team hi"
    chat.itemId("`hi` received from the channel") { it.content.text == "hi" && it.chatDir is CIDirection.ChannelRcv }
  }

  @Test fun subscriber_composeDisabled() = uiTest {
    val alice = app("alice"); val relay = cli("relay1", PeerFlags(relay = true)); val bob = cli("bob")
    val link = bob.createChannel("team", relay)
    alice.chatList.row("Private notes")
    openPreparedChannelFromSearch(link, "team")
    poll(what = "the banner `Tap Join channel` without `you are subscriber`") { seesText("Tap Join channel") && !seesText("you are subscriber") }
    joinPreparedChannel("team", bob, relay)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`you are subscriber` in the message field of team") { seesText("you are subscriber") }
    click(Ui.Chat.Message)
    poll(what = "the `You can't send messages!` alert") { seesText("You can't send messages!") && "OK" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the alert to close with team still read-only") { !seesText("You can't send messages!") && seesText("you are subscriber") }
  }
}
