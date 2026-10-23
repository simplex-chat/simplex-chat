package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.PeerFlags
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.channelLinkViaApi
import chat.simplex.app.ui.harness.createChannel
import chat.simplex.app.ui.harness.joinChannel
import chat.simplex.app.ui.harness.registerChatRelaysViaApi
import chat.simplex.common.ui.Ui
import org.junit.Test

class ChannelOwnerBroadcastTest : UiTest() {
  @Test fun channel_broadcastPlaceholderAndDelivery() = uiTest {
    val alice = app("alice"); val relay = cli("relay1", PeerFlags(relay = true)); val bob = cli("bob"); val cath = cli("cath")
    registerChatRelaysViaApi(relay)
    val chat = createChannel(alice, "team").continueToChannel()
    val link = channelLinkViaApi("team")
    bob.joinChannel("team", link, relay)
    cath.joinChannel("team", link, relay)
    poll(what = "the empty message field of team showing the placeholder `Broadcast`") { hintOf(Ui.Chat.Message) == "Broadcast" }
    chat.send("hi")
    bob.eventuallyMessage("#team> hi [>>]", CONNECT_TIMEOUT_MS)
    cath.eventuallyMessage("#team> hi [>>]", CONNECT_TIMEOUT_MS)
  }
}
