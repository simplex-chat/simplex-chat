package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.ChannelLinkScreen
import chat.simplex.app.ui.harness.PeerFlags
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.channelChatId
import chat.simplex.app.ui.harness.channelLinkViaApi
import chat.simplex.app.ui.harness.createChannel
import chat.simplex.app.ui.harness.groupInfoOf
import chat.simplex.app.ui.harness.joinChannel
import chat.simplex.app.ui.harness.registerChatRelaysViaApi
import chat.simplex.common.model.GroupMemberRole
import chat.simplex.common.ui.Ui
import org.junit.Test

class ChannelOwnerCreateTest : UiTest() {
  @Test fun createChannel_singleRelayCompletesAndShowsLink() = uiTest {
    val alice = app("alice"); val relay = cli("relay1", PeerFlags(relay = true))
    registerChatRelaysViaApi(relay)
    alice.newChat()
    click(Ui.NewChat.CreateChannel)
    typeInto(Ui.NewChannel.Name, "team")
    check(channelChatId("team") == null) { "a channel named team exists before `Create public channel` is clicked" }
    click(Ui.NewChannel.Create)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `Creating channel` step with `0/1 relays active` and no channel link") {
      seesText("Creating channel") && seesText("0/1 relays active") && !exists(Ui.GroupLink.Share)
    }
    relay.eventually("#team: you joined the group as relay", CONNECT_TIMEOUT_MS)
    val linkScreen = ChannelLinkScreen(this, "team")
    poll(what = "the channel link explanation in place of the `Creating channel` step") {
      seesText("You can share a link or a QR code - anybody will be able to join the channel.") && !seesText("Creating channel")
    }
    groupInfoOf("team").let { check(it.useRelays && it.membership.memberRole == GroupMemberRole.Owner) { "team is not a channel owned by alice: useRelays ${it.useRelays}, role ${it.membership.memberRole}" } }
    linkScreen.continueToChannel()
  }

  @Test fun channelLink_cliSubscriberJoins() = uiTest {
    val alice = app("alice"); val relay = cli("relay1", PeerFlags(relay = true)); val bob = cli("bob")
    registerChatRelaysViaApi(relay)
    val linkScreen = createChannel(alice, "team")
    val link = linkScreen.share()
    linkScreen.continueToChannel()
    bob.joinChannel("team", link, relay)
  }

  @Test fun channelInfo_subscriberCountUpdates() = uiTest {
    val alice = app("alice"); val relay = cli("relay1", PeerFlags(relay = true)); val bob = cli("bob"); val cath = cli("cath")
    registerChatRelaysViaApi(relay)
    val chat = createChannel(alice, "team").continueToChannel()
    val link = channelLinkViaApi("team")
    chat.groupInfo().apply {
      poll(what = "Group info of the new channel with `Subscribers` and `1 subscriber`") { seesText("Subscribers") && "1 subscriber" in textsOf(Ui.GroupInfo.Content) }
      close()
    }
    bob.joinChannel("team", link, relay)
    cath.joinChannel("team", link, relay)
    chat.groupInfo()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`3 subscribers` in Group info after bob and cath joined") { seesText("Subscribers") && "3 subscribers" in textsOf(Ui.GroupInfo.Content) }
  }
}
