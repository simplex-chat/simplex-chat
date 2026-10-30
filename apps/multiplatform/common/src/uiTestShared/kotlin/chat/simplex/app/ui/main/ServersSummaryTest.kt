package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.goBack
import chat.simplex.app.ui.harness.isCount
import chat.simplex.app.ui.harness.localSmpServer
import chat.simplex.app.ui.harness.openServersSummary
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.chatlist.PresentedUserCategory
import org.junit.Test

class ServersSummaryTest : UiTest() {
  @Test fun serversSummaryShowsLocalSmpConnected() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice hi"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's `hi` on his row") { "hi" in alice.chatList.rowTexts("bob") }
    val host = localSmpServer().hostnames.first()
    check(!seesText("Servers info")) { "`Servers info` is shown before the indicator is clicked" }
    val summary = openServersSummary()
    poll(what = "the local SMP server under `Connected servers` with sent and received messages and an active connection") {
      seesText("Connected servers") && exists(Ui.ServersSummary.SmpServer(host)) &&
        summary.value("Messages sent").isCount() && summary.value("Messages received").isCount() && summary.value("Active connections").isCount()
    }
    click(Ui.ServersSummary.SmpServer(host))
    poll(what = "`SMP server` with the local server's address, statistics and active connection") {
      seesText("SMP server") && seesText("Server address") && !seesText("Connected servers") && seesText(host, substring = true) &&
        summary.value("Messages received").isCount() && summary.value("Active connections").isCount()
    }
  }

  @Test fun serversSummaryResetStatsAndReconnectAll() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice hi"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's `hi` on his row") { "hi" in alice.chatList.rowTexts("bob") }
    val summary = openServersSummary()
    poll(what = "messages sent and received counted") { summary.value("Messages sent").isCount() && summary.value("Messages received").isCount() }
    click(Ui.ServersSummary.ResetStats)
    poll(what = "the `Reset all statistics?` alert with `Reset`") {
      seesText("Reset all statistics?") && seesText("Servers statistics will be reset - this cannot be undone!") &&
        "Reset" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "`-` for messages sent and received after the reset") { summary.value("Messages sent") == "-" && summary.value("Messages received") == "-" }
    summary.details()
    poll(what = "no subscriptions counted since the reset") { summary.subscribed() == "-" }
    summary.backToSummary()
    click(Ui.ServersSummary.ReconnectAll)
    poll(what = "the `Reconnect servers?` alert") {
      seesText("Reconnect servers?") && seesText("Reconnect all connected servers to force message delivery. It uses additional traffic.") &&
        exists(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the `Reconnect servers?` alert to close") { !seesText("Reconnect servers?") }
    bob sends "@alice after reconnect"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's message counted as received after the reconnect") { summary.value("Messages received").isCount() }
    check(!seesText("Error reconnecting servers")) { "reconnecting all servers showed an error" }
    summary.details()
    poll(what = "the resubscription after the reconnect counted under `Subscribed`") { summary.subscribed().isCount() }
  }

  @Test fun serversSummaryAllVsCurrentProfile() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice hi"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's `hi` on his row") { "hi" in alice.chatList.rowTexts("bob") }
    val host = localSmpServer().hostnames.first()
    openServersSummary()
    poll(what = "the local SMP server listed for the only profile, without `Showing info for`") {
      exists(Ui.ServersSummary.SmpServer(host)) && !exists(Ui.ServersSummary.UserCategory) && !seesText(SHOWING_INFO_FOR)
    }
    goBack()
    poll(what = "`Servers info` to close") { !seesText("Servers info") }
    alice.userPicker().profiles().apply { add("work"); close() }
    val summary = openServersSummary()
    poll(what = "`Showing info for` `All profiles` listing the local server and alice's sent messages") {
      seesText(SHOWING_INFO_FOR) && "All profiles" in textsOf(Ui.ServersSummary.UserCategory) &&
        exists(Ui.ServersSummary.SmpServer(host)) && seesText("Connected servers") && summary.value("Messages sent").isCount()
    }
    click(Ui.ServersSummary.UserCategory)
    click(Ui.ServersSummary.UserCategory.option(PresentedUserCategory.CURRENT_USER))
    poll(what = "`Current profile` for the connection-less work profile: no servers and `-` for messages sent") {
      "Current profile" in textsOf(Ui.ServersSummary.UserCategory) && !exists(Ui.ServersSummary.SmpServer(host)) &&
        !seesText("Connected servers") && summary.value("Messages sent") == "-"
    }
  }

  private companion object {
    const val SHOWING_INFO_FOR = "Showing info for"
  }
}
