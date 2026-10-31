package chat.simplex.app.ui.settings

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.TestServers
import chat.simplex.app.ui.harness.UNREACHABLE_HOST
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.addCustomSmpServerViaApi
import chat.simplex.app.ui.harness.coreOperators
import chat.simplex.app.ui.harness.customServers
import chat.simplex.app.ui.harness.customSmpServer
import chat.simplex.app.ui.harness.goBack
import chat.simplex.app.ui.harness.hostOf
import chat.simplex.app.ui.harness.networkAndServers
import chat.simplex.app.ui.harness.openNetworkFromSettings
import chat.simplex.app.ui.harness.unreachableSmpServer
import chat.simplex.common.model.ConditionsAcceptance
import chat.simplex.common.model.OperatorTag
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiIdle
import chat.simplex.common.views.helpers.AlertManager
import org.junit.Test

private const val TEST_FAILED = "Server test failed!"
private const val TEST_SERVERS = "Test servers"
private const val SOME_FAILED = "Some servers failed the test:"
private const val NO_MESSAGE_SERVERS = "No message servers."
private const val ACCEPTED_ON = "Conditions accepted on:"
private const val CONDITIONS_HEADING = "SimpleX Chat Operators Privacy Policy and Conditions of Use"
private const val INVALID_ADDRESS = "not a server address"
private val CONFIRM = Ui.Alert.Confirm(Ui.AlertSource.Shared)
private val DISMISS = Ui.Alert.Dismiss(Ui.AlertSource.Shared)

private fun listsServer(alertText: String, host: String): Boolean = Regex("\\s${Regex.escape(host)}: ").containsMatchIn(alertText)

class NetworkServersTest : UiTest() {
  @Test fun operators_enableDisableOperator() = uiTest {
    val alice = app("alice")
    val simplex = coreOperators().single { it.operatorTag == OperatorTag.SimpleX }
    val flux = coreOperators().single { it.operatorTag == OperatorTag.Flux }
    check(!simplex.enabled && !flux.enabled && simplex.conditionsAcceptance.let { it is ConditionsAcceptance.Required && it.deadline == null }) {
      "operators before the test: $simplex, $flux"
    }
    val network = networkAndServers(alice)
    poll(what = "the `Network & servers` sections") {
      seesText("Preset servers") && exists(Ui.Network.Operator(flux.operatorId)) && seesText("Messages and files") && seesText("Your servers") &&
        seesText("Use SOCKS proxy") && seesText("SOCKS proxy settings") && seesText("Advanced network settings") && seesText("Save servers") &&
        seesText("Calls") && seesText("WebRTC ICE servers")
    }
    check(!network.edited(Ui.Network.Operator(simplex.operatorId))) { "the SimpleX Chat row is marked as edited before the test" }
    var operator = network.operator(simplex.operatorId)
    poll(what = "`${simplex.tradeName} servers` with the operator off") { seesText("${simplex.tradeName} servers") }
    check(!seesText(TEST_SERVERS) && !seesText(ACCEPTED_ON, substring = true)) { "the disabled operator shows its servers or accepted conditions" }
    operator.info()
    simplex.legalName?.let { name -> check(seesText(name)) { "`Network operator` without `$name`: ${texts().take(30)}" } }
    operator.closeInfo()
    operator.toggleUse()
    poll(what = "`Use ${simplex.tradeName}` with the conditions text and `Accept conditions`") {
      seesText("Use ${simplex.tradeName}") && exists(Ui.Operator.AcceptConditions) && seesText(CONDITIONS_HEADING, substring = true)
    }
    click(Ui.Operator.AcceptConditions)
    poll(what = "the operator on with its servers and the conditions accepted") {
      !exists(Ui.Operator.AcceptConditions) && exists(Ui.Operator.Use) && seesText(TEST_SERVERS) && seesText(ACCEPTED_ON, substring = true)
    }
    coreOperators().single { it.operatorId == simplex.operatorId }.let {
      check(it.conditionsAcceptance.conditionsAccepted && !it.enabled) { "after `Accept conditions`, before `Save servers`: $it" }
    }
    operator.back()
    poll(what = "the SimpleX Chat row marked as edited") { network.edited(Ui.Network.Operator(simplex.operatorId)) }
    network.save(Ui.Network.Operator(simplex.operatorId))
    coreOperators().let { ops ->
      check(ops.single { it.operatorId == simplex.operatorId }.enabled && !ops.single { it.operatorId == flux.operatorId }.enabled) { "operators after saving: $ops" }
    }
    operator = network.operator(simplex.operatorId)
    operator.toggleUse()
    poll(what = "the operator off without the conditions sheet") { exists(Ui.Operator.Use) && !seesText(TEST_SERVERS) && !exists(Ui.Operator.AcceptConditions) }
    operator.back()
    poll(what = "the SimpleX Chat row marked as edited again") { network.edited(Ui.Network.Operator(simplex.operatorId)) }
    network.save(Ui.Network.Operator(simplex.operatorId))
    coreOperators().single { it.operatorId == simplex.operatorId }.let {
      check(!it.enabled && it.conditionsAcceptance.conditionsAccepted) { "the operator after turning it off and saving: $it" }
    }
  }

  @Test fun yourServers_testServers_allPass() = uiTest {
    val alice = app("alice")
    val smp = customServers().smpServers.single { it.enabled }
    val xftp = customServers().xftpServers.single { it.enabled }
    check(smp.tested == null && xftp.tested == null) { "the local servers are marked as tested before the test: $smp, $xftp" }
    val network = networkAndServers(alice)
    check(!network.edited(Ui.Network.YourServers)) { "`Your servers` is marked as edited before the test" }
    val servers = network.yourServers()
    poll(what = "the local SMP and XFTP servers under `Message servers` and `Media & file servers`") {
      seesText("Message servers") && seesText("Media & file servers") &&
        hostOf(smp.server) in textsOf(Ui.Servers.Server(smp.server)) && hostOf(xftp.server) in textsOf(Ui.Servers.Server(xftp.server))
    }
    servers.testAll()
    check(!AlertManager.shared.hasAlertsShown() && !seesText(TEST_FAILED)) { "a local server failed the test: ${texts().take(40)}" }
    servers.back()
    poll(what = "`Your servers` marked as edited with the test results") { network.edited(Ui.Network.YourServers) }
    network.save(Ui.Network.YourServers)
    customServers().let { saved ->
      check(saved.smpServers.single { it.server == smp.server }.tested == true && saved.xftpServers.single { it.server == xftp.server }.tested == true) {
        "test results in the core after saving: $saved"
      }
    }
  }

  @Test fun yourServers_testServers_someFail() = uiTest {
    val alice = app("alice")
    val smp = customServers().smpServers.single { it.enabled }
    val unreachable = unreachableSmpServer()
    val network = networkAndServers(alice)
    val servers = network.yourServers()
    val new = servers.add()
    new.enterAddress(unreachable)
    new.test()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `$TEST_FAILED` alert for the new server") { seesText(TEST_FAILED) && exists(CONFIRM) }
    check(!seesText(SOME_FAILED, substring = true)) { "testing one server showed the all-servers failure text" }
    click(CONFIRM)
    poll(what = "the alert to close") { !AlertManager.shared.hasAlertsShown() }
    new.toggleUse()
    new.back()
    poll(what = "the new server listed in `Your servers`") { exists(Ui.Servers.Server(unreachable)) && UNREACHABLE_HOST in textsOf(Ui.Servers.Server(unreachable)) }
    servers.testAll()
    var failed: String? = null
    poll(what = "the `$TEST_FAILED` alert listing the failed servers") {
      failed = texts().firstOrNull { it.startsWith(SOME_FAILED) }
      seesText(TEST_FAILED) && failed != null
    }
    check(listsServer(failed!!, UNREACHABLE_HOST) && !listsServer(failed!!, hostOf(smp.server)!!)) { "failed servers in the alert: $failed" }
    click(CONFIRM)
    poll(what = "the alert to close") { !AlertManager.shared.hasAlertsShown() }
    servers.back()
    network.save(Ui.Network.YourServers)
    val saved = customServers().smpServers
    check(customSmpServer(UNREACHABLE_HOST)?.let { it.enabled && it.tested == false } == true && saved.single { it.server == smp.server }.tested == true) {
      "message servers in the core after saving: $saved"
    }
  }

  @Test fun yourServers_addSmpManually() = uiTest {
    val alice = app("alice")
    val smp2 = TestServers.start("smp2")
    TestServers.stop("smp2")
    val host = hostOf(smp2)!!
    check(customSmpServer(host) == null) { "smp2 is in the core before it is added: ${customServers().smpServers}" }
    var network = networkAndServers(alice)
    val servers = network.yourServers()
    val new = servers.add()
    new.enterAddress(smp2)
    new.test()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `$TEST_FAILED` alert while smp2 is stopped") { seesText(TEST_FAILED) && exists(CONFIRM) }
    click(CONFIRM)
    poll(what = "the alert to close") { !AlertManager.shared.hasAlertsShown() }
    TestServers.start("smp2")
    new.test()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the test of the running smp2 to finish") { UiIdle.inFlight().isEmpty() }
    check(!AlertManager.shared.hasAlertsShown() && !seesText(TEST_FAILED)) { "the running smp2 failed the test: ${texts().take(40)}" }
    new.toggleUse()
    new.back()
    poll(what = "smp2 listed in `Your servers`") { exists(Ui.Servers.Server(smp2)) && host in textsOf(Ui.Servers.Server(smp2)) }
    servers.back()
    poll(what = "`Your servers` marked as edited") { network.edited(Ui.Network.YourServers) }
    network.save(Ui.Network.YourServers)
    val saved = customSmpServer(host) ?: throw AssertionError("smp2 is not in the core after saving: ${customServers().smpServers}")
    check(saved.enabled && saved.tested == true) { "smp2 in the core after saving: $saved" }
    network.close()
    network = openNetworkFromSettings()
    network.yourServers()
    poll(what = "smp2 listed after reopening `Your servers`") { exists(Ui.Servers.Server(saved.server)) && host in textsOf(Ui.Servers.Server(saved.server)) }
  }

  @Test fun newServer_invalidAddress() = uiTest {
    val alice = app("alice")
    val before = customServers()
    val network = networkAndServers(alice)
    val servers = network.yourServers()
    val new = servers.add()
    new.enterAddress(INVALID_ADDRESS)
    new.back()
    poll(what = "the `Invalid server address!` alert") { seesText("Invalid server address!") && seesText("Check server address and try again.") && exists(CONFIRM) }
    click(CONFIRM)
    poll(what = "the alert to close") { !AlertManager.shared.hasAlertsShown() }
    check(exists(Ui.Servers.Server(before.smpServers.single { it.enabled }.server)) && !exists(Ui.Servers.Server(INVALID_ADDRESS))) {
      "`Your servers` after the invalid address: ${texts().take(40)}"
    }
    servers.back()
    check(!network.edited(Ui.Network.YourServers)) { "`Your servers` is marked as edited after the invalid address" }
    check(customServers() == before) { "the core servers changed: ${customServers()}" }
  }

  @Test fun serverView_toggleUseAndSave() = uiTest {
    val alice = app("alice")
    val smp = customServers().smpServers.single { it.enabled }
    val second = addCustomSmpServerViaApi(unreachableSmpServer())
    var network = networkAndServers(alice)
    var servers = network.yourServers()
    servers.open(second).apply { toggleUse(); back() }
    servers.back()
    poll(what = "`Your servers` marked as edited") { network.edited(Ui.Network.YourServers) }
    network.save(Ui.Network.YourServers)
    check(customSmpServer(UNREACHABLE_HOST)?.enabled == false && customServers().smpServers.single { it.server == smp.server }.enabled) {
      "message servers after turning the second one off: ${customServers().smpServers}"
    }
    val after = restart()
    poll(what = "chat running after the restart") { chatModel.chatRunning.value == true }
    check(customSmpServer(UNREACHABLE_HOST)?.enabled == false) { "the second server is on again after the restart: ${customServers().smpServers}" }
    network = networkAndServers(after)
    servers = network.yourServers()
    check(!seesText(NO_MESSAGE_SERVERS)) { "`$NO_MESSAGE_SERVERS` is shown with the local server on" }
    servers.open(smp.server).apply { toggleUse(); back() }
    poll(what = "`$NO_MESSAGE_SERVERS` in `Your servers`") { seesText(NO_MESSAGE_SERVERS) }
    servers.back()
    poll(what = "`$NO_MESSAGE_SERVERS` on `Network & servers`") { seesText(NO_MESSAGE_SERVERS) && network.edited(Ui.Network.YourServers) }
    click(Ui.Network.SaveServers)
    poll(what = "app work to finish after `Save servers`") { UiIdle.inFlight().isEmpty() }
    check(!AlertManager.shared.hasAlertsShown() && network.edited(Ui.Network.YourServers)) { "`Save servers` acted with no message servers: ${texts().take(40)}" }
    check(customServers().smpServers.single { it.server == smp.server }.enabled) { "the local server was turned off in the core" }
    servers = network.yourServers()
    servers.open(smp.server).apply { toggleUse(); back() }
    poll(what = "`$NO_MESSAGE_SERVERS` gone with the local server on") { !seesText(NO_MESSAGE_SERVERS) && exists(Ui.Servers.Add) }
    servers.open(second).apply { toggleUse(); back() }
    servers.back()
    poll(what = "`Your servers` marked as edited") { network.edited(Ui.Network.YourServers) }
    network.save(Ui.Network.YourServers)
    check(customSmpServer(UNREACHABLE_HOST)?.enabled == true) { "the second server after turning it on again: ${customServers().smpServers}" }
  }

  @Test fun serverView_deleteServer() = uiTest {
    val alice = app("alice")
    val smp = customServers().smpServers.single { it.enabled && hostOf(it.server) != UNREACHABLE_HOST }
    val second = addCustomSmpServerViaApi(unreachableSmpServer())
    val network = networkAndServers(alice)
    val servers = network.yourServers()
    poll(what = "both message servers listed") { exists(Ui.Servers.Server(second)) && exists(Ui.Servers.Server(smp.server)) }
    servers.open(second).delete()
    poll(what = "the deleted server gone from `Your servers` with the local server still listed") {
      !exists(Ui.Servers.Server(second)) && exists(Ui.Servers.Server(smp.server))
    }
    servers.back()
    poll(what = "`Your servers` marked as edited") { network.edited(Ui.Network.YourServers) }
    network.save(Ui.Network.YourServers)
    check(customSmpServer(UNREACHABLE_HOST) == null && customServers().smpServers.any { it.server == smp.server && it.enabled }) {
      "message servers after deleting the second one: ${customServers().smpServers}"
    }
  }

  @Test fun network_unsavedServersAlert_save() = uiTest {
    val alice = app("alice")
    val second = addCustomSmpServerViaApi(unreachableSmpServer())
    networkAndServers(alice).close()
    check(!AlertManager.shared.hasAlertsShown()) { "leaving `Network & servers` without changes showed an alert" }
    var network = openNetworkFromSettings()
    network.yourServers().apply { open(second).apply { toggleUse(); back() }; back() }
    goBack()
    poll(what = "the `Save servers?` alert with `Save` and `Exit without saving`") {
      seesText("Save servers?") && exists(CONFIRM) && "Save" in textsOf(CONFIRM) && "Exit without saving" in textsOf(DISMISS)
    }
    click(DISMISS)
    network.awaitClosed()
    check(customSmpServer(UNREACHABLE_HOST)?.enabled == true) { "`Exit without saving` saved the change: ${customServers().smpServers}" }
    network = openNetworkFromSettings()
    check(!network.edited(Ui.Network.YourServers)) { "the discarded change is still pending" }
    network.yourServers().apply { open(second).apply { toggleUse(); back() }; back() }
    goBack()
    poll(what = "the `Save servers?` alert") { seesText("Save servers?") && exists(CONFIRM) }
    click(CONFIRM)
    network.awaitClosed()
    poll(what = "the change saved in the core") { customSmpServer(UNREACHABLE_HOST)?.enabled == false }
  }
}
