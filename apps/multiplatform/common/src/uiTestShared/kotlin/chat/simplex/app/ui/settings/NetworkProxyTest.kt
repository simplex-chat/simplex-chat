package chat.simplex.app.ui.settings

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.SOCKS_HOST
import chat.simplex.app.ui.harness.SocksProxy
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.closeDeveloperTools
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.coreNetworkConfig
import chat.simplex.app.ui.harness.goBack
import chat.simplex.app.ui.harness.localSmpServer
import chat.simplex.app.ui.harness.networkAndServers
import chat.simplex.app.ui.harness.saveNetworkConfigViaApi
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.HostMode
import chat.simplex.common.model.KeepAliveOpts
import chat.simplex.common.model.NetCfg
import chat.simplex.common.model.NetworkProxy
import chat.simplex.common.model.OnionHosts
import chat.simplex.common.model.SMPProxyFallback
import chat.simplex.common.model.SMPProxyMode
import chat.simplex.common.model.TransportSessionMode
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.AlertManager
import org.junit.Test

private class NetSetting(val name: String, val pref: String, val new: Long, val oldText: String, val newText: String, val core: (NetCfg) -> Long)

private val TIMEOUTS = listOf(
  NetSetting("tcpConnectTimeout", "NetworkTCPConnectTimeoutInteractive", 20_000_000, "15 sec", "20 sec") { it.tcpConnectTimeout.interactiveTimeout },
  NetSetting("tcpConnectTimeoutBackground", "NetworkTCPConnectTimeoutBackground", 60_000_000, "45 sec", "60 sec") { it.tcpConnectTimeout.backgroundTimeout },
  NetSetting("tcpTimeout", "NetworkTCPTimeoutInteractive", 15_000_000, "10 sec", "15 sec") { it.tcpTimeout.interactiveTimeout },
  NetSetting("tcpTimeoutBackground", "NetworkTCPTimeoutBackground", 45_000_000, "30 sec", "45 sec") { it.tcpTimeout.backgroundTimeout },
  NetSetting("tcpTimeoutPerKb", "networkTCPTimeoutPerKb", 20_000, "0.01 sec", "0.02 sec") { it.tcpTimeoutPerKb },
  NetSetting("smpPingInterval", "NetworkSMPPingInterval", 600_000_000, "1200 sec", "600 sec") { it.smpPingInterval },
  NetSetting("smpPingCount", "NetworkSMPPingCount", 5, "3", "5") { it.smpPingCount.toLong() },
)

private const val PER_KB = "networkTCPTimeoutPerKb"
private const val PING_COUNT = "NetworkSMPPingCount"
private const val KEEP_ALIVE = "NetworkEnableKeepAlive"
private const val PROXY_MODE = "NetworkSMPProxyMode"
private const val PROXY_FALLBACK = "NetworkSMPProxyFallback"
private const val USE_SOCKS = "NetworkUseSocksProxy"
private const val HOST_MODE = "NetworkHostMode"
private const val REQUIRED_HOST_MODE = "NetworkRequiredHostMode"
private const val UPDATE_TITLE = "Update network settings?"
private const val ONION_REQUIRED_FOOTER = "Onion hosts will be required for connection."
private const val PROXY = "NetworkProxyHostPort"
private const val SESSION_MODE = "NetworkSessionMode"
private const val CUSTOM_PER_KB = 20_000L
private const val WRONG_PROXY_TITLE = "Error saving proxy"
private const val WRONG_PROXY_TEXT = "Make sure proxy configuration is correct."
private const val PROXY_HOST = "127.0.0.1"
private const val PROXY_PORT = "9150"
private val ALERT_CONFIRM = Ui.Alert.Confirm(Ui.AlertSource.Shared)
private val ALERT_DISMISS = Ui.Alert.Dismiss(Ui.AlertSource.Shared)

class NetworkProxyTest : UiTest() {
  @Test fun socks_enable_confirm() = uiTest {
    val alice = app("alice")
    saveNetworkConfigViaApi(coreNetworkConfig().copy(tcpTimeoutPerKb = CUSTOM_PER_KB))
    val def = NetCfg.defaults
    val proxyDef = NetCfg.proxyDefaults
    coreNetworkConfig().let {
      check(it.socksProxy == null && it.tcpConnectTimeout == def.tcpConnectTimeout && it.tcpTimeout == def.tcpTimeout && it.rcvConcurrency == def.rcvConcurrency) { "network config before the test: $it" }
    }
    networkAndServers(alice)
    toggleSocks("Use SOCKS proxy?")
    poll(what = "the SOCKS proxy on with the proxy timeouts, keeping the changed per-KB timeout") {
      storedSetting(USE_SOCKS) == "true" && coreNetworkConfig().let {
        it.socksProxy != null && it.tcpConnectTimeout == proxyDef.tcpConnectTimeout && it.tcpTimeout == proxyDef.tcpTimeout &&
          it.rcvConcurrency == proxyDef.rcvConcurrency && it.tcpTimeoutPerKb == CUSTOM_PER_KB
      }
    }
    toggleSocks("Use direct Internet connection?")
    poll(what = "the SOCKS proxy off with the direct timeouts back, keeping the changed per-KB timeout") {
      storedSetting(USE_SOCKS) == "false" && coreNetworkConfig().let {
        it.socksProxy == null && it.tcpConnectTimeout == def.tcpConnectTimeout && it.tcpTimeout == def.tcpTimeout &&
          it.rcvConcurrency == def.rcvConcurrency && it.tcpTimeoutPerKb == CUSTOM_PER_KB
      }
    }
  }

  @Test fun socks_wrongConfigAlert() = uiTest {
    val alice = app("alice")
    val network = networkAndServers(alice)
    toggleSocks("Use SOCKS proxy?")
    poll(what = "the SOCKS proxy on in the core") { storedSetting(USE_SOCKS) == "true" && coreNetworkConfig().socksProxy != null }
    val socksBefore = coreNetworkConfig().socksProxy
    val socks = network.socks()
    check(editableText(Ui.Socks.Host) == "localhost" && editableText(Ui.Socks.Port) == "9050") { "SOCKS fields: `${editableText(Ui.Socks.Host)}`:`${editableText(Ui.Socks.Port)}`" }
    replaceText(Ui.Socks.Host, "proxy.example")
    socks.saveAndReconnect()
    poll(what = "the `$WRONG_PROXY_TITLE` alert") { seesText(WRONG_PROXY_TITLE) && seesText(WRONG_PROXY_TEXT) && exists(ALERT_CONFIRM) }
    click(ALERT_CONFIRM)
    poll(what = "the alert to close") { !AlertManager.shared.hasAlertsShown() }
    check(appPrefs.networkProxy.get() == NetworkProxy() && coreNetworkConfig().socksProxy == socksBefore && exists(Ui.Socks.Save)) {
      "the rejected proxy was kept: stored ${appPrefs.networkProxy.get()}, core ${coreNetworkConfig().socksProxy}"
    }
    replaceText(Ui.Socks.Host, PROXY_HOST)
    replaceText(Ui.Socks.Port, PROXY_PORT)
    socks.saveAndReconnect()
    poll(what = "the new proxy stored and applied, with SOCKS proxy settings still open") {
      appPrefs.networkProxy.get().let { it.host == PROXY_HOST && it.port.toString() == PROXY_PORT } &&
        coreNetworkConfig().socksProxy?.contains("$PROXY_HOST:$PROXY_PORT") == true && exists(Ui.Socks.Save) && !AlertManager.shared.hasAlertsShown()
    }
    click(Ui.Socks.Reset)
    poll(what = "the fields reset to localhost:9050") { editableText(Ui.Socks.Host) == "localhost" && editableText(Ui.Socks.Port) == "9050" }
    check(coreNetworkConfig().socksProxy?.contains("$PROXY_HOST:$PROXY_PORT") == true && appPrefs.networkProxy.get().host == PROXY_HOST) {
      "`Reset to defaults` saved before `Save and reconnect`: ${coreNetworkConfig().socksProxy}"
    }
    socks.saveAndReconnect()
    poll(what = "the default proxy stored and applied") { appPrefs.networkProxy.get() == NetworkProxy() && coreNetworkConfig().socksProxy == socksBefore }
  }

  @Test fun socks_unsavedAlert() = uiTest {
    val alice = app("alice")
    val storedBefore = storedSetting(PROXY)
    val network = networkAndServers(alice)
    var socks = network.socks()
    check(editableText(Ui.Socks.Host) == "localhost" && editableText(Ui.Socks.Port) == "9050") { "SOCKS fields: `${editableText(Ui.Socks.Host)}`:`${editableText(Ui.Socks.Port)}`" }
    for ((field, invalid) in listOf(Ui.Socks.Port to "70000", Ui.Socks.Host to "bad@host")) {
      replaceText(field, invalid)
      click(Ui.Socks.Save)
      settle()
      check(!AlertManager.shared.hasAlertsShown() && storedSetting(PROXY) == storedBefore) { "Save acted on the invalid `$invalid`" }
    }
    socks.back()
    check(!AlertManager.shared.hasAlertsShown() && storedSetting(PROXY) == storedBefore) { "leaving with invalid fields asked to save or saved" }
    socks = network.socks()
    check(editableText(Ui.Socks.Host) == "localhost" && editableText(Ui.Socks.Port) == "9050") { "the invalid values were kept" }
    replaceText(Ui.Socks.Port, PROXY_PORT)
    hideKeyboard()
    goBack()
    awaitUnsavedAlert()
    click(ALERT_DISMISS)
    poll(what = "`SOCKS proxy settings` closed by `Exit without saving`") { !exists(Ui.Socks.Save) && exists(Ui.Network.SaveServers) }
    check(storedSetting(PROXY) == storedBefore) { "`Exit without saving` saved the port: ${storedSetting(PROXY)}" }
    socks = network.socks()
    check(editableText(Ui.Socks.Port) == "9050") { "the discarded port is shown: ${editableText(Ui.Socks.Port)}" }
    replaceText(Ui.Socks.Port, PROXY_PORT)
    hideKeyboard()
    goBack()
    awaitUnsavedAlert()
    click(ALERT_CONFIRM)
    poll(what = "the port stored, `SOCKS proxy settings` still open") {
      !AlertManager.shared.hasAlertsShown() && appPrefs.networkProxy.get().port.toString() == PROXY_PORT && exists(Ui.Socks.Save) && editableText(Ui.Socks.Port) == PROXY_PORT
    }
    check(coreNetworkConfig().socksProxy == null && storedSetting(USE_SOCKS) != "true") { "saving the proxy settings turned the SOCKS proxy on: ${coreNetworkConfig().socksProxy}" }
    socks.back()
    check(!AlertManager.shared.hasAlertsShown()) { "leaving after the save asked to save again" }
  }

  @Test fun advNet_transportIsolation() = uiTest {
    val alice = app("alice")
    check(coreNetworkConfig().sessionMode == TransportSessionMode.Session && storedSetting(SESSION_MODE) != TransportSessionMode.Server.name) { "transport isolation before the test: ${coreNetworkConfig().sessionMode}" }
    var network = networkAndServers(alice)
    var adv = network.advanced()
    check("App session" in textsOf(Ui.AdvancedNetwork.SessionMode)) { "`Transport isolation`: ${textsOf(Ui.AdvancedNetwork.SessionMode)}" }
    pickSessionMode(TransportSessionMode.Server, listOf(TransportSessionMode.User to "Chat profile", TransportSessionMode.Session to "App session", TransportSessionMode.Server to "Server"), TransportSessionMode.Entity)
    poll(what = "`Server` on `Transport isolation`") { "Server" in textsOf(Ui.AdvancedNetwork.SessionMode) }
    adv.saveAndReconnect()
    poll(what = "transport isolation by server stored and applied to the core") {
      storedSetting(SESSION_MODE) == TransportSessionMode.Server.name && coreNetworkConfig().sessionMode == TransportSessionMode.Server
    }
    adv.back()
    network.close()
    click(Ui.Settings.Version)
    click(Ui.Settings.DeveloperTools)
    click(Ui.DevTools.ShowOptions)
    poll(what = "developer tools stored as on") { storedSetting("DeveloperTools") == "true" }
    closeDeveloperTools()
    network = networkAndServers(alice)
    adv = network.advanced()
    pickSessionMode(TransportSessionMode.Entity, listOf(TransportSessionMode.Server to "Server", TransportSessionMode.Entity to "Connection"), null)
    poll(what = "`Connection` on `Transport isolation`") { "Connection" in textsOf(Ui.AdvancedNetwork.SessionMode) }
    adv.saveAndReconnect()
    poll(what = "transport isolation by connection stored and applied to the core") {
      storedSetting(SESSION_MODE) == TransportSessionMode.Entity.name && coreNetworkConfig().sessionMode == TransportSessionMode.Entity
    }
  }

  private fun UiScope.toggleSocks(title: String) {
    click(Ui.Network.UseSocks)
    poll(what = "the `$title` alert with `Confirm`") { seesText(title) && exists(ALERT_CONFIRM) && "Confirm" in textsOf(ALERT_CONFIRM) }
    click(ALERT_CONFIRM)
    poll(what = "the `$title` alert to close") { !AlertManager.shared.hasAlertsShown() }
  }

  private fun UiScope.awaitUnsavedAlert() =
    poll(what = "the `$UPDATE_TITLE` alert with `Save` and `Exit without saving`") {
      seesText(UPDATE_TITLE) && "Save" in textsOf(ALERT_CONFIRM) && "Exit without saving" in textsOf(ALERT_DISMISS)
    }

  private fun UiScope.pickSessionMode(mode: TransportSessionMode, shown: List<Pair<TransportSessionMode, String>>, hidden: TransportSessionMode?) {
    val row = Ui.AdvancedNetwork.SessionMode
    click(row)
    poll(what = "the transport isolation options") { exists(row.option(mode)) && !exists(Ui.AdvancedNetwork.Save) }
    for ((value, title) in shown) check(title in textsOf(row.option(value))) { "option $value is not `$title`: ${textsOf(row.option(value))}" }
    if (hidden != null) check(!exists(row.option(hidden))) { "option $hidden is offered without developer tools" }
    click(row.option(mode))
    goBack()
    poll(what = "back on `Advanced settings` from the transport isolation options") { !exists(row.option(mode)) && exists(Ui.AdvancedNetwork.Save) }
  }

  @Test fun socks_onionHostsOptions() = uiTest {
    val alice = app("alice")
    coreNetworkConfig().let { check(it.socksProxy == null && !it.requiredHostMode) { "network config before the test: $it" } }
    check(storedSetting(REQUIRED_HOST_MODE) != "true") { "`.onion` hosts are stored as required before the test" }
    val network = networkAndServers(alice)
    var socks = network.socks()
    check("No" in socks.onionHosts() && "When available" !in socks.onionHosts()) { "`Use .onion hosts` with the SOCKS proxy off: ${socks.onionHosts()}" }
    click(Ui.Socks.OnionHosts)
    settle()
    check(exists(Ui.Socks.Save) && !exists(Ui.Socks.OnionHosts.option(OnionHosts.REQUIRED))) { "`Use .onion hosts` opened its options with the SOCKS proxy off" }
    socks.back()
    click(Ui.Network.UseSocks)
    poll(what = "the `Use SOCKS proxy?` alert with `Confirm`") { seesText("Use SOCKS proxy?") && exists(ALERT_CONFIRM) && "Confirm" in textsOf(ALERT_CONFIRM) }
    click(ALERT_CONFIRM)
    poll(what = "the SOCKS proxy on in the stored settings and the core") { storedSetting(USE_SOCKS) == "true" && coreNetworkConfig().socksProxy != null }
    socks = network.socks()
    poll(what = "`Use .onion hosts` at `When available` with the SOCKS proxy on") { "When available" in socks.onionHosts() }
    click(Ui.Socks.OnionHosts)
    click(Ui.Socks.OnionHosts.option(OnionHosts.REQUIRED))
    poll(what = "`Required` with its footer") { "Required" in socks.onionHosts() && seesText(ONION_REQUIRED_FOOTER, substring = true) }
    socks.saveAndReconnect()
    poll(what = "`.onion` hosts required in the stored settings and the core") {
      storedSetting(HOST_MODE) == HostMode.OnionViaSocks.name && storedSetting(REQUIRED_HOST_MODE) == "true" &&
        coreNetworkConfig().let { it.hostMode == HostMode.OnionViaSocks && it.requiredHostMode }
    }
    val after = restart()
    poll(what = "chat running after the restart") { chatModel.chatRunning.value == true }
    coreNetworkConfig().let { check(it.socksProxy != null && it.hostMode == HostMode.OnionViaSocks && it.requiredHostMode) { "network config after the restart: $it" } }
    val again = networkAndServers(after).socks()
    poll(what = "`Use .onion hosts` still `Required` after the restart") { "Required" in again.onionHosts() }
  }

  @Test fun socks_trafficRoutedThroughProxy() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    SocksProxy.start()
    connect(bob)
    alice.chatList.open("bob").send("direct")
    bob.expectsMessage("alice> direct", CONNECT_TIMEOUT_MS)
    check(SocksProxy.connections().isEmpty()) { "the app used the SOCKS proxy before it was enabled: ${SocksProxy.connections()}" }
    backToChatList()
    val network = networkAndServers(alice)
    network.socks().apply { saveProxy(SOCKS_HOST, SocksProxy.port); back() }
    check(coreNetworkConfig().socksProxy == null && SocksProxy.connections().isEmpty()) {
      "saving the proxy with `Use SOCKS proxy` off changed the network: ${coreNetworkConfig().socksProxy}, ${SocksProxy.connections()}"
    }
    network.enableSocks()
    poll(what = "the core using the SOCKS proxy on port ${SocksProxy.port}") { coreNetworkConfig().socksProxy?.endsWith(":${SocksProxy.port}") == true }
    val smp = localSmpServer()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "a connection to ${smp.hostnames.first()}:${smp.port} through the SOCKS proxy with random credentials") {
      SocksProxy.connections().any { it.host == smp.hostnames.first() && it.port.toString() == smp.port && !it.username.isNullOrEmpty() }
    }
    network.close()
    escape()
    poll(what = "the settings to close to the chat list") { !exists(Ui.Settings.Network) && exists(Ui.ChatList.Search) }
    alice.chatList.open("bob").send("via proxy")
    bob.expectsMessage("alice> via proxy", CONNECT_TIMEOUT_MS)
  }

  @Test fun advNet_privateRoutingMode() = uiTest {
    val alice = app("alice")
    coreNetworkConfig().let { check(it.smpProxyMode == SMPProxyMode.Always && it.smpProxyFallback == SMPProxyFallback.AllowProtected) { "network config before the test: $it" } }
    check(storedSetting(PROXY_MODE) != SMPProxyMode.Unknown.name && storedSetting(PROXY_FALLBACK) != SMPProxyFallback.Prohibit.name) { "private routing is stored as changed before the test" }
    val adv = networkAndServers(alice).advanced()
    check("Always" in textsOf(Ui.AdvancedNetwork.ProxyMode) && "When IP hidden" in textsOf(Ui.AdvancedNetwork.ProxyFallback)) {
      "private routing rows before the test: ${textsOf(Ui.AdvancedNetwork.ProxyMode)}, ${textsOf(Ui.AdvancedNetwork.ProxyFallback)}"
    }
    adv.pick(Ui.AdvancedNetwork.ProxyMode, SMPProxyMode.Never)
    poll(what = "`Never` on `Private routing`") { "Never" in textsOf(Ui.AdvancedNetwork.ProxyMode) }
    click(Ui.AdvancedNetwork.ProxyFallback)
    settle()
    check(exists(Ui.AdvancedNetwork.Save) && !exists(Ui.AdvancedNetwork.ProxyFallback.option(SMPProxyFallback.Prohibit))) {
      "`Allow downgrade` opened its options with private routing off"
    }
    adv.pick(Ui.AdvancedNetwork.ProxyMode, SMPProxyMode.Unknown)
    adv.pick(Ui.AdvancedNetwork.ProxyFallback, SMPProxyFallback.Prohibit)
    poll(what = "`Unknown servers` and `No` on the private routing rows") {
      "Unknown servers" in textsOf(Ui.AdvancedNetwork.ProxyMode) && "No" in textsOf(Ui.AdvancedNetwork.ProxyFallback)
    }
    adv.saveAndReconnect()
    poll(what = "private routing stored and applied to the core") {
      storedSetting(PROXY_MODE) == SMPProxyMode.Unknown.name && storedSetting(PROXY_FALLBACK) == SMPProxyFallback.Prohibit.name &&
        coreNetworkConfig().let { it.smpProxyMode == SMPProxyMode.Unknown && it.smpProxyFallback == SMPProxyFallback.Prohibit }
    }
    val after = restart()
    poll(what = "chat running after the restart") { chatModel.chatRunning.value == true }
    coreNetworkConfig().let { check(it.smpProxyMode == SMPProxyMode.Unknown && it.smpProxyFallback == SMPProxyFallback.Prohibit) { "network config after the restart: $it" } }
    networkAndServers(after).advanced()
    check("Unknown servers" in textsOf(Ui.AdvancedNetwork.ProxyMode) && "No" in textsOf(Ui.AdvancedNetwork.ProxyFallback)) {
      "private routing rows after the restart: ${textsOf(Ui.AdvancedNetwork.ProxyMode)}, ${textsOf(Ui.AdvancedNetwork.ProxyFallback)}"
    }
  }

  @Test fun advNet_timeoutsSaved() = uiTest {
    val alice = app("alice")
    val before = coreNetworkConfig()
    for (t in TIMEOUTS) check(t.core(before) != t.new && storedSetting(t.pref) != t.new.toString()) { "${t.name} is already ${t.new} before the test: $before" }
    val adv = networkAndServers(alice).advanced()
    for (t in TIMEOUTS) {
      check(adv.value(t.name) == t.oldText) { "${t.name} shows `${adv.value(t.name)}` instead of `${t.oldText}`" }
      adv.select(t.name, t.new)
      poll(what = "${t.name} showing `${t.newText}`") { adv.value(t.name) == t.newText }
    }
    adv.saveAndReconnect()
    poll(what = "all ${TIMEOUTS.size} values stored and applied to the core") {
      TIMEOUTS.all { storedSetting(it.pref) == it.new.toString() } && coreNetworkConfig().let { c -> TIMEOUTS.all { it.core(c) == it.new } }
    }
    val after = restart()
    poll(what = "chat running after the restart") { chatModel.chatRunning.value == true }
    coreNetworkConfig().let { c -> check(TIMEOUTS.all { it.core(c) == it.new }) { "network config after the restart: $c" } }
    val again = networkAndServers(after).advanced()
    for (t in TIMEOUTS) check(again.value(t.name) == t.newText) { "${t.name} shows `${again.value(t.name)}` after the restart" }
  }

  @Test fun advNet_keepAlive() = uiTest {
    val alice = app("alice")
    check(storedSetting(KEEP_ALIVE) != "false" && coreNetworkConfig().tcpKeepAlive == KeepAliveOpts.defaults) { "keep-alive before the test: ${coreNetworkConfig().tcpKeepAlive}" }
    var adv = networkAndServers(alice).advanced()
    check(adv.value("tcpKeepIdle") == "30 sec" && adv.value("tcpKeepCnt") == "4") { "TCP_KEEPIDLE `${adv.value("tcpKeepIdle")}`, TCP_KEEPCNT `${adv.value("tcpKeepCnt")}`" }
    click(Ui.AdvancedNetwork.KeepAlive)
    poll(what = "the TCP_KEEP* options turned into plain rows") {
      !exists(Ui.AdvancedNetwork.Setting("tcpKeepIdle")) && !exists(Ui.AdvancedNetwork.Setting("tcpKeepCnt")) && seesText("TCP_KEEPIDLE") && seesText("TCP_KEEPCNT")
    }
    adv.saveAndReconnect()
    poll(what = "keep-alive stored as off") { storedSetting(KEEP_ALIVE) == "false" }
    val after = restart()
    poll(what = "chat running after the restart") { chatModel.chatRunning.value == true }
    adv = networkAndServers(after).advanced()
    check(exists(Ui.AdvancedNetwork.KeepAlive) && seesText("TCP_KEEPIDLE") && !exists(Ui.AdvancedNetwork.Setting("tcpKeepIdle"))) { "keep-alive is on again after the restart" }
    click(Ui.AdvancedNetwork.KeepAlive)
    adv.select("tcpKeepIdle", 60)
    adv.select("tcpKeepCnt", 8)
    poll(what = "TCP_KEEPIDLE `60 sec` and TCP_KEEPCNT `8`") { adv.value("tcpKeepIdle") == "60 sec" && adv.value("tcpKeepCnt") == "8" }
    adv.saveAndReconnect()
    poll(what = "keep-alive stored as on with the new values and applied to the core") {
      storedSetting(KEEP_ALIVE) == "true" && storedSetting("NetworkTCPKeepIdle") == "60" && storedSetting("NetworkTCPKeepCnt") == "8" &&
        coreNetworkConfig().tcpKeepAlive == KeepAliveOpts(keepIdle = 60, keepIntvl = KeepAliveOpts.defaults.keepIntvl, keepCnt = 8)
    }
  }

  @Test fun advNet_resetToDefaults() = uiTest {
    val alice = app("alice")
    saveNetworkConfigViaApi(NetCfg.defaults.copy(tcpTimeoutPerKb = 20_000, smpPingCount = 5))
    val adv = networkAndServers(alice).advanced()
    check(adv.value("tcpTimeoutPerKb") == "0.02 sec" && adv.value("smpPingCount") == "5") {
      "the saved values are not shown: `${adv.value("tcpTimeoutPerKb")}`, `${adv.value("smpPingCount")}`"
    }
    click(Ui.AdvancedNetwork.Reset)
    poll(what = "the defaults shown after `Reset to defaults`") {
      adv.value("tcpTimeoutPerKb") == "0.01 sec" && adv.value("smpPingCount") == "3" && adv.value("tcpConnectTimeout") == "15 sec"
    }
    check(storedSetting(PER_KB) == "20000" && storedSetting(PING_COUNT) == "5" && coreNetworkConfig().let { it.tcpTimeoutPerKb == 20_000L && it.smpPingCount == 5 }) {
      "`Reset to defaults` applied the defaults before `Save and reconnect`: ${coreNetworkConfig()}"
    }
    adv.saveAndReconnect()
    poll(what = "the defaults stored and applied to the core") {
      storedSetting(PER_KB) == "10000" && storedSetting(PING_COUNT) == "3" && coreNetworkConfig().let { it.tcpTimeoutPerKb == 10_000L && it.smpPingCount == 3 }
    }
  }

  @Test fun advNet_unsavedChangesAlert() = uiTest {
    val alice = app("alice")
    val network = networkAndServers(alice)
    network.advanced().back()
    check(!AlertManager.shared.hasAlertsShown()) { "leaving `Advanced settings` without changes showed an alert" }
    var adv = network.advanced()
    adv.select("smpPingCount", 5)
    goBack()
    poll(what = "the `$UPDATE_TITLE` alert with `Save and reconnect` and `Exit without saving`") {
      seesText(UPDATE_TITLE) && exists(ALERT_CONFIRM) && "Save and reconnect" in textsOf(ALERT_CONFIRM) && "Exit without saving" in textsOf(ALERT_DISMISS)
    }
    click(ALERT_DISMISS)
    adv.awaitClosed()
    check(storedSetting(PING_COUNT) != "5" && coreNetworkConfig().smpPingCount == 3) { "`Exit without saving` saved the change: ${coreNetworkConfig()}" }
    adv = network.advanced()
    check(adv.value("smpPingCount") == "3") { "PING count shows `${adv.value("smpPingCount")}` after `Exit without saving`" }
    adv.select("smpPingCount", 8)
    goBack()
    poll(what = "the `$UPDATE_TITLE` alert") { seesText(UPDATE_TITLE) && exists(ALERT_CONFIRM) }
    click(ALERT_CONFIRM)
    adv.awaitClosed()
    poll(what = "PING count 8 stored and applied to the core") { storedSetting(PING_COUNT) == "8" && coreNetworkConfig().smpPingCount == 8 }
  }
}
