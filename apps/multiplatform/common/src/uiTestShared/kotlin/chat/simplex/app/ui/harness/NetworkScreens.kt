package chat.simplex.app.ui.harness

import chat.simplex.common.model.CC
import chat.simplex.common.model.CR
import chat.simplex.common.model.NetCfg
import chat.simplex.common.model.NetworkProxy
import chat.simplex.common.model.ServerAddress.Companion.parseServerAddress
import chat.simplex.common.model.ServerOperator
import chat.simplex.common.model.UserOperatorServers
import chat.simplex.common.model.UserServer
import chat.simplex.common.model.json
import chat.simplex.common.platform.chatController
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiKey
import chat.simplex.common.views.helpers.AlertManager
import kotlinx.coroutines.runBlocking

const val UNREACHABLE_HOST = "127.0.0.2"

fun coreNetworkConfig(): NetCfg {
  val r = runBlocking { chatController.sendCmd(null, CC.APIGetNetworkConfig()) }
  return (r.result as? CR.NetworkConfig)?.networkConfig ?: error("/network failed: ${r.responseType} ${r.details}")
}

fun saveNetworkConfigViaApi(cfg: NetCfg) {
  check(runBlocking { chatController.apiSetNetworkConfig(cfg) }) { "apiSetNetworkConfig failed" }
  chatController.setNetCfg(cfg)
}

fun coreOperators(): List<ServerOperator> =
  runBlocking { chatController.getServerOperators(null) }?.serverOperators ?: error("getServerOperators failed")

fun customServers(): UserOperatorServers =
  (runBlocking { chatController.getUserServers(null) } ?: error("getUserServers failed")).single { it.operator == null }

fun hostOf(server: String): String? = parseServerAddress(server)?.hostnames?.firstOrNull()

fun unreachableSmpServer(): String = "smp://${localSmpServer().keyHash}@$UNREACHABLE_HOST:1"

fun customSmpServer(host: String): UserServer? = customServers().smpServers.firstOrNull { hostOf(it.server) == host }

fun addCustomSmpServerViaApi(server: String): String {
  val groups = runBlocking { chatController.getUserServers(null) } ?: error("getUserServers failed")
  val added = UserServer(remoteHostId = null, serverId = null, server = server, preset = false, enabled = true, deleted = false)
  val updated = groups.map { g -> if (g.operator == null) g.copy(smpServers = g.smpServers + added) else g }
  check(runBlocking { chatController.setUserServers(null, updated) }) { "setUserServers failed" }
  return customSmpServer(hostOf(server)!!)?.server ?: error("the added server `$server` is not in the core")
}

fun UiScope.networkAndServers(app: App): NetworkScreen {
  app.settings()
  return openNetworkFromSettings()
}

fun UiScope.openNetworkFromSettings(): NetworkScreen {
  click(Ui.Settings.Network)
  return NetworkScreen(this)
}

class NetworkScreen(private val s: UiScope) {
  init {
    s.poll(what = "`Network & servers` with the servers loaded") { s.exists(Ui.Network.YourServers) && s.exists(Ui.Network.SaveServers) && !s.exists(Ui.Settings.Network) }
  }

  fun edited(key: UiKey): Boolean = EDITED in s.descriptionsOf(key)

  fun yourServers(): YourServersScreen {
    s.click(Ui.Network.YourServers)
    return YourServersScreen(s)
  }

  fun operator(operatorId: Long): OperatorScreen {
    s.click(Ui.Network.Operator(operatorId))
    return OperatorScreen(s)
  }

  fun save(row: UiKey) {
    s.click(Ui.Network.SaveServers)
    s.poll(what = "`Save servers` to save and clear the `edited` mark of ${row.tag}") { !edited(row) && !AlertManager.shared.hasAlertsShown() }
  }

  fun advanced(): AdvancedNetworkScreen {
    s.click(Ui.Network.Advanced)
    return AdvancedNetworkScreen(s)
  }

  fun socks(): SocksScreen {
    s.click(Ui.Network.SocksSettings)
    return SocksScreen(s)
  }

  fun enableSocks() {
    val confirm = Ui.Alert.Confirm(Ui.AlertSource.Shared)
    s.click(Ui.Network.UseSocks)
    s.poll(what = "the `Use SOCKS proxy?` alert with `Confirm`") { s.seesText("Use SOCKS proxy?") && s.exists(confirm) && "Confirm" in s.textsOf(confirm) }
    s.click(confirm)
    s.poll(what = "the `Use SOCKS proxy?` alert to close") { !s.seesText("Use SOCKS proxy?") }
  }

  fun close() {
    s.goBack()
    awaitClosed()
  }

  fun awaitClosed() = s.poll(what = "`Network & servers` to close to the settings") { !s.exists(Ui.Network.SaveServers) && s.exists(Ui.Settings.Network) }

  private companion object {
    const val EDITED = "edited"
  }
}

class YourServersScreen(private val s: UiScope) {
  init {
    s.poll(what = "`Your servers`") { s.exists(Ui.Servers.Add) && !s.exists(Ui.Network.SaveServers) }
  }

  fun open(server: String): ServerScreen {
    s.click(Ui.Servers.Server(server))
    return ServerScreen(s)
  }

  fun add(): ServerScreen {
    s.click(Ui.Servers.Add)
    val manually = Ui.Alert.Option(Ui.AlertSource.Shared, "enterManually")
    s.poll(what = "the `Add server` alert with `Enter server manually`") { s.exists(manually) && "Enter server manually" in s.textsOf(manually) }
    s.click(manually)
    s.poll(what = "`New server`") { s.seesText("New server") && s.exists(Ui.Servers.Address) && !s.exists(Ui.Servers.Add) }
    return ServerScreen(s)
  }

  fun testAll() {
    s.click(Ui.Servers.TestAll)
    s.poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the server tests to finish (`Add server` clickable again)") { s.exists(described(ADD_SERVER)) }
  }

  fun back() {
    s.goBack()
    s.poll(what = "back on `Network & servers`") { !s.exists(Ui.Servers.Add) && s.exists(Ui.Network.SaveServers) }
  }

  private companion object {
    const val ADD_SERVER = "Add server"
  }
}

class ServerScreen(private val s: UiScope) {
  init {
    s.poll(what = "the server screen with `Use for new connections`") { s.exists(Ui.Servers.Use) && !s.exists(Ui.Servers.Add) }
  }

  fun enterAddress(address: String) = s.typeInto(Ui.Servers.Address, address)

  fun toggleUse() = s.click(Ui.Servers.Use)

  fun test() = s.click(Ui.Servers.Test)

  fun delete() {
    s.click(Ui.Servers.Delete)
    awaitClosed()
  }

  fun back() {
    s.hideKeyboard()
    s.goBack()
    awaitClosed()
  }

  private fun awaitClosed() = s.poll(what = "back on `Your servers`") { !s.exists(Ui.Servers.Use) && s.exists(Ui.Servers.Add) }
}

class OperatorScreen(private val s: UiScope) {
  init {
    s.poll(what = "the operator screen with `Use servers`") { s.exists(Ui.Operator.Use) && !s.exists(Ui.Network.SaveServers) }
  }

  fun info() {
    s.click(Ui.Operator.Info)
    s.poll(what = "`Network operator`") { s.seesText("Network operator") && !s.exists(Ui.Operator.Use) }
  }

  fun closeInfo() {
    s.goBack()
    s.poll(what = "back on the operator screen") { !s.seesText("Network operator") && s.exists(Ui.Operator.Use) }
  }

  fun toggleUse() = s.click(Ui.Operator.Use)

  fun back() {
    s.goBack()
    s.poll(what = "back on `Network & servers`") { !s.exists(Ui.Operator.Use) && s.exists(Ui.Network.SaveServers) }
  }
}

class AdvancedNetworkScreen(private val s: UiScope) {
  init {
    s.poll(what = "`Advanced settings`") { s.exists(Ui.AdvancedNetwork.Save) && !s.exists(Ui.Network.SaveServers) }
  }

  fun value(name: String): String? = s.textsOf(Ui.AdvancedNetwork.Setting(name)).firstOrNull()?.trim()

  fun select(name: String, value: Any) {
    val row = Ui.AdvancedNetwork.Setting(name)
    s.click(row)
    s.click(row.option(value))
    s.poll(what = "the `$name` menu to close") { !s.exists(row.option(value)) }
  }

  fun pick(row: UiKey, value: Any) {
    s.click(row)
    s.poll(what = "the ${row.tag} options") { s.exists(row.option(value)) && !s.exists(Ui.AdvancedNetwork.Save) }
    s.click(row.option(value))
    s.goBack()
    s.poll(what = "back on `Advanced settings` from the ${row.tag} options") { !s.exists(row.option(value)) && s.exists(Ui.AdvancedNetwork.Save) }
  }

  fun saveAndReconnect() {
    s.click(Ui.AdvancedNetwork.Save)
    confirmUpdate(s)
  }

  fun back() {
    s.goBack()
    awaitClosed()
  }

  fun awaitClosed() = s.poll(what = "`Advanced settings` to close") { !s.exists(Ui.AdvancedNetwork.Save) && s.exists(Ui.Network.SaveServers) }
}

class SocksScreen(private val s: UiScope) {
  init {
    s.poll(what = "`SOCKS proxy settings`") { s.exists(Ui.Socks.Save) && s.exists(Ui.Socks.OnionHosts) && !s.exists(Ui.Network.SaveServers) }
  }

  fun onionHosts(): List<String> = s.textsOf(Ui.Socks.OnionHosts)

  fun saveProxy(host: String, port: Int) {
    s.replaceText(Ui.Socks.Host, host)
    s.replaceText(Ui.Socks.Port, port.toString())
    s.hideKeyboard()
    s.click(Ui.Socks.Save)
    s.poll(what = "the proxy $host:$port stored") { s.storedSetting(PROXY_PREF)?.let { json.decodeFromString(NetworkProxy.serializer(), it) }?.let { it.host == host && it.port == port } == true }
  }

  fun saveAndReconnect() {
    s.click(Ui.Socks.Save)
    confirmUpdate(s)
  }

  fun back() {
    s.hideKeyboard()
    s.goBack()
    s.poll(what = "`SOCKS proxy settings` to close") { !s.exists(Ui.Socks.Save) && s.exists(Ui.Network.SaveServers) }
  }

  private companion object {
    const val PROXY_PREF = "NetworkProxyHostPort"
  }
}

private fun confirmUpdate(s: UiScope) {
  val confirm = Ui.Alert.Confirm(Ui.AlertSource.Shared)
  s.poll(what = "the `Update network settings?` alert with `Update`") { s.seesText("Update network settings?") && s.exists(confirm) && "Update" in s.textsOf(confirm) }
  s.click(confirm)
  s.poll(what = "the `Update network settings?` alert to close") { !s.seesText("Update network settings?") }
}
