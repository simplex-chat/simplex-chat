package chat.simplex.app.ui.harness

import chat.simplex.common.model.RelayProfile
import chat.simplex.common.model.UserChatRelay
import chat.simplex.common.model.UserServer
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import kotlinx.coroutines.runBlocking

fun configureServers(smp: String, xftp: String) {
  val groups = runBlocking { chatController.getUserServers(null) } ?: error("getUserServers failed")
  fun custom(server: String) = UserServer(remoteHostId = null, serverId = null, server = server, preset = false, enabled = true, deleted = false)
  val updated = groups.map { g ->
    val op = g.operator
    if (op != null) g.copy(operator = op.copy(enabled = false))
    else g.copy(smpServers = g.smpServers.map { it.copy(enabled = false) } + custom(smp), xftpServers = g.xftpServers.map { it.copy(enabled = false) } + custom(xftp))
  }.let { if (it.any { g -> g.operator == null }) it else it + groups.first().copy(operator = null, smpServers = listOf(custom(smp)), xftpServers = listOf(custom(xftp)), chatRelays = emptyList()) }
  check(runBlocking { chatController.setUserServers(null, updated) }) { "setUserServers failed" }
  chatModel.conditions.value = runBlocking { chatController.getServerOperators(null) } ?: error("getServerOperators failed")
}

fun registerChatRelaysViaApi(vararg relays: CliPeer) {
  val groups = runBlocking { chatController.getUserServers(null) } ?: error("getUserServers failed")
  val added = relays.map {
    UserChatRelay(chatRelayId = null, address = it.relayAddress, relayProfile = RelayProfile(displayName = it.name, fullName = ""), domains = emptyList(), preset = false, enabled = true, deleted = false)
  }
  val updated = groups.map { g -> if (g.operator == null) g.copy(chatRelays = g.chatRelays + added) else g }
  check(updated.any { it.operator == null }) { "no custom server group to add chat relays to" }
  check(runBlocking { chatController.setUserServers(null, updated) }) { "setUserServers failed" }
}

const val SOCKS_HOST = "127.0.0.1"

data class SocksConnection(val host: String, val port: Int, val username: String?)
