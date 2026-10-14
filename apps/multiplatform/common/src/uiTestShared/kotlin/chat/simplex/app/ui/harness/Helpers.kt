package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.GroupMemberStatus
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiIdle
import chat.simplex.common.ui.UiKey

const val CONNECT_TIMEOUT_MS = 60_000L
const val QUIET_MS = 1_000L
const val DESCRIBED = "@described"

fun described(contentDescription: String) = chat.simplex.common.ui.UiKey(DESCRIBED, contentDescription)

fun UiScope.goBack() {
  if (chat.simplex.common.platform.appPlatform.isDesktop) click(described("Back")) else escape()
}

fun UiScope.quiesce() {
  poll(timeoutMs = 30_000, what = "no API calls in flight and no app events for $QUIET_MS ms") {
    chat.simplex.common.ui.UiIdle.inFlight().isEmpty() && System.currentTimeMillis() - chat.simplex.common.ui.UiIdle.lastEventAt >= QUIET_MS
  }
}

fun UiScope.connect(peer: CliPeer) {
  val (link, _) = kotlinx.coroutines.runBlocking { chat.simplex.common.platform.chatController.apiAddContact(null, incognito = false) }.first
    ?: error("apiAddContact failed")
  peer sends "/c ${link.simplexChatUri(short = true)}"
  peer expects "confirmation sent!"
  peer.eventually("${chat.simplex.common.platform.chatModel.currentUser.value!!.displayName}: contact is connected", CONNECT_TIMEOUT_MS)
}

fun UiScope.connectPeers(a: CliPeer, b: CliPeer) {
  val link = a.createOneTimeLink()
  b sends "/c $link"
  b expects "confirmation sent!"
  a.eventually("${b.name}: contact is connected", CONNECT_TIMEOUT_MS)
  b.eventually("${a.name}: contact is connected", CONNECT_TIMEOUT_MS)
}

fun UiScope.hostGroup(host: CliPeer, group: String, vararg members: String) {
  host.createGroup(group)
  for (m in members) {
    host sends "/a $group $m"
    host.eventually("invitation to join the group #$group sent to $m")
  }
}

fun CliPeer.createGroup(group: String) {
  this sends "/g $group"
  this expects "group #$group is created"
  this expects "to add members use /a $group <name> or /create link #$group"
}

fun CliPeer.createOneTimeLink(): String {
  this types "/c"
  var link: String? = null
  while (link == null) link = Regex("(https://\\S+|simplex:/\\S+)").find(nextLine())?.value
  return link
}

fun UiScope.openContextMenu(key: UiKey) {
  poll(what = "API calls to finish before opening the menu of ${key.tag}") { UiIdle.inFlight().isEmpty() }
  settle()
  rightClick(key)
}

fun UiScope.joinGroupFromList(app: App, host: CliPeer, group: String) {
  app.chatList.open(group)
  click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
  host.eventually("#$group: ${chatModel.currentUser.value!!.displayName} joined the group", CONNECT_TIMEOUT_MS)
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "membership of #$group to be connected") {
    (app.chatList.chat(group).chatInfo as ChatInfo.Group).groupInfo.membership.memberStatus == GroupMemberStatus.MemConnected
  }
  quiesce()
}
