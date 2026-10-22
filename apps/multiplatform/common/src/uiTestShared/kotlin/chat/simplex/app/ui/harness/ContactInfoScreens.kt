package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.ServerAddress
import chat.simplex.common.model.ServerAddress.Companion.parseServerAddress
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiKey
import kotlinx.coroutines.runBlocking

fun setContactAliasViaApi(name: String, alias: String) {
  val contactId = contactOf(name).contactId
  runBlocking { chatController.apiSetContactAlias(null, contactId, alias) } ?: error("apiSetContactAlias failed")
}

fun markContactVerifiedViaApi(name: String) {
  val contactId = contactOf(name).contactId
  val code = runBlocking { chatController.apiGetContactCode(null, contactId) }?.second ?: error("apiGetContactCode failed")
  val (verified, _) = runBlocking { chatController.apiVerifyContact(null, contactId, code) } ?: error("apiVerifyContact failed")
  check(verified) { "apiVerifyContact did not verify $name with its own code" }
}

fun localSmpServer(): ServerAddress {
  val groups = runBlocking { chatController.getUserServers(null) } ?: error("getUserServers failed")
  val server = groups.flatMap { it.smpServers }.first { it.enabled && !it.deleted && !it.preset }.server
  return parseServerAddress(server) ?: error("cannot parse the local SMP server `$server`")
}

fun isServer(text: String?, expected: ServerAddress): Boolean =
  text != null && parseServerAddress(text)?.let { it.hostnames == expected.hostnames && it.port == expected.port && it.keyHash == expected.keyHash } == true

fun UiScope.copiedServer(row: UiKey): String? {
  setClipboardText(null)
  click(row)
  var copied: String? = null
  poll(what = "the server of ${row.tag} on the clipboard") { copied = clipboardText()?.takeIf { it.isNotBlank() }; copied != null }
  return copied
}

fun UiScope.joinOneTimeLinkInSearch(link: String) {
  enterLinkInSearch(link)
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `Connect via one-time link?` alert with `Use current profile`") {
    seesText("Connect via one-time link?") && exists(planOption("useCurrentProfile"))
  }
  click(planOption("useCurrentProfile"))
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`Connection request sent!`") { seesText("Connection request sent!") && exists(planConfirm) }
  click(planConfirm)
  poll(what = "the alert closed with a pending connection in the list") {
    !seesText("Connection request sent!") && chatModel.chats.value.any { it.chatInfo is ChatInfo.ContactConnection }
  }
}

fun UiScope.infoNameText(): String? = if (exists(Ui.Info.Name)) textOf(Ui.Info.Name) else null
