package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatItem
import chat.simplex.common.model.GroupProfile
import chat.simplex.common.platform.chatController
import kotlinx.coroutines.runBlocking
import java.io.File

fun CliPeer.sendsFileTo(contact: String, path: String, fileId: Int, name: String) {
  this sends "/f @$contact $path"
  this expects "use /fc $fileId to cancel sending"
  eventually("completed uploading file $fileId ($name) for $contact", CONNECT_TIMEOUT_MS)
}

fun CliPeer.receivesFile(fileId: Int, name: String, from: String) {
  val dir = File(createFile(".fr-probe", "probe")).parent
  this sends "/fr $fileId $dir/"
  eventually("completed receiving file $fileId ($name) from $from", CONNECT_TIMEOUT_MS)
}

fun newGroupsViaApi(vararg names: String) = names.forEach {
  runBlocking { chatController.apiNewGroup(null, incognito = false, GroupProfile(it, "", null)) } ?: error("apiNewGroup($it) failed")
}

fun UiScope.awaitRowText(app: App, name: String, text: String) =
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`$text` on $name's row") { text in app.chatList.rowTexts(name) }

fun UiScope.awaitRowItem(app: App, name: String, what: String, match: (ChatItem) -> Boolean) =
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "$what as $name's last message in the chat list") { app.chatList.chat(name).chatItems.lastOrNull()?.let(match) == true }
