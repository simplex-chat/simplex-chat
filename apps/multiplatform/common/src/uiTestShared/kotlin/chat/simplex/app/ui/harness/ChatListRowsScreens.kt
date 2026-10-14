package chat.simplex.app.ui.harness

import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import kotlinx.coroutines.runBlocking

fun UiScope.switchAwayFromChat(app: App) {
  backToChatList()
  app.chatList.open("Private notes")
  poll(what = "the notes folder to open") { chatModel.chatId.value == app.chatList.chatId("Private notes") }
  backToChatList()
}

fun UiScope.closeInfoToChatList() {
  escape()
  backToChatList()
}


fun UiScope.connectIncognito(peer: CliPeer): String {
  val (link, _) = runBlocking { chatController.apiAddContact(null, incognito = true) }.first ?: error("apiAddContact failed")
  peer sends "/c ${link.simplexChatUri(short = true)}"
  peer expects "confirmation sent!"
  return peer.eventuallyMatching(Regex("(\\S+): contact is connected"), CONNECT_TIMEOUT_MS).groupValues[1]
}
