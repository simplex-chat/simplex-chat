package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.GroupMemberStatus
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import kotlinx.coroutines.runBlocking

fun UiScope.hasRowMark(name: String, mark: String): Boolean =
  exists(Ui.ChatList.Row(ChatListScreen(this).chatId(name)).part(mark))

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

fun UiScope.joinGroupFromList(app: App, host: CliPeer, group: String) {
  app.chatList.open(group)
  click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
  host.eventually("#$group: ${chatModel.currentUser.value!!.displayName} joined the group", CONNECT_TIMEOUT_MS)
  poll(what = "membership of #$group to become active") {
    (app.chatList.chat(group).chatInfo as ChatInfo.Group).groupInfo.membership.memberStatus !in listOf(GroupMemberStatus.MemInvited, GroupMemberStatus.MemAccepted)
  }
  quiesce()
}

fun UiScope.connectIncognito(peer: CliPeer): String {
  val (link, _) = runBlocking { chatController.apiAddContact(null, incognito = true) }.first ?: error("apiAddContact failed")
  peer sends "/c ${link.simplexChatUri(short = true)}"
  peer expects "confirmation sent!"
  return peer.eventuallyMatching(Regex("(\\S+): contact is connected"), CONNECT_TIMEOUT_MS).groupValues[1]
}
