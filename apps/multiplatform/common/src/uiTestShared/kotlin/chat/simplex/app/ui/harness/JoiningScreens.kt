package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatInfo
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui

fun CliPeer.createWelcomeGroupLink(group: String, welcome: String): String {
  createGroup(group)
  this sends "/set welcome #$group $welcome"
  this expects "welcome message changed to:"
  this expects welcome
  this sends "/create link #$group"
  this expects "Group link is created!"
  this expects ""
  return nextLine()
}

fun CliPeer.createFullGroupLink(group: String): String {
  createGroup(group)
  this sends "/create link #$group"
  this expects "Group link is created!"
  return eventuallyMatching(Regex("The group link for old clients: (\\S+)")).groupValues[1]
}

fun groupChatIds(name: String): List<String> =
  chatModel.chats.value.filter { it.chatInfo is ChatInfo.Group && it.chatInfo.chatViewName == name }.map { it.id }

fun UiScope.openPreparedGroupFromSearch(link: String, name: String): ChatScreen {
  enterLinkInSearch(link)
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the plan alert for group `$name` with `Open group`") {
    exists(planConfirm) && "Open group" in textsOf(planConfirm) && seesText(name)
  }
  click(planConfirm)
  poll(what = "`$name`'s prepared group open") {
    chatModel.chats.value.firstOrNull { it.chatInfo is ChatInfo.Group && it.chatInfo.chatViewName == name && it.chatInfo.nextConnectPrepared }
      ?.id?.let { it == chatModel.chatId.value } == true
  }
  return ChatScreen(this)
}

fun UiScope.openChatById(id: String): ChatScreen {
  click(Ui.ChatList.Row(id))
  poll(what = "chat $id to open") { chatModel.chatId.value == id }
  return ChatScreen(this)
}

fun UiScope.connectionRequestSentToGroup() {
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`Connection request sent!` for a group link") {
    seesText("Connection request sent!") && seesText("You will be connected to group when the group host's device is online, please wait or check later!")
  }
  click(planConfirm)
  poll(what = "the alert to close and the search to clear") {
    !seesText("Connection request sent!") && editableText(Ui.ChatList.Search).isNullOrEmpty() && isEnabled(Ui.ChatList.Search)
  }
}
