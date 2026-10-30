package chat.simplex.app.ui.harness

import chat.simplex.common.model.MsgContent
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui

val planConfirm = Ui.Alert.Confirm(Ui.AlertSource.PrivacySensitive)

val planDismiss = Ui.Alert.Dismiss(Ui.AlertSource.PrivacySensitive)
val planProfileName = Ui.Alert.ProfileName(Ui.AlertSource.PrivacySensitive)

fun planOption(name: String) = Ui.Alert.Option(Ui.AlertSource.PrivacySensitive, name)

fun UiScope.openPasteLink(app: App) {
  app.newChat()
  click(Ui.NewChat.PasteLink)
  poll(what = "the `Paste the link you received` section with `Tap to paste link`") {
    seesText("Paste the link you received") && exists(Ui.Link.Paste) && "Tap to paste link" in textsOf(Ui.Link.Paste)
  }
}

fun UiScope.enterLinkInNewChatSearch(app: App, link: String) {
  app.newChat()
  enterText(Ui.NewChat.Search, link)
}

fun UiScope.openNewChatFromSearch(link: String, name: String): ChatScreen {
  enterLinkInSearch(link)
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the plan alert for `$name` with `Open new chat`") {
    exists(planConfirm) && "Open new chat" in textsOf(planConfirm) && seesText(name)
  }
  click(planConfirm)
  poll(what = "`$name`'s prepared chat open") {
    chatModel.chats.value.firstOrNull { it.chatInfo.chatViewName == name && it.chatInfo.nextConnectPrepared }?.id?.let { it == chatModel.chatId.value } == true
  }
  return ChatScreen(this)
}

fun UiScope.chatLinkItemId(): Long {
  var id: Long? = null
  poll(what = "a chat link card in the open chat") {
    id = chatModel.chatsContext.chatItems.value.lastOrNull { it.content.msgContent is MsgContent.MCChat }?.id
    id != null && exists(Ui.Chat.Item(id!!))
  }
  return id!!
}

fun CliPeer.createFullOneTimeLink(): String {
  this types "/c"
  eventually("The invitation link for old clients:")
  return nextLine()
}

fun CliPeer.createFullAddress(): String {
  this sends "/ad"
  this expects "Your new chat address is created!"
  return eventuallyMatching(Regex("The contact link for old clients: (\\S+)")).groupValues[1]
}

fun CliPeer.becomeBot() {
  this sends """/_profile 1 {"displayName":"$name","fullName":"","peerType":"bot"}"""
  this sends "/u"
  eventually("user profile: $name (bot)")
  this expects "use /p <name> [<bio>] to change it"
}
