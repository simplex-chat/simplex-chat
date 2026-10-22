package chat.simplex.app.ui.harness

import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui

fun UiScope.visitNotesAndBack(app: App, name: String): ChatScreen {
  backToChatList()
  app.chatList.open("Private notes")
  poll(what = "the notes folder with an empty field and no context bar") {
    chatModel.chatId.value == app.chatList.chatId("Private notes") && editableText(Ui.Chat.Message).isNullOrEmpty() && !seesDescription("Context icon")
  }
  backToChatList()
  return app.chatList.open(name)
}
