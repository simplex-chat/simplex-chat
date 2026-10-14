package chat.simplex.app.ui.harness

import chat.simplex.common.ui.Ui

fun UiScope.openRowMenu(name: String): String {
  val id = ChatListScreen(this).chatId(name)
  openContextMenu(Ui.ChatList.Row(id))
  return id
}

