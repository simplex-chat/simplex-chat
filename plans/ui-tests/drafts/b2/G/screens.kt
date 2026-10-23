package chat.simplex.app.ui.harness

import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiIdle

fun UiScope.openRowMenu(app: App, name: String): String {
  val id = app.chatList.chatId(name)
  poll(what = "API calls to finish before opening the menu of `$name`") { UiIdle.inFlight().isEmpty() }
  settle()
  rightClick(Ui.ChatList.Row(id))
  return id
}
