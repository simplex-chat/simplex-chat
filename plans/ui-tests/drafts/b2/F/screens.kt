package chat.simplex.app.ui.harness

import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiIdle

fun UiScope.openRowMenu(name: String): String {
  val id = ChatListScreen(this).chatId(name)
  poll(what = "API calls to finish before opening the menu of `$name`") { UiIdle.inFlight().isEmpty() }
  settle()
  rightClick(Ui.ChatList.Row(id))
  return id
}

fun CliPeer.createOneTimeLink(): String {
  this types "/c"
  var link: String? = null
  while (link == null) link = Regex("(https://\\S+|simplex:/\\S+)").find(nextLine())?.value
  return link
}
