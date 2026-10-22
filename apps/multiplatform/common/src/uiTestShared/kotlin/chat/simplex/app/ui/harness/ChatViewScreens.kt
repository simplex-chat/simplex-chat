package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatItem
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiKey

fun contentFilter(name: String): UiKey = Ui.Chat.ContentFilter.option(name)

fun UiScope.pickContentFilter(name: String) {
  click(Ui.Chat.ContentFilter)
  click(contentFilter(name))
}

fun loadedItemIds(): Set<Long> = chatModel.chatsContext.chatItems.value.map { it.id }.toSet()

fun UiScope.loadedItemId(what: String, match: (ChatItem) -> Boolean): Long {
  var id: Long? = null
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = what) {
    id = chatModel.chatsContext.chatItems.value.lastOrNull(match)?.id
    id != null
  }
  return id!!
}

fun UiScope.closeChatSearch() {
  hideKeyboard()
  goBack()
  poll(what = "the chat search to close") { !exists(Ui.Chat.Bar.part("search")) }
}
