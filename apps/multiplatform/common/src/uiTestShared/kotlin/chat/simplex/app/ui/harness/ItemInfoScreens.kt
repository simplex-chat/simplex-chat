package chat.simplex.app.ui.harness

import chat.simplex.common.views.helpers.ModalManager

class ItemInfoScreen(private val s: UiScope) {
  fun history(): List<String> = s.texts().dropWhile { it != "History" }.drop(1)

  fun row(title: String): String? = s.texts().let { t -> t.indexOf(title).takeIf { it >= 0 }?.let { t.getOrNull(it + 1) } }

  fun close() {
    s.hideKeyboard()
    s.escape()
    s.poll(what = "the item info to close") { !ModalManager.end.hasModalsOpen() }
  }
}
