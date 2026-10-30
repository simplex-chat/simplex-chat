package chat.simplex.app.ui.harness

import chat.simplex.common.ui.Ui

class ServersSummaryScreen(private val s: UiScope) {
  init {
    s.poll(what = "`Servers info` with the loaded summary") { s.seesText("Servers info") && s.exists(Ui.ServersSummary.ResetStats) }
    s.waitStable(Ui.ServersSummary.Details)
  }

  fun value(title: String): String? = s.texts().let { t -> t.indexOf(title).takeIf { it >= 0 }?.let { t.getOrNull(it + 1) } }

  fun subscribed(): String? = s.texts().let { t -> t.indexOf("Subscribed").takeIf { it >= 0 }?.let { t.getOrNull(it + 3) } }

  fun details() {
    s.click(Ui.ServersSummary.Details)
    s.poll(what = "`Detailed statistics` with the `Subscribed` row") { s.seesText("Detailed statistics") && s.seesText("Subscribed") && !s.seesText("Connected servers") }
  }

  fun backToSummary() {
    s.goBack()
    s.poll(what = "back on `Servers info`") { !s.seesText("Detailed statistics") && s.exists(Ui.ServersSummary.ResetStats) }
    s.waitStable(Ui.ServersSummary.Details)
  }
}

fun UiScope.openServersSummary(): ServersSummaryScreen {
  backToChatList()
  click(Ui.ChatList.Subscriptions)
  return ServersSummaryScreen(this)
}

fun String?.isCount(): Boolean = this != null && isNotEmpty() && all(Char::isDigit) && this != "0"
