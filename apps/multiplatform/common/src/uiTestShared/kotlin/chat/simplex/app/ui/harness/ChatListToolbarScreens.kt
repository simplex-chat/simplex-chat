package chat.simplex.app.ui.harness

import chat.simplex.common.model.OperatorTag
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import kotlinx.coroutines.runBlocking

class ChatListToolbar(private val s: UiScope) {
  private val title = Ui.ChatList.Bar.part("title")

  fun titleTexts(): List<String> = s.textsOf(title)

  fun canScrollToTop(): Boolean {
    s.waitFor(title)
    return s.isEnabled(title)
  }

  fun scrollToTop() = s.click(title)

  fun profileBadge(): Boolean = s.textsOf(Ui.ChatList.UserPicker).isNotEmpty()

  fun percentage(): String? = s.textsOf(Ui.ChatList.Subscriptions).firstOrNull { it.endsWith("%") }

  fun toggleShowPercentage() {
    s.click(Ui.ChatList.Subscriptions)
    s.click(Ui.ServersSummary.ShowPercentage)
    s.escape()
    s.poll(what = "the servers info screen to close") { !s.exists(Ui.ServersSummary.ShowPercentage) }
  }
}

fun UiScope.chatListToolbar() = ChatListToolbar(this)

class WhatsNewScreen(private val s: UiScope) {
  fun shown(): Boolean = s.seesText(TITLE)

  fun ok() {
    s.click(Ui.WhatsNew.Ok)
    s.poll(what = "What's New to close") { !s.exists(Ui.WhatsNew.Ok) && !shown() }
  }

  companion object {
    val VERSION: String by lazy {
      CommonFields.whatsNewLastVersion.field.get(null) as String
    }
    val TITLE: String get() = "New in $VERSION"
    const val OLDER_VERSION = "v6.5"
  }
}

fun UiScope.whatsNewFromSettings(app: App): WhatsNewScreen {
  app.settings()
  click(Ui.Settings.Help)
  click(Ui.Help.WhatsNew)
  return WhatsNewScreen(this)
}

fun UiScope.backToChatList() {
  if (appPlatform.isAndroid && chatModel.chatId.value != null) {
    hideKeyboard()
    escape()
    poll(what = "the chat to close") { chatModel.chatId.value == null }
  }
  waitFor(Ui.ChatList.Search)
}

fun enableOperator(tag: OperatorTag) {
  val groups = runBlocking { chatController.getUserServers(null) } ?: error("getUserServers failed")
  val updated = groups.map { g -> g.operator?.takeIf { it.operatorTag == tag }?.let { g.copy(operator = it.copy(enabled = true)) } ?: g }
  check(updated != groups) { "no operator $tag to enable" }
  check(runBlocking { chatController.setUserServers(null, updated) }) { "setUserServers failed" }
}
