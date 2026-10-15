// Additions for apps/multiplatform/common/src/uiTestShared/kotlin/chat/simplex/app/ui/harness/Screens.kt
// (platform-neutral; merge into the existing classes)

class App(private val s: UiScope) {
  fun settings() {
    s.click(Ui.ChatList.UserPicker)
    s.click(Ui.UserPicker.Settings)
  }
}

class ChatScreen(private val s: UiScope) {
  fun openSearch() {
    s.click(Ui.Chat.More)
    s.click(Ui.Chat.Menu("search"))
  }

  fun itemTexts(): List<String> = chatModel.chatsContext.chatItems.value.map { it.content.text }
}
