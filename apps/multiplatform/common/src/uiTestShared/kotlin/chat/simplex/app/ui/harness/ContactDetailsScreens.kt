package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatFeature
import chat.simplex.common.model.FeatureAllowed
import chat.simplex.common.model.FullChatPreferences
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui

class UserPrefsScreen(private val s: UiScope) {
  init {
    s.poll(what = "the `Your preferences` screen") { s.exists(Ui.UserPrefs.Timed) && s.exists(Ui.UserPrefs.Allow(ChatFeature.Calls)) }
    s.waitStable(Ui.UserPrefs.Allow(ChatFeature.FullDelete))
  }

  fun allow(f: ChatFeature, option: FeatureAllowed) {
    s.click(Ui.UserPrefs.Allow(f))
    s.click(Ui.UserPrefs.Allow(f).option(option))
  }

  fun allowTexts(f: ChatFeature): List<String> = s.textsOf(Ui.UserPrefs.Allow(f))

  fun toggleTimed() = s.click(Ui.UserPrefs.Timed)

  fun reset() = s.click(Ui.UserPrefs.Reset)

  fun save() = s.click(Ui.UserPrefs.Save)

  fun backExpectingAlert() {
    s.goBack()
    s.poll(what = "the `Save preferences?` alert") { s.seesText("Save preferences?") }
  }

  fun backClosing() {
    s.goBack()
    s.poll(what = "your preferences to close without an alert") { !s.exists(Ui.UserPrefs.Save) && !s.seesText("Save preferences?") }
  }
}

fun UiScope.userPrefs(app: App): UserPrefsScreen {
  app.userPicker()
  click(described("Chat preferences"))
  return UserPrefsScreen(this)
}

fun savedUserPrefs(): FullChatPreferences = chatModel.currentUser.value!!.fullPreferences

fun UiScope.keepIncognitoOneTimeLink(app: App) {
  val before = pendingConnectionIds()
  app.newChat().createOneTimeLink()
  click(Ui.Link.ShareProfile)
  click(Ui.Link.ProfileIncognito)
  poll(what = "the 1-time link switched to incognito") { seesText(NEW_RANDOM_PROFILE) && !exists(Ui.Link.ProfileIncognito) }
  goBack()
  closeNewChatSheet()
  poll(what = "the incognito link's pending connection kept") { pendingConnectionIds().size == before.size + 1 }
}

fun UiScope.archiveContact(app: App, name: String) {
  app.chatList.rowMenu(name, "delete")
  click(Ui.Alert.Option(Ui.AlertSource.Shared, "onlyConversation"))
  poll(what = "`$name` archived and hidden from the chat list") { contactOf(name).chatDeleted && !app.chatList.shown(name) }
}

fun UiScope.openArchivedContact(app: App, name: String): ChatScreen {
  val id = app.chatList.chatId(name)
  app.newChat()
  click(described("Archived contacts"))
  click(Ui.NewChat.Contact(id))
  poll(what = "`$name`'s archived chat open") { chatModel.chatId.value == id }
  return ChatScreen(this)
}
