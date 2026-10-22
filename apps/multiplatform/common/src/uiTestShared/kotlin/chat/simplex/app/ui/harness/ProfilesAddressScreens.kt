package chat.simplex.app.ui.harness

import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiIdle
import chat.simplex.common.ui.UiKey

val PROFILES_SEARCH = Ui.Profiles.Bar.part("search")

fun userHidden(userId: Long): Boolean = chatModel.users.firstOrNull { it.user.userId == userId }?.user?.hidden == true

fun addressShared(): Boolean = chatModel.currentUser.value?.addressShared == true

fun UiScope.labelled(key: UiKey, text: String): Boolean = exists(key) && text in textsOf(key)

fun UiScope.awaitAppIdle(what: String) = poll(what = "app work started by $what to finish") { UiIdle.inFlight().isEmpty() }

fun UiScope.typePassword(key: UiKey, text: String) {
  val before = editableText(key).orEmpty().length
  enterText(key, text)
  poll(what = "field ${key.tag} to hold ${before + text.length} characters") { editableText(key)?.length == before + text.length }
}

fun UiScope.closeTopScreen(what: String, closed: () -> Boolean) {
  hideKeyboard()
  escape()
  poll(what = "$what to close", cond = closed)
}

fun UiScope.closeProfilesScreen() {
  hideKeyboard()
  click(described("Back"))
  poll(what = "the profiles screen to close") { !exists(PROFILES_SEARCH) }
}

fun UiScope.dismissNewChatSheet() {
  hideKeyboard()
  goBack()
  poll(what = "the New chat sheet to close") { !exists(Ui.NewChat.Search) }
}

fun UiScope.pickerRowOpens(app: App, row: UiKey, what: String, shown: () -> Boolean) {
  app.userPicker()
  click(row)
  poll(what = "$what, with the user picker closed") { shown() && !exists(Ui.UserPicker.Sheet) }
}

fun UiScope.pickerRowInert(app: App, row: UiKey, what: String, opened: () -> Boolean) {
  app.userPicker()
  waitFor(row)
  if (isEnabled(row)) click(row)
  settle()
  awaitAppIdle("tapping ${row.tag}")
  check(exists(Ui.ChatList.Search) && !opened()) { "${row.tag} opened $what while chat is stopped" }
  if (exists(Ui.UserPicker.Sheet)) {
    escape()
    poll(what = "the user picker to close") { !exists(Ui.UserPicker.Sheet) }
  }
}

fun UiScope.leaveAddressSettingsExpectingSaveAlert() {
  hideKeyboard()
  escape()
  poll(what = "the `Save settings?` alert with `Save SimpleX address settings` and `Exit without saving`") {
    seesText("Save settings?") &&
      labelled(Ui.Alert.Confirm(Ui.AlertSource.Shared), "Save SimpleX address settings") &&
      labelled(Ui.Alert.Dismiss(Ui.AlertSource.Shared), "Exit without saving")
  }
}

fun UiScope.awaitAddressScreenAfterSettings() {
  poll(what = "address settings to close to the address screen") { !exists(Ui.Address.AutoAccept) && exists(Ui.Address.Business) }
  waitStable(Ui.Address.Business)
}
