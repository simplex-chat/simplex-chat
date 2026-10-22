package chat.simplex.app.ui.harness

import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiKey
import kotlinx.coroutines.runBlocking

private const val RESEND_MS = 2_000L

fun userIdOf(name: String): Long = chatModel.users.first { it.user.displayName == name }.user.userId

fun userShowsNtfs(userId: Long): Boolean? = chatModel.users.firstOrNull { it.user.userId == userId }?.user?.showNtfs

fun hideUserViaApi(userId: Long, password: String) {
  val user = chatModel.users.first { it.user.userId == userId }.user
  chatModel.updateUser(runBlocking { chatController.apiHideUser(user, password) })
}

fun setFullNameViaApi(fullName: String) {
  val profile = chatModel.currentUser.value!!.profile.toProfile().copy(fullName = fullName)
  val (updated, _) = runBlocking { chatController.apiUpdateProfile(null, profile) } ?: error("apiUpdateProfile failed")
  chatModel.updateCurrentUser(null, updated)
}

fun UiScope.unreadBadge(key: UiKey): String? =
  textsOf(key).firstOrNull { it.isNotEmpty() && it.all(Char::isDigit) }

fun UiScope.openProfileMenu(userId: Long, action: String) {
  openContextMenu(Ui.Profiles.User(userId))
  waitFor(Ui.Profiles.UserMenu(userId, "delete"))
  check(exists(Ui.Profiles.UserMenu(userId, action))) { "the menu of user $userId has no `$action`" }
}

fun UiScope.profileEditorSave() {
  hideKeyboard()
  poll(what = "`Save and notify contacts` to be enabled") { isEnabled(Ui.Profile.Save) }
  click(Ui.Profile.Save)
}

fun UiScope.leaveProfileEditorExpectingSaveAlert() {
  hideKeyboard()
  goBack()
  poll(what = "the `Save preferences?` alert with `Save and notify contacts` and `Exit without saving`") {
    seesText("Save preferences?") &&
      "Save and notify contacts" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) &&
      "Exit without saving" in textsOf(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
  }
}

fun UiScope.sendUntilAuthFailed(peer: CliPeer, contact: String) {
  val failed = Regex("\\[$contact, contactId: \\d+, connId: \\d+] error: connection authorization failed.*")
  var sentAt = 0L
  var n = 0
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "${peer.name}'s messages to $contact to fail with `connection authorization failed`") {
    if (System.currentTimeMillis() - sentAt >= RESEND_MS) {
      peer sends "@$contact are you there ${++n}"
      sentAt = System.currentTimeMillis()
    }
    peer.pending().any { failed.matches(it) }
  }
  peer.eventuallyMatching(failed)
}
