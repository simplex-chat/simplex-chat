package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatInfo
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.AlertManager

const val NEW_RANDOM_PROFILE = "A new random profile will be shared."
private val INVITATION_PLAN = Regex("invitation link: ok to connect|error: connection authorization failed.*")

fun pendingConnectionIds(): List<String> =
  chatModel.chats.value.filter { it.chatInfo is ChatInfo.ContactConnection }.map { it.id }

fun UiScope.leaveInviteExpectingKeepAlert() {
  goBack()
  poll(what = "the `Keep unused invitation?` alert with `Delete` and `Keep`") {
    seesText("Keep unused invitation?") && seesText("You can view invitation link again in connection details.") &&
      "Delete" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) && "Keep" in textsOf(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
  }
}

fun UiScope.awaitInviteClosed() =
  poll(what = "the 1-time link screen to close, back on the New chat sheet or the chat list") {
    !exists(Ui.Link.Text) && (exists(Ui.NewChat.CreateLink) || !chatModel.newChatSheetVisible.value)
  }

fun UiScope.closeNewChatSheet() {
  awaitInviteClosed()
  poll(what = "no alert over the New chat sheet") { !AlertManager.shared.hasAlertsShown() }
  if (exists(Ui.NewChat.CreateLink)) goBack()
  poll(what = "the New chat sheet to close") { !chatModel.newChatSheetVisible.value && !exists(Ui.NewChat.CreateLink) }
}

fun UiScope.chooseLinkProfile(userId: Long, oldLink: String): String {
  scrollTo(Ui.Link.Profile(userId))
  click(Ui.Link.Profile(userId))
  var link: String? = null
  poll(what = "user $userId active, the profile list closed and a new link shown") {
    link = editableText(Ui.Link.Text)?.takeIf { it.isNotBlank() }
    chatModel.currentUser.value?.userId == userId && !exists(Ui.Link.Profile(userId)) && link != null && link != oldLink
  }
  return link!!
}

fun UiScope.awaitInvitationGone(peer: CliPeer, link: String) =
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the deleted 1-time link to fail its plan on the server") {
    peer sends "/_connect plan 1 $link"
    peer.eventuallyMatching(INVITATION_PLAN).value.startsWith("error")
  }
