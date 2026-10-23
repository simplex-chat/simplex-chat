package chat.simplex.app.ui.harness

import chat.simplex.common.model.GroupMemberRole
import chat.simplex.common.platform.chatController
import chat.simplex.common.ui.Ui
import kotlinx.coroutines.runBlocking

fun UiScope.openNewGroupLink() {
  click(Ui.GroupInfo.CreateLink)
  waitFor(Ui.GroupLink.Share, CONNECT_TIMEOUT_MS)
  waitStable(Ui.GroupLink.Share)
}

fun UiScope.sharedGroupLink(previous: String? = null): String {
  if (previous == null) setClipboardText(null)
  click(Ui.GroupLink.Share)
  var link: String? = null
  poll(what = "the group link shared") { link = clipboardText()?.takeIf { it.isNotBlank() && it != previous }; link != null }
  return link!!
}

fun UiScope.pickGroupLinkRole(role: GroupMemberRole) {
  click(Ui.GroupLink.Role)
  click(Ui.GroupLink.Role.option(role))
}

fun coreGroupLinkRole(groupId: Long): GroupMemberRole? =
  runBlocking { chatController.apiGetGroupLink(null, groupId) }?.acceptMemberRole

fun coreMemberContactId(groupId: Long, name: String): Long? =
  runBlocking { chatController.apiListMembers(null, groupId) }.firstOrNull { it.memberProfile.displayName == name }?.memberContactId

fun UiScope.closeGroupLink() {
  escape()
  poll(what = "the group link screen to close to group info") {
    !exists(Ui.GroupLink.Share) && !seesText("Create link") && (exists(Ui.GroupInfo.CreateLink) || exists(Ui.GroupInfo.GroupLink))
  }
}
