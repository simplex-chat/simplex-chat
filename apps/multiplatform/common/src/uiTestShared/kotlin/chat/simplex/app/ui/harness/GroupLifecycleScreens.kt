package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.GroupMemberRole
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiKey
import chat.simplex.common.views.helpers.ModalManager

class InviteMembersScreen(private val s: UiScope) {
  init {
    s.poll(what = "the `Invite members` sheet with `New member role`") { s.exists(Ui.InviteMembers.Role) }
    s.waitStable(Ui.InviteMembers.Role)
  }

  fun listed(name: String): Boolean = s.exists(Ui.InviteMembers.Contact(contactId(name)))

  fun toggle(name: String) = s.click(Ui.InviteMembers.Contact(contactId(name)))

  fun roleTexts(): List<String> = s.textsOf(Ui.InviteMembers.Role)

  fun role(r: GroupMemberRole) {
    s.click(Ui.InviteMembers.Role)
    s.click(Ui.InviteMembers.Role.option(r))
  }

  fun invite() {
    s.click(Ui.InviteMembers.Invite)
    s.poll(what = "the invite sheet to close") { !s.exists(Ui.InviteMembers.Role) }
  }

  private fun contactId(name: String): Long =
    chatModel.chats.value.firstNotNullOf { (it.chatInfo as? ChatInfo.Direct)?.contact?.takeIf { c -> c.displayName == name }?.contactId }
}

class GroupProfileScreen(private val s: UiScope) {
  init {
    s.waitFor(Ui.GroupProfile.Name)
    s.waitStable(Ui.GroupProfile.Name)
  }

  fun name(): String? = s.editableText(Ui.GroupProfile.Name)

  fun shortDescr(): String? = s.editableText(Ui.GroupProfile.ShortDescr)

  fun rename(name: String) = s.replaceText(Ui.GroupProfile.Name, name)

  fun describe(text: String) = s.typeInto(Ui.GroupProfile.ShortDescr, text)

  fun save() {
    s.click(Ui.GroupProfile.Save)
    s.poll(what = "the group profile editor to close after `Save group profile`") { !s.exists(Ui.GroupProfile.Name) }
  }
}

class WelcomeMessageScreen(private val s: UiScope) {
  init {
    s.poll(what = "the `Welcome message` editor or read-only text") { s.exists(Ui.Welcome.Text) || s.exists(Ui.Welcome.Preview) }
  }

  fun text(): String = s.editableText(Ui.Welcome.Text).orEmpty()

  fun type(text: String) = s.typeInto(Ui.Welcome.Text, text)

  fun replace(text: String) = s.replaceText(Ui.Welcome.Text, text)

  fun save() = s.click(Ui.Welcome.Save)

  fun close() {
    s.goBack()
    s.poll(what = "the welcome message screen to close without an alert") {
      !s.exists(Ui.Welcome.Text) && !s.exists(Ui.Welcome.Preview) && !s.seesText("Save welcome message?")
    }
  }
}

fun UiScope.groupInfoRow(key: UiKey) {
  scrollTo(key)
  click(key)
}

fun UiScope.inviteMembers(): InviteMembersScreen {
  groupInfoRow(Ui.GroupInfo.AddMembers)
  return InviteMembersScreen(this)
}

fun UiScope.editGroupProfile(): GroupProfileScreen {
  groupInfoRow(Ui.GroupInfo.EditProfile)
  return GroupProfileScreen(this)
}

fun UiScope.welcomeMessage(): WelcomeMessageScreen {
  groupInfoRow(Ui.GroupInfo.WelcomeMessage)
  return WelcomeMessageScreen(this)
}

fun UiScope.welcomeRowText(): List<String> {
  scrollTo(Ui.GroupInfo.WelcomeMessage)
  return textsOf(Ui.GroupInfo.WelcomeMessage)
}

fun UiScope.leaveGroupFromInfo(group: String) {
  groupInfoRow(Ui.GroupInfo.Leave)
  poll(what = "the `Leave group?` alert for `$group` with `Leave`") {
    seesText("Leave group?") && seesText("$group\n\nYou will stop receiving messages from this group. Chat history will be preserved.") &&
      "Leave" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
  }
  click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "group info to close after leaving `$group`") { !ModalManager.end.hasModalsOpen() }
}

fun UiScope.deleteGroupFromInfo(group: String, warning: String) {
  groupInfoRow(Ui.GroupInfo.Delete)
  poll(what = "the `Delete group?` alert for `$group` saying `$warning`, with `Delete`") {
    seesText("Delete group?") && seesText("$group\n\n$warning") && "Delete" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
  }
  click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
}

fun UiScope.clearChatFromInfo(group: String) {
  groupInfoRow(Ui.Info.ClearChat)
  poll(what = "the `Clear chat?` alert for `$group` with `Clear`") {
    seesText("Clear chat?") && seesText("$group\n\nAll messages will be deleted - this cannot be undone! The messages will be deleted ONLY for you.") &&
      "Clear" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
  }
  click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
}

fun UiScope.offersOnlyDelete(): Boolean {
  scrollTo(Ui.GroupInfo.Delete)
  return exists(Ui.GroupInfo.Delete) && !exists(Ui.GroupInfo.Leave)
}

fun UiScope.offersOnlyLeave(): Boolean {
  scrollTo(Ui.GroupInfo.Leave)
  return exists(Ui.GroupInfo.Leave) && !exists(Ui.GroupInfo.Delete)
}

fun UiScope.hostGroupWithWelcome(host: CliPeer, group: String, welcome: String, member: String) {
  host.createGroup(group)
  host sends "/set welcome #$group $welcome"
  host expects "welcome message changed to:"
  host expects welcome
  host sends "/a $group $member"
  host.eventually("invitation to join the group #$group sent to $member")
}
