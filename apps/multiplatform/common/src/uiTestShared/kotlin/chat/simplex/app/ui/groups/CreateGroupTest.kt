package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.ChatScreen
import chat.simplex.app.ui.harness.InviteMembersScreen
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.alertSaying
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.groupInfoOf
import chat.simplex.app.ui.harness.quiesce
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.ModalManager
import org.junit.Assert.assertNotEquals
import org.junit.Test

class CreateGroupTest : UiTest() {
  @Test fun createGroup_buttonDisabledForEmptyOrInvalidName() = uiTest {
    val alice = app("alice")
    alice.newChat()
    click(Ui.NewChat.CreateGroup)
    waitFor(Ui.NewGroup.Name)
    check(editableText(Ui.NewGroup.Name).isNullOrEmpty() && !exists(Ui.NewGroup.Name.part("invalid"))) { "the new group name is not empty, or is flagged invalid while empty" }
    click(Ui.NewGroup.Create)
    typeInto(Ui.NewGroup.Name, "a_1")
    waitFor(Ui.NewGroup.Name.part("invalid"))
    click(Ui.NewGroup.Create)
    click(Ui.NewGroup.Name.part("invalid"))
    poll(what = "the `Invalid name!` alert offering `a-1`") { seesText("Invalid name!") && seesText("Correct name to a-1?") }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the name corrected to `a-1` without the invalid-name icon") {
      editableText(Ui.NewGroup.Name) == "a-1" && !exists(Ui.NewGroup.Name.part("invalid")) && !seesText("Invalid name!")
    }
    check(chatModel.chats.value.none { it.chatInfo is ChatInfo.Group }) { "`Create group` created a group from an empty or invalid name" }
    click(Ui.NewGroup.Create)
    val id = alice.chatList.chatId("a-1")
    poll(what = "group `a-1` open with the invite members sheet and no contacts to add") { chatModel.chatId.value == id && seesText("Invite members") && seesText("No contacts to add") }
    val groups = chatModel.chats.value.filter { it.chatInfo is ChatInfo.Group }.map { it.chatInfo.chatViewName }
    check(groups == listOf("a-1")) { "groups after creating `a-1` are $groups" }
  }

  @Test fun createGroup_skipInviting() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.newChat().createGroup("team")
    val id = alice.chatList.chatId("team")
    val inv = InviteMembersScreen(this)
    poll(what = "bob offered, with `Skip inviting members` and no `Invite to group`") {
      inv.listed("bob") && exists(Ui.InviteMembers.Skip) && !exists(Ui.InviteMembers.Invite) && seesText("No contacts selected")
    }
    inv.toggle("bob")
    poll(what = "`Invite to group` in place of `Skip inviting members` with bob selected") {
      exists(Ui.InviteMembers.Invite) && !exists(Ui.InviteMembers.Skip) && seesText("1 contact(s) selected")
    }
    inv.toggle("bob")
    poll(what = "`Skip inviting members` back after unselecting bob") {
      exists(Ui.InviteMembers.Skip) && !exists(Ui.InviteMembers.Invite) && seesText("No contacts selected")
    }
    click(Ui.InviteMembers.Skip)
    poll(what = "the invite sheet closed with team's chat open") {
      !exists(Ui.InviteMembers.Role) && !ModalManager.end.hasModalsOpen() && chatModel.chatId.value == id
    }
    backToChatList()
    alice.chatList.open("bob").send("after skip")
    bob.expectsMessage("alice> after skip", CONNECT_TIMEOUT_MS)
  }

  @Test fun createGroupIncognito_cliJoinerSeesIncognitoHost() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    alice.newChat()
    click(Ui.NewChat.CreateGroup)
    typeInto(Ui.NewGroup.Name, "team")
    hideKeyboard()
    poll(what = "`Incognito` off with the main profile footer") { exists(Ui.NewGroup.Incognito) && !seesText(RANDOM_PROFILE, substring = true) }
    click(Ui.NewGroup.Incognito)
    poll(what = "the random profile footer after turning `Incognito` on") { seesText(RANDOM_PROFILE, substring = true) }
    click(Ui.NewGroup.Create)
    val id = alice.chatList.chatId("team")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "team open with the `Group link` step showing `Share link` and `Continue`, without `Delete link` or the invite sheet") {
      chatModel.chatId.value == id && exists(Ui.GroupLink.Share) && exists(Ui.GroupLink.Continue) && !seesText("Delete link") && !exists(Ui.InviteMembers.Role)
    }
    val membership = groupInfoOf("team").membership
    check(membership.memberIncognito) { "team was created with alice's main profile" }
    val incognito = membership.memberProfile.displayName
    assertNotEquals("alice", incognito)
    setClipboardText(null)
    click(Ui.GroupLink.Share)
    var link: String? = null
    poll(what = "team's link shared") { link = clipboardText()?.takeIf { it.isNotBlank() }; link != null }
    click(Ui.GroupLink.Continue)
    poll(what = "the link step closed with team's chat open") { !exists(Ui.GroupLink.Share) && !ModalManager.end.hasModalsOpen() && chatModel.chatId.value == id }
    bob sends "/c ${link!!}"
    bob expects "connection request sent!"
    bob.eventually("#team: you joined the group", CONNECT_TIMEOUT_MS)
    quiesce()
    val chat = ChatScreen(this)
    chat.send("hello from the host")
    bob.eventuallyMessage("#team $incognito> hello from the host", CONNECT_TIMEOUT_MS)
    chat.groupInfo()
    scrollTo(Ui.GroupInfo.AddMembers)
    check("Invite members" in textsOf(Ui.GroupInfo.AddMembers)) { "the add button of an incognito group is not `Invite members`: ${textsOf(Ui.GroupInfo.AddMembers)}" }
    click(Ui.GroupInfo.AddMembers)
    alertSaying("Can't invite contacts!", "You're using an incognito profile for this group - to prevent sharing your main profile inviting contacts is not allowed")
    check(!exists(Ui.InviteMembers.Role)) { "the invite sheet opened for an incognito group" }
  }

  private companion object {
    const val RANDOM_PROFILE = "A new random profile will be shared."
  }
}
