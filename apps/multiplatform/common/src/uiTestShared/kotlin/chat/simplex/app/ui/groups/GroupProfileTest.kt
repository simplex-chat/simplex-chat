package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.editGroupProfile
import chat.simplex.app.ui.harness.emptyGroup
import chat.simplex.app.ui.harness.groupInfoOf
import chat.simplex.app.ui.harness.hostGroupWithWelcome
import chat.simplex.app.ui.harness.inviteMembers
import chat.simplex.app.ui.harness.joinGroupFromList
import chat.simplex.app.ui.harness.ownGroup
import chat.simplex.app.ui.harness.welcomeMessage
import chat.simplex.app.ui.harness.welcomeRowText
import chat.simplex.common.model.CIDirection
import chat.simplex.common.model.ChatItem
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test

private const val WELCOME = "Welcome to the team!"

class GroupProfileTest : UiTest() {
  @Test fun editGroupProfile_renameNotifiesMembers() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    ownGroup(alice, "team", bob).groupInfo()
    var p = editGroupProfile()
    poll(what = "the profile editor showing `team` with no short description") { p.name() == "team" && p.shortDescr().isNullOrEmpty() }
    p.rename("my_team")
    p.save()
    bob.expectsMessage("alice updated group #team: (signed)", CONNECT_TIMEOUT_MS)
    bob expects "changed to #my_team"
    poll(what = "team renamed to `my_team` in group info") {
      chatModel.chats.value.any { it.chatInfo.chatViewName == "my_team" } && seesText("my_team")
    }
    p = editGroupProfile()
    poll(what = "the profile editor showing `my_team`") { p.name() == "my_team" && p.shortDescr().isNullOrEmpty() }
    p.describe("Our team")
    p.save()
    bob.expectsMessage("alice updated group #my_team: (signed)", CONNECT_TIMEOUT_MS)
    bob expects "description changed to: Our team"
    poll(what = "`Our team` shown in group info and saved") { seesText("Our team", substring = true) && groupInfoOf("my_team").groupProfile.shortDescr == "Our team" }
  }

  @Test fun welcomeMessage_addNotifiesMembers() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    ownGroup(alice, "team", bob).groupInfo()
    poll(what = "`Add welcome message` in team's info") { "Add welcome message" in welcomeRowText() }
    var w = welcomeMessage()
    poll(what = "an empty welcome message editor") { w.text().isEmpty() }
    w.type(WELCOME)
    w.save()
    bob.expectsMessage("alice updated group #team: (signed)", CONNECT_TIMEOUT_MS)
    bob expects "welcome message changed to:"
    bob expects WELCOME
    poll(what = "the welcome message saved for team") { groupInfoOf("team").groupProfile.description == WELCOME }
    w.close()
    poll(what = "`Welcome message` in team's info after saving") { welcomeRowText().let { "Welcome message" in it && "Add welcome message" !in it } }
    w = welcomeMessage()
    poll(what = "the editor holding the saved welcome message") { w.text() == WELCOME }
    w.replace("")
    w.save()
    bob.expectsMessage("alice updated group #team: (signed)", CONNECT_TIMEOUT_MS)
    bob expects "welcome message removed"
    poll(what = "the welcome message removed from team") { groupInfoOf("team").groupProfile.description == null }
    w.close()
    poll(what = "`Add welcome message` back in team's info") { "Add welcome message" in welcomeRowText() }
  }

  @Test fun welcomeMessage_newMemberReceivesItFirst() = uiTest {
    val alice = app("alice"); val cath = cli("cath")
    connect(cath)
    emptyGroup(alice, "team").groupInfo()
    val w = welcomeMessage()
    w.type(WELCOME)
    w.save()
    poll(what = "the welcome message saved for team") { groupInfoOf("team").groupProfile.description == WELCOME }
    w.close()
    val inv = inviteMembers()
    poll(what = "cath offered") { inv.listed("cath") }
    inv.toggle("cath")
    inv.invite()
    cath.expectsMessage("#team: alice invites you to join the group as member", CONNECT_TIMEOUT_MS)
    cath expects "use /j team to accept"
    cath sends "/j team"
    cath.eventually("#team: you joined the group", CONNECT_TIMEOUT_MS)
    cath.expectsMessage("#team alice> $WELCOME", CONNECT_TIMEOUT_MS)
  }

  @Test fun welcomeMessage_receivedAsFirstItem() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    hostGroupWithWelcome(bob, "team", WELCOME, "alice")
    joinGroupFromList(alice, bob, "team")
    backToChatList()
    val chat = alice.chatList.open("team")
    val welcome = chat.itemId("bob's welcome message in team") { it.content.text == WELCOME && it.isFrom("bob") }
    bob sends "#team after welcome"
    val after = chat.itemId("bob's `after welcome` in team") { it.content.text == "after welcome" && it.isFrom("bob") }
    val items = chatModel.chatsContext.chatItems.value
    val messages = items.filter { it.content.msgContent != null }.map { it.id }
    check(messages.firstOrNull() == welcome && messages.indexOf(after) > messages.indexOf(welcome)) {
      "bob's welcome message is not the first message in team, before `after welcome`: ${items.map { it.content.text }}"
    }
    chat.groupInfo()
    poll(what = "`Welcome message` and no `Edit group profile` in a member's group info") {
      welcomeRowText().let { "Welcome message" in it && "Add welcome message" !in it } && !exists(Ui.GroupInfo.EditProfile)
    }
    welcomeMessage()
    poll(what = "bob's welcome message shown read-only with `Copy`, without the editor or `Save and update group profile`") {
      textsOf(Ui.Welcome.Preview).any { WELCOME in it } && seesText("Copy") && !exists(Ui.Welcome.Text) && !exists(Ui.Welcome.Save)
    }
  }

  private fun ChatItem.isFrom(name: String) =
    (chatDir as? CIDirection.GroupRcv)?.groupMember?.memberProfile?.displayName == name
}
