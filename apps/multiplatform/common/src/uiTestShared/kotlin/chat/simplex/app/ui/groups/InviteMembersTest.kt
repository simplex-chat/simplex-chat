package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.connectIncognito
import chat.simplex.app.ui.harness.emptyGroup
import chat.simplex.app.ui.harness.inviteMembers
import chat.simplex.app.ui.harness.ownGroup
import chat.simplex.common.model.CIContent
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test

class InviteMembersTest : UiTest() {
  @Test fun inviteMembers_listsOnlyEligibleContacts() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath"); val dan = cli("dan")
    connect(bob)
    connect(cath)
    val incognito = connectIncognito(dan)
    val info = ownGroup(alice, "team", bob).groupInfo()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob a connected member of team") { "connected" in info.memberTexts("bob") }
    val inv = inviteMembers()
    poll(what = "cath and the incognito contact dan offered, the member bob not offered, nothing selected") {
      inv.listed("cath") && inv.listed("dan") && !inv.listed("bob") && seesText("No contacts selected")
    }
    inv.toggle("dan")
    poll(what = "the `Can't invite contact!` alert") {
      seesText("Can't invite contact!") &&
        seesText("You're trying to invite contact with whom you've shared an incognito profile to the group in which you're using your main profile")
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the alert closed with dan not selected") { !seesText("Can't invite contact!") && seesText("No contacts selected") }
    inv.toggle("cath")
    poll(what = "cath selected") { seesText("1 contact(s) selected") }
    inv.invite()
    cath.expectsMessage("#team: alice invites you to join the group as member", CONNECT_TIMEOUT_MS)
    cath expects "use /j team to accept"
    info.close()
    backToChatList()
    alice.chatList.open("dan").send("hi dan")
    dan.expectsMessage("$incognito> hi dan", CONNECT_TIMEOUT_MS)
  }

  @Test fun inviteMembers_reAddInvitedMember() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val info = emptyGroup(alice, "team").groupInfo()
    val first = inviteMembers()
    poll(what = "bob offered") { first.listed("bob") }
    first.toggle("bob")
    first.invite()
    bob.expectsMessage("#team: alice invites you to join the group as member", CONNECT_TIMEOUT_MS)
    bob expects "use /j team to accept"
    poll(what = "bob listed as invited in team's members") { "invited" in info.memberTexts("bob") }
    val again = inviteMembers()
    poll(what = "the invited bob still offered, nothing selected") { again.listed("bob") && seesText("No contacts selected") }
    again.toggle("bob")
    again.invite()
    bob.expectsMessage("#team: alice invites you to join the group as member", CONNECT_TIMEOUT_MS)
    bob expects "use /j team to accept"
  }

  @Test fun inviteMembers_selectionCounterAndClear() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob)
    connect(cath)
    val info = emptyGroup(alice, "team").groupInfo()
    val inv = inviteMembers()
    poll(what = "bob and cath offered with nothing selected and no `Clear`") {
      inv.listed("bob") && inv.listed("cath") && seesText("No contacts selected") && !exists(Ui.InviteMembers.Clear)
    }
    typeInto(Ui.InviteMembers.Search, "CA")
    poll(what = "only cath offered for `CA`") { inv.listed("cath") && !inv.listed("bob") }
    hideKeyboard()
    inv.toggle("cath")
    poll(what = "`1 contact(s) selected` with `Clear`") { seesText("1 contact(s) selected") && exists(Ui.InviteMembers.Clear) }
    replaceText(Ui.InviteMembers.Search, "")
    poll(what = "bob offered again with cath still selected") { inv.listed("bob") && inv.listed("cath") && seesText("1 contact(s) selected") }
    hideKeyboard()
    inv.toggle("bob")
    poll(what = "`2 contact(s) selected` with `Clear`") { seesText("2 contact(s) selected") && "Clear" in textsOf(Ui.InviteMembers.Clear) }
    click(Ui.InviteMembers.Clear)
    poll(what = "`No contacts selected` without `Clear` after clearing") {
      seesText("No contacts selected") && !seesText("2 contact(s) selected") && !exists(Ui.InviteMembers.Clear)
    }
    inv.toggle("cath")
    poll(what = "only cath selected") { seesText("1 contact(s) selected") }
    inv.invite()
    cath.expectsMessage("#team: alice invites you to join the group as member", CONNECT_TIMEOUT_MS)
    cath expects "use /j team to accept"
    info.close()
    backToChatList()
    val cathChat = alice.chatList.open("cath")
    val sent = cathChat.itemId("the sent invitation to team in cath's chat") {
      (it.content as? CIContent.SndGroupInvitation)?.groupInvitation?.groupProfile?.displayName == "team"
    }
    poll(what = "`You sent group invitation` in cath's chat") { textsOf(Ui.Chat.Item(sent)).any { "You sent group invitation" in it } }
    backToChatList()
    val bobChat = alice.chatList.open("bob")
    bobChat.send("not invited")
    bob.expectsMessage("alice> not invited", CONNECT_TIMEOUT_MS)
    bobChat.itemId("not invited")
    check(chatModel.chatsContext.chatItems.value.none { it.content is CIContent.SndGroupInvitation }) { "bob's chat shows a group invitation sent to him" }
  }
}
