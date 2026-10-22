package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.App
import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.ChatScreen
import chat.simplex.app.ui.harness.CliPeer
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.clearChatFromInfo
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.deleteGroupFromInfo
import chat.simplex.app.ui.harness.hostGroup
import chat.simplex.app.ui.harness.joinGroupFromList
import chat.simplex.app.ui.harness.leaveGroupFromInfo
import chat.simplex.app.ui.harness.offersOnlyDelete
import chat.simplex.app.ui.harness.offersOnlyLeave
import chat.simplex.app.ui.harness.ownGroup
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.ModalManager
import org.junit.Test

class LeaveDeleteClearTest : UiTest() {
  @KnownAppIssue("15.5 item 14", "a list to scroll to groupInfo.delete", intermittent = true)
  @Test fun leaveGroup_thenDeleteLocalCopy() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val chat = memberOfBobsTeam(alice, bob)
    chat.groupInfo()
    check(offersOnlyLeave()) { "a current member's group info does not offer only `Leave group`" }
    leaveGroupFromInfo("team")
    bob.eventually("#team: alice left the group (signed)", CONNECT_TIMEOUT_MS)
    poll(what = "`you left` in team's compose field") { seesText("you left") }
    chat.groupInfo()
    check(offersOnlyDelete()) { "group info after leaving does not offer only `Delete group`" }
    deleteGroupFromInfo("team", "Group will be deleted for you - this cannot be undone!")
    alice.chatList.gone("team")
    alice.chatList.open("bob").send("after delete")
    bob.expectsMessage("alice> after delete", CONNECT_TIMEOUT_MS)
  }

  @Test fun deleteGroup_ownerDeletesForAll() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    ownGroup(alice, "team", bob).groupInfo()
    deleteGroupFromInfo("team", "Group will be deleted for all members - this cannot be undone!")
    bob.expectsMessage("#team: alice deleted the group (signed)", CONNECT_TIMEOUT_MS)
    bob expects "use /d #team to delete the local copy of the group"
    alice.chatList.gone("team")
  }

  @Test fun deleteGroup_memberSeesGroupDeleted() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val chat = memberOfBobsTeam(alice, bob)
    bob sends "/d #team"
    bob.eventuallyMatching(Regex("#team: you deleted the group( \\(signed\\))?"))
    chat.itemId("bob's `deleted group` event in team") { it.content.text == "deleted group" }
    poll(what = "`group is deleted` in team's compose field") { seesText("group is deleted") }
    chat.groupInfo()
    check(offersOnlyDelete()) { "group info of a deleted group does not offer only `Delete group`" }
  }

  @Test fun clearChat_removesLocalItemsOnly() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = ownGroup(alice, "team", bob)
    chat.send("hello")
    bob.expectsMessage("#team alice> hello", CONNECT_TIMEOUT_MS)
    bob sends "#team hi there"
    val hello = chat.itemId("hello")
    val hi = chat.itemId("bob's `hi there` in team") { it.content.text == "hi there" }
    hideKeyboard()
    chat.groupInfo()
    clearChatFromInfo("team")
    poll(what = "group info closed with `hello` and `hi there` gone from team's chat") {
      !ModalManager.end.hasModalsOpen() && !exists(Ui.Chat.Item(hello)) && !exists(Ui.Chat.Item(hi)) &&
        chatModel.chatsContext.chatItems.value.none { it.content.text == "hello" || it.content.text == "hi there" }
    }
    check((alice.chatList.chat("team").chatInfo as ChatInfo.Group).groupInfo.membership.memberCurrent) { "alice is no longer a current member of team after clearing its chat" }
    bob sends "#team after clear"
    chat.itemId("bob's `after clear` in team") { it.content.text == "after clear" }
    chat.send("still here")
    bob.expectsMessage("#team alice> still here", CONNECT_TIMEOUT_MS)
  }

  @Test fun removedFromGroup_composeDisabled() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val chat = memberOfBobsTeam(alice, bob)
    bob sends "/rm team alice"
    bob.eventuallyMatching(Regex("#team: you removed alice from the group( \\(signed\\))?"))
    chat.itemId("bob's `removed you` event in team") { it.content.text == "removed you" }
    poll(what = "`removed from group` in team's compose field") { seesText("removed from group") }
    chat.groupInfo()
    check(offersOnlyDelete()) { "group info after removal does not offer only `Delete group`" }
  }

  private fun UiScope.memberOfBobsTeam(alice: App, bob: CliPeer): ChatScreen {
    connect(bob)
    hostGroup(bob, "team", "alice")
    joinGroupFromList(alice, bob, "team")
    backToChatList()
    val chat = alice.chatList.open("team")
    poll(what = "team's compose field with no reason it can't send") {
      exists(Ui.Chat.Message) && !seesText("you left") && !seesText("group is deleted") && !seesText("removed from group")
    }
    return chat
  }
}
