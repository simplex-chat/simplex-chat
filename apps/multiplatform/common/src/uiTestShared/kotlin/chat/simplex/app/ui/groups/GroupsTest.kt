package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.connectPeers
import chat.simplex.app.ui.harness.hostGroup
import chat.simplex.app.ui.harness.joinGroupFromList
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test

class GroupsTest : UiTest() {
  @Test fun createGroup_opensChatAndInviteSheet() = uiTest {
    val alice = app("alice")
    alice.newChat().createGroup("team")
    val id = alice.chatList.chatId("team")
    poll(what = "the group chat open with the invite members sheet and no contacts to add") {
      chatModel.chatId.value == id && seesText("Invite members") && seesText("No contacts to add")
    }
  }

  @Test fun groupSend_allMembersReceive() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob)
    connectPeers(bob, cath)
    hostGroup(bob, "team", "alice", "cath")
    joinGroupFromList(alice, bob, "team")
    cath sends "/j team"
    cath.eventually("#team: you joined the group")
    bob.eventually("#team: cath joined the group")
    cath.eventually("#team: member alice is connected", CONNECT_TIMEOUT_MS)
    alice.chatList.open("team").send("hello")
    bob.eventuallyMessage("#team alice> hello", CONNECT_TIMEOUT_MS)
    cath.eventuallyMessage("#team alice> hello", CONNECT_TIMEOUT_MS)
  }

  @Test fun leaveGroup_fromChatListMenu() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    hostGroup(bob, "team", "alice")
    joinGroupFromList(alice, bob, "team")
    alice.chatList.rowMenu("team", "leave")
    poll(what = "the `Leave group?` alert with `Leave`") { seesText("Leave group?") && "Leave" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    bob.eventually("#team: alice left the group (signed)", CONNECT_TIMEOUT_MS)
  }
}
