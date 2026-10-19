package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.hostGroup
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.GroupMemberStatus
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertFalse
import org.junit.Test

class JoiningCompatTest : UiTest() {
  @Test fun groupInvitation_tapRowShowsJoinAlert() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    hostGroup(bob, "team", "alice")
    val membership = { (alice.chatList.chat("team").chatInfo as ChatInfo.Group).groupInfo.membership.memberStatus }
    alice.chatList.row("team")
    poll(what = "alice invited to #team") { membership() == GroupMemberStatus.MemInvited }
    assertFalse(seesText("Join group?"))
    alice.chatList.open("team")
    poll(what = "the `Join group?` alert with `Join`") { seesText("Join group?") && "Join" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    bob.eventually("#team: alice joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "membership of #team to be connected") { membership() == GroupMemberStatus.MemConnected }
  }
}
