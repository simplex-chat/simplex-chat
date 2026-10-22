package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.emptyGroup
import chat.simplex.app.ui.harness.inviteMembers
import chat.simplex.common.model.GroupMemberRole
import org.junit.Test

class InviteMembersCompatTest : UiTest() {
  @Test fun inviteMembers_asObserver() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val info = emptyGroup(alice, "team").groupInfo()
    val inv = inviteMembers()
    poll(what = "bob offered with `New member role` set to `member`") { inv.listed("bob") && "member" in inv.roleTexts() }
    inv.role(GroupMemberRole.Observer)
    poll(what = "`New member role` set to `observer`") { "observer" in inv.roleTexts() && "member" !in inv.roleTexts() }
    inv.toggle("bob")
    inv.invite()
    bob.expectsMessage("#team: alice invites you to join the group as observer", CONNECT_TIMEOUT_MS)
    bob expects "use /j team to accept"
    bob sends "/j team"
    bob.eventually("#team: you joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob listed in team's members as a connected observer") {
      info.memberTexts("bob").let { "observer" in it && "connected" in it }
    }
    bob sends "#team hi"
    bob expects "#team: you don't have permission to send messages"
  }
}
