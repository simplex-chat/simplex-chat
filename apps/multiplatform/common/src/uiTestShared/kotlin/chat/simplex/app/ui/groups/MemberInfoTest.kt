package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.ownGroup
import chat.simplex.common.model.GroupMemberRole
import chat.simplex.common.platform.chatModel
import org.junit.Test

class MemberInfoTest : UiTest() {
  @Test fun memberInfo_changeRoleToAdmin() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    ownGroup(alice, "team", bob).groupInfo().member("bob").changeRole(GroupMemberRole.Admin)
    bob.eventually("#team: alice changed your role from member to admin (signed)", CONNECT_TIMEOUT_MS)
    poll(what = "bob is admin") { chatModel.groupMembers.value.any { it.memberProfile.displayName == "bob" && it.memberRole == GroupMemberRole.Admin } }
  }

  @Test fun memberInfo_removeMember() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    ownGroup(alice, "team", bob, cath).groupInfo().member("bob").remove()
    bob.eventually("#team: alice removed you from the group (signed)", CONNECT_TIMEOUT_MS)
    cath.eventually("#team: alice removed bob from the group (signed)", CONNECT_TIMEOUT_MS)
    poll(what = "bob no longer a current member") { chatModel.groupMembers.value.none { it.memberProfile.displayName == "bob" && it.memberCurrent } }
  }
}
