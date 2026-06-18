package chat.simplex.app

import chat.simplex.common.model.GroupInfo
import chat.simplex.common.model.GroupMember
import chat.simplex.common.model.GroupMemberRole
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

class CanChangeRoleToTest {
  private fun groupInfo(useRelays: Boolean, viewerRole: GroupMemberRole): GroupInfo =
    GroupInfo.sampleData.copy(
      useRelays = useRelays,
      membership = GroupMember.sampleData.copy(memberRole = viewerRole)
    )

  private fun member(role: GroupMemberRole): GroupMember =
    GroupMember.sampleData.copy(memberRole = role)

  @Test
  fun channelOwnerSeesPickerWithoutOwner() {
    val roles = member(GroupMemberRole.Member)
      .canChangeRoleTo(groupInfo(useRelays = true, viewerRole = GroupMemberRole.Owner))
    assertEquals(
      listOf(GroupMemberRole.Observer, GroupMemberRole.Member, GroupMemberRole.Moderator, GroupMemberRole.Admin),
      roles
    )
  }

  @Test
  fun channelNonOwnerSeesNoPicker() {
    val roles = member(GroupMemberRole.Member)
      .canChangeRoleTo(groupInfo(useRelays = true, viewerRole = GroupMemberRole.Admin))
    assertNull(roles)
  }

  @Test
  fun channelRelayMemberSeesNoPicker() {
    val roles = member(GroupMemberRole.Relay)
      .canChangeRoleTo(groupInfo(useRelays = true, viewerRole = GroupMemberRole.Owner))
    assertNull(roles)
  }

  @Test
  fun groupOwnerStillSeesOwner() {
    val roles = member(GroupMemberRole.Member)
      .canChangeRoleTo(groupInfo(useRelays = false, viewerRole = GroupMemberRole.Owner))
    assertEquals(
      listOf(
        GroupMemberRole.Observer, GroupMemberRole.Member, GroupMemberRole.Moderator,
        GroupMemberRole.Admin, GroupMemberRole.Owner
      ),
      roles
    )
  }

  @Test
  fun groupAdminUnchanged() {
    val roles = member(GroupMemberRole.Member)
      .canChangeRoleTo(groupInfo(useRelays = false, viewerRole = GroupMemberRole.Admin))
    assertEquals(
      listOf(GroupMemberRole.Observer, GroupMemberRole.Member, GroupMemberRole.Moderator, GroupMemberRole.Admin),
      roles
    )
  }
}
