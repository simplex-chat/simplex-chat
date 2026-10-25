package chat.simplex.app

import chat.simplex.common.model.*
import kotlinx.datetime.Clock
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertTrue

class GroupMemberPermissionsUnitTest {

  private fun group(
    userRole: GroupMemberRole,
    userStatus: GroupMemberStatus = GroupMemberStatus.MemConnected,
    useRelays: Boolean = false,
  ): GroupInfo =
    GroupInfo.sampleData.copy(
      useRelays = useRelays,
      membership = GroupMember.sampleData.copy(
        groupMemberId = 100,
        memberRole = userRole,
        memberStatus = userStatus,
        memberCategory = GroupMemberCategory.UserMember,
      )
    )

  private fun member(role: GroupMemberRole, status: GroupMemberStatus = GroupMemberStatus.MemConnected): GroupMember =
    GroupMember.sampleData.copy(groupMemberId = 2, memberRole = role, memberStatus = status)

  @Test
  fun adminCannotRemoveChangeOrBlockOwner() {
    val g = group(GroupMemberRole.Admin)
    val m = member(GroupMemberRole.Owner)
    assertFalse(m.canBeRemoved(g))
    assertNull(m.canChangeRoleTo(g))
    assertFalse(m.canBlockForAll(g))
  }

  @Test
  fun adminCanRemoveMemberAndAssignRolesUpToAdmin() {
    val g = group(GroupMemberRole.Admin)
    val m = member(GroupMemberRole.Member)
    assertTrue(m.canBeRemoved(g))
    assertEquals(
      listOf(GroupMemberRole.Observer, GroupMemberRole.Member, GroupMemberRole.Moderator, GroupMemberRole.Admin),
      m.canChangeRoleTo(g)
    )
    assertTrue(m.canBlockForAll(g))
  }

  @Test
  fun ownerCanAssignEverySelectableRoleIncludingOwner() {
    val g = group(GroupMemberRole.Owner)
    assertEquals(
      listOf(GroupMemberRole.Observer, GroupMemberRole.Member, GroupMemberRole.Moderator, GroupMemberRole.Admin, GroupMemberRole.Owner),
      member(GroupMemberRole.Member).canChangeRoleTo(g)
    )
  }

  @Test
  fun moderatorCanBlockForAllButCannotRemoveOrChangeRole() {
    val g = group(GroupMemberRole.Moderator)
    val m = member(GroupMemberRole.Member)
    assertFalse(m.canBeRemoved(g))
    assertNull(m.canChangeRoleTo(g))
    assertTrue(m.canBlockForAll(g))
  }

  @Test
  fun moderatorsCannotBeBlockedForAllEvenByAdmin() {
    val g = group(GroupMemberRole.Admin)
    val m = member(GroupMemberRole.Moderator)
    assertTrue(m.canBeRemoved(g))
    assertEquals(
      listOf(GroupMemberRole.Observer, GroupMemberRole.Member, GroupMemberRole.Moderator, GroupMemberRole.Admin),
      m.canChangeRoleTo(g)
    )
    assertFalse(m.canBlockForAll(g))
  }

  @Test
  fun userWhoLeftHasNoMemberPermissions() {
    val g = group(GroupMemberRole.Owner, userStatus = GroupMemberStatus.MemLeft)
    val m = member(GroupMemberRole.Member)
    assertFalse(m.canBeRemoved(g))
    assertNull(m.canChangeRoleTo(g))
    assertFalse(m.canBlockForAll(g))
  }

  @Test
  fun memberPendingReviewCanBeRemovedButNotRoleChangedOrBlocked() {
    val g = group(GroupMemberRole.Admin)
    val m = member(GroupMemberRole.Member, GroupMemberStatus.MemPendingReview)
    assertTrue(m.canBeRemoved(g))
    assertNull(m.canChangeRoleTo(g))
    assertFalse(m.canBlockForAll(g))
  }

  @Test
  fun relayMemberRoleCannotBeChangedOrBlocked() {
    val g = group(GroupMemberRole.Owner, useRelays = true)
    val m = member(GroupMemberRole.Relay)
    assertNull(m.canChangeRoleTo(g))
    assertFalse(m.canBlockForAll(g))
  }

  @Test
  fun removedOrLeftMemberRoleCannotBeChanged() {
    val g = group(GroupMemberRole.Owner)
    assertNull(member(GroupMemberRole.Member, GroupMemberStatus.MemRemoved).canChangeRoleTo(g))
    assertNull(member(GroupMemberRole.Member, GroupMemberStatus.MemLeft).canChangeRoleTo(g))
  }

  @Test
  fun invitedMemberRoleCanStillBeChanged() {
    val g = group(GroupMemberRole.Admin)
    assertEquals(
      listOf(GroupMemberRole.Observer, GroupMemberRole.Member, GroupMemberRole.Moderator, GroupMemberRole.Admin),
      member(GroupMemberRole.Member, GroupMemberStatus.MemInvited).canChangeRoleTo(g)
    )
  }

  @Test
  fun channelAdminWhoIsNotOwnerCannotChangeRoles() {
    val g = group(GroupMemberRole.Admin, useRelays = true)
    val m = member(GroupMemberRole.Member)
    assertTrue(m.canBeRemoved(g))
    assertNull(m.canChangeRoleTo(g))
  }

  @Test
  fun channelOwnerCanOnlyAssignObserverOrMember() {
    val g = group(GroupMemberRole.Owner, useRelays = true)
    assertEquals(listOf(GroupMemberRole.Observer, GroupMemberRole.Member), member(GroupMemberRole.Observer).canChangeRoleTo(g))
  }

  @Test
  fun isOwnerRequiresOwnerRoleAndCurrentMembership() {
    assertTrue(group(GroupMemberRole.Owner).isOwner)
    assertFalse(group(GroupMemberRole.Admin).isOwner)
    assertFalse(group(GroupMemberRole.Owner, userStatus = GroupMemberStatus.MemPendingReview).isOwner)
    assertFalse(group(GroupMemberRole.Owner, userStatus = GroupMemberStatus.MemLeft).isOwner)
  }

  @Test
  fun canDeleteForOwnerOrWhenNoLongerCurrentOrPending() {
    assertTrue(group(GroupMemberRole.Owner).canDelete)
    assertFalse(group(GroupMemberRole.Member).canDelete)
    assertFalse(group(GroupMemberRole.Admin).canDelete)
    assertFalse(group(GroupMemberRole.Member, userStatus = GroupMemberStatus.MemPendingReview).canDelete)
    assertTrue(group(GroupMemberRole.Member, userStatus = GroupMemberStatus.MemLeft).canDelete)
    assertTrue(group(GroupMemberRole.Member, userStatus = GroupMemberStatus.MemRemoved).canDelete)
    assertTrue(group(GroupMemberRole.Member, userStatus = GroupMemberStatus.MemGroupDeleted).canDelete)
  }

  @Test
  fun canAddMembersRequiresActiveAdmin() {
    assertTrue(group(GroupMemberRole.Admin).canAddMembers)
    assertTrue(group(GroupMemberRole.Owner).canAddMembers)
    assertFalse(group(GroupMemberRole.Moderator).canAddMembers)
    assertFalse(group(GroupMemberRole.Member).canAddMembers)
    assertFalse(group(GroupMemberRole.Admin, userStatus = GroupMemberStatus.MemLeft).canAddMembers)
  }

  @Test
  fun canModerateRequiresActiveModerator() {
    assertTrue(group(GroupMemberRole.Moderator).canModerate)
    assertTrue(group(GroupMemberRole.Admin).canModerate)
    assertFalse(group(GroupMemberRole.Member).canModerate)
    assertFalse(group(GroupMemberRole.Observer).canModerate)
    assertFalse(group(GroupMemberRole.Moderator, userStatus = GroupMemberStatus.MemRemoved).canModerate)
  }

  private fun rcvItem(sender: GroupMember, itemDeleted: CIDeleted? = null): ChatItem =
    ChatItem.getSampleData(1, CIDirection.GroupRcv(sender), itemDeleted = itemDeleted)

  @Test
  fun moderatorCanModerateMessageOfLowerOrEqualRole() {
    val g = group(GroupMemberRole.Moderator)
    val sender = member(GroupMemberRole.Member)
    assertEquals(g to sender, rcvItem(sender).memberToModerate(ChatInfo.Group(g, groupChatScope = null)))
    val peer = member(GroupMemberRole.Moderator)
    assertEquals(g to peer, rcvItem(peer).memberToModerate(ChatInfo.Group(g, groupChatScope = null)))
  }

  @Test
  fun moderatorCannotModerateMessageOfHigherRole() {
    val g = group(GroupMemberRole.Moderator)
    assertNull(rcvItem(member(GroupMemberRole.Admin)).memberToModerate(ChatInfo.Group(g, groupChatScope = null)))
  }

  @Test
  fun memberCannotModerateMessages() {
    val g = group(GroupMemberRole.Member)
    assertNull(rcvItem(member(GroupMemberRole.Member)).memberToModerate(ChatInfo.Group(g, groupChatScope = null)))
  }

  @Test
  fun deletedMessageCannotBeModerated() {
    val g = group(GroupMemberRole.Admin)
    val item = rcvItem(member(GroupMemberRole.Member), itemDeleted = CIDeleted.Deleted(Clock.System.now()))
    assertNull(item.memberToModerate(ChatInfo.Group(g, groupChatScope = null)))
  }

  @Test
  fun moderatorCanModerateOwnSentMessage() {
    val item = ChatItem.getSampleData(1, CIDirection.GroupSnd())
    val moderator = group(GroupMemberRole.Moderator)
    assertEquals(moderator to null, item.memberToModerate(ChatInfo.Group(moderator, groupChatScope = null)))
    assertNull(item.memberToModerate(ChatInfo.Group(group(GroupMemberRole.Member), groupChatScope = null)))
  }

  @Test
  fun channelMessagesAndDirectChatsCannotBeModerated() {
    val g = group(GroupMemberRole.Owner)
    assertNull(ChatItem.getSampleData(1, CIDirection.ChannelRcv()).memberToModerate(ChatInfo.Group(g, groupChatScope = null)))
    assertNull(ChatItem.getSampleData(1, CIDirection.DirectRcv()).memberToModerate(ChatInfo.Direct(Contact.sampleData)))
  }
}
