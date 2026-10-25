package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.*
import dev.icerock.moko.resources.StringResource
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertTrue

class GroupCantSendReasonUnitTest {

  private fun groupInfo(
    role: GroupMemberRole = GroupMemberRole.Member,
    status: GroupMemberStatus = GroupMemberStatus.MemConnected,
    useRelays: Boolean = false,
    preparedGroup: PreparedGroup? = null,
  ): GroupInfo =
    GroupInfo.sampleData.copy(
      useRelays = useRelays,
      preparedGroup = preparedGroup,
      membership = GroupMember.sampleData.copy(
        groupMemberId = 100,
        memberRole = role,
        memberStatus = status,
        memberCategory = GroupMemberCategory.UserMember,
      )
    )

  private fun mainChat(g: GroupInfo): ChatInfo.Group = ChatInfo.Group(g, groupChatScope = null)

  private fun supportChat(g: GroupInfo, member: GroupMember? = null): ChatInfo.Group =
    ChatInfo.Group(g, groupChatScope = GroupChatScopeInfo.MemberSupport(groupMember_ = member))

  private fun reason(title: StringResource, description: StringResource? = null): Pair<String, String?> =
    generalGetString(title) to description?.let { generalGetString(it) }

  private fun scopeMember(maxVersion: Int, status: GroupMemberStatus = GroupMemberStatus.MemConnected): GroupMember =
    GroupMember.sampleData.copy(
      groupMemberId = 7,
      memberStatus = status,
      activeConn = Connection.sampleData.copy(peerChatVRange = VersionRange(1, maxVersion)),
      memberChatVRange = VersionRange(1, maxVersion),
    )

  @Test
  fun userWhoLeftSeesYouLeft() {
    assertEquals(reason(MR.strings.cant_send_message_you_left), mainChat(groupInfo(status = GroupMemberStatus.MemLeft)).userCantSendReason())
  }

  @Test
  fun removedUserSeesRemovedFromGroup() {
    assertEquals(reason(MR.strings.cant_send_message_mem_removed), mainChat(groupInfo(status = GroupMemberStatus.MemRemoved)).userCantSendReason())
  }

  @Test
  fun deletedGroupSeesGroupIsDeleted() {
    assertEquals(reason(MR.strings.cant_send_message_group_deleted), mainChat(groupInfo(status = GroupMemberStatus.MemGroupDeleted)).userCantSendReason())
  }

  @Test
  fun rejectedUserSeesRequestRejected() {
    assertEquals(reason(MR.strings.cant_send_message_rejected), mainChat(groupInfo(status = GroupMemberStatus.MemRejected)).userCantSendReason())
  }

  @Test
  fun otherInactiveStatusSeesGenericReason() {
    assertEquals(reason(MR.strings.cant_send_message_generic), mainChat(groupInfo(status = GroupMemberStatus.MemInvited)).userCantSendReason())
  }

  @Test
  fun pendingUserSeesReviewedByAdminsInMainChat() {
    val expected = reason(MR.strings.reviewed_by_admins, MR.strings.observer_cant_send_message_desc)
    assertEquals(expected, mainChat(groupInfo(status = GroupMemberStatus.MemPendingReview)).userCantSendReason())
    assertEquals(expected, mainChat(groupInfo(status = GroupMemberStatus.MemPendingApproval)).userCantSendReason())
  }

  @Test
  fun pendingUserCanSendInOwnSupportChat() {
    assertNull(supportChat(groupInfo(status = GroupMemberStatus.MemPendingReview)).userCantSendReason())
  }

  @Test
  fun observerInGroupSeesObserverTitleAndDescription() {
    assertEquals(
      reason(MR.strings.observer_cant_send_message_title, MR.strings.observer_cant_send_message_desc),
      mainChat(groupInfo(role = GroupMemberRole.Observer)).userCantSendReason()
    )
  }

  @Test
  fun observerInChannelSeesYouAreSubscriber() {
    assertEquals(reason(MR.strings.you_are_subscriber), mainChat(groupInfo(role = GroupMemberRole.Observer, useRelays = true)).userCantSendReason())
  }

  @Test
  fun observerCanSendInSupportChat() {
    assertNull(supportChat(groupInfo(role = GroupMemberRole.Observer)).userCantSendReason())
  }

  @Test
  fun channelWithAllRelaysBrokenCannotBroadcast() {
    val g = groupInfo(role = GroupMemberRole.Owner, useRelays = true)
    assertEquals(reason(MR.strings.cant_broadcast_message), mainChat(g).userCantSendReason(allRelaysBroken = true))
    assertNull(mainChat(g).userCantSendReason(allRelaysBroken = false))
  }

  @Test
  fun brokenRelaysFlagIsIgnoredInGroupsWithoutRelays() {
    assertNull(mainChat(groupInfo()).userCantSendReason(allRelaysBroken = true))
  }

  @Test
  fun preparedGroupCanSendToConnect() {
    val prepared = PreparedGroup(
      connLinkToConnect = CreatedConnLink(connFullLink = "https://simplex.chat/contact#/?v=2-7", connShortLink = null),
      connLinkPreparedConnection = false,
      connLinkStartedConnection = false,
    )
    assertNull(mainChat(groupInfo(status = GroupMemberStatus.MemUnknown, preparedGroup = prepared)).userCantSendReason())
  }

  @Test
  fun connectedMemberCanSend() {
    val chat = mainChat(groupInfo())
    assertNull(chat.userCantSendReason())
    assertTrue(chat.sendMsgEnabled)
  }

  @Test
  fun supportChatWithOldVersionMemberCannotSend() {
    val g = groupInfo(role = GroupMemberRole.Admin)
    assertEquals(
      reason(MR.strings.cant_send_message_member_has_old_version),
      supportChat(g, scopeMember(GROUP_KNOCKING_VERSION - 1)).userCantSendReason()
    )
  }

  @Test
  fun supportChatWithPendingOldVersionMemberCanSend() {
    val g = groupInfo(role = GroupMemberRole.Admin)
    assertNull(supportChat(g, scopeMember(GROUP_KNOCKING_VERSION - 1, GroupMemberStatus.MemPendingReview)).userCantSendReason())
  }

  @Test
  fun supportChatWithCurrentVersionMemberCanSend() {
    val g = groupInfo(role = GroupMemberRole.Admin)
    assertNull(supportChat(g, scopeMember(GROUP_KNOCKING_VERSION)).userCantSendReason())
  }

  @Test
  fun sndReadyMatchesWhoCanSendInTheMainChat() {
    assertTrue(mainChat(groupInfo()).sndReady)
    assertFalse(mainChat(groupInfo(role = GroupMemberRole.Observer)).sndReady)
    assertFalse(mainChat(groupInfo(status = GroupMemberStatus.MemPendingReview)).sndReady)
    assertFalse(mainChat(groupInfo(status = GroupMemberStatus.MemLeft)).sndReady)
  }

  @Test
  fun sndReadyInSupportChatForObserversAndPendingMembers() {
    assertTrue(supportChat(groupInfo(role = GroupMemberRole.Observer)).sndReady)
    assertTrue(supportChat(groupInfo(status = GroupMemberStatus.MemPendingReview)).sndReady)
    assertFalse(supportChat(groupInfo(status = GroupMemberStatus.MemRemoved)).sndReady)
  }

  @Test
  fun channelOwnerSendsAsChannelOnlyInMainChat() {
    val owner = groupInfo(role = GroupMemberRole.Owner, useRelays = true)
    assertTrue(mainChat(owner).sendAsGroup)
    assertFalse(supportChat(owner).sendAsGroup)
  }

  @Test
  fun nonOwnersAndGroupsWithoutRelaysDoNotSendAsGroup() {
    assertFalse(mainChat(groupInfo(role = GroupMemberRole.Admin, useRelays = true)).sendAsGroup)
    assertFalse(mainChat(groupInfo(role = GroupMemberRole.Owner, useRelays = false)).sendAsGroup)
  }
}
