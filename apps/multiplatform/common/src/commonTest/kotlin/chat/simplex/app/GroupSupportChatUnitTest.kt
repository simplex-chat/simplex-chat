package chat.simplex.app

import chat.simplex.common.model.*
import kotlinx.datetime.Clock
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class GroupSupportChatUnitTest {

  private fun supportChat(unread: Int = 0, memberAttention: Int = 0, mentions: Int = 0): GroupSupportChat =
    GroupSupportChat(chatTs = Clock.System.now(), unread = unread, memberAttention = memberAttention, mentions = mentions)

  private fun groupChat(
    userRole: GroupMemberRole,
    userStatus: GroupMemberStatus = GroupMemberStatus.MemConnected,
    ownSupportChat: GroupSupportChat? = null,
    membersRequireAttention: Int = 0,
  ): Chat =
    Chat(
      remoteHostId = null,
      chatInfo = ChatInfo.Group(
        GroupInfo.sampleData.copy(
          membership = GroupMember.sampleData.copy(memberRole = userRole, memberStatus = userStatus, supportChat = ownSupportChat),
          membersRequireAttention = membersRequireAttention,
        ),
        groupChatScope = null
      ),
      chatItems = emptyList()
    )

  @Test
  fun moderatorSeesMembersRequiringAttention() {
    assertEquals(3, groupChat(GroupMemberRole.Moderator, ownSupportChat = supportChat(unread = 5), membersRequireAttention = 3).supportUnreadCount)
    assertEquals(2, groupChat(GroupMemberRole.Owner, membersRequireAttention = 2).supportUnreadCount)
  }

  @Test
  fun memberSeesUnreadOfOwnSupportChat() {
    assertEquals(5, groupChat(GroupMemberRole.Member, ownSupportChat = supportChat(unread = 5), membersRequireAttention = 3).supportUnreadCount)
  }

  @Test
  fun memberWithoutSupportChatHasNoUnread() {
    assertEquals(0, groupChat(GroupMemberRole.Member, membersRequireAttention = 3).supportUnreadCount)
  }

  @Test
  fun moderatorWhoLeftSeesOwnSupportChatUnread() {
    assertEquals(
      4,
      groupChat(GroupMemberRole.Moderator, userStatus = GroupMemberStatus.MemLeft, ownSupportChat = supportChat(unread = 4), membersRequireAttention = 3).supportUnreadCount
    )
  }

  @Test
  fun directChatHasNoSupportUnread() {
    assertEquals(0, Chat(remoteHostId = null, chatInfo = ChatInfo.Direct(Contact.sampleData), chatItems = emptyList()).supportUnreadCount)
  }

  @Test
  fun memberWithoutSupportChatIsRead() {
    assertFalse(GroupMember.sampleData.copy(supportChat = null).supportChatNotRead)
  }

  @Test
  fun supportChatWithNothingPendingIsRead() {
    assertFalse(GroupMember.sampleData.copy(supportChat = supportChat()).supportChatNotRead)
  }

  @Test
  fun unreadMentionsOrAttentionMakeSupportChatNotRead() {
    assertTrue(GroupMember.sampleData.copy(supportChat = supportChat(unread = 1)).supportChatNotRead)
    assertTrue(GroupMember.sampleData.copy(supportChat = supportChat(mentions = 1)).supportChatNotRead)
    assertTrue(GroupMember.sampleData.copy(supportChat = supportChat(memberAttention = 1)).supportChatNotRead)
  }
}
