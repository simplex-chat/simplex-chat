package chat.simplex.app

import chat.simplex.common.model.*
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class ChatListUnreadTagUnitTest {
  private fun directChat(enableNtfs: MsgFilter, stats: Chat.ChatStats): Chat =
    Chat(
      remoteHostId = null,
      chatInfo = ChatInfo.Direct(Contact.sampleData.copy(chatSettings = ChatSettings(enableNtfs = enableNtfs, sendRcpts = null, favorite = false))),
      chatItems = emptyList(),
      chatStats = stats
    )

  private fun groupChat(enableNtfs: MsgFilter, stats: Chat.ChatStats): Chat =
    Chat(
      remoteHostId = null,
      chatInfo = ChatInfo.Group(GroupInfo.sampleData.copy(chatSettings = ChatSettings(enableNtfs = enableNtfs, sendRcpts = null, favorite = false)), groupChatScope = null),
      chatItems = emptyList(),
      chatStats = stats
    )

  @Test
  fun allModeWithUnreadMessageIsUnread() {
    assertTrue(directChat(MsgFilter.All, Chat.ChatStats(unreadCount = 1)).unreadTag)
  }

  @Test
  fun allModeMarkedUnreadWithoutMessagesIsUnread() {
    assertTrue(directChat(MsgFilter.All, Chat.ChatStats(unreadCount = 0, unreadChat = true)).unreadTag)
  }

  @Test
  fun allModeWithNothingUnreadIsRead() {
    assertFalse(directChat(MsgFilter.All, Chat.ChatStats()).unreadTag)
  }

  @Test
  fun mentionsModeWithUnreadMessagesButNoMentionsIsRead() {
    assertFalse(groupChat(MsgFilter.Mentions, Chat.ChatStats(unreadCount = 3, unreadMentions = 0)).unreadTag)
  }

  @Test
  fun mentionsModeWithUnreadMentionIsUnread() {
    assertTrue(groupChat(MsgFilter.Mentions, Chat.ChatStats(unreadCount = 1, unreadMentions = 1)).unreadTag)
  }

  @Test
  fun mentionsModeMarkedUnreadIsUnread() {
    assertTrue(groupChat(MsgFilter.Mentions, Chat.ChatStats(unreadChat = true)).unreadTag)
  }

  @Test
  fun mutedWithUnreadMessagesAndMentionsIsRead() {
    assertFalse(groupChat(MsgFilter.None, Chat.ChatStats(unreadCount = 5, unreadMentions = 2)).unreadTag)
  }

  @Test
  fun mutedMarkedUnreadIsUnread() {
    assertTrue(directChat(MsgFilter.None, Chat.ChatStats(unreadChat = true)).unreadTag)
  }

  @Test
  fun noteFolderMarkedUnreadIsUnread() {
    assertTrue(Chat(remoteHostId = null, chatInfo = ChatInfo.Local(NoteFolder.sampleData), chatItems = emptyList(), chatStats = Chat.ChatStats(unreadChat = true)).unreadTag)
  }

  @Test
  fun noteFolderWithUnreadCountButNotMarkedIsRead() {
    assertFalse(Chat(remoteHostId = null, chatInfo = ChatInfo.Local(NoteFolder.sampleData), chatItems = emptyList(), chatStats = Chat.ChatStats(unreadCount = 4)).unreadTag)
  }
}
