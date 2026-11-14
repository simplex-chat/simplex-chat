package chat.simplex.app

import chat.simplex.common.model.*
import kotlin.test.Test
import kotlin.test.assertEquals

class HighPriorityUnreadTagTest {
  private fun chat(enableNtfs: MsgFilter, stats: Chat.ChatStats): Chat {
    val contact = Contact.sampleData.copy(chatSettings = ChatSettings(enableNtfs = enableNtfs, sendRcpts = null, favorite = false))
    return Chat(remoteHostId = null, chatInfo = ChatInfo.Direct(contact), chatItems = listOf(), chatStats = stats)
  }

  @Test
  fun highPriorityUnreadIsHighPriorityInModeAll() {
    assertEquals(UnreadTag.HighPriority, chat(MsgFilter.All, Chat.ChatStats(unreadCount = 3, unreadMentions = 1, unreadSilent = 2, unreadHighPriority = 1)).unreadTag)
  }

  @Test
  fun highPriorityUnreadIsHighPriorityInModeMentions() {
    assertEquals(UnreadTag.HighPriority, chat(MsgFilter.Mentions, Chat.ChatStats(unreadCount = 1, unreadMentions = 1, unreadHighPriority = 1)).unreadTag)
  }

  @Test
  fun highPriorityUnreadIsReadInModeNone() {
    assertEquals(UnreadTag.Read, chat(MsgFilter.None, Chat.ChatStats(unreadCount = 1, unreadMentions = 1, unreadHighPriority = 1)).unreadTag)
  }
}
