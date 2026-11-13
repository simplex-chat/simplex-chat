package chat.simplex.app

import chat.simplex.common.model.*
import kotlin.test.Test
import kotlin.test.assertEquals

class SilentUnreadTagTest {
  private fun chat(enableNtfs: MsgFilter, stats: Chat.ChatStats): Chat {
    val contact = Contact.sampleData.copy(chatSettings = ChatSettings(enableNtfs = enableNtfs, sendRcpts = null, favorite = false))
    return Chat(remoteHostId = null, chatInfo = ChatInfo.Direct(contact), chatItems = listOf(), chatStats = stats)
  }

  @Test
  fun onlySilentUnreadIsSilentInModeAll() {
    assertEquals(UnreadTag.Silent, chat(MsgFilter.All, Chat.ChatStats(unreadCount = 2, unreadSilent = 2)).unreadTag)
  }

  @Test
  fun normalUnreadIsUnreadInModeAll() {
    assertEquals(UnreadTag.Unread, chat(MsgFilter.All, Chat.ChatStats(unreadCount = 2, unreadSilent = 1)).unreadTag)
  }

  @Test
  fun chatMarkedUnreadIsUnread() {
    assertEquals(UnreadTag.Unread, chat(MsgFilter.All, Chat.ChatStats(unreadCount = 1, unreadSilent = 1, unreadChat = true)).unreadTag)
  }

  @Test
  fun onlySilentUnreadIsReadInModeMentions() {
    assertEquals(UnreadTag.Read, chat(MsgFilter.Mentions, Chat.ChatStats(unreadCount = 2, unreadSilent = 2)).unreadTag)
  }
}
