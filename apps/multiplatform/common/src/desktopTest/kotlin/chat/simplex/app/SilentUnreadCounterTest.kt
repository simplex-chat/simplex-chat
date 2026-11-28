package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withContext
import kotlin.test.Test
import kotlin.test.assertEquals

class SilentUnreadCounterTest {
  private fun rcvItem(id: Long, silent: Boolean, mention: Boolean = false): ChatItem {
    val ci = ChatItem.getSampleData(id, CIDirection.DirectRcv(), status = CIStatus.RcvNew())
    return ci.copy(meta = ci.meta.copy(priority = if (silent) -1 else null, userMention = mention))
  }

  private fun counts(chatsCtx: ChatModel.ChatsContext): Triple<Int, Int, Int> {
    val stats = chatsCtx.chats.value[0].chatStats
    return Triple(stats.unreadCount, stats.unreadSilent, chatModel.users[0].unreadCount)
  }

  private fun withOpenChat(enableNtfs: MsgFilter, test: suspend (ChatModel.ChatsContext, ChatInfo) -> Unit) = runBlocking {
    val contact = Contact.sampleData.copy(chatSettings = ChatSettings(enableNtfs = enableNtfs, sendRcpts = null, favorite = false))
    val cInfo = ChatInfo.Direct(contact)
    val chatsCtx = ChatModel.ChatsContext(null)
    val currentUser = chatModel.currentUser.value
    val users = chatModel.users.toList()
    val chatId = chatModel.chatId.value
    try {
      withContext(Dispatchers.Main) {
        chatModel.currentUser.value = User.sampleData
        chatModel.users.clear()
        chatModel.users.add(UserInfo(User.sampleData, 0))
        chatModel.chatId.value = cInfo.id
        chatsCtx.chats.add(Chat(remoteHostId = null, chatInfo = cInfo, chatItems = listOf()))
        test(chatsCtx, cInfo)
      }
    } finally {
      withContext(Dispatchers.Main) {
        chatModel.currentUser.value = currentUser
        chatModel.users.clear()
        chatModel.users.addAll(users)
        chatModel.chatId.value = chatId
      }
    }
  }

  @Test
  fun silentItemsAreNotCountedForUserInModeAll() = withOpenChat(MsgFilter.All) { chatsCtx, cInfo ->
    val normal = rcvItem(1, silent = false)
    val silent1 = rcvItem(2, silent = true)
    chatsCtx.addChatItem(null, cInfo, normal)
    chatsCtx.addChatItem(null, cInfo, silent1)
    chatsCtx.addChatItem(null, cInfo, rcvItem(3, silent = true))
    assertEquals(Triple(3, 2, 1), counts(chatsCtx))
    chatsCtx.removeChatItem(null, cInfo, silent1)
    assertEquals(Triple(2, 1, 1), counts(chatsCtx))
    chatsCtx.markChatItemsRead(null, cInfo.id, listOf(normal.id))
    assertEquals(Triple(1, 1, 0), counts(chatsCtx))
    chatsCtx.clearChat(null, cInfo)
    assertEquals(Triple(0, 0, 0), counts(chatsCtx))
  }

  @Test
  fun silentMentionIsCountedForUserInModeMentions() = withOpenChat(MsgFilter.Mentions) { chatsCtx, cInfo ->
    chatsCtx.addChatItem(null, cInfo, rcvItem(1, silent = true, mention = true))
    assertEquals(Triple(1, 1, 1), counts(chatsCtx))
    chatsCtx.markChatItemsRead(null, cInfo.id, listOf(1))
    assertEquals(Triple(0, 0, 0), counts(chatsCtx))
  }
}
