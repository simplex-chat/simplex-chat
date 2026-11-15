package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withContext
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse

class ChatItemUpdateTest {
  private fun rcvItem(id: Long, text: String, status: CIStatus = CIStatus.RcvNew(), mention: Boolean = false): ChatItem {
    val ci = ChatItem.getSampleData(id, CIDirection.DirectRcv(), text = text, status = status)
    return ci.copy(meta = ci.meta.copy(userMention = mention))
  }

  private fun counts(chatsCtx: ChatModel.ChatsContext): Triple<Int, Int, Int> {
    val stats = chatsCtx.chats.value[0].chatStats
    return Triple(stats.unreadCount, stats.unreadMentions, chatModel.users[0].unreadCount)
  }

  private fun withOpenChat(test: suspend (ChatModel.ChatsContext, ChatInfo) -> Unit) = runBlocking {
    val cInfo = ChatInfo.Direct(Contact.sampleData)
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
  fun updateWithPreviousItemReplacesLoadedItemAndNeverInserts() = withOpenChat { chatsCtx, cInfo ->
    chatsCtx.addChatItem(null, cInfo, rcvItem(1, "hello"))
    assertFalse(chatsCtx.applyChatItemUpdate(null, cInfo, rcvItem(1, "hello edited"), rcvItem(1, "hello")))
    assertEquals(listOf("hello edited"), chatsCtx.chatItems.value.map { it.text })
    assertEquals("hello edited", chatsCtx.chats.value[0].chatItems.last().text)
    assertFalse(chatsCtx.applyChatItemUpdate(null, cInfo, rcvItem(5, "not loaded edited"), rcvItem(5, "not loaded")))
    assertEquals(listOf(1L), chatsCtx.chatItems.value.map { it.id })
    assertEquals(Triple(1, 0, 1), counts(chatsCtx))
  }

  @Test
  fun countersFollowPreviousAndNewItem() = withOpenChat { chatsCtx, cInfo ->
    chatsCtx.addChatItem(null, cInfo, rcvItem(1, "hello"))
    chatsCtx.applyChatItemUpdate(null, cInfo, rcvItem(1, "hello @alice", mention = true), rcvItem(1, "hello"))
    assertEquals(Triple(1, 1, 1), counts(chatsCtx))
    chatsCtx.applyChatItemUpdate(null, cInfo, rcvItem(1, "hello", status = CIStatus.RcvRead()), rcvItem(1, "hello @alice", mention = true))
    assertEquals(Triple(0, 0, 0), counts(chatsCtx))
  }
}
