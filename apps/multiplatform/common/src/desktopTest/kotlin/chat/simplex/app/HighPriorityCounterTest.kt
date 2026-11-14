package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withContext
import kotlin.test.Test
import kotlin.test.assertEquals

class HighPriorityCounterTest {
  private val tag = 1L

  private data class Counts(
    val unread: Int,
    val mentions: Int,
    val silent: Int,
    val highPriority: Int,
    val user: Int,
    val unreadTag: Int,
    val priorityTag: Int,
    val silentTag: Int
  )

  private fun rcvItem(id: Long, priority: Int?, mention: Boolean = false): ChatItem {
    val ci = ChatItem.getSampleData(id, CIDirection.DirectRcv(), status = CIStatus.RcvNew())
    return ci.copy(meta = ci.meta.copy(priority = priority, userMention = mention))
  }

  private fun raised(ci: ChatItem): ChatItem = ci.copy(meta = ci.meta.copy(priority = 1, userMention = true))

  private fun counts(chatsCtx: ChatModel.ChatsContext): Counts {
    val stats = chatsCtx.chats.value[0].chatStats
    return Counts(
      stats.unreadCount, stats.unreadMentions, stats.unreadSilent, stats.unreadHighPriority, chatModel.users[0].unreadCount,
      chatModel.unreadTags[tag] ?: 0, chatModel.priorityTags[tag] ?: 0, chatModel.silentTags[tag] ?: 0
    )
  }

  private fun withOpenChat(enableNtfs: MsgFilter, test: suspend (ChatModel.ChatsContext, ChatInfo) -> Unit) = runBlocking {
    val contact = Contact.sampleData.copy(chatSettings = ChatSettings(enableNtfs = enableNtfs, sendRcpts = null, favorite = false), chatTags = listOf(tag))
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
        chatModel.unreadTags.clear()
        chatModel.priorityTags.clear()
        chatModel.silentTags.clear()
        chatsCtx.chats.add(Chat(remoteHostId = null, chatInfo = cInfo, chatItems = listOf()))
        test(chatsCtx, cInfo)
      }
    } finally {
      withContext(Dispatchers.Main) {
        chatModel.currentUser.value = currentUser
        chatModel.users.clear()
        chatModel.users.addAll(users)
        chatModel.chatId.value = chatId
        chatModel.unreadTags.clear()
        chatModel.priorityTags.clear()
        chatModel.silentTags.clear()
      }
    }
  }

  @Test
  fun highPriorityItemIsCountedAndRead() = withOpenChat(MsgFilter.All) { chatsCtx, cInfo ->
    chatsCtx.addChatItem(null, cInfo, rcvItem(1, priority = 1, mention = true))
    assertEquals(Counts(1, 1, 0, 1, 1, 1, 1, 0), counts(chatsCtx))
    chatsCtx.markChatItemsRead(null, cInfo.id, listOf(1))
    assertEquals(Counts(0, 0, 0, 0, 0, 0, 0, 0), counts(chatsCtx))
  }

  @Test
  fun raisedSilentItemLeavesGrayTierInModeAll() = withOpenChat(MsgFilter.All) { chatsCtx, cInfo ->
    val silent = rcvItem(1, priority = -1)
    chatsCtx.addChatItem(null, cInfo, silent)
    assertEquals(Counts(1, 0, 1, 0, 0, 0, 0, 1), counts(chatsCtx))
    chatsCtx.raisePriority(null, cInfo, raised(silent), PriorityRaised(newAttention = true, wasSilent = true))
    assertEquals(Counts(1, 1, 0, 1, 1, 1, 1, 0), counts(chatsCtx))
    chatsCtx.removeChatItem(null, cInfo, raised(silent))
    assertEquals(Counts(0, 0, 0, 0, 0, 0, 0, 0), counts(chatsCtx))
  }

  @Test
  fun raisedMentionIsCountedOnceInModeMentions() = withOpenChat(MsgFilter.Mentions) { chatsCtx, cInfo ->
    val mention = rcvItem(1, priority = null, mention = true)
    chatsCtx.addChatItem(null, cInfo, mention)
    assertEquals(Counts(1, 1, 0, 0, 1, 1, 0, 0), counts(chatsCtx))
    chatsCtx.raisePriority(null, cInfo, raised(mention), PriorityRaised(newAttention = false, wasSilent = false))
    assertEquals(Counts(1, 1, 0, 1, 1, 1, 1, 0), counts(chatsCtx))
    chatsCtx.markChatItemsRead(null, cInfo.id, null)
    assertEquals(Counts(0, 0, 0, 0, 0, 0, 0, 0), counts(chatsCtx))
  }

  @Test
  fun raiseOfReadItemDoesNotChangeCounters() = withOpenChat(MsgFilter.All) { chatsCtx, cInfo ->
    val item = rcvItem(1, priority = null)
    chatsCtx.addChatItem(null, cInfo, item)
    chatsCtx.markChatItemsRead(null, cInfo.id, listOf(1))
    val read = raised(item).withStatus(CIStatus.RcvRead())
    chatsCtx.raisePriority(null, cInfo, read, PriorityRaised(newAttention = true, wasSilent = false))
    assertEquals(Counts(0, 0, 0, 0, 0, 0, 0, 0), counts(chatsCtx))
  }
}
