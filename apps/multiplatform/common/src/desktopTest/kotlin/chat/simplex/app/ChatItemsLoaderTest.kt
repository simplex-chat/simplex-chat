package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.model.ChatModel.ChatsContext.ItemsAddedWhileLoading
import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.chat.processLoadedChat
import chat.simplex.common.model.replaceAll
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withContext
import kotlin.test.Test
import kotlin.test.assertEquals

class ChatItemsLoaderTest {

  private fun groupChat(groupId: Long, itemIds: List<Long>): Chat =
    Chat(
      remoteHostId = null,
      chatInfo = ChatInfo.Group(GroupInfo.sampleData.copy(groupId = groupId), groupChatScope = null),
      chatItems = itemIds.map { ChatItem.getSampleData(it, CIDirection.GroupRcv(GroupMember.sampleData)) }
    )

  private suspend fun openChatWithItems(itemIds: List<Long>): Pair<ChatModel.ChatsContext, Chat> {
    val chatsCtx = ChatModel.ChatsContext(null)
    val opened = groupChat(groupId = 2, itemIds = itemIds)
    withContext(Dispatchers.Main) {
      chatsCtx.chatItems.replaceAll(opened.chatItems)
      chatModel.chatId.value = opened.chatInfo.id
    }
    return chatsCtx to opened
  }

  private fun itemIds(chatsCtx: ChatModel.ChatsContext): List<Long> = chatsCtx.chatItems.value.map { it.id }

  private suspend fun startRecordingAddedItems(chatsCtx: ChatModel.ChatsContext): ItemsAddedWhileLoading =
    withContext(Dispatchers.Main) { chatsCtx.startRecordingAddedItems() }

  private fun receivedItem(id: Long, status: CIStatus = CIStatus.RcvRead()): ChatItem =
    ChatItem.getSampleData(id, CIDirection.GroupRcv(GroupMember.sampleData), status = status)

  @Test
  fun lastPageLoadedForAnotherChatIsNotAddedToOpenedChat() = runBlocking {
    val (chatsCtx, _) = openChatWithItems(listOf(101, 102))
    val anotherChat = groupChat(groupId = 1, itemIds = listOf(201, 202))
    processLoadedChat(chatsCtx, anotherChat, NavigationInfo(), ChatPagination.Last(2), openAroundItemId = null, ItemsAddedWhileLoading())
    assertEquals(listOf(101L, 102L), itemIds(chatsCtx))
  }

  @Test
  fun lastPageLoadedForOpenedChatIsAdded() = runBlocking {
    val (chatsCtx, _) = openChatWithItems(listOf(101, 102))
    val sameChat = groupChat(groupId = 2, itemIds = listOf(103, 104))
    processLoadedChat(chatsCtx, sameChat, NavigationInfo(), ChatPagination.Last(2), openAroundItemId = null, ItemsAddedWhileLoading())
    assertEquals(listOf(101L, 102L, 103L, 104L), itemIds(chatsCtx))
  }

  @Test
  fun beforePageLoadedForAnotherChatIsNotAddedToOpenedChat() = runBlocking {
    val (chatsCtx, _) = openChatWithItems(listOf(101, 102))
    val anotherChat = groupChat(groupId = 1, itemIds = listOf(201, 202))
    processLoadedChat(chatsCtx, anotherChat, NavigationInfo(), ChatPagination.Before(101, 2), openAroundItemId = null, ItemsAddedWhileLoading())
    assertEquals(listOf(101L, 102L), itemIds(chatsCtx))
  }

  @Test
  fun aroundPageLoadedForAnotherChatIsNotAddedToOpenedChat() = runBlocking {
    val (chatsCtx, _) = openChatWithItems(listOf(101, 102))
    val anotherChat = groupChat(groupId = 1, itemIds = listOf(201, 202))
    processLoadedChat(chatsCtx, anotherChat, NavigationInfo(), ChatPagination.Around(101, 2), openAroundItemId = null, ItemsAddedWhileLoading())
    assertEquals(listOf(101L, 102L), itemIds(chatsCtx))
  }

  @Test
  fun itemReceivedWhileChatOpensIsShownInOpenedChat() = runBlocking {
    val chatsCtx = ChatModel.ChatsContext(null)
    val opening = groupChat(groupId = 3, itemIds = listOf(101, 102))
    withContext(Dispatchers.Main) { chatModel.chatId.value = null }
    val addedWhileLoading = startRecordingAddedItems(chatsCtx)
    chatsCtx.addChatItem(null, opening.chatInfo, receivedItem(103))
    processLoadedChat(chatsCtx, opening, NavigationInfo(), ChatPagination.Initial(2), openAroundItemId = null, addedWhileLoading)
    assertEquals(listOf(101L, 102L, 103L), itemIds(chatsCtx))
  }

  @Test
  fun itemReceivedWhileOpenedChatReloadsIsKept() = runBlocking {
    val (chatsCtx, opened) = openChatWithItems(listOf(101, 102))
    val addedWhileLoading = startRecordingAddedItems(chatsCtx)
    chatsCtx.addChatItem(null, opened.chatInfo, receivedItem(103))
    processLoadedChat(chatsCtx, opened, NavigationInfo(), ChatPagination.Initial(2), openAroundItemId = null, addedWhileLoading)
    assertEquals(listOf(101L, 102L, 103L), itemIds(chatsCtx))
  }

  @Test
  fun unreadCountOfItemReceivedWhileChatOpensIsKept() = runBlocking {
    val chatsCtx = chatModel.chatsContext
    val opening = groupChat(groupId = 4, itemIds = listOf(101, 102)).copy(chatStats = Chat.ChatStats(unreadCount = 2))
    withContext(Dispatchers.Main) {
      chatModel.currentUser.value = User.sampleData
      chatModel.chatId.value = null
      chatsCtx.addChat(opening)
    }
    val addedWhileLoading = startRecordingAddedItems(chatsCtx)
    chatsCtx.addChatItem(null, opening.chatInfo, receivedItem(103, CIStatus.RcvNew()))
    processLoadedChat(chatsCtx, opening, NavigationInfo(), ChatPagination.Initial(2), openAroundItemId = null, addedWhileLoading)
    assertEquals(3, chatsCtx.getChat(opening.id)?.chatStats?.unreadCount)
  }
}
