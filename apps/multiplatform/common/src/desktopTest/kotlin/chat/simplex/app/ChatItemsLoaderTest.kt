package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.model.ChatModel.ChatsContext.ItemUpdatesWhileLoading
import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.chat.processLoadedChat
import chat.simplex.common.views.chat.recordItemUpdatesWhileLoading
import chat.simplex.common.model.replaceAll
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withContext
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertIs

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

  private fun receivedItem(id: Long): ChatItem =
    ChatItem.getSampleData(id, CIDirection.GroupRcv(GroupMember.sampleData), status = CIStatus.RcvNew())

  @Test
  fun lastPageLoadedForAnotherChatIsNotAddedToOpenedChat() = runBlocking {
    val (chatsCtx, _) = openChatWithItems(listOf(101, 102))
    val anotherChat = groupChat(groupId = 1, itemIds = listOf(201, 202))
    processLoadedChat(chatsCtx, anotherChat, NavigationInfo(), ChatPagination.Last(2), openAroundItemId = null, ItemUpdatesWhileLoading())
    assertEquals(listOf(101L, 102L), itemIds(chatsCtx))
  }

  @Test
  fun lastPageLoadedForOpenedChatIsAdded() = runBlocking {
    val (chatsCtx, _) = openChatWithItems(listOf(101, 102))
    val sameChat = groupChat(groupId = 2, itemIds = listOf(103, 104))
    processLoadedChat(chatsCtx, sameChat, NavigationInfo(), ChatPagination.Last(2), openAroundItemId = null, ItemUpdatesWhileLoading())
    assertEquals(listOf(101L, 102L, 103L, 104L), itemIds(chatsCtx))
  }

  @Test
  fun beforePageLoadedForAnotherChatIsNotAddedToOpenedChat() = runBlocking {
    val (chatsCtx, _) = openChatWithItems(listOf(101, 102))
    val anotherChat = groupChat(groupId = 1, itemIds = listOf(201, 202))
    processLoadedChat(chatsCtx, anotherChat, NavigationInfo(), ChatPagination.Before(101, 2), openAroundItemId = null, ItemUpdatesWhileLoading())
    assertEquals(listOf(101L, 102L), itemIds(chatsCtx))
  }

  @Test
  fun aroundPageLoadedForAnotherChatIsNotAddedToOpenedChat() = runBlocking {
    val (chatsCtx, _) = openChatWithItems(listOf(101, 102))
    val anotherChat = groupChat(groupId = 1, itemIds = listOf(201, 202))
    processLoadedChat(chatsCtx, anotherChat, NavigationInfo(), ChatPagination.Around(101, 2), openAroundItemId = null, ItemUpdatesWhileLoading())
    assertEquals(listOf(101L, 102L), itemIds(chatsCtx))
  }

  @Test
  fun itemReceivedWhileChatOpensIsShownAndCounted() = runBlocking {
    val chatsCtx = chatModel.chatsContext
    val opening = groupChat(groupId = 3, itemIds = listOf(101, 102)).copy(chatStats = Chat.ChatStats(unreadCount = 2))
    withContext(Dispatchers.Main) {
      chatModel.currentUser.value = User.sampleData
      chatModel.chatId.value = null
      chatsCtx.addChat(opening)
    }
    recordItemUpdatesWhileLoading(chatsCtx) { itemUpdates ->
      chatsCtx.addChatItem(null, opening.chatInfo, receivedItem(103))
      processLoadedChat(chatsCtx, opening, NavigationInfo(), ChatPagination.Initial(2), openAroundItemId = null, itemUpdates)
    }
    assertEquals(listOf(101L, 102L, 103L), itemIds(chatsCtx))
    assertEquals(3, chatsCtx.getChat(opening.id)?.chatStats?.unreadCount)
  }

  @Test
  fun itemReceivedWhileOpenedChatReloadsIsKept() = runBlocking {
    val (chatsCtx, opened) = openChatWithItems(listOf(101, 102))
    recordItemUpdatesWhileLoading(chatsCtx) { itemUpdates ->
      chatsCtx.addChatItem(null, opened.chatInfo, receivedItem(103))
      processLoadedChat(chatsCtx, opened, NavigationInfo(), ChatPagination.Initial(2), openAroundItemId = null, itemUpdates)
    }
    assertEquals(listOf(101L, 102L, 103L), itemIds(chatsCtx))
  }

  @Test
  fun itemDeletedWhileChatOpensIsNotShown() = runBlocking {
    val chatsCtx = ChatModel.ChatsContext(null)
    val opening = groupChat(groupId = 4, itemIds = listOf(101, 102))
    withContext(Dispatchers.Main) {
      chatModel.currentUser.value = User.sampleData
      chatModel.chatId.value = null
    }
    val received = receivedItem(103)
    recordItemUpdatesWhileLoading(chatsCtx) { itemUpdates ->
      chatsCtx.addChatItem(null, opening.chatInfo, received)
      withContext(Dispatchers.Main) { chatsCtx.removeChatItem(null, opening.chatInfo, received) }
      val pageReadBeforeDeletion = opening.copy(chatItems = opening.chatItems + received)
      processLoadedChat(chatsCtx, pageReadBeforeDeletion, NavigationInfo(), ChatPagination.Initial(3), openAroundItemId = null, itemUpdates)
    }
    assertEquals(listOf(101L, 102L), itemIds(chatsCtx))
  }

  @Test
  fun groupItemDeletedWhileChatOpensIsShownDeleted() = runBlocking {
    val chatsCtx = chatModel.chatsContext
    val user = User.sampleData
    val opening = groupChat(groupId = 5, itemIds = listOf(101, 102))
    withContext(Dispatchers.Main) {
      chatModel.currentUser.value = user
      chatModel.chatId.value = null
    }
    val received = receivedItem(103)
    val deleted = CR.GroupChatItemsDeleted(
      UserRef(user.userId, user.localDisplayName, activeUser = true, showNtfs = true),
      (opening.chatInfo as ChatInfo.Group).groupInfo,
      chatItemIDs = listOf(103),
      byUser = false,
      member_ = GroupMember.sampleData
    )
    recordItemUpdatesWhileLoading(chatsCtx) { itemUpdates ->
      chatModel.controller.groupChatItemsDeleted(null, deleted)
      val pageReadBeforeDeletion = opening.copy(chatItems = opening.chatItems + received)
      processLoadedChat(chatsCtx, pageReadBeforeDeletion, NavigationInfo(), ChatPagination.Initial(3), openAroundItemId = null, itemUpdates)
    }
    assertIs<CIDeleted.Deleted>(chatsCtx.chatItems.value.firstOrNull { it.id == 103L }?.meta?.itemDeleted)
  }

  @Test
  fun itemDeletedBeforeItsAdditionWhileChatOpensIsNotShown() = runBlocking {
    val chatsCtx = ChatModel.ChatsContext(null)
    val opening = groupChat(groupId = 8, itemIds = listOf(101, 102))
    withContext(Dispatchers.Main) {
      chatModel.currentUser.value = User.sampleData
      chatModel.chatId.value = null
    }
    val received = receivedItem(103)
    recordItemUpdatesWhileLoading(chatsCtx) { itemUpdates ->
      withContext(Dispatchers.Main) { chatsCtx.removeChatItem(null, opening.chatInfo, received) }
      chatsCtx.addChatItem(null, opening.chatInfo, received)
      processLoadedChat(chatsCtx, opening, NavigationInfo(), ChatPagination.Initial(2), openAroundItemId = null, itemUpdates)
    }
    assertEquals(listOf(101L, 102L), itemIds(chatsCtx))
  }

  @Test
  fun itemReceivedWhileChatOpensAtItemIsShown() = runBlocking {
    val chatsCtx = ChatModel.ChatsContext(null)
    val opening = groupChat(groupId = 7, itemIds = listOf(101, 102))
    withContext(Dispatchers.Main) { chatModel.chatId.value = null }
    recordItemUpdatesWhileLoading(chatsCtx) { itemUpdates ->
      chatsCtx.addChatItem(null, opening.chatInfo, receivedItem(103))
      processLoadedChat(chatsCtx, opening, NavigationInfo(), ChatPagination.Around(101, 2), openAroundItemId = 101, itemUpdates)
    }
    assertEquals(listOf(101L, 102L, 103L), itemIds(chatsCtx))
  }

  @Test
  fun itemReadWhileOpenedChatReloadsStaysRead() = runBlocking {
    val (chatsCtx, opened) = openChatWithItems(listOf(101, 102))
    withContext(Dispatchers.Main) { chatModel.currentUser.value = User.sampleData }
    val received = receivedItem(103)
    recordItemUpdatesWhileLoading(chatsCtx) { itemUpdates ->
      chatsCtx.addChatItem(null, opened.chatInfo, received)
      withContext(Dispatchers.Main) { chatsCtx.markChatItemsRead(null, opened.id, listOf(103)) }
      val pageReadBeforeMark = opened.copy(chatItems = opened.chatItems + received)
      processLoadedChat(chatsCtx, pageReadBeforeMark, NavigationInfo(), ChatPagination.Initial(3), openAroundItemId = null, itemUpdates)
    }
    assertEquals(false, chatsCtx.chatItems.value.firstOrNull { it.id == 103L }?.isRcvNew)
  }

  @Test
  fun itemReceivedWhileChatOpensAtOlderPageIsNotAdded() = runBlocking {
    val chatsCtx = ChatModel.ChatsContext(null)
    val opening = groupChat(groupId = 6, itemIds = listOf(101, 102))
    withContext(Dispatchers.Main) { chatModel.chatId.value = null }
    recordItemUpdatesWhileLoading(chatsCtx) { itemUpdates ->
      chatsCtx.addChatItem(null, opening.chatInfo, receivedItem(104))
      processLoadedChat(chatsCtx, opening, NavigationInfo(afterUnread = 2, afterTotal = 2), ChatPagination.Initial(2), openAroundItemId = null, itemUpdates)
    }
    assertEquals(listOf(101L, 102L), itemIds(chatsCtx))
  }
}
