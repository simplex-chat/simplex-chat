package chat.simplex.common.views.chat

import androidx.compose.runtime.snapshots.SnapshotStateList
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.StateFlow
import kotlinx.datetime.Instant
import kotlin.math.min

const val TRIM_KEEP_COUNT = 200

suspend fun apiLoadSingleMessage(
  chatsCtx: ChatModel.ChatsContext,
  rhId: Long?,
  chatType: ChatType,
  apiId: Long,
  itemId: Long
): ChatItem? = coroutineScope {
  val (chat, _) = chatModel.controller.apiGetChat(rhId, chatType, apiId, chatsCtx.contentTag, ChatPagination.Around(itemId, 0), "") ?: return@coroutineScope null
  chat.chatItems.firstOrNull()
}

suspend fun apiLoadMessages(
  chatsCtx: ChatModel.ChatsContext,
  rhId: Long?,
  chatType: ChatType,
  apiId: Long,
  pagination: ChatPagination,
  search: String = "",
  openAroundItemId: Long? = null,
  visibleItemIndexesNonReversed: () -> IntRange = { 0 .. 0 }
) = coroutineScope {
  val (chat, navInfo) = chatModel.controller.apiGetChat(rhId, chatType, apiId, chatsCtx.contentTag, pagination, search) ?: return@coroutineScope
  // For .initial allow the chatItems to be empty as well as chatModel.chatId to not match this chat because these values become set after .initial finishes
  /** When [openAroundItemId] is provided, chatId can be different too */
  if (((chatModel.chatId.value != chat.id || chat.chatItems.isEmpty()) && pagination !is ChatPagination.Initial && pagination !is ChatPagination.Last && openAroundItemId == null)
    || !isActive) return@coroutineScope
  processLoadedChat(chatsCtx, chat, navInfo, pagination, openAroundItemId, visibleItemIndexesNonReversed)
}

suspend fun processLoadedChat(
  chatsCtx: ChatModel.ChatsContext,
  chat: Chat,
  navInfo: NavigationInfo,
  pagination: ChatPagination,
  openAroundItemId: Long?,
  visibleItemIndexesNonReversed: () -> IntRange = { 0 .. 0 }
) {
  val chatState = chatsCtx.chatState
  val (splits, unreadAfterItemId, totalAfter, unreadTotal, unreadAfter, unreadAfterNewestLoaded) = chatState
  val oldItems = chatsCtx.chatItems.value
  val newItems = SnapshotStateList<ChatItem>()
  when (pagination) {
    is ChatPagination.Initial -> {
      val newSplits = if (chat.chatItems.isNotEmpty() && navInfo.afterTotal > 0) listOf(chat.chatItems.last().id) else emptyList()
      if (chatsCtx.contentTag == null) {
        // update main chats, not content tagged
        withContext(Dispatchers.Main) {
          val oldChat = chatModel.chatsContext.getChat(chat.id)
          if (oldChat == null) {
            chatModel.chatsContext.addChat(chat)
          } else {
            chatModel.chatsContext.updateChatInfo(chat.remoteHostId, chat.chatInfo)
            // unreadChat is currently not actual in getChat query (always false)
            chatModel.chatsContext.updateChatStats(chat.remoteHostId, chat.id, chat.chatStats.copy(unreadChat = oldChat.chatStats.unreadChat))
          }
        }
      }
      withContext(Dispatchers.Main) {
        chatsCtx.chatItemStatuses.clear()
        chatsCtx.chatItems.replaceAll(chat.chatItems)
        chatModel.chatId.value = chat.id
        splits.value = newSplits
        if (chat.chatItems.isNotEmpty()) {
          unreadAfterItemId.value = chat.chatItems.last().id
        }
        totalAfter.value = navInfo.afterTotal
        unreadTotal.value = chat.chatStats.unreadCount
        unreadAfter.value = navInfo.afterUnread
        unreadAfterNewestLoaded.value = navInfo.afterUnread
      }
    }
    is ChatPagination.Before -> {
      newItems.addAll(oldItems)
      val indexInCurrentItems: Int = oldItems.indexOfFirst { it.id == pagination.chatItemId }
      if (indexInCurrentItems == -1) return
      val (newIds, _) = mapItemsToIds(chat.chatItems)
      val wasSize = newItems.size
      val (oldUnreadSplitIndex, newUnreadSplitIndex, trimmedIds, newSplits) = removeDuplicatesAndModifySplitsOnBeforePagination(
        unreadAfterItemId, newItems, newIds, splits, visibleItemIndexesNonReversed
      )
      val insertAt = (indexInCurrentItems - (wasSize - newItems.size) + trimmedIds.size).coerceAtLeast(0)
      newItems.addAll(insertAt, chat.chatItems)
      withContext(Dispatchers.Main) {
        chatsCtx.chatItems.replaceAll(newItems)
        splits.value = newSplits
        chatState.moveUnreadAfterItem(oldUnreadSplitIndex, newUnreadSplitIndex, oldItems)
      }
    }
    is ChatPagination.After -> {
      newItems.addAll(oldItems)
      val indexInCurrentItems: Int = oldItems.indexOfFirst { it.id == pagination.chatItemId }
      if (indexInCurrentItems == -1) return

      val mappedItems = mapItemsToIds(chat.chatItems)
      val newIds = mappedItems.first
      val (newSplits, unreadInLoaded) = removeDuplicatesAndModifySplitsOnAfterPagination(
        mappedItems.second, pagination.chatItemId, newItems, newIds, chat, splits
      )
      val indexToAdd = min(indexInCurrentItems + 1, newItems.size)
      val indexToAddIsLast = indexToAdd == newItems.size
      newItems.addAll(indexToAdd, chat.chatItems)
      withContext(Dispatchers.Main) {
        chatsCtx.chatItems.replaceAll(newItems)
        splits.value = newSplits
        chatState.moveUnreadAfterItem(splits.value.firstOrNull() ?: newItems.last().id, newItems)
        // loading clear bottom area, updating number of unread items after the newest loaded item
        if (indexToAddIsLast) {
          unreadAfterNewestLoaded.value -= unreadInLoaded
        }
      }
    }
    is ChatPagination.Around -> {
      val newSplits: ArrayList<Long> = if (openAroundItemId == null) {
        newItems.addAll(oldItems)
        ArrayList(removeDuplicatesAndUpperSplits(newItems, chat, splits, visibleItemIndexesNonReversed))
      } else {
        arrayListOf()
      }
      val (itemIndex, splitIndex) = indexToInsertAround(chat.chatInfo.chatType, chat.chatItems.lastOrNull(), to = newItems, newSplits.toSet())
      //indexToInsertAroundTest()
      newItems.addAll(itemIndex, chat.chatItems)
      newSplits.add(splitIndex, chat.chatItems.last().id)

      withContext(Dispatchers.Main) {
        chatsCtx.chatItems.replaceAll(newItems)
        splits.value = newSplits
        unreadAfterItemId.value = chat.chatItems.last().id
        totalAfter.value = navInfo.afterTotal
        unreadTotal.value = chat.chatStats.unreadCount
        unreadAfter.value = navInfo.afterUnread

        if (openAroundItemId != null) {
          unreadAfterNewestLoaded.value = navInfo.afterUnread
          chatModel.openAroundItemId.value = openAroundItemId
          chatModel.chatId.value = chat.id
        } else {
          // no need to set it, count will be wrong
          // unreadAfterNewestLoaded.value = navInfo.afterUnread
        }
      }
    }
    is ChatPagination.Last -> {
      newItems.addAll(oldItems)
      val newSplits = removeDuplicatesAndUnusedSplits(newItems, chat, chatState.splits.value)
      removeDuplicates(newItems, chat)
      newItems.addAll(chat.chatItems)
      withContext(Dispatchers.Main) {
        chatsCtx.chatItems.replaceAll(newItems)
        chatState.splits.value = newSplits
        unreadAfterNewestLoaded.value = 0
      }
    }
  }
}

private data class ModifiedSplits (
  val oldUnreadSplitIndex: Int,
  val newUnreadSplitIndex: Int,
  val trimmedIds: Set<Long>,
  val newSplits: List<Long>,
)

private fun removeDuplicatesAndModifySplitsOnBeforePagination(
  unreadAfterItemId: StateFlow<Long>,
  newItems: SnapshotStateList<ChatItem>,
  newIds: Set<Long>,
  splits: StateFlow<List<Long>>,
  visibleItemIndexesNonReversed: () -> IntRange
): ModifiedSplits {
  var oldUnreadSplitIndex: Int = -1
  var newUnreadSplitIndex: Int = -1
  val visibleItemIndexes = visibleItemIndexesNonReversed()
  var lastSplitIndexTrimmed = -1
  var allowedTrimming = true
  var index = 0
  /** keep the newest [TRIM_KEEP_COUNT] items (bottom area) and oldest [TRIM_KEEP_COUNT] items, trim others */
  val trimRange = visibleItemIndexes.last + TRIM_KEEP_COUNT .. newItems.size - TRIM_KEEP_COUNT
  val trimmedIds = mutableSetOf<Long>()
  val prevItemTrimRange = visibleItemIndexes.last + TRIM_KEEP_COUNT + 1 .. newItems.size - TRIM_KEEP_COUNT
  var newSplits = splits.value

  newItems.removeAll {
    val invisibleItemToTrim = trimRange.contains(index) && allowedTrimming
    val prevItemWasTrimmed = prevItemTrimRange.contains(index) && allowedTrimming
    // may disable it after clearing the whole split range
    if (splits.value.isNotEmpty() && it.id == splits.value.firstOrNull()) {
      // trim only in one split range
      allowedTrimming = false
    }
    val indexInSplits = splits.value.indexOf(it.id)
    if (indexInSplits != -1) {
      lastSplitIndexTrimmed = indexInSplits
    }
    if (invisibleItemToTrim) {
      if (prevItemWasTrimmed) {
        trimmedIds.add(it.id)
      } else {
        newUnreadSplitIndex = index
        // prev item is not supposed to be trimmed, so exclude current one from trimming and set a split here instead.
        // this allows to define splitRange of the oldest items and to start loading trimmed items when user scrolls in the opposite direction
        if (lastSplitIndexTrimmed == -1) {
          newSplits = listOf(it.id) + newSplits
        } else {
          val new = ArrayList(newSplits)
          new[lastSplitIndexTrimmed] = it.id
          newSplits = new
        }
      }
    }
    if (unreadAfterItemId.value == it.id) {
      oldUnreadSplitIndex = index
    }
    index++
    (invisibleItemToTrim && prevItemWasTrimmed) || newIds.contains(it.id)
  }
  // will remove any splits that now becomes obsolete because items were merged
  newSplits = newSplits.filterNot { split -> newIds.contains(split) || trimmedIds.contains(split) }
  return ModifiedSplits(oldUnreadSplitIndex, newUnreadSplitIndex, trimmedIds, newSplits)
}

private fun removeDuplicatesAndModifySplitsOnAfterPagination(
  unreadInLoaded: Int,
  paginationChatItemId: Long,
  newItems: SnapshotStateList<ChatItem>,
  newIds: Set<Long>,
  chat: Chat,
  splits: StateFlow<List<Long>>
): Pair<List<Long>, Int> {
  var unreadInLoaded = unreadInLoaded
  var firstItemIdBelowAllSplits: Long? = null
  val splitsToRemove = ArrayList<Long>()
  val indexInSplitRanges = splits.value.indexOf(paginationChatItemId)
  // Currently, it should always load from split range
  val loadingFromSplitRange = indexInSplitRanges != -1
  val topSplits: List<Long>
  val splitsToMerge: ArrayList<Long>
  if (loadingFromSplitRange && indexInSplitRanges + 1 <= splits.value.size) {
    splitsToMerge = ArrayList(splits.value.subList(indexInSplitRanges + 1, splits.value.size))
    topSplits = splits.value.take(indexInSplitRanges + 1)
  } else {
    splitsToMerge = ArrayList()
    topSplits = emptyList()
  }
  newItems.removeAll {
    val duplicate = newIds.contains(it.id)
    if (loadingFromSplitRange && duplicate) {
      if (splitsToMerge.contains(it.id)) {
        splitsToMerge.remove(it.id)
        splitsToRemove.add(it.id)
      } else if (firstItemIdBelowAllSplits == null && splitsToMerge.isEmpty()) {
        // we passed all splits and found duplicated item below all of them, which means no splits anymore below the loaded items
        firstItemIdBelowAllSplits = it.id
      }
    }
    if (duplicate && it.isRcvNew) {
      unreadInLoaded--
    }
    duplicate
  }
  var newSplits: List<Long> = emptyList()
  if (firstItemIdBelowAllSplits != null) {
    // no splits below anymore, all were merged with bottom items
    newSplits = topSplits
  } else {
    if (splitsToRemove.isNotEmpty()) {
      val new = ArrayList(splits.value)
      new.removeAll(splitsToRemove.toSet())
      newSplits = new
    }
    val enlargedSplit = splits.value.indexOf(paginationChatItemId)
    if (enlargedSplit != -1) {
      // move the split to the end of loaded items
      val new = ArrayList(splits.value)
      new[enlargedSplit] = chat.chatItems.last().id
      newSplits = new
      // Log.d(TAG, "Enlarged split range $newSplits")
    }
  }
  return newSplits to unreadInLoaded
}

private fun removeDuplicatesAndUpperSplits(
  newItems: SnapshotStateList<ChatItem>,
  chat: Chat,
  splits: StateFlow<List<Long>>,
  visibleItemIndexesNonReversed: () -> IntRange
): List<Long> {
  if (splits.value.isEmpty()) {
    removeDuplicates(newItems, chat)
    return splits.value
  }

  val newSplits = splits.value.toMutableList()
  val visibleItemIndexes = visibleItemIndexesNonReversed()
  val (newIds, _) = mapItemsToIds(chat.chatItems)
  val idsToTrim = ArrayList<MutableSet<Long>>()
  idsToTrim.add(mutableSetOf())
  var index = 0
  newItems.removeAll {
    val duplicate = newIds.contains(it.id)
    if (!duplicate && visibleItemIndexes.first > index) {
      idsToTrim.last().add(it.id)
    }
    if (visibleItemIndexes.first > index && splits.value.contains(it.id)) {
      newSplits -= it.id
      // closing previous range. All items in idsToTrim that ends with empty set should be deleted.
      // Otherwise, the last set should be excluded from trimming because it is in currently visible split range
      idsToTrim.add(mutableSetOf())
    }

    index++
    duplicate
  }
  if (idsToTrim.last().isNotEmpty()) {
    // it has some elements to trim from currently visible range which means the items shouldn't be trimmed
    // Otherwise, the last set would be empty
    idsToTrim.removeLast()
  }
  val allItemsToDelete = idsToTrim.flatten()
  if (allItemsToDelete.isNotEmpty()) {
    newItems.removeAll { allItemsToDelete.contains(it.id) }
  }
  return newSplits
}

private fun removeDuplicatesAndUnusedSplits(
  newItems: SnapshotStateList<ChatItem>,
  chat: Chat,
  splits: List<Long>
): List<Long> {
  if (splits.isEmpty()) {
    removeDuplicates(newItems, chat)
    return splits
  }

  val newSplits = splits.toMutableList()
  val (newIds, _) = mapItemsToIds(chat.chatItems)
  newItems.removeAll {
    val duplicate = newIds.contains(it.id)
    if (duplicate) {
      val firstIndex = newSplits.indexOf(it.id)
      if (firstIndex != -1) {
        newSplits.removeAt(firstIndex)
      }
    }
    duplicate
  }
  return newSplits
}

// ids, number of unread items
private fun mapItemsToIds(items: List<ChatItem>): Pair<Set<Long>, Int> {
  var unreadInLoaded = 0
  val ids = mutableSetOf<Long>()
  var i = 0
  while (i < items.size) {
    val item = items[i]
    ids.add(item.id)
    if (item.isRcvNew) {
      unreadInLoaded++
    }
    i++
  }
  return ids to unreadInLoaded
}

private fun removeDuplicates(newItems: SnapshotStateList<ChatItem>, chat: Chat) {
  val (newIds, _) = mapItemsToIds(chat.chatItems)
  newItems.removeAll { newIds.contains(it.id) }
}

private data class SameTimeItem(val index: Int, val item: ChatItem)

// return (item index, split index)
private fun indexToInsertAround(chatType: ChatType, lastNew: ChatItem?, to: List<ChatItem>, splits: Set<Long>): Pair<Int, Int> {
  if (to.size <= 0 || lastNew == null) {
    return 0 to 0
  }
  // group sorting: item_ts, item_id
  // everything else: created_at, item_id
  val compareByTimeTs = chatType == ChatType.Group
  // in case several items have the same time as another item in the `to` array
  var sameTime: ArrayList<SameTimeItem> = arrayListOf()

  // trying to find new split index for item looks difficult but allows to not use one more loop.
  // The idea is to memorize how many splits were till any index (map number of splits until index)
  // and use resulting itemIndex to decide new split index position.
  // Because of the possibility to have many items with the same timestamp, it's possible to see `itemIndex < || == || > i`.
  val splitsTillIndex: ArrayList<Int> = arrayListOf()
  var splitsPerPrevIndex = 0

  for (i in to.indices) {
    val item = to[i]

    splitsPerPrevIndex = if (splits.contains(item.id)) splitsPerPrevIndex + 1 else splitsPerPrevIndex
    splitsTillIndex.add(splitsPerPrevIndex)
    val itemIsNewer = (if (compareByTimeTs) item.meta.itemTs > lastNew.meta.itemTs else item.meta.createdAt > lastNew.meta.createdAt)
    if (itemIsNewer || i + 1 == to.size) {
      val same = if (compareByTimeTs) lastNew.meta.itemTs == item.meta.itemTs else lastNew.meta.createdAt == item.meta.createdAt
      if (same) {
        sameTime.add(SameTimeItem(i, item))
      }
      // time to stop the loop. Item is newer, or it's the last item in `to` array, taking previous items and checking position inside them
      val itemIndex: Int
      val first = if (sameTime.size > 1) sameTime.sortedWith { prev, next -> prev.item.meta.itemId.compareTo(next.item.id) }.firstOrNull { same -> same.item.id > lastNew.id } else null
      if (sameTime.size > 1 && first != null) {
        itemIndex = first.index
      } else if (sameTime.size == 1) {
        itemIndex = if (sameTime[0].item.id > lastNew.id) sameTime[0].index else sameTime[0].index + 1
      } else {
        itemIndex = if (itemIsNewer) i else i + 1
      }
      val splitIndex = splitsTillIndex[min(itemIndex, splitsTillIndex.size - 1)]
      val prevItemSplitIndex = if (itemIndex == 0) 0 else splitsTillIndex[min(itemIndex - 1, splitsTillIndex.size - 1)]
      return Pair(itemIndex, if (splitIndex == prevItemSplitIndex) splitIndex else prevItemSplitIndex)
    }
    val same = if (compareByTimeTs) lastNew.meta.itemTs == item.meta.itemTs else lastNew.meta.createdAt == item.meta.createdAt
    if (same) {
      sameTime.add(SameTimeItem(index = i, item = item))
    } else {
      sameTime = arrayListOf()
    }
  }
  // shouldn't be here
  return Pair(to.size, splits.size)
}

private fun indexToInsertAroundTest() {
  fun assert(one: Pair<Int, Int>, two: Pair<Int, Int>) {
    if (one != two) {
      throw Exception("$one != $two")
    }
  }

  val itemsToInsert = listOf(ChatItem.getSampleData(3, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds( 3), ""))
  val items1 = listOf(
    ChatItem.getSampleData(0, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds( 0), ""),
  ChatItem.getSampleData(1, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds( 1), ""),
  ChatItem.getSampleData(2, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds( 2), "")
  )
  assert(indexToInsertAround(ChatType.Group, itemsToInsert.lastOrNull(), to = items1, setOf(1)), Pair(3, 1))

  val items2 = listOf(
    ChatItem.getSampleData(0, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(0), ""),
  ChatItem.getSampleData(1, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(1), ""),
  ChatItem.getSampleData(2, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(3), "")
  )
  assert(indexToInsertAround(ChatType.Group, itemsToInsert.lastOrNull(), to = items2, setOf(2)), Pair(3, 1))

  val items3 = listOf(
    ChatItem.getSampleData(0, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(0), ""),
  ChatItem.getSampleData(1, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(3), ""),
  ChatItem.getSampleData(2, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(3), "")
  )
  assert(indexToInsertAround(ChatType.Group, itemsToInsert.lastOrNull(), to = items3, setOf(1)), Pair(3, 1))

  val items4 = listOf(
    ChatItem.getSampleData(0, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(0), ""),
  ChatItem.getSampleData(4, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(3), ""),
  ChatItem.getSampleData(5, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(3), "")
  )
  assert(indexToInsertAround(ChatType.Group, itemsToInsert.lastOrNull(), to = items4, setOf(4)), Pair(1, 0))

  val items5 = listOf(
    ChatItem.getSampleData(0, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(0), ""),
  ChatItem.getSampleData(2, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(3), ""),
  ChatItem.getSampleData(4, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(3), "")
  )
  assert(indexToInsertAround(ChatType.Group, itemsToInsert.lastOrNull(), to = items5, setOf(2)), Pair(2, 1))

  val items6 = listOf(
    ChatItem.getSampleData(4, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(4), ""),
  ChatItem.getSampleData(5, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(4), ""),
  ChatItem.getSampleData(6, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(4), "")
  )
  assert(indexToInsertAround(ChatType.Group, itemsToInsert.lastOrNull(), to = items6, setOf(5)), Pair(0, 0))

  val items7 = listOf(
    ChatItem.getSampleData(4, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(4), ""),
  ChatItem.getSampleData(5, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(4), ""),
  ChatItem.getSampleData(6, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(4), "")
  )
  assert(indexToInsertAround(ChatType.Group, null, to = items7, setOf(6)), Pair(0, 0))

  val items8 = listOf(
    ChatItem.getSampleData(2, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(4), ""),
  ChatItem.getSampleData(4, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(3), ""),
  ChatItem.getSampleData(5, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(4), "")
  )
  assert(indexToInsertAround(ChatType.Group, itemsToInsert.lastOrNull(), to = items8, setOf(2)), Pair(0, 0))

  val items9 = listOf(
    ChatItem.getSampleData(2, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(3), ""),
  ChatItem.getSampleData(4, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(3), ""),
  ChatItem.getSampleData(5, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(4), "")
  )
  assert(indexToInsertAround(ChatType.Group, itemsToInsert.lastOrNull(), to = items9, setOf(5)), Pair(1, 0))

  val items10 = listOf(
    ChatItem.getSampleData(4, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(3), ""),
  ChatItem.getSampleData(5, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(3), ""),
  ChatItem.getSampleData(6, CIDirection.GroupSnd(), Instant.fromEpochMilliseconds(4), "")
  )
  assert(indexToInsertAround(ChatType.Group, itemsToInsert.lastOrNull(), to = items10, setOf(4)), Pair(0, 0))

  val items11: List<ChatItem> = listOf()
  assert(indexToInsertAround(ChatType.Group, itemsToInsert.lastOrNull(), to = items11, emptySet()), Pair(0, 0))
}
