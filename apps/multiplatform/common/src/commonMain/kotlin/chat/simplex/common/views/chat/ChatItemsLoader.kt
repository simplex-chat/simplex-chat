package chat.simplex.common.views.chat

import androidx.compose.runtime.snapshots.SnapshotStateList
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatModel.withChats
import chat.simplex.common.platform.chatModel
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.StateFlow
import kotlin.math.min

const val TRIM_KEEP_COUNT = 200

suspend fun apiLoadSingleMessage(
  rhId: Long?,
  chatType: ChatType,
  apiId: Long,
  itemId: Long,
  contentTag: MsgContentTag?,
): ChatItem? = coroutineScope {
  val contentFilter = if (contentTag != null) ContentFilter(contentTag, true) else null
  val (chat, _) = chatModel.controller.apiGetChat(rhId, chatType, apiId, contentFilter, ChatPagination.Around(itemId, 0), "") ?: return@coroutineScope null
  chat.chatItems.firstOrNull()
}

suspend fun apiLoadMessages(
  rhId: Long?,
  chatType: ChatType,
  apiId: Long,
  contentFilter: ContentFilter?,
  pagination: ChatPagination,
  search: String = "",
  visibleItemIndexesNonReversed: () -> IntRange = { 0 .. 0 }
) = coroutineScope {
  val (chat, navInfo) = chatModel.controller.apiGetChat(rhId, chatType, apiId, contentFilter, pagination, search) ?: return@coroutineScope
  // For .initial allow the chatItems to be empty as well as chatModel.chatId to not match this chat because these values become set after .initial finishes
  if (((chatModel.chatId.value != chat.id || chat.chatItems.isEmpty()) && pagination !is ChatPagination.Initial && pagination !is ChatPagination.Last)
    || !isActive) return@coroutineScope

  val chatState = chatModel.chatStateForContent(contentFilter?.mcTag)
  val contentTag = contentFilter?.mcTag
  val (splits, unreadAfterItemId, totalAfter, unreadTotal, unreadAfter, unreadAfterNewestLoaded) = chatState
  val oldItems = chatModel.chatItemsForContent(contentFilter?.mcTag).value
  val newItems = SnapshotStateList<ChatItem>()
  when (pagination) {
    is ChatPagination.Initial -> {
      val newSplits = if (chat.chatItems.isNotEmpty() && navInfo.afterTotal > 0) listOf(chat.chatItems.last().id) else emptyList()
      if (contentTag == null) {
        // update main chats, not content tagged
        withChats {
          if (getChat(chat.id) == null) {
            addChat(chat)
          } else {
            updateChatInfo(chat.remoteHostId, chat.chatInfo)
            updateChatStats(chat.remoteHostId, chat.id, chat.chatStats)
          }
        }
      }
      withChats(contentTag) {
        chatModel.chatItemStatuses.clear()
        chatItems.replaceAll(chat.chatItems)
        chatModel.chatId.value = chat.chatInfo.id
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
      if (indexInCurrentItems == -1) return@coroutineScope
      val (newIds, _) = mapItemsToIds(chat.chatItems)
      val wasSize = newItems.size
      val (oldUnreadSplitIndex, newUnreadSplitIndex, trimmedIds, newSplits) = removeDuplicatesAndModifySplitsOnBeforePagination(
        unreadAfterItemId, newItems, newIds, splits, visibleItemIndexesNonReversed
      )
      val insertAt = (indexInCurrentItems - (wasSize - newItems.size) + trimmedIds.size).coerceAtLeast(0)
      newItems.addAll(insertAt, chat.chatItems)
      withChats(contentTag) {
        chatItems.replaceAll(newItems)
        splits.value = newSplits
        chatState.moveUnreadAfterItem(oldUnreadSplitIndex, newUnreadSplitIndex, oldItems)
      }
    }
    is ChatPagination.After -> {
      newItems.addAll(oldItems)
      val indexInCurrentItems: Int = oldItems.indexOfFirst { it.id == pagination.chatItemId }
      if (indexInCurrentItems == -1) return@coroutineScope

      val mappedItems = mapItemsToIds(chat.chatItems)
      val newIds = mappedItems.first
      val (newSplits, unreadInLoaded) = removeDuplicatesAndModifySplitsOnAfterPagination(
        mappedItems.second, pagination.chatItemId, newItems, newIds, chat, splits
      )
      val indexToAdd = min(indexInCurrentItems + 1, newItems.size)
      val indexToAddIsLast = indexToAdd == newItems.size
      newItems.addAll(indexToAdd, chat.chatItems)
      withChats(contentTag) {
        chatItems.replaceAll(newItems)
        splits.value = newSplits
        chatState.moveUnreadAfterItem(splits.value.firstOrNull() ?: newItems.last().id, newItems)
        // loading clear bottom area, updating number of unread items after the newest loaded item
        if (indexToAddIsLast) {
          unreadAfterNewestLoaded.value -= unreadInLoaded
        }
      }
    }
    is ChatPagination.Around -> {
      newItems.addAll(oldItems)
      val newSplits = removeDuplicatesAndUpperSplits(newItems, chat, splits, visibleItemIndexesNonReversed)
      // currently, items will always be added on top, which is index 0
      newItems.addAll(0, chat.chatItems)
      withChats(contentTag) {
        chatItems.replaceAll(newItems)
        splits.value = listOf(chat.chatItems.last().id) + newSplits
        unreadAfterItemId.value = chat.chatItems.last().id
        totalAfter.value = navInfo.afterTotal
        unreadTotal.value = chat.chatStats.unreadCount
        unreadAfter.value = navInfo.afterUnread
        // no need to set it, count will be wrong
        // unreadAfterNewestLoaded.value = navInfo.afterUnread
      }
    }
    is ChatPagination.Last -> {
      newItems.addAll(oldItems)
      removeDuplicates(newItems, chat)
      newItems.addAll(chat.chatItems)
      withChats(contentTag) {
        chatItems.replaceAll(newItems)
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
  val splitsToMerge = if (loadingFromSplitRange && indexInSplitRanges + 1 <= splits.value.size) ArrayList(splits.value.subList(indexInSplitRanges + 1, splits.value.size)) else ArrayList()
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
    // no splits anymore, all were merged with bottom items
    newSplits = emptyList()
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
