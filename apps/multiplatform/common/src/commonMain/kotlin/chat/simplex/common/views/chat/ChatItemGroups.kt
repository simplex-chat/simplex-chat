package chat.simplex.common.views.chat

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyListState
import androidx.compose.runtime.*
import androidx.compose.runtime.snapshots.SnapshotStateList
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatModel.withChats
import chat.simplex.common.platform.chatModel
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlin.math.min

data class MergedItems (
  val items: List<MergedItem>,
  val splits: List<SplitRange>,
  // chat item id, index in list
  val indexInParentItems: Map<Long, Int>,
) {
  companion object {
    fun create(items: List<ChatItem>, unreadCount: State<Int>, revealedItems: Set<Long>, chatState: ActiveChatState): MergedItems {
      if (items.isEmpty()) return MergedItems(emptyList(), emptyList(), emptyMap())

      val unreadAfterItemId = chatState.unreadAfterItemId
      val itemSplits = chatState.splits.value
      val mergedItems = ArrayList<MergedItem>()
      // Indexes of splits here will be related to reversedChatItems, not chatModel.chatItems
      val splitRanges = ArrayList<SplitRange>()
      val indexInParentItems = mutableMapOf<Long, Int>()
      var index = 0
      var unclosedSplitIndex: Int? = null
      var unclosedSplitIndexInParent: Int? = null
      var visibleItemIndexInParent = -1
      var unreadBefore = unreadCount.value - chatState.unreadAfterNewestLoaded.value
      var lastRevealedIdsInMergedItems: MutableList<Long>? = null
      var lastRangeInReversedForMergedItems: MutableStateFlow<IntRange>? = null
      var recent: MergedItem? = null
      while (index < items.size) {
        val item = items[index]
        val prev = items.getOrNull(index - 1)
        val next = items.getOrNull(index + 1)
        val category = item.mergeCategory
        val itemIsSplit = itemSplits.contains(item.id)

        if (item.id == unreadAfterItemId.value) {
          unreadBefore = unreadCount.value - chatState.unreadAfter.value
        }
        if (item.isRcvNew) unreadBefore--

        val revealed = item.mergeCategory == null || revealedItems.contains(item.id)
        if (recent is MergedItem.Grouped && recent.mergeCategory == category && !revealedItems.contains(recent.items.first().item.id) && !itemIsSplit) {
          val listItem = ListItem(item, prev, next, unreadBefore)
          recent.items.add(listItem)

          if (item.isRcvNew) {
            recent.unreadIds.add(item.id)
          }
          if (lastRevealedIdsInMergedItems != null && lastRangeInReversedForMergedItems != null) {
            if (revealed) {
              lastRevealedIdsInMergedItems += item.id
            }
            lastRangeInReversedForMergedItems.value = lastRangeInReversedForMergedItems.value.first..index
          }
        } else {
          visibleItemIndexInParent++
          val listItem = ListItem(item, prev, next, unreadBefore)
          recent = if (item.mergeCategory != null) {
            if (item.mergeCategory != prev?.mergeCategory || lastRevealedIdsInMergedItems == null) {
              lastRevealedIdsInMergedItems = if (revealedItems.contains(item.id)) mutableListOf(item.id) else mutableListOf()
            } else if (revealed) {
              lastRevealedIdsInMergedItems += item.id
            }
            lastRangeInReversedForMergedItems = MutableStateFlow(index .. index)
            MergedItem.Grouped(
              items = arrayListOf(listItem),
              revealed = revealed,
              revealedIdsWithinGroup = lastRevealedIdsInMergedItems,
              rangeInReversed = lastRangeInReversedForMergedItems,
              mergeCategory = item.mergeCategory,
              startIndexInReversedItems = index,
              unreadIds = if (item.isRcvNew) mutableSetOf(item.id) else mutableSetOf()
            )
          } else {
            lastRangeInReversedForMergedItems = null
            MergedItem.Single(
              item = listItem,
              startIndexInReversedItems = index
            )
          }
          mergedItems.add(recent)
        }
        if (itemIsSplit) {
          // found item that is considered as a split
          if (unclosedSplitIndex != null && unclosedSplitIndexInParent != null) {
            // it was at least second split in the list
            splitRanges.add(SplitRange(unclosedSplitIndex until index, unclosedSplitIndexInParent until visibleItemIndexInParent))
          }
          unclosedSplitIndex = index
          unclosedSplitIndexInParent = visibleItemIndexInParent
        } else if (index + 1 == items.size && unclosedSplitIndex != null && unclosedSplitIndexInParent != null) {
          // just one split for the whole list, there will be no more, it's the end
          splitRanges.add(SplitRange(unclosedSplitIndex .. index, unclosedSplitIndexInParent .. visibleItemIndexInParent))
        }
        indexInParentItems[item.id] = visibleItemIndexInParent
        index++
      }
      return MergedItems(
        mergedItems,
        splitRanges,
        indexInParentItems
      )
    }
  }
}

sealed class MergedItem {
  abstract val startIndexInReversedItems: Int

  // the item that is always single, cannot be grouped and always revealed
  data class Single(
    val item: ListItem,
    override val startIndexInReversedItems: Int,
  ): MergedItem()

  /** The item that can contain multiple items or just one depending on revealed state. When the whole group of merged items is revealed,
   * there will be multiple [Grouped] items with revealed flag set to true. When the whole group is collapsed, it will be just one instance
   *  of [Grouped] item with all grouped items inside [items]. In other words, number of [MergedItem] will always be equal to number of
   *  visible rows in ChatView LazyColumn  */
  @Stable
  data class Grouped (
    val items: ArrayList<ListItem>,
    val revealed: Boolean,
    // it stores ids for all consecutive revealed items from the same group in order to hide them all on user's action
    // it's the same list instance for all Grouped items within revealed group
    /** @see reveal */
    val revealedIdsWithinGroup: MutableList<Long>,
    val rangeInReversed: MutableStateFlow<IntRange>,
    val mergeCategory: CIMergeCategory?,
    val unreadIds: MutableSet<Long>,
    override val startIndexInReversedItems: Int,
  ): MergedItem() {
    fun reveal(reveal: Boolean, revealedItems: MutableState<Set<Long>>) {
      val newRevealed = revealedItems.value.toMutableSet()
      var i = 0
      if (reveal) {
        while (i < items.size) {
          newRevealed.add(items[i].item.id)
          i++
        }
      } else {
        while (i < revealedIdsWithinGroup.size) {
          newRevealed.remove(revealedIdsWithinGroup[i])
          i++
        }
        revealedIdsWithinGroup.clear()
      }
      revealedItems.value = newRevealed
    }
  }

  fun hasUnread(): Boolean = when (this) {
    is Single -> item.item.isRcvNew
    is Grouped -> unreadIds.isNotEmpty()
  }

  fun newest(): ListItem = when (this) {
    is Single -> item
    is Grouped -> items.first()
  }

  fun oldest(): ListItem = when (this) {
    is Single -> item
    is Grouped -> items.last()
  }

  fun lastIndexInReversed(): Int = when (this) {
    is Single -> startIndexInReversedItems
    is Grouped -> startIndexInReversedItems + items.lastIndex
  }
}

data class SplitRange(
  /** range of indexes inside reversedChatItems where the first element is the split (it's index is [indexRangeInReversed.first])
   * so [0, 1, 2, -100-, 101] if the 3 is a split, SplitRange(indexRange = 3 .. 4) will be this SplitRange instance
   * (3, 4 indexes of the splitRange with the split itself at index 3)
   * */
  val indexRangeInReversed: IntRange,
  /** range of indexes inside LazyColumn where the first element is the split (it's index is [indexRangeInParentItems.first]) */
  val indexRangeInParentItems: IntRange
)

data class ListItem(
  val item: ChatItem,
  val prevItem: ChatItem?,
  val nextItem: ChatItem?,
  // how many unread items before (older than) this one (excluding this one)
  val unreadBefore: Int
)

data class ActiveChatState (
  val splits: MutableStateFlow<List<Long>> = MutableStateFlow(emptyList()),
  val unreadAfterItemId: MutableStateFlow<Long> = MutableStateFlow(-1L),
  // total items after unread after item (exclusive)
  val totalAfter: MutableStateFlow<Int> = MutableStateFlow(0),
  val unreadTotal: MutableStateFlow<Int> = MutableStateFlow(0),
  // exclusive
  val unreadAfter: MutableStateFlow<Int> = MutableStateFlow(0),
  // exclusive
  val unreadAfterNewestLoaded: MutableStateFlow<Int> = MutableStateFlow(0)
) {
  fun moveUnreadAfterItem(toItemId: Long?, nonReversedItems: List<ChatItem>) {
    toItemId ?: return
    val currentIndex = nonReversedItems.indexOfFirst { it.id == unreadAfterItemId.value }
    val newIndex = nonReversedItems.indexOfFirst { it.id == toItemId }
    if (currentIndex == -1 || newIndex == -1) return
    unreadAfterItemId.value = toItemId
    val unreadDiff = if (newIndex > currentIndex) {
      -nonReversedItems.subList(currentIndex + 1, newIndex + 1).count { it.isRcvNew }
    } else {
      nonReversedItems.subList(newIndex + 1, currentIndex + 1).count { it.isRcvNew }
    }
    unreadAfter.value += unreadDiff
  }

  fun moveUnreadAfterItem(fromIndex: Int, toIndex: Int, nonReversedItems: List<ChatItem>) {
    if (fromIndex == -1 || toIndex == -1) return
    unreadAfterItemId.value = nonReversedItems[toIndex].id
    val unreadDiff = if (toIndex > fromIndex) {
      -nonReversedItems.subList(fromIndex + 1, toIndex + 1).count { it.isRcvNew }
    } else {
      nonReversedItems.subList(toIndex + 1, fromIndex + 1).count { it.isRcvNew }
    }
    unreadAfter.value += unreadDiff
  }

  fun clear() {
    splits.value = emptyList()
    unreadAfterItemId.value = -1L
    totalAfter.value = 0
    unreadTotal.value = 0
    unreadAfter.value = 0
    unreadAfterNewestLoaded.value = 0
  }
}

fun visibleItemIndexesNonReversed(mergedItems: State<MergedItems>, listState: LazyListState): IntRange {
  val zero = 0 .. 0
  if (listState.layoutInfo.totalItemsCount == 0) return zero
  val newest = mergedItems.value.items.getOrNull(listState.firstVisibleItemIndex)?.startIndexInReversedItems
  val oldest = mergedItems.value.items.getOrNull(listState.layoutInfo.visibleItemsInfo.last().index)?.lastIndexInReversed()
  if (newest == null || oldest == null) return zero
  val size = chatModel.chatItems.value.size
  val range = size - oldest .. size - newest
  if (range.first < 0 || range.last < 0) return zero

  // visible items mapped to their underlying data structure which is chatModel.chatItems
  return range
}

fun recalculateChatStatePositions(chatState: ActiveChatState) = object: ChatItemsChangesListener {
  override fun read(itemIds: Set<Long>?, newItems: List<ChatItem>) {
    val (_, unreadAfterItemId, _, unreadTotal, unreadAfter) = chatState
    if (itemIds == null) {
      // special case when the whole chat became read
      unreadTotal.value = 0
      unreadAfter.value = 0
      return
    }
    var unreadAfterItemIndex: Int = -1
    // since it's more often that the newest items become read, it's logical to loop from the end of the list to finish it faster
    var i = newItems.lastIndex
    val ids = itemIds.toMutableSet()
    // intermediate variables to prevent re-setting state value a lot of times without reason
    var newUnreadTotal = unreadTotal.value
    var newUnreadAfter = unreadAfter.value
    while (i >= 0) {
      val item = newItems[i]
      if (item.id == unreadAfterItemId.value) {
        unreadAfterItemIndex = i
      }
      if (ids.contains(item.id)) {
        // was unread, now this item is read
        if (unreadAfterItemIndex == -1) {
          newUnreadAfter--
        }
        newUnreadTotal--
        ids.remove(item.id)
        if (ids.isEmpty()) break
      }
      i--
    }
    unreadTotal.value = newUnreadTotal
    unreadAfter.value = newUnreadAfter
  }
  override fun added(item: Pair<Long, Boolean>, index: Int) {
    if (item.second) {
      chatState.unreadAfter.value++
      chatState.unreadTotal.value++
    }
  }
  override fun removed(itemIds: List<Triple<Long, Int, Boolean>>, newItems: List<ChatItem>) {
    val (splits, unreadAfterItemId, totalAfter, unreadTotal, unreadAfter) = chatState
    val newSplits = ArrayList<Long>()
    for (split in splits.value) {
      val index = itemIds.indexOfFirst { it.first == split }
      // deleted the item that was right before the split between items, find newer item so it will act like the split
      if (index != -1) {
        val newSplit = newItems.getOrNull(itemIds[index].second - itemIds.count { it.second <= index })?.id
        // it the  whole section is gone and splits overlap, don't add it at all
        if (newSplit != null && !newSplits.contains(newSplit)) {
          newSplits.add(newSplit)
        }
      } else {
        newSplits.add(split)
      }
    }
    splits.value = newSplits

    val index = itemIds.indexOfFirst { it.first == unreadAfterItemId.value }
    // unread after item was removed
    if (index != -1) {
      var newUnreadAfterItemId = newItems.getOrNull(itemIds[index].second - itemIds.count { it.second <= index })?.id
      val newUnreadAfterItemWasNull = newUnreadAfterItemId == null
      if (newUnreadAfterItemId == null) {
        // everything on top (including unread after item) were deleted, take top item as unread after id
        newUnreadAfterItemId = newItems.firstOrNull()?.id
      }
      if (newUnreadAfterItemId != null) {
        unreadAfterItemId.value = newUnreadAfterItemId
        totalAfter.value -= itemIds.count { it.second > index }
        unreadTotal.value -= itemIds.count { it.second <= index && it.third }
        unreadAfter.value -= itemIds.count { it.second > index && it.third }
        if (newUnreadAfterItemWasNull) {
          // since the unread after item was moved one item after initial position, adjust counters accordingly
          if (newItems.firstOrNull()?.isRcvNew == true) {
            unreadTotal.value++
            unreadAfter.value--
          }
        }
      } else {
        // all items were deleted, 0 items in chatItems
        unreadAfterItemId.value = -1L
        totalAfter.value = 0
        unreadTotal.value = 0
        unreadAfter.value = 0
      }
    } else {
      totalAfter.value -= itemIds.size
    }
  }
  override fun cleared() { chatState.clear() }
}

/** Helps in debugging */
//@Composable
//fun BoxScope.ShowChatState() {
//  Box(Modifier.align(Alignment.Center).size(200.dp).background(Color.Black)) {
//    val s = chatModel.chatState
//    Text(
//      "itemId ${s.unreadAfterItemId.value} / ${chatModel.chatItems.value.firstOrNull { it.id == s.unreadAfterItemId.value }?.text}, \nunreadAfter ${s.unreadAfter.value}, afterNewest ${s.unreadAfterNewestLoaded.value}",
//      color = Color.White
//    )
//  }
//}
//// Returns items mapping for easy checking the structure
//fun MergedItems.mappingToString(): String = items.mapIndexed { index, g ->
//  when (g) {
//    is MergedItem.Single ->
//      "\nstartIndexInParentItems $index, startIndexInReversedItems ${g.startIndexInReversedItems}, " +
//          "revealed true, " +
//          "mergeCategory null " +
//          "\nunreadBefore ${g.item.unreadBefore}"
//
//    is MergedItem.Grouped ->
//      "\nstartIndexInParentItems $index, startIndexInReversedItems ${g.startIndexInReversedItems}, " +
//          "revealed ${g.revealed}, " +
//          "mergeCategory ${g.items[0].item.mergeCategory} " +
//          g.items.mapIndexed { i, it ->
//            "\nunreadBefore ${it.unreadBefore} ${Triple(index, g.startIndexInReversedItems + i, it.item.id)}"
//          }
//  }
//}.toString()

const val TRIM_KEEP_COUNT = 200

suspend fun apiLoadMessages(
  rhId: Long?,
  chatType: ChatType,
  apiId: Long,
  pagination: ChatPagination,
  chatState: ActiveChatState,
  search: String = "",
  visibleItemIndexesNonReversed: () -> IntRange = { 0 .. 0 }
) = coroutineScope {
  val (chat, navInfo) = chatModel.controller.apiGetChat(rhId, chatType, apiId, pagination, search) ?: return@coroutineScope
  // For .initial allow the chatItems to be empty as well as chatModel.chatId to not match this chat because these values become set after .initial finishes
  if (((chatModel.chatId.value != chat.id || chat.chatItems.isEmpty()) && pagination !is ChatPagination.Initial && pagination !is ChatPagination.Last)
    || !isActive) return@coroutineScope

  val (splits, unreadAfterItemId, totalAfter, unreadTotal, unreadAfter, unreadAfterNewestLoaded) = chatState
  val oldItems = chatModel.chatItems.value
  val newItems = SnapshotStateList<ChatItem>()
  when (pagination) {
    is ChatPagination.Initial -> {
      val newSplits = if (chat.chatItems.isNotEmpty() && navInfo.afterTotal > 0) listOf(chat.chatItems.last().id) else emptyList()
      withChats {
        if (chatModel.getChat(chat.id) == null) {
          addChat(chat)
        }
      }
      withContext(Dispatchers.Main) {
        chatModel.chatItemStatuses.clear()
        chatModel.chatItems.replaceAll(chat.chatItems)
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
      val insertAt = indexInCurrentItems - (wasSize - newItems.size) + trimmedIds.size
      newItems.addAll(insertAt, chat.chatItems)
      withContext(Dispatchers.Main) {
        chatModel.chatItems.replaceAll(newItems)
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
      withContext(Dispatchers.Main) {
        chatModel.chatItems.replaceAll(newItems)
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
      removeDuplicates(newItems, chat)
      // currently, items will always be added on top, which is index 0
      newItems.addAll(0, chat.chatItems)
      withContext(Dispatchers.Main) {
        chatModel.chatItems.replaceAll(newItems)
        splits.value = listOf(chat.chatItems.last().id) + splits.value
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
      withContext(Dispatchers.Main) {
        chatModel.chatItems.replaceAll(newItems)
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
  visibleItemIndexesNonReversed: () -> IntRange = { 0..0 }
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
