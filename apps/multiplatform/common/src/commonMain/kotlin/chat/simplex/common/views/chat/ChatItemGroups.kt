package chat.simplex.common.views.chat

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyListState
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.runtime.snapshots.SnapshotStateList
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.model.CIDirection.GroupRcv
import chat.simplex.common.model.ChatModel.withChats
import chat.simplex.common.platform.chatModel
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.MutableStateFlow
import kotlin.math.abs
import kotlin.math.min

data class MergedItems (
  val items: List<MergedItem>,
  val splits: List<SplitRange>,
  // chat item id, index in list
  val indexInParentItems: Map<Long, Int>,
) {
  /** Returns items mapping for easy checking the structure */
  fun mappingToString(): String = items.mapIndexed { index, g ->
    when (g) {
      is MergedItem.Single ->
        "\nstartIndexInParentItems $index, startIndexInReversedItems ${g.startIndexInReversedItems}, " +
            "revealed true, " +
            "mergeCategory null " +
            "\nunreadBefore ${g.item.unreadBefore}"

      is MergedItem.Grouped ->
        "\nstartIndexInParentItems $index, startIndexInReversedItems ${g.startIndexInReversedItems}, " +
            "revealed ${g.revealed}, " +
            "mergeCategory ${g.items[0].item.mergeCategory} " +
            g.items.mapIndexed { i, it ->
              "\nunreadBefore ${it.unreadBefore} ${Triple(index, g.startIndexInReversedItems + i, it.item.id)}"
            }
    }
  }.toString()

  companion object {
    fun create(items: List<ChatItem>, unreadCount: State<Int>, revealedItems: Set<Long>, chatState: ActiveChatState): MergedItems {
      if (items.isEmpty()) return MergedItems(emptyList(), emptyList(), emptyMap())

      val unreadAnchorItemId = chatState.unreadAnchorItemId
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
        val next = items.getOrNull(index + 1)
        val category = item.mergeCategory
        val itemIsSplit = itemSplits.contains(item.id)

        val itemSeparation: ItemSeparation
        val prevItemSeparationLargeGap: Boolean
        if (item.id == unreadAnchorItemId.value) {
          unreadBefore = unreadCount.value - chatState.unreadAfter.value
        }
        if (item.isRcvNew) unreadBefore--

        val revealed = item.mergeCategory == null || revealedItems.contains(item.id)
        if (recent is MergedItem.Grouped && recent.mergeCategory == category && !revealedItems.contains(recent.items.first().item.id) && !itemIsSplit) {
//          if (recent.revealed.value) {
//            val prev = items.getOrNull(index - 1)
//            itemSeparation = getItemSeparation(item, prev)
//            val nextForGap = if ((category != null && category == prev?.mergeCategory) || index + 1 == items.size) null else next
//            prevItemSeparationLargeGap = if (nextForGap == null) false else getItemSeparationLargeGap(nextForGap, item)

//            visibleItemIndexInParent++
//          } else {
            itemSeparation = getItemSeparation(item, null)
            prevItemSeparationLargeGap = false
//          }
          val listItem = ListItem(item, itemSeparation, prevItemSeparationLargeGap, unreadBefore)
          recent.items.add(listItem)
          if (shouldShowAvatar(item, next)) {
            recent.showAvatar.add(item.id)
          }
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

          val prev = items.getOrNull(index - 1)
          if (revealed) {
            itemSeparation = getItemSeparation(item, prev)
            val nextForGap = if ((category != null && category == prev?.mergeCategory) || index + 1 == items.size) null else next
            prevItemSeparationLargeGap = if (nextForGap == null) false else getItemSeparationLargeGap(nextForGap, item)
          } else {
            itemSeparation = getItemSeparation(item, null)
            prevItemSeparationLargeGap = false
          }
          val listItem = ListItem(item, itemSeparation, prevItemSeparationLargeGap, unreadBefore)
          recent = if (item.mergeCategory != null) {
            if (item.mergeCategory != prev?.mergeCategory || lastRevealedIdsInMergedItems == null || lastRangeInReversedForMergedItems == null) {
              lastRangeInReversedForMergedItems = MutableStateFlow(index .. index)
              lastRevealedIdsInMergedItems = if (revealedItems.contains(item.id)) mutableListOf(item.id) else mutableListOf()
            } else {
              if (revealed) {
                lastRevealedIdsInMergedItems += item.id
              }
              lastRangeInReversedForMergedItems.value = lastRangeInReversedForMergedItems.value.first .. index
            }
            MergedItem.Grouped(
              items = arrayListOf(listItem),
              revealed = revealed,
              revealedIds = lastRevealedIdsInMergedItems,
              rangeInReversed = lastRangeInReversedForMergedItems,
              mergeCategory = item.mergeCategory,
              showAvatar = if (shouldShowAvatar(item, next)) {
                mutableSetOf(item.id)
              } else {
                mutableSetOf()
              },
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

  data class Single(
    val item: ListItem,
    override val startIndexInReversedItems: Int,
  ): MergedItem()

  @Stable
  data class Grouped (
    val items: ArrayList<ListItem>,
    val revealed: Boolean,
    val revealedIds: MutableList<Long>,
    val rangeInReversed: MutableStateFlow<IntRange>,
    val mergeCategory: CIMergeCategory?,
    val showAvatar: MutableSet<Long>, // ?
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
        while (i < revealedIds.size) {
          newRevealed.remove(revealedIds[i])
          i++
        }
        revealedIds.clear()
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
   * so [0, 1, 2, -100-, 101] if the 3 is a split, SplitRange(itemId = 100, indexRange = 3 .. 4) will be this SplitRange instance
   * (3, 4 indexes of the splitRange with the split itself at index 3)
   * */
  val indexRangeInReversed: IntRange,
  /** range of indexes inside LazyColumn (taking revealed/hidden items into account) where the first element is the split (it's index is [indexRangeInParentItems.first]) */
  val indexRangeInParentItems: IntRange
)

data class ListItem(
  val item: ChatItem,
  // nextItem
  // prevItem
  val separation: ItemSeparation, // remove
  val prevItemSeparationLargeGap: Boolean, // remove
  // how many unread items before (older than) this one (excluding this one)
  val unreadBefore: Int // ?
)

data class ActiveChatState (
  val splits: MutableStateFlow<List<Long>> = MutableStateFlow(emptyList()),
  val unreadAnchorItemId: MutableStateFlow<Long> = MutableStateFlow(-1L),
  // total items after unread anchor item (exclusive)
  val totalAfter: MutableStateFlow<Int> = MutableStateFlow(0),
  val unreadTotal: MutableStateFlow<Int> = MutableStateFlow(0),
  // exclusive
  val unreadAfter: MutableStateFlow<Int> = MutableStateFlow(0),
  // exclusive
  val unreadAfterNewestLoaded: MutableStateFlow<Int> = MutableStateFlow(0)
) {
  fun moveUnreadAnchor(toItemId: Long?, nonReversedItems: List<ChatItem>) {
    toItemId ?: return
    val currentIndex = nonReversedItems.indexOfFirst { it.id == unreadAnchorItemId.value }
    val newIndex = nonReversedItems.indexOfFirst { it.id == toItemId }
    if (currentIndex == -1 || newIndex == -1) return
    unreadAnchorItemId.value = toItemId
    val unreadDiff = if (newIndex > currentIndex) {
      -nonReversedItems.subList(currentIndex + 1, newIndex + 1).count { it.isRcvNew }
    } else {
      nonReversedItems.subList(newIndex + 1, currentIndex + 1).count { it.isRcvNew }
    }
    unreadAfter.value += unreadDiff
  }

  fun moveUnreadAnchor(fromIndex: Int, toIndex: Int, nonReversedItems: List<ChatItem>) {
    if (fromIndex == -1 || toIndex == -1) return
    unreadAnchorItemId.value = nonReversedItems[toIndex].id
    val unreadDiff = if (toIndex > fromIndex) {
      -nonReversedItems.subList(fromIndex + 1, toIndex + 1).count { it.isRcvNew }
    } else {
      nonReversedItems.subList(toIndex + 1, fromIndex + 1).count { it.isRcvNew }
    }
    unreadAfter.value += unreadDiff
  }

  fun clear() {
    splits.value = emptyList()
    unreadAnchorItemId.value = -1L
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
    val (_, unreadAnchorItemId, _, unreadTotal, unreadAfter) = chatState
    if (itemIds == null) {
      // special case when the whole chat became read
      unreadTotal.value = 0
      unreadAfter.value = 0
      return
    }
    var unreadAnchorIndex: Int = -1
    // since it's more often that the newest items become read, it's logical to loop from the end of the list to finish it faster
    var i = newItems.lastIndex
    val ids = itemIds.toMutableSet()
    // intermediate variables to prevent re-setting state value a lot of times without reason
    var newUnreadTotal = unreadTotal.value
    var newUnreadAfter = unreadAfter.value
    while (i >= 0) {
      val item = newItems[i]
      if (item.id == unreadAnchorItemId.value) {
        unreadAnchorIndex = i
      }
      if (ids.contains(item.id)) {
        // was unread, now this item is read
        if (unreadAnchorIndex == -1) {
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
    val (splits, unreadAnchorItemId, totalAfter, unreadTotal, unreadAfter) = chatState
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

    val index = itemIds.indexOfFirst { it.first == unreadAnchorItemId.value }
    // unread anchor item was removed
    if (index != -1) {
      var newUnreadAnchorItemId = newItems.getOrNull(itemIds[index].second - itemIds.count { it.second <= index })?.id
      val newUnreadAnchorWasNull = newUnreadAnchorItemId == null
      if (newUnreadAnchorItemId == null) {
        // everything on top (including unread anchor) were deleted, take top item as unread anchor id
        newUnreadAnchorItemId = newItems.firstOrNull()?.id
      }
      if (newUnreadAnchorItemId != null) {
        unreadAnchorItemId.value = newUnreadAnchorItemId
        totalAfter.value -= itemIds.count { it.second > index }
        unreadTotal.value -= itemIds.count { it.second <= index && it.third }
        unreadAfter.value -= itemIds.count { it.second > index && it.third }
        if (newUnreadAnchorWasNull) {
          // since the unread anchor was moved one item after initial position, adjust counters accordingly
          if (newItems.firstOrNull()?.isRcvNew == true) {
            unreadTotal.value++
            unreadAfter.value--
          }
        }
      } else {
        // all items were deleted, 0 items in chatItems
        unreadAnchorItemId.value = -1L
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

private fun getItemSeparation(chatItem: ChatItem, prevItem: ChatItem?): ItemSeparation {
  if (prevItem == null) {
    return ItemSeparation(timestamp = true, largeGap = true, date = null)
  }

  val sameMemberAndDirection = if (prevItem.chatDir is GroupRcv && chatItem.chatDir is GroupRcv) {
    chatItem.chatDir.groupMember.groupMemberId == prevItem.chatDir.groupMember.groupMemberId
  } else chatItem.chatDir.sent == prevItem.chatDir.sent
  val largeGap = !sameMemberAndDirection || (abs(prevItem.meta.createdAt.epochSeconds - chatItem.meta.createdAt.epochSeconds) >= 60)

  return ItemSeparation(
    timestamp = largeGap || prevItem.meta.timestampText != chatItem.meta.timestampText,
    largeGap = largeGap,
    date = if (getTimestampDateText(chatItem.meta.itemTs) == getTimestampDateText(prevItem.meta.itemTs)) null else prevItem.meta.itemTs
  )
}

private fun getItemSeparationLargeGap(chatItem: ChatItem, nextItem: ChatItem?): Boolean {
  if (nextItem == null) {
    return true
  }

  val sameMemberAndDirection = if (nextItem.chatDir is GroupRcv && chatItem.chatDir is GroupRcv) {
    chatItem.chatDir.groupMember.groupMemberId == nextItem.chatDir.groupMember.groupMemberId
  } else chatItem.chatDir.sent == nextItem.chatDir.sent
  return !sameMemberAndDirection || (abs(nextItem.meta.createdAt.epochSeconds - chatItem.meta.createdAt.epochSeconds) >= 60)
}

private fun shouldShowAvatar(current: ChatItem, older: ChatItem?) =
  current.chatDir is CIDirection.GroupRcv && (older == null || (older.chatDir !is CIDirection.GroupRcv || older.chatDir.groupMember.memberId != current.chatDir.groupMember.memberId))

@Composable
fun BoxScope.ShowChatState() {
  Box(Modifier.align(Alignment.Center).size(200.dp).background(Color.Black)) {
    val s = chatModel.chatState
    Text(
      "itemId ${s.unreadAnchorItemId.value} / ${chatModel.chatItems.value.firstOrNull { it.id == s.unreadAnchorItemId.value }?.text}, \nunreadAfter ${s.unreadAfter.value}, afterNewest ${s.unreadAfterNewestLoaded.value}",
      color = Color.White
    )
  }
}

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

  val (splits, unreadAnchorItemId, totalAfter, unreadTotal, unreadAfter, unreadAfterNewestLoaded) = chatState
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
          unreadAnchorItemId.value = chat.chatItems.last().id
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
      val newIds = mutableSetOf<Long>()
      var i = 0
      while (i < chat.chatItems.size) {
        newIds.add(chat.chatItems[i].id)
        i++
      }
      val wasSize = newItems.size
      val visibleItemIndexes = visibleItemIndexesNonReversed()
      val trimmedIds = mutableSetOf<Long>()
      var lastSplitIndexTrimmed = -1
      var allowedTrimming = true//splits.value.isNotEmpty()
      var index = 0
      /** keep the newest [TRIM_KEEP_COUNT] items (bottom area) and oldest [TRIM_KEEP_COUNT] items, trim others */
      val trimRange = visibleItemIndexes.last + TRIM_KEEP_COUNT .. newItems.size - TRIM_KEEP_COUNT
      val prevItemTrimRange = visibleItemIndexes.last + TRIM_KEEP_COUNT + 1 .. newItems.size - TRIM_KEEP_COUNT
      var oldUnreadSplitIndex: Int = -1
      var newUnreadSplitIndex: Int = -1
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
              splits.value = listOf(it.id) + splits.value
            } else {
              val newSplits = ArrayList(splits.value)
              newSplits[lastSplitIndexTrimmed] = it.id
              splits.value = newSplits
            }
          }
        }
        if (unreadAnchorItemId.value == it.id) {
          oldUnreadSplitIndex = index
        }
        index++
        (invisibleItemToTrim && prevItemWasTrimmed) || newIds.contains(it.id)
      }
      val insertAt = indexInCurrentItems - (wasSize - newItems.size) + trimmedIds.size
      newItems.addAll(insertAt, chat.chatItems)
      withContext(Dispatchers.Main) {
        chatModel.chatItems.replaceAll(newItems)
        // will remove any splits that now becomes obsolete because items were merged
        splits.value = splits.value.filterNot { split -> newIds.contains(split) || trimmedIds.contains(split) }
        chatState.moveUnreadAnchor(oldUnreadSplitIndex, newUnreadSplitIndex, oldItems)
      }
    }
    is ChatPagination.After -> {
      newItems.addAll(oldItems)
      val indexInCurrentItems: Int = oldItems.indexOfFirst { it.id == pagination.chatItemId }
      if (indexInCurrentItems == -1) return@coroutineScope

      var unreadInLoaded = 0
      val newIds = mutableSetOf<Long>()
      var i = 0
      while (i < chat.chatItems.size) {
        val item = chat.chatItems[i]
        newIds.add(item.id)
        if (item.isRcvNew) {
          unreadInLoaded++
        }
        i++
      }
      val indexInSplitRanges = splits.value.indexOf(pagination.chatItemId)
      // Currently, it should always load from split range. Code that did different things were removed but let's keep it for the future
      val loadingFromSplitRange = indexInSplitRanges != -1
      val splitsToMerge = if (loadingFromSplitRange && indexInSplitRanges + 1 <= splits.value.size) ArrayList(splits.value.subList(indexInSplitRanges + 1, splits.value.size)) else ArrayList()
      val splitsToRemove = ArrayList<Long>()
      var firstItemIdBelowAllSplits: Long? = null
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
      val indexToAdd = min(indexInCurrentItems + 1, newItems.size)
      val indexToAddIsLast = indexToAdd == newItems.size
      newItems.addAll(indexToAdd, chat.chatItems)
      withContext(Dispatchers.Main) {
        chatModel.chatItems.replaceAll(newItems)
        if (splitsToRemove.isNotEmpty()) {
          val newSplits = ArrayList(splits.value)
          newSplits.removeAll(splitsToRemove.toSet())
          splits.value = newSplits
        }
        if (firstItemIdBelowAllSplits != null) {
          // no splits anymore, all were merged with bottom items
          splits.value = emptyList()
        } else {
          val enlargedSplit = splits.value.indexOf(pagination.chatItemId)
          if (enlargedSplit != -1) {
            // move the split to the end of loaded items
            val newSplits = ArrayList(splits.value)
            newSplits[enlargedSplit] = chat.chatItems.last().id
            splits.value = newSplits
//            Log.d(TAG, "Enlarged split range ${splits.value}")
          }
        }
        chatState.moveUnreadAnchor(splits.value.firstOrNull() ?: newItems.last().id, newItems)
        // loading clear bottom area, updating number of unread items after the newest loaded item
        if (indexToAddIsLast) {
          unreadAfterNewestLoaded.value -= unreadInLoaded
        }
      }
    }
    is ChatPagination.Around -> {
      newItems.addAll(oldItems)
      val newIds = mutableSetOf<Long>()
      var i = 0
      while (i < chat.chatItems.size) {
        newIds.add(chat.chatItems[i].id)
        i++
      }
      newItems.removeAll { newIds.contains(it.id) }
      // currently, items will always be added on top, which is index 0
      newItems.addAll(0, chat.chatItems)
      withContext(Dispatchers.Main) {
        chatModel.chatItems.replaceAll(newItems)
        splits.value = listOf(chat.chatItems.last().id) + splits.value
        unreadAnchorItemId.value = chat.chatItems.last().id
        totalAfter.value = navInfo.afterTotal
        unreadTotal.value = chat.chatStats.unreadCount
        unreadAfter.value = navInfo.afterUnread
        // no need to set it, count will be wrong
        // unreadAfterNewestLoaded.value = navInfo.afterUnread
      }
    }
    is ChatPagination.Last -> {
      newItems.addAll(oldItems)
      val newIds = mutableSetOf<Long>()
      var i = 0
      while (i < chat.chatItems.size) {
        newIds.add(chat.chatItems[i].id)
        i++
      }
      newItems.removeAll { newIds.contains(it.id) }
      newItems.addAll(chat.chatItems)
      withContext(Dispatchers.Main) {
        chatModel.chatItems.replaceAll(newItems)
//        unreadAnchorItemId.value = newItems.last().id
//        totalAfter.value = 0
//        unreadTotal.value = chat.chatStats.unreadCount
//        unreadAfter.value = 0
        unreadAfterNewestLoaded.value = 0
      }
    }
  }
}
