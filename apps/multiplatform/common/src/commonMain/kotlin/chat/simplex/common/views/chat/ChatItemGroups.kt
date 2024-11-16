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

data class SectionGroups(
  val sections: List<SectionItems>,
  val anchoredRanges: List<AnchoredRange>
)

data class AnchoredRange(
  /** itemId that was the last item in received list (ordered from old to new items) loaded using [ChatPagination.Around], see [apiLoadMessages] */
  val itemId: Long,
  /** range of indexes inside reversedChatItems where the first element is the anchor (it's index is [indexRange.first])
   * so [0, 1, 2, -100-, 101] if the 3 is an anchor, AnchoredRange(itemId = 100, indexRange = 3 .. 4) will be this AnchoredRange instance (3, 4 indexes of the anchoredRange with the anchor itself at index 3)
   * */
  val indexRange: IntRange,
  /** range of indexes inside LazyColumn (taking revealed/hidden items into account) where the first element is the anchor (it's index is [indexRangeInParentItems.first]) */
  val indexRangeInParentItems: IntRange
)

data class ActiveChatState (
  val anchors: MutableStateFlow<List<Long>> = MutableStateFlow(emptyList()),
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
    anchors.value = emptyList()
    unreadAnchorItemId.value = -1L
    totalAfter.value = 0
    unreadTotal.value = 0
    unreadAfter.value = 0
    unreadAfterNewestLoaded.value = 0
  }
}

data class ListItem(
  val item: ChatItem,
  val separation: ItemSeparation,
  val prevItemSeparationLargeGap: Boolean,
  // how many unread items before (older than) this one (excluding this one)
  val unreadBefore: Int
)

data class SectionItems (
  val mergeCategory: CIMergeCategory?,
  val items: ArrayList<ListItem>,
  val revealed: MutableState<Boolean>,
  val showAvatar: MutableSet<Long>,
  /** min index of row in LazyColumn:
   - for unrevealed row, it will match listState's index
   - for revealed row, this number represents index in listState for the first item in [items].
  */
  val startIndexInParentItems: Int,
  val startIndexInReversedItems: Int,
  val unreadIds: MutableSet<Long>,
) {
  fun reveal(reveal: Boolean, revealedItems: MutableState<Set<Long>>) {
    val newRevealed = revealedItems.value.toMutableSet()
    var i = 0
    while (i < items.size) {
      val item = items[i]
      if (reveal) {
        newRevealed.add(item.item.id)
      } else {
        newRevealed.remove(item.item.id)
      }
      i++
    }
    revealedItems.value = newRevealed
    revealed.value = reveal
  }
}

fun List<ChatItem>.putIntoGroups(unreadCount: State<Int>, revealedItems: Set<Long>, chatState: ActiveChatState): SectionGroups {
  if (isEmpty()) return SectionGroups(emptyList(), emptyList())

  val unreadAnchorItemId = chatState.unreadAnchorItemId
  val itemAnchors = chatState.anchors.value
  val groups = ArrayList<SectionItems>()
  // Indexes of anchors here will be related to reversedChatItems, not chatModel.chatItems
  val anchoredRanges = ArrayList<AnchoredRange>()
  var index = 0
  var unclosedAnchorIndex: Int? = null
  var unclosedAnchorIndexInParent: Int? = null
  var unclosedAnchorItemId: Long? = null
  var visibleItemIndexInParent = -1
  var unreadBefore = unreadCount.value - chatState.unreadAfterNewestLoaded.value
  var recent: SectionItems? = null
  while (index < size) {
    val item = this[index]
    val next = getOrNull(index + 1)
    val category = item.mergeCategory
    val itemIsAnchor = itemAnchors.contains(item.id)

    val itemSeparation: ItemSeparation
    val prevItemSeparationLargeGap: Boolean
    if (item.id == unreadAnchorItemId.value) {
      unreadBefore = unreadCount.value - chatState.unreadAfter.value
    }
    if (item.isRcvNew) unreadBefore--

    if (index > 0 && recent!!.mergeCategory == category && !itemIsAnchor) {
      if (recent.revealed.value) {
        val prev = getOrNull(index - 1)
        itemSeparation = getItemSeparation(item, prev)
        val nextForGap = if ((category != null && category == prev?.mergeCategory) || index + 1 == size) null else next
        prevItemSeparationLargeGap = if (nextForGap == null) false else getItemSeparationLargeGap(nextForGap, item)

        visibleItemIndexInParent++
      } else {
        itemSeparation = getItemSeparation(item, null)
        prevItemSeparationLargeGap = false
      }
      val listItem = ListItem(item, itemSeparation, prevItemSeparationLargeGap, unreadBefore)

      recent.items.add(listItem)
      if (shouldShowAvatar(item, next)) {
        recent.showAvatar.add(item.id)
      }
      if (item.isRcvNew) {
        recent.unreadIds.add(item.id)
      }
    } else {
      val revealed = item.mergeCategory == null || revealedItems.contains(item.id)
      visibleItemIndexInParent++

      val prev = getOrNull(index - 1)
      itemSeparation = getItemSeparation(item, prev)
      if (revealed) {
        val nextForGap = if ((category != null && category == prev?.mergeCategory) || index + 1 == size) null else next
        prevItemSeparationLargeGap = if (nextForGap == null) false else getItemSeparationLargeGap(nextForGap, item)
      } else {
        prevItemSeparationLargeGap = false
      }
      val listItem = ListItem(item, itemSeparation, prevItemSeparationLargeGap, unreadBefore)
      recent = SectionItems(
        mergeCategory = item.mergeCategory,
        items = arrayListOf(listItem),
        revealed = mutableStateOf(revealed),
        showAvatar = if (shouldShowAvatar(item, next)) {
          mutableSetOf(item.id)
        } else {
          mutableSetOf()
        },
        startIndexInParentItems = visibleItemIndexInParent,
        startIndexInReversedItems = index,
        unreadIds = if (item.isRcvNew) mutableSetOf(item.id) else mutableSetOf()
      )
      groups.add(recent)
    }
    if (itemIsAnchor) {
      // found item that is considered as an anchor
      if (unclosedAnchorIndex != null && unclosedAnchorItemId != null && unclosedAnchorIndexInParent != null) {
        // it was at least second anchor in the list
        anchoredRanges.add(AnchoredRange(unclosedAnchorItemId, unclosedAnchorIndex until index, unclosedAnchorIndexInParent until visibleItemIndexInParent))
      }
      unclosedAnchorIndex = index
      unclosedAnchorIndexInParent = visibleItemIndexInParent
      unclosedAnchorItemId = item.id
    } else if (index + 1 == size && unclosedAnchorIndex != null && unclosedAnchorItemId != null && unclosedAnchorIndexInParent != null) {
      // just one anchor for the whole list, there will be no more, it's the end
      anchoredRanges.add(AnchoredRange(unclosedAnchorItemId, unclosedAnchorIndex .. index, unclosedAnchorIndexInParent .. visibleItemIndexInParent))
    }
    index++
  }
  return SectionGroups(groups, anchoredRanges)
}

fun List<SectionItems>.indexInParentItems(itemId: Long): Int {
  for (group in this) {
    val index = group.items.indexOfFirst { it.item.id == itemId }
    if (index != -1) {
      return group.startIndexInParentItems + if (group.revealed.value) index else 0
    }
  }
  return -1
}

fun List<SectionItems>.indexInReversedItems(itemId: Long): Int {
  for (group in this) {
    val index = group.items.indexOfFirst { it.item.id == itemId }
    if (index != -1) {
      return group.startIndexInReversedItems + if (group.revealed.value) index else 0
    }
  }
  return -1
}

fun List<SectionItems>.lastIndexInParentItems(): Int {
  val last = lastOrNull() ?: return -1
  return if (last.revealed.value) {
    last.startIndexInParentItems + last.items.lastIndex
  } else {
    last.startIndexInParentItems
  }
}

fun List<SectionItems>.newestItemInReversedOrNull(parentIndex: Int): ChatItem? {
  val group = lastOrNull { it.startIndexInParentItems <= parentIndex } ?: return null
  return if (group.revealed.value) {
    group.items.getOrNull(parentIndex - group.startIndexInParentItems)?.item
  } else {
    group.items.firstOrNull()?.item
  }
}

// returns index of newest item in reversedChatItems on that row by receiving index from LazyColumn
fun List<SectionItems>.newestItemIndexInReversedOrNull(parentIndex: Int): Int? {
  val group = lastOrNull { it.startIndexInParentItems <= parentIndex } ?: return null
  return if (group.revealed.value) {
    group.startIndexInReversedItems + (parentIndex - group.startIndexInParentItems)
  } else {
    group.startIndexInReversedItems
  }
}

fun List<SectionItems>.oldestListItemInReversedOrNull(parentIndex: Int): ListItem? {
  val group = lastOrNull { it.startIndexInParentItems <= parentIndex } ?: return null
  return if (group.revealed.value) {
    group.items.getOrNull(parentIndex - group.startIndexInParentItems)
  } else {
    group.items.lastOrNull()
  }
}

// returns index of oldest item in reversedChatItems on that row by receiving index from LazyColumn
fun List<SectionItems>.oldestItemIndexInReversedOrNull(parentIndex: Int): Int? {
  val group = lastOrNull { it.startIndexInParentItems <= parentIndex } ?: return null
  return if (group.revealed.value) {
    group.startIndexInReversedItems + (parentIndex - group.startIndexInParentItems)
  } else {
    group.startIndexInReversedItems + group.items.lastIndex
  }
}

/** Returns groups mapping for easy checking the structure */
fun List<SectionItems>.mappingToString(): String = map { g ->
  "\nstartIndexInParentItems ${g.startIndexInParentItems}, startIndexInReversedItems ${g.startIndexInReversedItems}, revealed ${g.revealed.value}, mergeCategory ${g.items[0].item.mergeCategory} ${g.items.mapIndexed { index, it ->
    "\nunreadBefore ${it.unreadBefore} ${if (g.revealed.value) Triple(g.startIndexInParentItems + index, g.startIndexInReversedItems + index, it.item.id) else Triple(g.startIndexInParentItems, g.startIndexInReversedItems, it.item.id)}"
  }}"
}.toString()

fun visibleItemIndexesNonReversed(groups: State<SectionGroups>, listState: LazyListState): IntRange {
  val zero = 0 .. 0
  if (listState.layoutInfo.totalItemsCount == 0) return zero
  val newest = groups.value.sections.newestItemIndexInReversedOrNull(listState.firstVisibleItemIndex)
  val oldest = groups.value.sections.oldestItemIndexInReversedOrNull(listState.layoutInfo.visibleItemsInfo.last().index)
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
    val (anchors, unreadAnchorItemId, totalAfter, unreadTotal, unreadAfter) = chatState
    val newAnchors = ArrayList<Long>()
    for (anchor in anchors.value) {
      val index = itemIds.indexOfFirst { it.first == anchor }
      // deleted the item that was right before the anchor between items, find newer item so it will act like the anchor
      if (index != -1) {
        val newAnchor = newItems.getOrNull(itemIds[index].second - itemIds.count { it.second <= index })?.id
        // it the  whole section is gone and anchors overlap, don't add it at all
        if (newAnchor != null && !newAnchors.contains(newAnchor)) {
          newAnchors.add(newAnchor)
        }
      } else {
        newAnchors.add(anchor)
      }
    }
    anchors.value = newAnchors

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

private fun getItemSeparation(chatItem: ChatItem, nextItem: ChatItem?): ItemSeparation {
  if (nextItem == null) {
    return ItemSeparation(timestamp = true, largeGap = true, date = null)
  }

  val sameMemberAndDirection = if (nextItem.chatDir is GroupRcv && chatItem.chatDir is GroupRcv) {
    chatItem.chatDir.groupMember.groupMemberId == nextItem.chatDir.groupMember.groupMemberId
  } else chatItem.chatDir.sent == nextItem.chatDir.sent
  val largeGap = !sameMemberAndDirection || (abs(nextItem.meta.createdAt.epochSeconds - chatItem.meta.createdAt.epochSeconds) >= 60)

  return ItemSeparation(
    timestamp = largeGap || nextItem.meta.timestampText != chatItem.meta.timestampText,
    largeGap = largeGap,
    date = if (getTimestampDateText(chatItem.meta.itemTs) == getTimestampDateText(nextItem.meta.itemTs)) null else nextItem.meta.itemTs
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

  val (anchors, unreadAnchorItemId, totalAfter, unreadTotal, unreadAfter, unreadAfterNewestLoaded) = chatState
  val oldItems = chatModel.chatItems.value
  val newItems = SnapshotStateList<ChatItem>()
  when (pagination) {
    is ChatPagination.Initial -> {
      val newAnchors = if (chat.chatItems.isNotEmpty() && navInfo.afterTotal > 0) listOf(chat.chatItems.last().id) else emptyList()
      withChats {
        if (chatModel.getChat(chat.id) == null) {
          addChat(chat)
        }
      }
      withContext(Dispatchers.Main) {
        chatModel.chatItemStatuses.clear()
        chatModel.chatItems.replaceAll(chat.chatItems)
        chatModel.chatId.value = chat.chatInfo.id
        anchors.value = newAnchors
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
      var lastAnchorIndexTrimmed = -1
      var allowedTrimming = true//anchors.value.isNotEmpty()
      var index = 0
      /** keep the newest [TRIM_KEEP_COUNT] items (bottom area) and oldest [TRIM_KEEP_COUNT] items, trim others */
      val trimRange = visibleItemIndexes.last + TRIM_KEEP_COUNT .. newItems.size - TRIM_KEEP_COUNT
      val prevItemTrimRange = visibleItemIndexes.last + TRIM_KEEP_COUNT + 1 .. newItems.size - TRIM_KEEP_COUNT
      var oldUnreadAnchorIndex: Int = -1
      var newUnreadAnchorIndex: Int = -1
      newItems.removeAll {
        val invisibleItemToTrim = trimRange.contains(index) && allowedTrimming
        val prevItemWasTrimmed = prevItemTrimRange.contains(index) && allowedTrimming
        // may disable it after clearing the whole anchored range
        if (anchors.value.isNotEmpty() && it.id == anchors.value.firstOrNull()) {
          // trim only in one anchored range
          allowedTrimming = false
        }
        val indexInAnchors = anchors.value.indexOf(it.id)
        if (indexInAnchors != -1) {
          lastAnchorIndexTrimmed = indexInAnchors
        }
        if (invisibleItemToTrim) {
          if (prevItemWasTrimmed) {
            trimmedIds.add(it.id)
          } else {
            newUnreadAnchorIndex = index
            // prev item is not supposed to be trimmed, so exclude current one from trimming and set an anchor here instead.
            // this allows to define anchoredRange of the oldest items and to start loading trimmed items when user scrolls in the opposite direction
            if (lastAnchorIndexTrimmed == -1) {
              anchors.value = listOf(it.id) + anchors.value
            } else {
              val newAnchors = ArrayList(anchors.value)
              newAnchors[lastAnchorIndexTrimmed] = it.id
              anchors.value = newAnchors
            }
          }
        }
        if (unreadAnchorItemId.value == it.id) {
          oldUnreadAnchorIndex = index
        }
        index++
        (invisibleItemToTrim && prevItemWasTrimmed) || newIds.contains(it.id)
      }
      val insertAt = indexInCurrentItems - (wasSize - newItems.size) + trimmedIds.size
      newItems.addAll(insertAt, chat.chatItems)
      withContext(Dispatchers.Main) {
        chatModel.chatItems.replaceAll(newItems)
        // will remove any anchors that now becomes obsolete because items were merged
        anchors.value = anchors.value.filterNot { anchor -> newIds.contains(anchor) || trimmedIds.contains(anchor) }
        chatState.moveUnreadAnchor(oldUnreadAnchorIndex, newUnreadAnchorIndex, oldItems)
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
      val indexInAnchoredRanges = anchors.value.indexOf(pagination.chatItemId)
      // LALAL isn't it always from anchoredRange?
      val loadingFromAnchoredRange = indexInAnchoredRanges != -1
      val anchorsToMerge = if (loadingFromAnchoredRange && indexInAnchoredRanges + 1 <= anchors.value.size) ArrayList(anchors.value.subList(indexInAnchoredRanges + 1, anchors.value.size)) else ArrayList()
      val anchorsToRemove = ArrayList<Long>()
      var firstItemIdBelowAllAnchors: Long? = null
      newItems.removeAll {
        val duplicate = newIds.contains(it.id)
        if (loadingFromAnchoredRange && duplicate) {
          if (anchorsToMerge.contains(it.id)) {
            anchorsToMerge.remove(it.id)
            anchorsToRemove.add(it.id)
          } else if (firstItemIdBelowAllAnchors == null && anchorsToMerge.isEmpty()) {
            // we passed all anchors and found duplicated item below all of them, which means no anchors anymore below the loaded items
            firstItemIdBelowAllAnchors = it.id
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
        if (anchorsToRemove.isNotEmpty()) {
          val newAnchors = ArrayList(anchors.value)
          newAnchors.removeAll(anchorsToRemove.toSet())
          anchors.value = newAnchors
        }
        if (firstItemIdBelowAllAnchors != null) {
          // no anchors anymore, all were merged with bottom items
          anchors.value = emptyList()
        } else {
          val enlargedAnchor = anchors.value.indexOf(pagination.chatItemId)
          if (enlargedAnchor != -1) {
            // move the anchor to the end of loaded items
            val newAnchors = ArrayList(anchors.value)
            newAnchors[enlargedAnchor] = chat.chatItems.last().id
            anchors.value = newAnchors
//            Log.d(TAG, "Enlarged anchored range ${anchors.value}")
          }
        }
        chatState.moveUnreadAnchor(anchors.value.firstOrNull() ?: newItems.last().id, newItems)
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
        anchors.value = listOf(chat.chatItems.last().id) + anchors.value
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
