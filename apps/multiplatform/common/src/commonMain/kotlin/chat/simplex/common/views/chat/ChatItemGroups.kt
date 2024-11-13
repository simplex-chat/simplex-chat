package chat.simplex.common.views.chat

import androidx.compose.foundation.lazy.LazyListState
import androidx.compose.runtime.*
import androidx.compose.runtime.snapshots.SnapshotStateList
import chat.simplex.common.model.*
import chat.simplex.common.model.CIDirection.GroupRcv
import chat.simplex.common.model.ChatModel.withChats
import chat.simplex.common.platform.chatModel
import kotlinx.coroutines.*
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

data class ListItem(
  val item: ChatItem,
  val separation: ItemSeparation,
  val prevItemSeparationLargeGap: Boolean
)

data class SectionItems (
  val mergeCategory: CIMergeCategory?,
  val items: ArrayList<ListItem>,
  val revealed: MutableState<Boolean>,
  val showAvatar: MutableSet<Long>,
  val startIndexInParentItems: Int,
  val unreadIds: MutableSet<Long>,
) {
  fun reveal(reveal: Boolean, revealedItems: MutableState<Set<Long>>) {
    println("LALAL REVEAL $reveal  ${revealedItems.value}")
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

fun List<ChatItem>.putIntoGroups(revealedItems: Set<Long>, itemAnchors: List<Long>): SectionGroups {
  println("LALAL PUT ${revealedItems}")
  if (isEmpty()) return SectionGroups(emptyList(), emptyList())

  val groups = ArrayList<SectionItems>()
  // Indexes of anchors here will be related to reversedChatItems, not chatModel.chatItems
  val anchoredRanges = ArrayList<AnchoredRange>()
  var index = 0
  var unclosedAnchorIndex: Int? = null
  var unclosedAnchorIndexInParent: Int? = null
  var unclosedAnchorItemId: Long? = null
  var visibleItemIndexInParent = -1
  var recent: SectionItems? = null
  while (index < size) {
    val item = this[index]
    val next = getOrNull(index + 1)
    val category = item.mergeCategory
    val itemIsAnchor = itemAnchors.contains(item.id)

    val itemSeparation: ItemSeparation
    val prevItemSeparationLargeGap: Boolean
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
      val listItem = ListItem(item, itemSeparation, prevItemSeparationLargeGap)

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

      if (revealed) {
        val prev = getOrNull(index - 1)
        itemSeparation = getItemSeparation(item, prev)
        val nextForGap = if ((category != null && category == prev?.mergeCategory) || index + 1 == size) null else next
        prevItemSeparationLargeGap = if (nextForGap == null) false else getItemSeparationLargeGap(nextForGap, item)
      } else {
        itemSeparation = getItemSeparation(item, null)
        prevItemSeparationLargeGap = false
      }
      val listItem = ListItem(item, itemSeparation, prevItemSeparationLargeGap)
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
//  println("LALAL GROUPS ${groups.map { it.startIndexInParentItems to it.items.map { it.id to it.text } }}")
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

fun List<SectionItems>.lastIndexInParentItems(): Int {
  val last = lastOrNull() ?: return -1
  return if (last.revealed.value) {
    last.startIndexInParentItems + last.items.lastIndex
  } else {
    last.startIndexInParentItems
  }
}

fun List<SectionItems>.newestItemAtParentIndexOrNull(parentIndex: Int): ChatItem? {
  println("LALAL PARENTINDEX $parentIndex")
  for (group in this) {
    val range = group.startIndexInParentItems..group.startIndexInParentItems + group.items.lastIndex
    if (range.contains(parentIndex)) {
      return (if (group.revealed.value) {
        group.items[parentIndex - group.startIndexInParentItems].item
      } else {
        group.items.first().item
      }).also { println("LALAL PARENTINDEX $parentIndex  ${it.meta.createdAt}") }
    }
  }
  return null
}

// returns index of newest item in reversedChatItems on that row by receiving index from LazyColumn
fun List<SectionItems>.newestItemIndexAtParentIndexOrNull(parentIndex: Int): Int? {
  for (group in this) {
    val range = group.startIndexInParentItems..group.startIndexInParentItems + group.items.lastIndex
    if (range.contains(parentIndex)) {
      return if (group.revealed.value) {
        parentIndex
      } else {
        group.startIndexInParentItems
      }
    }
  }
  return null
}

// returns index of oldest item in reversedChatItems on that row by receiving index from LazyColumn
fun List<SectionItems>.oldestItemIndexAtParentIndexOrNull(parentIndex: Int): Int? {
  for (group in this) {
    val range = group.startIndexInParentItems..group.startIndexInParentItems + group.items.lastIndex
    if (range.contains(parentIndex)) {
      return if (group.revealed.value) {
        parentIndex
      } else {
        group.startIndexInParentItems + group.items.lastIndex
      }
    }
  }
  return null
}

/** Returns groups mapping for easy checking the structure */
fun List<SectionItems>.mappingToString(): String = map { g ->
  "\nstartIndexInParentItems ${g.startIndexInParentItems}, revealed ${g.revealed.value}, mergeCategory ${g.items[0].item.mergeCategory} ${g.items.mapIndexed { index, it -> 
    if (g.revealed.value) g.startIndexInParentItems + index to it.item.id else g.startIndexInParentItems to it.item.id }
  }"
}.toString()

fun visibleItemIndexesNonReversed(groups: State<SectionGroups>, listState: LazyListState): IntRange {
  val zero = 0 .. 0
  if (listState.layoutInfo.totalItemsCount == 0) return zero
  val newest = groups.value.sections.newestItemIndexAtParentIndexOrNull(listState.firstVisibleItemIndex)
  val oldest = groups.value.sections.oldestItemIndexAtParentIndexOrNull(listState.layoutInfo.visibleItemsInfo.last().index)
  if (newest == null || oldest == null) return zero
  val size = chatModel.chatItems.value.size
  val range = size - oldest .. size - newest
  if (range.first < 0 || range.last < 0) return zero

  // visible items mapped to their underlying data structure which is chatModel.chatItems
  return range
}

fun recalculateAnchorPositions(anchors: MutableState<List<Long>>) = object: ChatItemsChangesListener {
  override fun added(itemId: Long, index: Int) {}
  override fun removed(itemIds: List<Pair<Long, Int>>, newItems: List<ChatItem>) {
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
  }
  override fun cleared() { anchors.value = emptyList() }
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

suspend fun apiLoadMessages(
  rhId: Long?,
  chatType: ChatType,
  apiId: Long,
  pagination: ChatPagination,
  search: String,
  anchors: MutableState<List<Long>>,
  visibleItemIndexesNonReversed: () -> IntRange
) = coroutineScope {
  val (chat, navInfo) = chatModel.controller.apiGetChat(rhId, chatType, apiId, pagination, search) ?: return@coroutineScope
  // For .initial allow the chatItems to be empty as well as chatModel.chatId to not match this chat because these values become set after .initial finishes
  if (((chatModel.chatId.value != chat.id || chat.chatItems.isEmpty()) && pagination !is ChatPagination.Initial && pagination !is ChatPagination.Last)
    || !isActive) return@coroutineScope

  val oldItems = chatModel.chatItems.value
  val newItems = SnapshotStateList<ChatItem>()
  when (pagination) {
    is ChatPagination.Initial -> {
      val newAnchors = if (chat.chatItems.isNotEmpty() && navInfo.belowTotal > 0) listOf(chat.chatItems.last().id) else emptyList()
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
      var index = 0
      newItems.removeAll {
        // keep the newest 200 items (bottom area) and oldest 200 items, trim others
        val invisibleItemToTrim = index > 200 && newItems.size - index > 200 && visibleItemIndexes.last < index
        val prevItemWasTrimmed = index - 1 > 200 && newItems.size - index + 1 > 200 && visibleItemIndexes.last < index - 1
        val indexInAnchors = anchors.value.indexOf(it.id)
        if (indexInAnchors != -1) {
          lastAnchorIndexTrimmed = indexInAnchors
        }
        if (invisibleItemToTrim) {
          if (prevItemWasTrimmed) {
            trimmedIds.add(it.id)
          } else {
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
        index++
        (invisibleItemToTrim && prevItemWasTrimmed) || newIds.contains(it.id)
      }
      println("LALAL TRIMMED ITEMS ${trimmedIds}")
      val insertAt = indexInCurrentItems - (wasSize - newItems.size) + trimmedIds.size
      newItems.addAll(insertAt, chat.chatItems)
      println("LALAL TRIMMED LEFT ${newItems.map { it.id }}")
      withContext(Dispatchers.Main) {
        chatModel.chatItems.replaceAll(newItems)
        println("LALAL GAPS2 ${anchors.value}  ${newIds} insertAt $insertAt indexInCurrentItems $indexInCurrentItems wasSize $wasSize")
        // will remove any anchors that now becomes obsolete because items were merged
        anchors.value = anchors.value.filterNot { anchor -> (newIds.contains(anchor) || trimmedIds.contains(anchor)).also { if (it) println("LALAL GAP WAS REMOVED $anchor") } }
      }
    }
    is ChatPagination.After -> {
      newItems.addAll(oldItems)
      val indexInCurrentItems: Int = oldItems.indexOfFirst { it.id == pagination.chatItemId }
      if (indexInCurrentItems == -1) return@coroutineScope
      val newIds = mutableSetOf<Long>()
      var i = 0
      while (i < chat.chatItems.size) {
        newIds.add(chat.chatItems[i].id)
        i++
      }
      val indexInAnchoredRanges = anchors.value.indexOf(pagination.chatItemId)
      val loadingFromAnchoredRange = indexInAnchoredRanges != -1
      val anchorsToMerge = if (loadingFromAnchoredRange && indexInAnchoredRanges + 1 <= anchors.size) ArrayList(anchors.value.subList(indexInAnchoredRanges + 1, anchors.size)) else ArrayList()
      println("LALAL GAPS TO MERGE $anchorsToMerge")
      println("LALAL ALL GAPS ${anchors.value}")
      val anchorsToRemove = ArrayList<Long>()
      var firstItemIdBelowAllAnchors: Long? = null
      newItems.removeAll {
        val duplicate = newIds.contains(it.id)
        if (loadingFromAnchoredRange && duplicate) {
          println("LALAL duplicate ${it.id}  $newIds")
          if (anchorsToMerge.contains(it.id)) {
            anchorsToMerge.remove(it.id)
            anchorsToRemove.add(it.id)
          } else if (firstItemIdBelowAllAnchors == null && anchorsToMerge.isEmpty()) {
            // we passed all anchors and found duplicated item below all of them, which means no anchors anymore below the loaded items
            println("LALAL FOUND firstItemIdBelowAllGaps ${it.id}")
            firstItemIdBelowAllAnchors = it.id
          }
        }
        duplicate
      }
      println("LALAL GAPS TO REMOVE $anchorsToRemove")
      newItems.addAll(min(indexInCurrentItems + 1, newItems.size), chat.chatItems)
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
            println("LALAL ENLARGED GAPS TO ${anchors.value}")
          }
        }
        println("LALAL GAPSss1 ${anchors.value}  ${newIds}")
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
      }
      println("LALAL GAPS0 ${anchors.value}  ${newIds}")
    }
    is ChatPagination.Last -> {
      withContext(Dispatchers.Main) {
        chatModel.chatItems.replaceAll(chat.chatItems)
        anchors.value = emptyList()
      }
    }
  }
}
