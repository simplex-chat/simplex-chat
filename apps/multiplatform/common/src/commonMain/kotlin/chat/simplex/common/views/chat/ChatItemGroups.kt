package chat.simplex.common.views.chat

import androidx.compose.runtime.*
import chat.simplex.common.model.*
import chat.simplex.common.views.chatlist.apiLoadMessages

data class SectionGroups(
  val sections: List<SectionItems>,
  val gaps: List<Gap>
)

data class Gap(
  /** itemId that was the last item in received list (ordered from old to new items) loaded using [ChatPagination.Around], see [apiLoadMessages] */
  val itemId: Long,
  /** range of indexes inside reversedChatItems where the first element is the gap itself (it's index is [indexRange.first])
   * so [0, 1, 2, -100-, 101] if the 3 is a gap, Gap(itemId = 100, indexRange = 3 .. 4) will be this Gap instance (3, 4 indexes of the gap with the gap itself at index 3)
   * */
  val indexRange: IntRange,
  /** range of indexes inside LazyColumn (taking revealed/hidden items into account) where the first element is the gap itself (it's index is [indexRangeInParentItems.first]) */
  val indexRangeInParentItems: IntRange
)

data class SectionItems (
  val mergeCategory: CIMergeCategory?,
  val items: ArrayList<ChatItem>,
  val revealed: MutableState<Boolean>,
  val showAvatar: MutableSet<Long>,
  val startIndexInParentItems: Int,
) {
  fun reveal(reveal: Boolean, revealedItems: MutableState<Set<Long>>) {
    println("LALAL REVEAL $reveal  ${revealedItems.value}")
    val newRevealed = revealedItems.value.toMutableSet()
    var i = 0
    while (i < items.size) {
      val item = items[i]
      if (reveal) {
        newRevealed.add(item.id)
      } else {
        newRevealed.remove(item.id)
      }
      i++
    }
    revealedItems.value = newRevealed
    revealed.value = reveal
  }
}

fun List<ChatItem>.putIntoGroups(revealedItems: Set<Long>, itemGaps: List<Long>): SectionGroups {
  println("LALAL PUT ${revealedItems}")
  if (isEmpty()) return SectionGroups(emptyList(), emptyList())

  val groups = ArrayList<SectionItems>()
  // Indexes of gaps here will be related to reversedChatItems, not chatModel.chatItems
  val gaps = ArrayList<Gap>()
  var index = 0
  var unclosedGapIndex: Int? = null
  var unclosedGapIndexInParent: Int? = null
  var unclosedGapItemId: Long? = null
  var visibleItemIndexInParent = 0
  var recent: SectionItems? = null
  while (index < size) {
    val item = this[index]
    val next = getOrNull(index + 1)
    val category = item.mergeCategory
    if (index > 0 && recent!!.mergeCategory == category) {
      recent.items.add(item)
      if (shouldShowAvatar(item, next)) {
        recent.showAvatar.add(item.id)
      }
    } else {
      recent = SectionItems(
        mergeCategory = item.mergeCategory,
        items = arrayListOf(item),
        revealed = mutableStateOf(item.mergeCategory == null || revealedItems.contains(item.id)),
        showAvatar = if (shouldShowAvatar(item, next)) {
          mutableSetOf(item.id)
        } else {
          mutableSetOf()
        },
        startIndexInParentItems = index
      )
      groups.add(recent)
    }
    if (itemGaps.contains(item.id)) {
      // found item that is considered as a gap
      if (unclosedGapIndex != null && unclosedGapItemId != null && unclosedGapIndexInParent != null) {
        // it was at least second gap in the list
        gaps.add(Gap(unclosedGapItemId, unclosedGapIndex until index, unclosedGapIndexInParent until visibleItemIndexInParent))
      }
      unclosedGapIndex = index
      unclosedGapIndexInParent = visibleItemIndexInParent
      unclosedGapItemId = item.id
    } else if (index + 1 == size && unclosedGapIndex != null && unclosedGapItemId != null && unclosedGapIndexInParent != null) {
      // just one gap for the whole list, there will be no more, it's the end
      gaps.add(Gap(unclosedGapItemId, unclosedGapIndex .. index, unclosedGapIndexInParent .. visibleItemIndexInParent))
    }
    index++
    if (recent.revealed.value) {
      visibleItemIndexInParent++
    }
  }
  return SectionGroups(groups, gaps)
}

fun List<SectionItems>.indexInParentItems(itemId: Long): Int {
  var collapsedItemsCount = 0
  for (group in this) {
    val index = group.items.indexOfFirst { it.id == itemId }
    if (index != -1) {
      return group.startIndexInParentItems + index - collapsedItemsCount
    } else if (!group.revealed.value) {
      collapsedItemsCount += group.items.size - 1
    }
  }
  return -1
}

fun recalculateGapsPositions(gaps: MutableState<List<Long>>) = object: ChatItemsChangesListener {
  override fun added(itemId: Long, index: Int) {}
  override fun removed(itemIds: List<Pair<Long, Int>>, newItems: List<ChatItem>) {
    val newGaps = ArrayList<Long>()
    for (gap in gaps.value) {
      val index = itemIds.indexOfFirst { it.first == gap }
      // deleted the item that was right before the gap between items, find newer item so it will act like the gap
      if (index != -1) {
        val newGap = newItems.getOrNull(itemIds[index].second - itemIds.count { it.second <= index })?.id
        if (newGap != null) {
          newGaps.add(newGap)
        }
      } else {
        newGaps.add(gap)
      }
    }
    gaps.value = newGaps
  }
  override fun cleared() { gaps.value = emptyList() }
}

//object: ChatItemsChangesListener {
//  override fun added(itemId: Long) { manuallyAddedItems.value += itemId }
//  override fun removed(vararg itemId: Long) { manuallyAddedItems.value = manuallyAddedItems.value.subtract(itemId.toSet()) }
//  override fun cleared() { manuallyAddedItems.value = emptySet() }
//}

private fun shouldShowAvatar(current: ChatItem, older: ChatItem?) =
  current.chatDir is CIDirection.GroupRcv && (older == null || (older.chatDir !is CIDirection.GroupRcv || older.chatDir.groupMember.memberId != current.chatDir.groupMember.memberId))