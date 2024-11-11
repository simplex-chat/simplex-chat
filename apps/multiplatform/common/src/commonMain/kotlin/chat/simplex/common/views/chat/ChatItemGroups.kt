package chat.simplex.common.views.chat

import androidx.compose.runtime.*
import chat.simplex.common.model.*
import chat.simplex.common.views.chatlist.apiLoadMessages

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
    if (index > 0 && recent!!.mergeCategory == category && !itemIsAnchor) {
      recent.items.add(item)
      if (shouldShowAvatar(item, next)) {
        recent.showAvatar.add(item.id)
      }
      if (recent.revealed.value) {
        visibleItemIndexInParent++
      }
    } else {
      visibleItemIndexInParent++
      recent = SectionItems(
        mergeCategory = item.mergeCategory,
        items = arrayListOf(item),
        revealed = mutableStateOf(item.mergeCategory == null || revealedItems.contains(item.id)),
        showAvatar = if (shouldShowAvatar(item, next)) {
          mutableSetOf(item.id)
        } else {
          mutableSetOf()
        },
        startIndexInParentItems = visibleItemIndexInParent
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
    val index = group.items.indexOfFirst { it.id == itemId }
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

fun recalculateAnchorPositions(anchors: MutableState<List<Long>>) = object: ChatItemsChangesListener {
  override fun added(itemId: Long, index: Int) {}
  override fun removed(itemIds: List<Pair<Long, Int>>, newItems: List<ChatItem>) {
    val newAnchors = ArrayList<Long>()
    for (anchor in anchors.value) {
      val index = itemIds.indexOfFirst { it.first == anchor }
      // deleted the item that was right before the anchor between items, find newer item so it will act like the anchor
      if (index != -1) {
        val newAnchor = newItems.getOrNull(itemIds[index].second - itemIds.count { it.second <= index })?.id
        if (newAnchor != null) {
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

//object: ChatItemsChangesListener {
//  override fun added(itemId: Long) { manuallyAddedItems.value += itemId }
//  override fun removed(vararg itemId: Long) { manuallyAddedItems.value = manuallyAddedItems.value.subtract(itemId.toSet()) }
//  override fun cleared() { manuallyAddedItems.value = emptySet() }
//}

private fun shouldShowAvatar(current: ChatItem, older: ChatItem?) =
  current.chatDir is CIDirection.GroupRcv && (older == null || (older.chatDir !is CIDirection.GroupRcv || older.chatDir.groupMember.memberId != current.chatDir.groupMember.memberId))