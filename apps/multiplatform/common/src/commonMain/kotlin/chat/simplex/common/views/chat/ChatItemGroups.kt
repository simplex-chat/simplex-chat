package chat.simplex.common.views.chat

import androidx.compose.runtime.MutableState
import androidx.compose.runtime.mutableStateOf
import chat.simplex.common.model.*

data class SectionItems (
  val mergeCategory: CIMergeCategory?,
  val items: ArrayList<ChatItem>,
  val revealed: MutableState<Boolean>,
  val showAvatar: MutableSet<Long>,
  val startIndexInParentItems: Int,
) {
  fun reveal(reveal: Boolean, revealedItems: MutableState<Set<Long>>) {
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
fun List<ChatItem>.putIntoGroups(revealedItems: Set<Long>): List<SectionItems> {
  if (isEmpty()) return emptyList()

  val groups = ArrayList<SectionItems>()
  val first = this[0]
  var recent = SectionItems(
    mergeCategory = first.mergeCategory,
    items = arrayListOf(first),
    revealed = mutableStateOf(first.mergeCategory == null || revealedItems.contains(first.id)),
    showAvatar = if (shouldShowAvatar(first, getOrNull(1))) {
      mutableSetOf(first.id)
    } else {
      mutableSetOf()
    },
    startIndexInParentItems = 0
  )
  groups.add(recent)
  var index = 0
  while (index < size) {
    if (index == 0) {
      index++
      continue
    }
    val item = this[index]
    val next = getOrNull(index + 1)
    val category = item.mergeCategory
    if (recent.mergeCategory == category) {
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
    index++
  }
  return groups
}

private fun shouldShowAvatar(current: ChatItem, older: ChatItem?) =
  current.chatDir is CIDirection.GroupRcv && (older == null || (older.chatDir !is CIDirection.GroupRcv || older.chatDir.groupMember.memberId != current.chatDir.groupMember.memberId))