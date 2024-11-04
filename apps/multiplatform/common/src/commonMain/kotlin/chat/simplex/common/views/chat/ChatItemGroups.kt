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
  println("LALAL LENGTH ${size}")
  if (isEmpty()) return emptyList()
  val start = System.currentTimeMillis()
  val groups = ArrayList<SectionItems>()
  val first = this[0]
  var recent = SectionItems(
    mergeCategory = first.mergeCategory,
    items = arrayListOf(first),
    revealed = mutableStateOf(first.mergeCategory == null || revealedItems.contains(first.id)),
    showAvatar = mutableSetOf<Long>().also { if (first.chatDir is CIDirection.GroupRcv) it.add(first.id) },
    startIndexInParentItems = 0
  )
  groups.add(recent)
  var prev = this[0]
  var index = 0
  while (index < size) {
    if (index == 0) {
      index++
      continue
    }
    val item = this[index]
    val category = item.mergeCategory
    if (recent.mergeCategory == category) {
      recent.items.add(item)
      if (item.chatDir is CIDirection.GroupRcv && prev.chatDir is CIDirection.GroupRcv && item.chatDir.groupMember == (prev.chatDir as CIDirection.GroupRcv).groupMember) {
        recent.showAvatar.add(item.id)
      }
    } else {
      recent = SectionItems(
        mergeCategory = item.mergeCategory,
        items = arrayListOf(item),
        revealed = mutableStateOf(item.mergeCategory == null || revealedItems.contains(item.id)),
        showAvatar = if (item.chatDir is CIDirection.GroupRcv && (prev.chatDir !is CIDirection.GroupRcv || (prev.chatDir as CIDirection.GroupRcv).groupMember != item.chatDir.groupMember)) {
          mutableSetOf(item.id)
        } else {
          mutableSetOf()
        },
        startIndexInParentItems = index
      )
      groups.add(recent)
    }
    prev = item
    index++
  }
  println("LALAL RES ${System.currentTimeMillis() - start}, groups: ${groups.size}")
  return groups
}
