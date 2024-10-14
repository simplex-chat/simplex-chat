package chat.simplex.common.views.chat

import androidx.compose.runtime.snapshots.SnapshotStateList
import chat.simplex.common.model.*

data class SectionItems (
  val mergeCategory: CIMergeCategory?,
  val items: SnapshotStateList<ChatItem>,
  val revealed: Boolean,
  val showAvatar: MutableSet<Long>,
)

fun List<ChatItem>.putIntoGroups(revealedItems: Set<Long>): List<SectionItems> {
  println("LALAL LENGTH ${size}")
  val start = System.currentTimeMillis()
  val groups = ArrayList<SectionItems>()
  var recent: SectionItems = if (isNotEmpty()) {
    val first = this[0]
    SectionItems(
      mergeCategory = first.mergeCategory,
      items = SnapshotStateList<ChatItem>().also { it.add(first) },
      revealed = first.mergeCategory == null || revealedItems.contains(first.id),
      showAvatar = mutableSetOf<Long>().also { if (first.chatDir is CIDirection.GroupRcv) it.add(first.id) })
  } else {
    return emptyList()
  }
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
        items = SnapshotStateList<ChatItem>().also { it.add(item) },
        revealed = item.mergeCategory == null || revealedItems.contains(item.id),
        showAvatar = mutableSetOf<Long>().also {
          if (item.chatDir is CIDirection.GroupRcv && (prev.chatDir !is CIDirection.GroupRcv || (prev.chatDir as CIDirection.GroupRcv).groupMember != item.chatDir.groupMember)) {
            it.add(item.id)
          }
        }
      )
      groups.add(recent)
    }
    prev = item
    index++
  }
  println("LALAL RES ${System.currentTimeMillis() - start}, groups: ${groups.size}")
  return groups
}
