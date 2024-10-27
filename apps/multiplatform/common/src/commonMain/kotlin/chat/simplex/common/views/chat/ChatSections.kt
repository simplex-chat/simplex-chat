package chat.simplex.common.views.chat

import androidx.compose.runtime.snapshots.SnapshotStateList
import chat.simplex.common.model.*

data class SectionItems (
  val mergeCategory: CIMergeCategory?,
  val items: SnapshotStateList<ChatItem>,
  val revealed: Boolean,
  val showAvatar: MutableSet<Long>,
  val itemPositions: MutableMap<Long, Int>
)

fun SectionItems.getPreviousChatItem(chatItem: ChatItem): ChatItem? {
  val itemIndex = items.indexOfFirst { it.id == chatItem.id }
  if (itemIndex == -1) return null
  return items.getOrNull(itemIndex + 1)
}

fun List<ChatItem>.putIntoSections(revealedItems: Set<Long>): List<SectionItems> {
  val sections = mutableListOf<SectionItems>()

  var recent: SectionItems = if (isNotEmpty()) {
    val first = this[0]

    SectionItems(
      mergeCategory = first.mergeCategory,
      items = SnapshotStateList<ChatItem>().also { it.add(first) },
      revealed = first.mergeCategory == null || revealedItems.contains(first.id),
      showAvatar = mutableSetOf<Long>().also {
        if (first.chatDir is CIDirection.GroupRcv) {
          val second = getOrNull(1)

          if (second != null) {
            if (second.chatDir !is CIDirection.GroupRcv || second.chatDir.groupMember.memberId != first.chatDir.groupMember.memberId) {
              it.add(first.id)
            }
          } else {
            it.add(first.id)
          }
        }
      },
      itemPositions = mutableMapOf(first.id to 0),
    )
  } else {
    return emptyList()
  }

  sections.add(recent)

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
      if (item.chatDir is CIDirection.GroupRcv && prev.chatDir is CIDirection.GroupRcv && item.chatDir.groupMember != (prev.chatDir as CIDirection.GroupRcv).groupMember) {
        recent.showAvatar.add(item.id)
      }

      recent.items.add(item)
      recent.itemPositions[item.id] = index
    } else {
      recent = SectionItems(
        mergeCategory = item.mergeCategory,
        items = SnapshotStateList<ChatItem>().also { it.add(item) },
        revealed = item.mergeCategory == null || revealedItems.contains(item.id),
        showAvatar = mutableSetOf<Long>().also {
          if (item.chatDir is CIDirection.GroupRcv && (prev.chatDir !is CIDirection.GroupRcv || (prev.chatDir as CIDirection.GroupRcv).groupMember != item.chatDir.groupMember)) {
            it.add(item.id)
          }
        },
        itemPositions = mutableMapOf(item.id to index),
      )
      sections.add(recent)
    }
    prev = item
    index++
  }

  return sections
}