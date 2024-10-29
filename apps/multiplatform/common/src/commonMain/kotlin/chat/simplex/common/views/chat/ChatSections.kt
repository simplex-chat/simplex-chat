package chat.simplex.common.views.chat

import androidx.compose.runtime.snapshots.SnapshotStateList
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext

enum class ChatSectionArea {
  Bottom,
  Current,
  Destination
}

data class ChatSectionAreaBoundary (
  var minIndex: Int,
  var maxIndex: Int,
  val area: ChatSectionArea
)

data class ChatSection (
  val items: MutableList<SectionItems>,
  val area: ChatSectionArea,
  val boundary: ChatSectionAreaBoundary
)

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

fun List<ChatItem>.putIntoSections(revealedItems: Set<Long>): List<ChatSection> {
  val chatItemsSectionArea = chatModel.chatItemsSectionArea
  val sections = mutableListOf<ChatSection>()
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

  val area = chatItemsSectionArea[recent.items[0].id] ?: ChatSectionArea.Bottom

  sections.add(
    ChatSection(
      items = mutableListOf(recent),
      area = area,
      boundary = ChatSectionAreaBoundary(minIndex = 0, maxIndex = 0, area = area)
    )
  )

  var prev = this[0]
  var index = 0
  while (index < size) {
    if (index == 0) {
      index++
      continue
    }
    val item = this[index]
    val itemArea = chatItemsSectionArea[item.id] ?: ChatSectionArea.Bottom
    val existingSection = sections.find { it.area == itemArea }

    if (existingSection == null) {
      val newSection = SectionItems(
        mergeCategory = item.mergeCategory,
        items = SnapshotStateList<ChatItem>().also { it.add(item) },
        revealed = item.mergeCategory == null || revealedItems.contains(item.id),
        showAvatar = mutableSetOf<Long>().also {
          it.add(item.id)
        },
        itemPositions = mutableMapOf(item.id to index),
      )
      sections.add(
        ChatSection(items = mutableListOf(newSection), area = itemArea, boundary = ChatSectionAreaBoundary(minIndex = index, maxIndex = index, area = itemArea))
      )
    } else {
      recent = existingSection.items.last()
      val category = item.mergeCategory
      if (recent.mergeCategory == category) {
        if (item.chatDir is CIDirection.GroupRcv && prev.chatDir is CIDirection.GroupRcv && item.chatDir.groupMember != (prev.chatDir as CIDirection.GroupRcv).groupMember) {
          recent.showAvatar.add(item.id)
        }

        recent.items.add(item)
        recent.itemPositions[item.id] = index
      } else {
        val newSectionItems = SectionItems(
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
        existingSection.items.add(newSectionItems)
      }
      existingSection.boundary.maxIndex = index
    }
    prev = item
    index++
  }

  return sections
}

data class ChatSectionLoad (
  val position: Int,
  val sectionArea: ChatSectionArea
) {
  fun prepareItems(items: List<ChatItem>): List<ChatItem> {
    val chatItemsSectionArea = chatModel.chatItemsSectionArea
    val itemsToAdd = mutableListOf<ChatItem>()
    for (cItem in items) {
      val itemSectionArea = chatItemsSectionArea[cItem.id]
      if (itemSectionArea == null) {
        itemsToAdd.add(cItem)
      } else if (itemSectionArea != this.sectionArea) {
        val targetSection = when (itemSectionArea) {
          ChatSectionArea.Bottom -> ChatSectionArea.Bottom
          ChatSectionArea.Current -> if (this.sectionArea == ChatSectionArea.Bottom) ChatSectionArea.Bottom else ChatSectionArea.Current
          ChatSectionArea.Destination -> if (this.sectionArea == ChatSectionArea.Bottom) ChatSectionArea.Bottom else ChatSectionArea.Destination
        }

        chatItemsSectionArea.filter { it.value == itemSectionArea }
          .forEach { chatItemsSectionArea[it.key] = targetSection }
      }
    }
    chatItemsSectionArea.putAll(itemsToAdd.associate { it.id to sectionArea })

    return itemsToAdd
  }
}

suspend fun apiLoadMessagesAroundItem(chatInfo: ChatInfo, chatModel: ChatModel, aroundItemId: Long, rhId: Long?, chatSectionLoad: ChatSectionLoad) {
  val pagination = ChatPagination.Around(aroundItemId, ChatPagination.PRELOAD_COUNT * 2)
  val chat = chatModel.controller.apiGetChat(rhId, chatInfo.chatType, chatInfo.apiId, pagination) ?: return
  if (chatModel.chatId.value != chat.id) return
  withContext(Dispatchers.Main) {
    val itemsToAdd = chatSectionLoad.prepareItems(chat.chatItems)
    println("prepared items ${itemsToAdd.size}")
    if (itemsToAdd.isNotEmpty()) {
      chatModel.chatItems.addAll(chatSectionLoad.position, itemsToAdd)
    }
  }
}
