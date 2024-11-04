package chat.simplex.common.views.chat

import androidx.compose.runtime.toMutableStateList
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import kotlin.math.max

const val MAX_SECTION_SIZE = 500

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
  val boundary: ChatSectionAreaBoundary,
  val itemPositions: MutableMap<Long, Int>
)

data class SectionItems (
  val mergeCategory: CIMergeCategory?,
  val items: MutableList<ChatItem>,
  val revealed: Boolean,
  val showAvatar: MutableSet<Long>,
  var originalItemsRange: IntRange
)

data class ChatSectionLoader (
  val position: Int,
  val sectionArea: ChatSectionArea
) {
  fun prepareItems(items: List<ChatItem>): List<ChatItem> {
    val chatItemsSectionArea = chatModel.chatItemsSectionArea
    val itemsToAdd = mutableListOf<ChatItem>()
    val sectionsToMerge = mutableMapOf<ChatSectionArea, ChatSectionArea>()
    val itemsThatCouldRequireMerge = mutableListOf<ChatItem>()
    for (cItem in items) {
      val itemSectionArea = chatItemsSectionArea[cItem.id]
      if (itemSectionArea == null) {
        itemsToAdd.add(cItem)
        val targetSection = sectionsToMerge[sectionArea] ?: this.sectionArea
        chatItemsSectionArea[cItem.id] = targetSection

        if (targetSection == this.sectionArea) {
          itemsThatCouldRequireMerge.add(cItem)
        }
      } else if (itemSectionArea != this.sectionArea) {
        val (targetSection, sectionToDrop) = when (itemSectionArea) {
          ChatSectionArea.Bottom -> ChatSectionArea.Bottom to this.sectionArea
          ChatSectionArea.Current -> if (this.sectionArea == ChatSectionArea.Bottom) ChatSectionArea.Bottom to itemSectionArea else itemSectionArea to this.sectionArea
          ChatSectionArea.Destination -> if (this.sectionArea == ChatSectionArea.Bottom) ChatSectionArea.Bottom to itemSectionArea else itemSectionArea to this.sectionArea
        }

        if (targetSection != sectionToDrop) {
          sectionsToMerge[sectionToDrop] = targetSection
        }
      }
    }

    if (sectionsToMerge.isNotEmpty()) {
      chatModel.chatItems.value.forEach {
        val currentSection = chatItemsSectionArea[it.id]
        val newSection = sectionsToMerge[currentSection]
        if (newSection != null) {
          chatItemsSectionArea[it.id] = newSection
        }
      }

      itemsThatCouldRequireMerge.forEach {
        val targetSection = sectionsToMerge[sectionArea] ?: sectionArea
        chatItemsSectionArea[it.id] = targetSection
      }
    }

    return itemsToAdd
  }
}

fun ChatSection.getPreviousShownItem(sectionIndex: Int, itemIndex: Int): ChatItem? {
  val section = items.getOrNull(sectionIndex) ?: return null

  return if (section.mergeCategory == null) {
    section.items.getOrNull(itemIndex + 1) ?: items.getOrNull(sectionIndex + 1)?.items?.firstOrNull()
  } else {
    items.getOrNull(sectionIndex + 1)?.items?.firstOrNull()
  }
}

fun ChatSection.getNextShownItem(sectionIndex: Int, itemIndex: Int): ChatItem? {
  val section = items.getOrNull(sectionIndex) ?: return null

  return if (section.mergeCategory == null) {
    section.items.getOrNull(itemIndex - 1) ?: items.getOrNull(sectionIndex - 1)?.items?.lastOrNull()
  } else {
    items.getOrNull(sectionIndex - 1)?.items?.lastOrNull()
  }
}

fun List<ChatItem>.putIntoSections(revealedItems: Set<Long>): List<ChatSection> {
  if (isEmpty()) return emptyList()

  val chatItemsSectionArea = chatModel.chatItemsSectionArea
  val sections = mutableListOf<ChatSection>()
  val first = this[0]

  val showAvatar = mutableSetOf<Long>()
  if (first.chatDir is CIDirection.GroupRcv) {
    val second = getOrNull(1)
    if (second != null) {
      if (second.chatDir !is CIDirection.GroupRcv || second.chatDir.groupMember.memberId != first.chatDir.groupMember.memberId) {
        showAvatar.add(first.id)
      }
    } else {
      showAvatar.add(first.id)
    }
  }

  var recent = SectionItems(
    mergeCategory = first.mergeCategory,
    items = mutableListOf(first),
    revealed = first.mergeCategory == null || revealedItems.contains(first.id),
    showAvatar = showAvatar,
    originalItemsRange = 0..0
  )

  val area = chatItemsSectionArea[recent.items[0].id] ?: ChatSectionArea.Bottom

  sections.add(
    ChatSection(
      items = mutableListOf(recent),
      boundary = ChatSectionAreaBoundary(minIndex = 0, maxIndex = 0, area = area),
      itemPositions = mutableMapOf(recent.items[0].id to 0)
    )
  )

  var prev = this[0]
  var index = 0
  var positionInList = 0;
  while (index < size) {
    if (index == 0) {
      index++
      continue
    }
    val item = this[index]
    val itemArea = chatItemsSectionArea[item.id] ?: ChatSectionArea.Bottom
    val existingSection = sections.find { it.boundary.area == itemArea }

    if (existingSection == null) {
      positionInList++
      val newSection = SectionItems(
        mergeCategory = item.mergeCategory,
        items = mutableListOf(item),
        revealed = item.mergeCategory == null || revealedItems.contains(item.id),
        showAvatar = mutableSetOf(item.id),
        originalItemsRange = index..index
      )
      sections.add(
        ChatSection(
          items = mutableListOf(newSection),
          boundary = ChatSectionAreaBoundary(minIndex = index, maxIndex = index, area = itemArea),
          itemPositions = mutableMapOf(item.id to positionInList)
        )
      )
    } else {
      recent = existingSection.items.last()
      val category = item.mergeCategory
      if (recent.mergeCategory == category) {
        if (category == null || recent.revealed || revealedItems.contains(item.id)) {
          positionInList++
        }
        if (item.chatDir is CIDirection.GroupRcv && prev.chatDir is CIDirection.GroupRcv && item.chatDir.groupMember.memberId != (prev.chatDir as CIDirection.GroupRcv).groupMember.memberId) {
          recent.showAvatar.add(item.id)
        }
        recent.items.add(item)
        recent.originalItemsRange = recent.originalItemsRange.first..index
        existingSection.itemPositions[item.id] = positionInList
      } else {
        positionInList++
        val newSectionItems = SectionItems(
          mergeCategory = item.mergeCategory,
          items = mutableListOf(item),
          revealed = item.mergeCategory == null || revealedItems.contains(item.id),
          showAvatar = if (item.chatDir is CIDirection.GroupRcv && (prev.chatDir !is CIDirection.GroupRcv || (prev.chatDir as CIDirection.GroupRcv).groupMember.memberId != item.chatDir.groupMember.memberId)) {
            mutableSetOf(item.id)
          } else {
            mutableSetOf()
          },
          originalItemsRange = index..index
        )
        existingSection.itemPositions[item.id] = positionInList
        existingSection.items.add(newSectionItems)
      }
      existingSection.boundary.maxIndex = index
    }
    prev = item
    index++
  }

  return sections
}

fun List<ChatSection>.chatItemPosition(chatItemId: Long): Int? {
  for (section in this) {
    val position = section.itemPositions[chatItemId]
    if (position != null) {
      return position
    }
  }

  return null
}

fun List<ChatSection>.revealedItemCount(): Int {
  var count = 0
  for (section in this) {
    var i = 0;
    while (i < section.items.size) {
      val item = section.items[i]
      if (item.revealed) {
        count += item.items.size
      } else {
        count++
      }
      i++
    }
  }

  return count
}

fun List<ChatSection>.dropTemporarySections() {
  val bottomSection = this.find { it.boundary.area == ChatSectionArea.Bottom }
  if (bottomSection != null) {
    val itemsOutsideOfSection = chatModel.chatItems.value.lastIndex - bottomSection.boundary.maxIndex
    chatModel.chatItems.removeRange(fromIndex = 0, toIndex = itemsOutsideOfSection + bottomSection.excessItemCount())
    chatModel.chatItemsSectionArea.clear()
    chatModel.chatItems.value.associateTo(chatModel.chatItemsSectionArea) { it.id to ChatSectionArea.Bottom }
  }
}

private fun ChatSection.excessItemCount(): Int {
  return max(boundary.maxIndex - boundary.minIndex + 1 - MAX_SECTION_SIZE, 0)
}

fun landingSectionToArea(chatLandingSection: ChatLandingSection) = when (chatLandingSection) {
  ChatLandingSection.Latest -> ChatSectionArea.Bottom
  ChatLandingSection.Unread -> ChatSectionArea.Current
}

suspend fun apiLoadBottomSection(chatInfo: ChatInfo, rhId: Long?) {
  val chat = chatController.apiGetChat(rh = rhId, type = chatInfo.chatType, id = chatInfo.apiId)
  if (chatModel.chatId.value != chatInfo.id || chat == null) return
  withContext(Dispatchers.Main) {
    val updatedItems = chatModel.chatItems.value.toMutableStateList()
    var insertIndex = updatedItems.size
    var needsMerge = false

    for (cItem in chat.first.chatItems.asReversed()) {
      if (chatModel.chatItemsSectionArea[cItem.id] == null) {
        updatedItems.add(insertIndex, cItem)
        chatModel.chatItemsSectionArea[cItem.id] = ChatSectionArea.Bottom
      } else {
        needsMerge = true
        chatModel.chatItemsSectionArea[cItem.id] = ChatSectionArea.Bottom
        insertIndex = max(0, insertIndex - 1)
      }
    }

    if (needsMerge) {
      updatedItems.associateTo(chatModel.chatItemsSectionArea) { it.id to ChatSectionArea.Bottom }
    }

    chatModel.chatItems.replaceAll(updatedItems)
  }
}