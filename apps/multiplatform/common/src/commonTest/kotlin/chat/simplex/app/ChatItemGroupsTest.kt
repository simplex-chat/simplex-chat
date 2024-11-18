package chat.simplex.app

import androidx.compose.runtime.mutableStateOf
import chat.simplex.common.model.*
import chat.simplex.common.views.chat.*
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.datetime.Clock
import kotlin.test.Test
import kotlin.test.assertEquals

class ChatItemGroupsTest {

  @Test
  fun testRecalculateAnchorPositions() {
    val oldItems = listOf(ChatItem.getSampleData(0), ChatItem.getSampleData(123L), ChatItem.getSampleData(124L), ChatItem.getSampleData(125L))

    val anchors1 = MutableStateFlow(listOf(123L))
    val chatState1 = ActiveChatState(anchors = anchors1)
    val removed1 = listOf(oldItems[1])
    val newItems1 = oldItems - removed1
    val recalc1 = recalculateChatStatePositions(chatState1)
    recalc1.removed(removed1.map { Triple(it.id, oldItems.indexOf(removed1[0]), it.isRcvNew) }, newItems1)
    assertEquals(1, anchors1.value.size)
    assertEquals(124L, anchors1.value.first())

    val anchors2 = MutableStateFlow(listOf(123L))
    val chatState2 = ActiveChatState(anchors = anchors2)
    val removed2 = listOf(oldItems[1], oldItems[2])
    val newItems2 = oldItems - removed2
    val recalc2 = recalculateChatStatePositions(chatState2)
    recalc2.removed(removed2.mapIndexed { index, it -> Triple(it.id, oldItems.indexOf(removed2[index]), it.isRcvNew) }, newItems2)
    assertEquals(1, anchors2.value.size)
    assertEquals(125L, anchors2.value.first())

    val anchors3 = MutableStateFlow(listOf(123L))
    val chatState3 = ActiveChatState(anchors = anchors3)
    val removed3 = listOf(oldItems[1], oldItems[2], oldItems[3])
    val newItems3 = oldItems - removed3
    val recalc3 = recalculateChatStatePositions(chatState3)
    recalc3.removed(removed3.mapIndexed { index, it -> Triple(it.id, oldItems.indexOf(removed3[index]), it.isRcvNew) }, newItems3)
    assertEquals(0, anchors3.value.size)

    val anchors4 = MutableStateFlow(listOf(123L))
    val chatState4 = ActiveChatState(anchors = anchors4)
    val recalc4 = recalculateChatStatePositions(chatState4)
    recalc4.cleared()
    assertEquals(0, anchors4.value.size)
  }

  @Test
  fun testPutIntoGroups() {
    val items = listOf(
      ChatItem(CIDirection.DirectRcv(), CIMeta.getSample(100L, Clock.System.now(), text = ""), CIContent.SndGroupFeature(GroupFeature.Voice, GroupPreference(GroupFeatureEnabled.ON), memberRole_ = null), reactions = emptyList()),
      ChatItem(CIDirection.DirectRcv(), CIMeta.getSample(99L, Clock.System.now(), text = ""), CIContent.SndGroupFeature(GroupFeature.FullDelete, GroupPreference(GroupFeatureEnabled.ON), memberRole_ = null), reactions = emptyList()),
      ChatItem(CIDirection.DirectRcv(), CIMeta.getSample(98L, Clock.System.now(), text = "", itemDeleted = CIDeleted.Deleted(null)), CIContent.RcvDeleted(CIDeleteMode.cidmBroadcast), reactions = emptyList()),
      ChatItem(CIDirection.DirectRcv(), CIMeta.getSample(97L, Clock.System.now(), text = "", itemDeleted = CIDeleted.Deleted(null)), CIContent.RcvDeleted(CIDeleteMode.cidmBroadcast), reactions = emptyList()),
      ChatItem(CIDirection.DirectRcv(), CIMeta.getSample(96L, Clock.System.now(), text = ""), CIContent.RcvMsgContent(MsgContent.MCText("")), reactions = emptyList()),
      ChatItem(CIDirection.DirectRcv(), CIMeta.getSample(95L, Clock.System.now(), text = ""), CIContent.RcvMsgContent(MsgContent.MCText("")), reactions = emptyList()),
      ChatItem(CIDirection.DirectRcv(), CIMeta.getSample(94L, Clock.System.now(), text = ""), CIContent.RcvMsgContent(MsgContent.MCText("")), reactions = emptyList()),
    )

    val unreadCount = mutableStateOf(0)
    val chatState = ActiveChatState()
    val groups1 = Sections.create(items, unreadCount, emptySet(), chatState)
    assertEquals(
      listOf(
        listOf(0, 0, false,
          listOf(
            listOf(0, 100, CIMergeCategory.ChatFeature),
            listOf(1, 99, CIMergeCategory.ChatFeature)
          )
        ),
        listOf(1, 2, false,
          listOf(
            listOf(0, 98, CIMergeCategory.RcvItemDeleted),
            listOf(1, 97, CIMergeCategory.RcvItemDeleted)
          )
        ),
        listOf(2, 4, true,
          listOf(
            listOf(0, 96, null),
            listOf(1, 95, null),
            listOf(2, 94, null)
          )
        )
      ).toList().toString(),
      groups1.sections.map {
        listOf(
          it.startIndexInParentItems,
          it.startIndexInReversedItems,
          it.revealed.value,
          it.items.mapIndexed { index, listItem ->
            listOf(index, listItem.item.id, listItem.item.mergeCategory)
          }
        )
      }.toString()
    )

    val groups2 = Sections.create(items, unreadCount, setOf(98L, 97L), chatState)
    assertEquals(
      listOf(
        listOf(0, 0, false,
          listOf(
            listOf(0, 100, CIMergeCategory.ChatFeature),
            listOf(1, 99, CIMergeCategory.ChatFeature)
          )
        ),
        listOf(1, 2, true,
          listOf(
            listOf(0, 98, CIMergeCategory.RcvItemDeleted),
            listOf(1, 97, CIMergeCategory.RcvItemDeleted)
          )
        ),
        listOf(3, 4, true,
          listOf(
            listOf(0, 96, null),
            listOf(1, 95, null),
            listOf(2, 94, null)
          )
        )
      ).toList().toString(),
      groups2.sections.map {
        listOf(
          it.startIndexInParentItems,
          it.startIndexInReversedItems,
          it.revealed.value,
          it.items.mapIndexed { index, listItem ->
            listOf(index, listItem.item.id, listItem.item.mergeCategory)
          }
        )
      }.toString()
    )
  }
}
