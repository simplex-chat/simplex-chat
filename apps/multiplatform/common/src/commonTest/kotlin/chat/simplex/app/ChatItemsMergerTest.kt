package chat.simplex.app

import androidx.compose.runtime.mutableStateOf
import chat.simplex.common.model.*
import chat.simplex.common.views.chat.*
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.datetime.Clock
import kotlin.test.Test
import kotlin.test.assertEquals

class ChatItemsMergerTest {

  @Test
  fun testRecalculateSplitPositions() {
    val oldItems = listOf(ChatItem.getSampleData(0), ChatItem.getSampleData(123L), ChatItem.getSampleData(124L), ChatItem.getSampleData(125L))

    val splits1 = MutableStateFlow(listOf(123L))
    val chatState1 = ActiveChatState(splits = splits1)
    val removed1 = listOf(oldItems[1])
    val newItems1 = oldItems - removed1
    val recalc1 = recalculateChatStatePositions(chatState1)
    recalc1.removed(removed1.map { Triple(it.id, oldItems.indexOf(removed1[0]), it.isRcvNew) }, newItems1)
    assertEquals(1, splits1.value.size)
    assertEquals(124L, splits1.value.first())

    val splits2 = MutableStateFlow(listOf(123L))
    val chatState2 = ActiveChatState(splits = splits2)
    val removed2 = listOf(oldItems[1], oldItems[2])
    val newItems2 = oldItems - removed2
    val recalc2 = recalculateChatStatePositions(chatState2)
    recalc2.removed(removed2.mapIndexed { index, it -> Triple(it.id, oldItems.indexOf(removed2[index]), it.isRcvNew) }, newItems2)
    assertEquals(1, splits2.value.size)
    assertEquals(125L, splits2.value.first())

    val splits3 = MutableStateFlow(listOf(123L))
    val chatState3 = ActiveChatState(splits = splits3)
    val removed3 = listOf(oldItems[1], oldItems[2], oldItems[3])
    val newItems3 = oldItems - removed3
    val recalc3 = recalculateChatStatePositions(chatState3)
    recalc3.removed(removed3.mapIndexed { index, it -> Triple(it.id, oldItems.indexOf(removed3[index]), it.isRcvNew) }, newItems3)
    assertEquals(0, splits3.value.size)

    val splits4 = MutableStateFlow(listOf(123L))
    val chatState4 = ActiveChatState(splits = splits4)
    val recalc4 = recalculateChatStatePositions(chatState4)
    recalc4.cleared()
    assertEquals(0, splits4.value.size)
  }

  @Test
  fun testItemsMerging() {
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
    val merged1 = MergedItems.create(items, unreadCount, emptySet(), chatState)
    assertEquals(
      listOf(
        listOf(0, false,
          listOf(
            listOf(0, 100, CIMergeCategory.ChatFeature),
            listOf(1, 99, CIMergeCategory.ChatFeature)
          )
        ),
        listOf(2, false,
          listOf(
            listOf(0, 98, CIMergeCategory.RcvItemDeleted),
            listOf(1, 97, CIMergeCategory.RcvItemDeleted)
          )
        ),
        listOf(4, true,
          listOf(
            listOf(0, 96, null),
          )
        ),
        listOf(5, true,
          listOf(
            listOf(0, 95, null),
          )
        ),
        listOf(6, true,
          listOf(
            listOf(0, 94, null)
          )
        )
      ).toList().toString(),
      merged1.items.map {
        listOf(
          it.startIndexInReversedItems,
          if (it is MergedItem.Grouped) it.revealed else true,
          when (it) {
            is MergedItem.Grouped -> it.items.mapIndexed { index, listItem ->
              listOf(index, listItem.item.id, listItem.item.mergeCategory)
            }
            is MergedItem.Single -> listOf(listOf(0, it.item.item.id, it.item.item.mergeCategory))
          }
        )
      }.toString()
    )

    val merged2 = MergedItems.create(items, unreadCount, setOf(98L, 97L), chatState)
    assertEquals(
      listOf(
        listOf(0, false,
          listOf(
            listOf(0, 100, CIMergeCategory.ChatFeature),
            listOf(1, 99, CIMergeCategory.ChatFeature)
          )
        ),
        listOf(2, true,
          listOf(
            listOf(0, 98, CIMergeCategory.RcvItemDeleted),
          )
        ),
        listOf(3, true,
          listOf(
            listOf(0, 97, CIMergeCategory.RcvItemDeleted)
          )
        ),
        listOf(4, true,
          listOf(
            listOf(0, 96, null),
          )
        ),
        listOf(5, true,
          listOf(
            listOf(0, 95, null),
          )
        ),
        listOf(6, true,
          listOf(
            listOf(0, 94, null)
          )
        )
      ).toList().toString(),
      merged2.items.map {
        listOf(
          it.startIndexInReversedItems,
          if (it is MergedItem.Grouped) it.revealed else true,
          when (it) {
            is MergedItem.Grouped -> it.items.mapIndexed { index, listItem ->
              listOf(index, listItem.item.id, listItem.item.mergeCategory)
            }
            is MergedItem.Single -> listOf(listOf(0, it.item.item.id, it.item.item.mergeCategory))
          }
        )
      }.toString()
    )
  }
}
