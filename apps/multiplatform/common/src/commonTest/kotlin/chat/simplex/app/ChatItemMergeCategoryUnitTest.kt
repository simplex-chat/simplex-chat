package chat.simplex.app

import androidx.compose.runtime.mutableStateOf
import chat.simplex.common.model.*
import chat.simplex.common.views.chat.*
import kotlinx.datetime.Clock
import kotlin.test.Test
import kotlin.test.assertEquals

class ChatItemMergeCategoryUnitTest {
  private fun item(content: CIContent, id: Long, dir: CIDirection): ChatItem =
    ChatItem(dir, CIMeta.getSample(id, Clock.System.now(), ""), content, reactions = emptyList())

  private fun rcvChatFeature(id: Long = 1) =
    item(CIContent.RcvChatFeature(ChatFeature.Voice, FeatureEnabled(forUser = true, forContact = true)), id, CIDirection.DirectRcv())

  private fun sndChatFeature(id: Long = 1) =
    item(CIContent.SndChatFeature(ChatFeature.Reactions, FeatureEnabled(forUser = false, forContact = false)), id, CIDirection.DirectSnd())

  private fun rcvEvent(event: RcvGroupEvent, id: Long) = item(CIContent.RcvGroupEventContent(event), id, CIDirection.GroupRcv(GroupMember.sampleData))

  @Test
  fun directChatFeatureItemsAreChatFeatureCategory() {
    assertEquals(CIMergeCategory.ChatFeature, rcvChatFeature().mergeCategory, "received")
    assertEquals(CIMergeCategory.ChatFeature, sndChatFeature().mergeCategory, "sent")
  }

  @Test
  fun featureRunsAreCollapsedAndSplitByOtherCategoriesAndMessages() {
    val items = listOf(
      rcvChatFeature(10),
      sndChatFeature(9),
      rcvEvent(RcvGroupEvent.MemberConnected(), 8),
      rcvEvent(RcvGroupEvent.MemberLeft(), 7),
      item(CIContent.RcvMsgContent(MsgContent.MCText("hello")), 6, CIDirection.DirectRcv()),
      rcvChatFeature(5),
    )
    val merged = MergedItems.create(items, mutableStateOf(0), emptySet(), ActiveChatState())
    assertEquals(
      listOf(
        listOf(0, false, listOf(10L, 9L), CIMergeCategory.ChatFeature),
        listOf(2, false, listOf(8L, 7L), CIMergeCategory.RcvGroupEvent),
        listOf(4, true, listOf(6L), null),
        listOf(5, false, listOf(5L), CIMergeCategory.ChatFeature),
      ),
      merged.items.map {
        when (it) {
          is MergedItem.Grouped -> listOf(it.startIndexInReversedItems, it.revealed, it.items.map { li -> li.item.id }, it.mergeCategory)
          is MergedItem.Single -> listOf(it.startIndexInReversedItems, true, listOf(it.item.item.id), it.item.item.mergeCategory)
        }
      }
    )
  }

  @Test
  fun revealedFeatureItemsAreShownOneRowEach() {
    val items = listOf(rcvChatFeature(3), rcvChatFeature(2), rcvChatFeature(1))
    val merged = MergedItems.create(items, mutableStateOf(0), setOf(3L, 2L, 1L), ActiveChatState())
    assertEquals(listOf(listOf(3L), listOf(2L), listOf(1L)), merged.items.map { (it as MergedItem.Grouped).items.map { li -> li.item.id } })
    assertEquals(listOf(true, true, true), merged.items.map { (it as MergedItem.Grouped).revealed })
  }
}
