package chat.simplex.app

import androidx.compose.runtime.mutableStateOf
import chat.simplex.common.model.ChatItem
import chat.simplex.common.model.size
import chat.simplex.common.views.chat.recalculateAnchorPositions
import kotlin.test.Test
import kotlin.test.assertEquals

class ChatItemGroupsTest {

  @Test
  fun testRecalculateAnchorPositions() {
    val oldItems = listOf(ChatItem.getSampleData(0), ChatItem.getSampleData(123L), ChatItem.getSampleData(124L), ChatItem.getSampleData(125L))

    val anchors1 = mutableStateOf(listOf(123L))
    val removed1 = listOf(oldItems[1])
    val newItems1 = oldItems - removed1
    val recalc1 = recalculateAnchorPositions(anchors1)
    recalc1.removed(removed1.map { it.id to oldItems.indexOf(removed1[0]) }, newItems1)
    assertEquals(1, anchors1.size)
    assertEquals(124L, anchors1.value.first())

    val anchors2 = mutableStateOf(listOf(123L))
    val removed2 = listOf(oldItems[1], oldItems[2])
    val newItems2 = oldItems - removed2
    val recalc2 = recalculateAnchorPositions(anchors2)
    recalc2.removed(removed2.mapIndexed { index, it -> it.id to oldItems.indexOf(removed2[index]) }, newItems2)
    assertEquals(1, anchors2.size)
    assertEquals(125L, anchors2.value.first())

    val anchors3 = mutableStateOf(listOf(123L))
    val removed3 = listOf(oldItems[1], oldItems[2], oldItems[3])
    val newItems3 = oldItems - removed3
    val recalc3 = recalculateAnchorPositions(anchors3)
    recalc3.removed(removed3.mapIndexed { index, it -> it.id to oldItems.indexOf(removed3[index]) }, newItems3)
    assertEquals(0, anchors3.size)

    val anchors4 = mutableStateOf(listOf(123L))
    val recalc4 = recalculateAnchorPositions(anchors4)
    recalc4.cleared()
    assertEquals(0, anchors4.size)
  }
}