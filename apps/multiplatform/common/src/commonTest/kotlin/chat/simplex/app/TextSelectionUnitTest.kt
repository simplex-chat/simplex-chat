package chat.simplex.app

import androidx.compose.runtime.mutableStateOf
import chat.simplex.common.model.*
import chat.simplex.common.views.chat.*
import kotlinx.datetime.Clock
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

class TextSelectionUnitTest {
  private fun text(id: Long, s: String): ChatItem = ChatItem.getSampleData(id, text = s)

  private fun manager(vararg newestFirst: ChatItem): Pair<SelectionManager, List<MergedItem>> {
    val items = newestFirst.mapIndexed { i, ci -> MergedItem.Single(ListItem(ci, null, null, 0), i) }
    val m = SelectionManager()
    m.mergedItemsState = mutableStateOf(MergedItems(items, emptyList(), emptyMap()))
    return m to items
  }

  private fun SelectionManager.select(startIndex: Int, startOffset: Int, endIndex: Int, endOffset: Int) {
    startSelection(startIndex, 0f, 0f)
    setAnchorOffset(startOffset)
    updateFocusIndex(endIndex)
    updateFocusOffset(endOffset)
  }

  private fun copied(m: SelectionManager, items: List<MergedItem>, revealed: Set<Long> = emptySet()) =
    m.getSelectedCopiedText(items, revealed, SimplexLinkMode.DESCRIPTION)

  private fun range(startIndex: Int, startOffset: Int, endIndex: Int, endOffset: Int) =
    SelectionRange(startIndex, startIndex.toLong(), startOffset, endIndex, endIndex.toLong(), endOffset)

  @Test
  fun noRangeCopiesNothing() {
    val (m, items) = manager(text(1, "newest"))
    assertEquals("", copied(m, items))
  }

  @Test
  fun dragDownAcrossItemsCopiesOldestFirstJoinedByNewline() {
    val (m, items) = manager(text(3, "newest"), text(2, "middle"), text(1, "oldest"))
    m.select(startIndex = 2, startOffset = 2, endIndex = 0, endOffset = 3)
    assertEquals("dest\nmiddle\nnew", copied(m, items))
  }

  @Test
  fun dragUpAcrossItemsCopiesTheSameText() {
    val (m, items) = manager(text(3, "newest"), text(2, "middle"), text(1, "oldest"))
    m.select(startIndex = 0, startOffset = 3, endIndex = 2, endOffset = 2)
    assertEquals("dest\nmiddle\nnew", copied(m, items))
  }

  @Test
  fun singleItemCopiesCharactersBetweenCursorsInEitherDirection() {
    val (m, items) = manager(text(3, "newest"), text(2, "middle"), text(1, "oldest"))
    m.select(startIndex = 1, startOffset = 4, endIndex = 1, endOffset = 1)
    assertEquals("idd", copied(m, items))
    m.select(startIndex = 1, startOffset = 1, endIndex = 1, endOffset = 4)
    assertEquals("idd", copied(m, items))
  }

  @Test
  fun singleItemWithEqualCursorsCopiesNothing() {
    val (m, items) = manager(text(1, "middle"))
    m.select(startIndex = 0, startOffset = 2, endIndex = 0, endOffset = 2)
    assertEquals("", copied(m, items))
  }

  @Test
  fun eventItemInRangeIsSkipped() {
    val event = ChatItem(
      CIDirection.DirectRcv(),
      CIMeta.getSample(2, Clock.System.now(), text = ""),
      CIContent.SndGroupFeature(GroupFeature.Voice, GroupPreference(GroupFeatureEnabled.ON), memberRole_ = null),
      reactions = emptyList()
    )
    val (m, items) = manager(text(3, "newest"), event, text(1, "oldest"))
    m.select(startIndex = 2, startOffset = 0, endIndex = 0, endOffset = 6)
    assertEquals("oldest\nnewest", copied(m, items))
  }

  @Test
  fun deletedItemIsCopiedOnlyWhenRevealed() {
    val deleted = ChatItem.getSampleData(2, text = "secret", itemDeleted = CIDeleted.Deleted(null))
    val (m, items) = manager(text(3, "newest"), deleted, text(1, "oldest"))
    m.select(startIndex = 2, startOffset = 0, endIndex = 0, endOffset = 6)
    assertEquals("oldest\nnewest", copied(m, items))
    assertEquals("oldest\nsecret\nnewest", copied(m, items, revealed = setOf(2L)))
  }

  @Test
  fun deletedContentItemIsSkippedEvenWhenRevealed() {
    val deleted = ChatItem.getDeletedContentSampleData(2)
    val (m, items) = manager(text(3, "newest"), deleted, text(1, "oldest"))
    m.select(startIndex = 2, startOffset = 0, endIndex = 0, endOffset = 6)
    assertEquals("oldest\nnewest", copied(m, items, revealed = setOf(2L)))
  }

  @Test
  fun linkWithShownTextCopiesFullUrlWhenPartlySelected() {
    val link = "https://example.com/page"
    val ci = text(1, "see $link").copy(formattedText = listOf(FormattedText("see "), FormattedText(link, Format.HyperLink("docs", link))))
    val (m, items) = manager(ci)
    m.select(startIndex = 0, startOffset = 5, endIndex = 0, endOffset = 6)
    assertEquals(link, copied(m, items))
    m.select(startIndex = 0, startOffset = 0, endIndex = 0, endOffset = 8)
    assertEquals("see $link", copied(m, items))
    m.select(startIndex = 0, startOffset = 1, endIndex = 0, endOffset = 3)
    assertEquals("ee", copied(m, items))
  }

  @Test
  fun clearSelectionCopiesNothing() {
    val (m, items) = manager(text(1, "newest"))
    m.select(startIndex = 0, startOffset = 0, endIndex = 0, endOffset = 3)
    m.clearSelection()
    assertEquals("", copied(m, items))
  }

  @Test
  fun selectedRangeOutsideSelectionIsNull() {
    assertNull(selectedRange(null, 0))
    assertNull(selectedRange(range(1, 0, 3, 2), 0))
    assertNull(selectedRange(range(1, 0, 3, 2), 4))
  }

  @Test
  fun selectedRangeSingleItem() {
    assertEquals(1..3, selectedRange(range(2, 1, 2, 4), 2))
    assertEquals(1..3, selectedRange(range(2, 4, 2, 1), 2))
    assertNull(selectedRange(range(2, 3, 2, 3), 2))
    assertNull(selectedRange(range(2, -1, 2, 3), 2))
    assertNull(selectedRange(range(2, 3, 2, -1), 2))
  }

  @Test
  fun selectedRangeDraggingDownFromOlderItem() {
    val r = range(3, 2, 1, 4)
    assertEquals(2 until Int.MAX_VALUE, selectedRange(r, 3))
    assertEquals(0 until Int.MAX_VALUE, selectedRange(r, 2))
    assertEquals(0 until 4, selectedRange(r, 1))
  }

  @Test
  fun selectedRangeDraggingUpFromNewerItem() {
    val r = range(1, 4, 3, 2)
    assertEquals(0 until 4, selectedRange(r, 1))
    assertEquals(0 until Int.MAX_VALUE, selectedRange(r, 2))
    assertEquals(2 until Int.MAX_VALUE, selectedRange(r, 3))
  }

  @Test
  fun selectedRangeMultiItemWithoutOffsetsAtEnds() {
    val r = range(3, -1, 1, -1)
    assertNull(selectedRange(r, 3))
    assertEquals(0 until Int.MAX_VALUE, selectedRange(r, 2))
    assertNull(selectedRange(r, 1))
  }
}
