package chat.simplex.app

import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.views.chat.ListItem
import chat.simplex.common.views.chat.MergedItem
import chat.simplex.common.views.chat.terminalRowsSeparated
import chat.simplex.common.views.chat.item.TERMINAL_LEVEL_STEP
import chat.simplex.common.views.chat.item.terminalHeadingGap
import chat.simplex.common.views.chat.item.terminalItemShowsSender
import chat.simplex.common.views.chat.item.terminalRowGap
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.datetime.Instant
import kotlinx.datetime.LocalDate
import kotlinx.datetime.TimeZone
import kotlinx.datetime.atStartOfDayIn
import kotlinx.serialization.json.JsonPrimitive
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class TerminalItemViewTest {
  private val noon = Instant.parse("2026-10-12T12:00:00Z")
  private val alice = GroupMember.sampleData.copy(groupMemberId = 1, memberId = "alice")
  private val bob = GroupMember.sampleData.copy(groupMemberId = 2, memberId = "bob")

  private fun msgAt(id: Long, dir: CIDirection, ts: Instant) =
    ChatItem.getSampleData(id, dir, ts).let {
      if (dir.sent) it else it.copy(content = CIContent.RcvMsgContent(MsgContent.MCText("hello")))
    }

  private fun msg(id: Long, dir: CIDirection, secondsAfterNoon: Long) =
    msgAt(id, dir, Instant.fromEpochSeconds(noon.epochSeconds + secondsAfterNoon))

  private fun integrityError(id: Long, secondsAfterNoon: Long) =
    msg(id, CIDirection.DirectRcv(), secondsAfterNoon).copy(content = CIContent.RcvIntegrityError(MsgErrorType.MsgSkipped(1, 2)))

  private fun featureEvent(id: Long, secondsAfterNoon: Long) =
    ChatItem(
      CIDirection.DirectRcv(),
      CIMeta.getSample(id, Instant.fromEpochSeconds(noon.epochSeconds + secondsAfterNoon), ""),
      CIContent.RcvChatFeature(ChatFeature.Voice, FeatureEnabled(forUser = true, forContact = true)),
      reactions = emptyList()
    )

  @Test
  fun senderShownForOldestItem() {
    assertTrue(terminalItemShowsSender(msg(1, CIDirection.DirectRcv(), 0), older = null))
  }

  @Test
  fun senderHiddenForSameSender() {
    assertFalse(terminalItemShowsSender(msg(2, CIDirection.DirectSnd(), 30), older = msg(1, CIDirection.DirectSnd(), 0)))
    assertFalse(terminalItemShowsSender(msg(2, CIDirection.GroupRcv(alice), 59), older = msg(1, CIDirection.GroupRcv(alice), 0)))
    assertFalse(terminalItemShowsSender(msg(2, CIDirection.DirectRcv(), 3600), older = msg(1, CIDirection.DirectRcv(), 0)))
  }

  @Test
  fun senderShownForDifferentGroupMember() {
    assertTrue(terminalItemShowsSender(msg(2, CIDirection.GroupRcv(bob), 10), older = msg(1, CIDirection.GroupRcv(alice), 0)))
  }

  @Test
  fun senderShownWhenDirectionChanges() {
    assertTrue(terminalItemShowsSender(msg(2, CIDirection.DirectSnd(), 10), older = msg(1, CIDirection.DirectRcv(), 0)))
    assertTrue(terminalItemShowsSender(msg(2, CIDirection.GroupSnd(), 10), older = msg(1, CIDirection.GroupRcv(alice), 0)))
  }

  @Test
  fun senderShownAfterEventItem() {
    assertTrue(terminalItemShowsSender(msg(2, CIDirection.DirectRcv(), 10), older = featureEvent(1, 0)))
  }

  @Test
  fun senderHiddenOnDateChange() {
    val midnight = LocalDate(2026, 10, 13).atStartOfDayIn(TimeZone.currentSystemDefault())
    val older = msgAt(1, CIDirection.DirectRcv(), Instant.fromEpochSeconds(midnight.epochSeconds - 10))
    val item = msgAt(2, CIDirection.DirectRcv(), Instant.fromEpochSeconds(midnight.epochSeconds + 10))
    assertFalse(terminalItemShowsSender(item, older))
  }

  @Test
  fun senderShownWhenChannelPostFollowsMemberMessage() {
    assertTrue(terminalItemShowsSender(msg(2, CIDirection.GroupRcv(alice), 10), older = msg(1, CIDirection.ChannelRcv(), 0)))
    assertTrue(terminalItemShowsSender(msg(2, CIDirection.ChannelRcv(), 10), older = msg(1, CIDirection.GroupRcv(alice), 0)))
    assertFalse(terminalItemShowsSender(msg(2, CIDirection.ChannelRcv(), 10), older = msg(1, CIDirection.ChannelRcv(), 0)))
  }

  @Test
  fun senderShownWhenOwnerSwitchesBetweenChannelAndOwnName() {
    val asChannel = msg(1, CIDirection.GroupSnd(), 0).let { it.copy(meta = it.meta.copy(showGroupAsSender = true)) }
    assertTrue(terminalItemShowsSender(msg(2, CIDirection.GroupSnd(), 10), older = asChannel))
    val asChannelToo = msg(2, CIDirection.GroupSnd(), 10).let { it.copy(meta = it.meta.copy(showGroupAsSender = true)) }
    assertFalse(terminalItemShowsSender(asChannelToo, older = asChannel))
    assertFalse(terminalItemShowsSender(msg(2, CIDirection.ChannelRcv(), 10), older = asChannel))
  }

  @Test
  fun integrityErrorGroupedWithSender() {
    assertFalse(terminalItemShowsSender(integrityError(2, 10), older = msg(1, CIDirection.DirectRcv(), 0)))
    assertFalse(terminalItemShowsSender(msg(3, CIDirection.DirectRcv(), 20), older = integrityError(2, 10)))
  }

  @Test
  fun senderHiddenAfterDeletedMessageFromSameSender() {
    val deleted = msg(1, CIDirection.DirectRcv(), 0).let { it.copy(meta = it.meta.copy(itemDeleted = CIDeleted.Deleted(null))) }
    val legacyDeleted = msg(1, CIDirection.DirectRcv(), 0).copy(content = CIContent.RcvDeleted(CIDeleteMode.cidmBroadcast))
    assertFalse(terminalItemShowsSender(msg(2, CIDirection.DirectRcv(), 10), older = deleted))
    assertFalse(terminalItemShowsSender(msg(2, CIDirection.DirectRcv(), 10), older = legacyDeleted))
  }

  @Test
  fun senderGroupsLocalNotes() {
    assertTrue(terminalItemShowsSender(msg(1, CIDirection.LocalSnd(), 0), older = null))
    assertFalse(terminalItemShowsSender(msg(2, CIDirection.LocalSnd(), 10), older = msg(1, CIDirection.LocalSnd(), 0)))
  }

  @Test
  fun noSenderForEventItem() {
    assertFalse(terminalItemShowsSender(featureEvent(2, 10), older = msg(1, CIDirection.DirectRcv(), 0)))
    assertFalse(terminalItemShowsSender(featureEvent(1, 0), older = null))
  }

  private fun withBlocks(item: ChatItem, vararg blocks: MarkdownBlock) = item.copy(markdownBlocks = blocks.toList())

  private fun heading() = MarkdownBlock.Heading(1, SegmentRange(0, 1))

  private fun quoted(item: ChatItem) = item.copy(quotedItem = CIQuote(CIDirection.DirectRcv(), 1, sentAt = noon, content = MsgContent.MCText("question")))

  private fun forwarded(item: ChatItem) = item.copy(meta = item.meta.copy(itemForwarded = CIForwardedFrom.Unknown))

  private fun gap(item: ChatItem, older: ChatItem?, separated: Boolean = false, largeGap: Boolean = false, hasSenderRow: Boolean = false) =
    terminalRowGap(item, older, separated, largeGap, hasSenderRow)

  private fun single(item: ChatItem, prev: ChatItem?) = MergedItem.Single(ListItem(item, prev, null, 0), 0)

  private fun grouped(item: ChatItem, prev: ChatItem?, revealed: Boolean) =
    MergedItem.Grouped(arrayListOf(ListItem(item, prev, null, 0)), revealed, mutableListOf(), MutableStateFlow(0..0), item.mergeCategory, mutableSetOf(), 0)

  @Test
  fun rowGapZeroForOldestItem() {
    assertEquals(0.sp, gap(msg(1, CIDirection.DirectSnd(), 0), older = null, largeGap = true, hasSenderRow = true))
  }

  @Test
  fun rowGapZeroBetweenCloseMessages() {
    assertEquals(0.sp, gap(msg(2, CIDirection.DirectSnd(), 10), msg(1, CIDirection.DirectSnd(), 0)))
  }

  @Test
  fun rowGapAfterLargeGapOrAboveSenderRow() {
    val older = msg(1, CIDirection.DirectSnd(), 0)
    assertEquals(TERMINAL_LEVEL_STEP, gap(msg(2, CIDirection.DirectSnd(), 70), older, largeGap = true))
    assertEquals(TERMINAL_LEVEL_STEP, gap(msg(2, CIDirection.DirectSnd(), 10), older, hasSenderRow = true))
  }

  @Test
  fun rowGapNextToEvent() {
    assertEquals(TERMINAL_LEVEL_STEP, gap(featureEvent(2, 10), msg(1, CIDirection.DirectRcv(), 0)))
    assertEquals(TERMINAL_LEVEL_STEP, gap(msg(2, CIDirection.DirectRcv(), 10), featureEvent(1, 0)))
  }

  @Test
  fun rowGapZeroWhenRowsAreSeparated() {
    assertEquals(0.sp, gap(featureEvent(2, 10), msg(1, CIDirection.DirectRcv(), 0), separated = true, largeGap = true, hasSenderRow = true))
  }

  @Test
  fun rowsSeparatedByDateBannerOrRevealedRow() {
    val midnight = LocalDate(2026, 10, 13).atStartOfDayIn(TimeZone.currentSystemDefault())
    val older = msgAt(1, CIDirection.DirectRcv(), Instant.fromEpochSeconds(midnight.epochSeconds - 10))
    val nextDay = msgAt(2, CIDirection.DirectRcv(), Instant.fromEpochSeconds(midnight.epochSeconds + 10))
    assertFalse(terminalRowsSeparated(null))
    assertTrue(terminalRowsSeparated(single(older, nextDay)))
    assertFalse(terminalRowsSeparated(single(msg(1, CIDirection.DirectRcv(), 0), msg(2, CIDirection.DirectRcv(), 10))))
    val banner = ChatItem(CIDirection.DirectRcv(), CIMeta.getSample(1, Instant.fromEpochSeconds(midnight.epochSeconds + 5), ""), CIContent.ChatBanner, reactions = emptyList())
    assertTrue(terminalRowsSeparated(single(banner, nextDay)))
    val event = featureEvent(1, 0).copy(meta = featureEvent(1, 0).meta.copy(itemTs = older.meta.itemTs))
    assertFalse(terminalRowsSeparated(grouped(event, nextDay, revealed = false)))
    assertTrue(terminalRowsSeparated(grouped(event, nextDay, revealed = true)))
  }

  @Test
  fun headingAtItemTopTakesRowGap() {
    val item = withBlocks(msg(2, CIDirection.DirectSnd(), 10), heading())
    assertEquals(TERMINAL_LEVEL_STEP, gap(item, msg(1, CIDirection.DirectSnd(), 0)))
    assertEquals(0.sp, terminalHeadingGap(item))
  }

  @Test
  fun headingBelowQuoteOrHeaderTakesHeadingGap() {
    val older = msg(1, CIDirection.DirectSnd(), 0)
    listOf(quoted(msg(2, CIDirection.DirectSnd(), 10)), forwarded(msg(2, CIDirection.DirectSnd(), 10))).forEach {
      val item = withBlocks(it, heading())
      assertEquals(0.sp, gap(item, older))
      assertEquals(TERMINAL_LEVEL_STEP, terminalHeadingGap(item))
    }
  }

  @Test
  fun headingBelowDeletionLiveFileOrMediaTakesHeadingGap() {
    val older = msg(1, CIDirection.DirectSnd(), 0)
    val base = msg(2, CIDirection.DirectSnd(), 10)
    val file = CIFile(fileId = 1, fileName = "a.txt", fileSize = 1, fileStatus = CIFileStatus.SndComplete, fileProtocol = FileProtocol.XFTP)
    listOf(
      base.copy(meta = base.meta.copy(itemDeleted = CIDeleted.Deleted(null))),
      base.copy(meta = base.meta.copy(itemLive = true)),
      base.copy(content = CIContent.SndMsgContent(MsgContent.MCUnknown(text = "# Notes", json = JsonPrimitive("unknown"))), file = file),
      base.copy(content = CIContent.SndMsgContent(MsgContent.MCImage("# Caption", "")))
    ).forEach {
      val item = withBlocks(it, heading())
      assertEquals(0.sp, gap(item, older))
      assertEquals(TERMINAL_LEVEL_STEP, terminalHeadingGap(item))
    }
  }

  @Test
  fun headingInUnknownContentWithoutFileTakesRowGap() {
    val base = msg(2, CIDirection.DirectSnd(), 10)
    val item = withBlocks(base.copy(content = CIContent.SndMsgContent(MsgContent.MCUnknown(text = "# Notes", json = JsonPrimitive("unknown")))), heading())
    assertEquals(TERMINAL_LEVEL_STEP, gap(item, msg(1, CIDirection.DirectSnd(), 0)))
    assertEquals(0.sp, terminalHeadingGap(item))
  }

  @Test
  fun noHeadingGapsWithoutLeadingHeading() {
    val older = msg(1, CIDirection.DirectSnd(), 0)
    val headingSecond = withBlocks(msg(2, CIDirection.DirectSnd(), 10), MarkdownBlock.Paragraph(SegmentRange(0, 1)), heading())
    assertEquals(0.sp, gap(headingSecond, older))
    assertEquals(0.sp, terminalHeadingGap(headingSecond))
    assertEquals(0.sp, terminalHeadingGap(quoted(msg(2, CIDirection.DirectSnd(), 10))))
  }
}
