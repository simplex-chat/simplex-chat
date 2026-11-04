package chat.simplex.app

import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.views.chat.ListItem
import chat.simplex.common.views.chat.MergedItem
import chat.simplex.common.views.chat.terminalRowsSeparated
import chat.simplex.common.views.chat.item.TERMINAL_LEVEL_STEP
import chat.simplex.common.views.chat.item.TERMINAL_NAME_REPEAT_SECONDS
import chat.simplex.common.views.chat.item.TERMINAL_PARAGRAPH_GAP
import chat.simplex.common.views.chat.item.isOneLine
import chat.simplex.common.views.chat.item.terminalCardTop
import chat.simplex.common.views.chat.item.terminalItemShowsSender
import chat.simplex.common.views.chat.item.terminalQuoteTop
import chat.simplex.common.views.chat.item.terminalReactionsGap
import chat.simplex.common.views.chat.item.terminalRowGap
import chat.simplex.common.views.chat.item.terminalTextTop
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

  private fun msgAt(id: Long, dir: CIDirection, ts: Instant, text: String = "hello") =
    ChatItem.getSampleData(id, dir, ts, text).let {
      if (dir.sent) it else it.copy(content = CIContent.RcvMsgContent(MsgContent.MCText(text)))
    }

  private fun msg(id: Long, dir: CIDirection, secondsAfterNoon: Long, text: String = "hello") =
    msgAt(id, dir, Instant.fromEpochSeconds(noon.epochSeconds + secondsAfterNoon), text)

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
    assertFalse(terminalItemShowsSender(msg(2, CIDirection.DirectRcv(), TERMINAL_NAME_REPEAT_SECONDS - 1L), older = msg(1, CIDirection.DirectRcv(), 0)))
    assertFalse(terminalItemShowsSender(msg(2, CIDirection.DirectRcv(), 0), older = msg(1, CIDirection.DirectRcv(), TERMINAL_NAME_REPEAT_SECONDS.toLong())))
  }

  @Test
  fun senderShownAgainAfterPause() {
    assertTrue(terminalItemShowsSender(msg(2, CIDirection.DirectRcv(), TERMINAL_NAME_REPEAT_SECONDS.toLong()), older = msg(1, CIDirection.DirectRcv(), 0)))
    assertTrue(terminalItemShowsSender(msg(2, CIDirection.GroupRcv(alice), 3600), older = msg(1, CIDirection.GroupRcv(alice), 0)))
    assertFalse(terminalItemShowsSender(featureEvent(2, 3600), older = featureEvent(1, 0)))
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

  private val file = CIFile(fileId = 1, fileName = "a.txt", fileSize = 1, fileStatus = CIFileStatus.SndComplete, fileProtocol = FileProtocol.XFTP)

  private val thumbsUp = listOf(CIReactionCount(MsgReaction.Emoji(MREmojiChar.ThumbsUp), userReacted = false, totalReacted = 1))

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
  fun rowGapZeroBetweenCloseOneLineMessages() {
    assertEquals(0.sp, gap(msg(2, CIDirection.DirectSnd(), 10), msg(1, CIDirection.DirectSnd(), 0)))
    assertEquals(0.sp, gap(msg(2, CIDirection.DirectRcv(), 10, "👍"), msg(1, CIDirection.DirectRcv(), 0)))
  }

  @Test
  fun rowGapBetweenCloseMessagesUnlessBothAreOneLine() {
    val older = msg(1, CIDirection.DirectSnd(), 0)
    val twoLines = msg(2, CIDirection.DirectSnd(), 10, "line one\nline two")
    assertEquals(TERMINAL_PARAGRAPH_GAP, gap(twoLines, older))
    assertEquals(TERMINAL_PARAGRAPH_GAP, gap(msg(3, CIDirection.DirectSnd(), 20), twoLines))
    assertEquals(TERMINAL_PARAGRAPH_GAP, gap(quoted(msg(2, CIDirection.DirectSnd(), 10)), older))
    assertEquals(TERMINAL_PARAGRAPH_GAP, gap(withBlocks(msg(2, CIDirection.DirectSnd(), 10), heading()), older))
  }

  @Test
  fun rowGapBelowReactions() {
    val older = msg(1, CIDirection.DirectSnd(), 0)
    assertEquals(TERMINAL_PARAGRAPH_GAP, gap(msg(2, CIDirection.DirectSnd(), 10), older.copy(reactions = thumbsUp)))
    assertEquals(0.sp, gap(msg(2, CIDirection.DirectSnd(), 10).copy(reactions = thumbsUp), older))
  }

  @Test
  fun oneLineMessages() {
    assertTrue(msg(1, CIDirection.DirectSnd(), 0).isOneLine)
    assertTrue(msg(1, CIDirection.DirectRcv(), 0, "👍").isOneLine)
    assertFalse(msg(1, CIDirection.DirectSnd(), 0, "hello\n").isOneLine)
    assertTrue(msg(1, CIDirection.DirectSnd(), 0).copy(reactions = thumbsUp).isOneLine)
    assertFalse(quoted(msg(1, CIDirection.DirectSnd(), 0)).isOneLine)
    assertFalse(forwarded(msg(1, CIDirection.DirectSnd(), 0)).isOneLine)
    assertFalse(withBlocks(msg(1, CIDirection.DirectSnd(), 0), heading()).isOneLine)
    val base = msg(1, CIDirection.DirectSnd(), 0)
    assertFalse(base.copy(content = CIContent.SndMsgContent(MsgContent.MCImage("hello", ""))).isOneLine)
    assertFalse(base.copy(content = CIContent.SndMsgContent(MsgContent.MCUnknown(text = "hello", json = JsonPrimitive("unknown"))), file = file).isOneLine)
    assertFalse(featureEvent(1, 0).isOneLine)
  }

  @Test
  fun rowGapAfterLargeGapOrAboveSenderRow() {
    val older = msg(1, CIDirection.DirectSnd(), 0)
    assertEquals(TERMINAL_PARAGRAPH_GAP, gap(msg(2, CIDirection.DirectSnd(), 70), older, largeGap = true))
    assertEquals(TERMINAL_PARAGRAPH_GAP, gap(msg(2, CIDirection.DirectSnd(), 10), older, hasSenderRow = true))
  }

  @Test
  fun rowGapNextToEvent() {
    assertEquals(TERMINAL_PARAGRAPH_GAP, gap(featureEvent(2, 10), msg(1, CIDirection.DirectRcv(), 0)))
    assertEquals(TERMINAL_PARAGRAPH_GAP, gap(msg(2, CIDirection.DirectRcv(), 10), featureEvent(1, 0)))
    assertEquals(TERMINAL_PARAGRAPH_GAP, gap(featureEvent(2, 10), featureEvent(1, 0)))
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
    assertEquals(TERMINAL_PARAGRAPH_GAP, gap(item, msg(1, CIDirection.DirectSnd(), 0)))
    assertEquals(0.sp, terminalTextTop(item))
  }

  @Test
  fun headingBelowHeaderTakesHeadingGap() {
    val base = msg(2, CIDirection.DirectSnd(), 10)
    listOf(
      forwarded(base),
      base.copy(meta = base.meta.copy(itemDeleted = CIDeleted.Deleted(null))),
      base.copy(meta = base.meta.copy(itemLive = true))
    ).forEach {
      assertEquals(TERMINAL_LEVEL_STEP, terminalTextTop(withBlocks(it, heading())))
      assertEquals(0.sp, terminalTextTop(it))
    }
  }

  @Test
  fun textBelowCardTakesParagraphGap() {
    val base = msg(2, CIDirection.DirectSnd(), 10)
    listOf(
      quoted(base),
      base.copy(content = CIContent.SndMsgContent(MsgContent.MCUnknown(text = "# Notes", json = JsonPrimitive("unknown"))), file = file),
      base.copy(content = CIContent.SndMsgContent(MsgContent.MCImage("# Caption", ""))),
      forwarded(base).copy(content = CIContent.SndMsgContent(MsgContent.MCVoice("# Caption", 10)))
    ).forEach {
      assertEquals(TERMINAL_PARAGRAPH_GAP, terminalTextTop(withBlocks(it, heading())))
      assertEquals(TERMINAL_PARAGRAPH_GAP, terminalTextTop(it))
    }
  }

  @Test
  fun emptyLinesBelowCardReplaceParagraphGap() {
    val caption = msg(2, CIDirection.DirectSnd(), 10).copy(
      content = CIContent.SndMsgContent(MsgContent.MCImage("\n\nCaption", "")),
      formattedText = listOf(FormattedText("\n\n", Format.Markup(BlockToken.ParagraphBreak(2))), FormattedText("Caption")),
      markdownBlocks = listOf(MarkdownBlock.Paragraph(SegmentRange(1, 2)))
    )
    assertEquals(0.sp, terminalTextTop(caption))
    val base = msg(2, CIDirection.DirectSnd(), 10)
    assertEquals(0.sp, terminalTextTop(base.copy(content = CIContent.SndMsgContent(MsgContent.MCImage("\nCaption", "")))))
    assertEquals(0.sp, terminalTextTop(base.copy(content = CIContent.SndMsgContent(MsgContent.MCImage("\r\nCaption", "")))))
    assertEquals(TERMINAL_PARAGRAPH_GAP, terminalTextTop(quoted(base).copy(content = CIContent.SndMsgContent(MsgContent.MCReport("\nComment", ReportReason.Spam)))))
  }

  @Test
  fun cardStartingTextBelowHeaderTakesParagraphGap() {
    val code = withBlocks(msg(2, CIDirection.DirectSnd(), 10), MarkdownBlock.CodeBlock(content = SegmentRange(0, 1)))
    assertEquals(0.sp, terminalTextTop(code))
    assertEquals(TERMINAL_PARAGRAPH_GAP, terminalTextTop(forwarded(code)))
    assertEquals(TERMINAL_PARAGRAPH_GAP, terminalTextTop(code.copy(meta = code.meta.copy(itemLive = true))))
  }

  @Test
  fun headingInUnknownContentWithoutFileTakesRowGap() {
    val base = msg(2, CIDirection.DirectSnd(), 10)
    val item = withBlocks(base.copy(content = CIContent.SndMsgContent(MsgContent.MCUnknown(text = "# Notes", json = JsonPrimitive("unknown")))), heading())
    assertEquals(TERMINAL_PARAGRAPH_GAP, gap(item, msg(1, CIDirection.DirectSnd(), 0)))
    assertEquals(0.sp, terminalTextTop(item))
  }

  @Test
  fun noHeadingGapsWithoutLeadingHeading() {
    val headingSecond = withBlocks(msg(2, CIDirection.DirectSnd(), 10), MarkdownBlock.Paragraph(SegmentRange(0, 1)), heading())
    assertEquals(0.sp, terminalTextTop(headingSecond))
    assertEquals(0.sp, terminalTextTop(forwarded(headingSecond)))
  }

  @Test
  fun cardBelowQuoteOrHeaderTakesParagraphGap() {
    val base = msg(2, CIDirection.DirectSnd(), 10)
    assertEquals(0.sp, terminalCardTop(base))
    assertEquals(TERMINAL_PARAGRAPH_GAP, terminalCardTop(quoted(base)))
    assertEquals(TERMINAL_PARAGRAPH_GAP, terminalCardTop(forwarded(base)))
    assertEquals(0.sp, terminalQuoteTop(quoted(base)))
    assertEquals(TERMINAL_PARAGRAPH_GAP, terminalQuoteTop(quoted(base).let { it.copy(meta = it.meta.copy(itemLive = true)) }))
  }

  @Test
  fun lastCardTakesParagraphGapAboveReactions() {
    val base = msg(2, CIDirection.DirectSnd(), 10)
    assertEquals(0.sp, terminalReactionsGap(base))
    assertEquals(TERMINAL_PARAGRAPH_GAP, terminalReactionsGap(base.copy(reactions = thumbsUp)))
  }
}
