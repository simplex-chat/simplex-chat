package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.views.chat.item.terminalHoverBarVisible
import chat.simplex.common.views.chat.item.terminalItemHasHeader
import chat.simplex.common.views.chat.item.terminalMetaVisible
import androidx.compose.ui.graphics.Color
import kotlinx.datetime.Instant
import kotlinx.datetime.LocalDate
import kotlinx.datetime.TimeZone
import kotlinx.datetime.atStartOfDayIn
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class TerminalItemViewTest {
  private val noon = Instant.parse("2026-10-12T12:00:00Z")
  private val alice = GroupMember.sampleData.copy(groupMemberId = 1, memberId = "alice")
  private val bob = GroupMember.sampleData.copy(groupMemberId = 2, memberId = "bob")

  private fun msg(id: Long, dir: CIDirection, secondsAfterNoon: Long) =
    ChatItem.getSampleData(id, dir, Instant.fromEpochSeconds(noon.epochSeconds + secondsAfterNoon))

  private fun featureEvent(id: Long, secondsAfterNoon: Long) =
    ChatItem(
      CIDirection.DirectRcv(),
      CIMeta.getSample(id, Instant.fromEpochSeconds(noon.epochSeconds + secondsAfterNoon), ""),
      CIContent.RcvChatFeature(ChatFeature.Voice, FeatureEnabled(forUser = true, forContact = true)),
      reactions = emptyList()
    )

  @Test
  fun headerShownForOldestItem() {
    assertTrue(terminalItemHasHeader(msg(1, CIDirection.DirectRcv(), 0), older = null))
  }

  @Test
  fun headerHiddenForSameSenderWithinMinute() {
    assertFalse(terminalItemHasHeader(msg(2, CIDirection.DirectSnd(), 30), older = msg(1, CIDirection.DirectSnd(), 0)))
    assertFalse(terminalItemHasHeader(msg(2, CIDirection.GroupRcv(alice), 59), older = msg(1, CIDirection.GroupRcv(alice), 0)))
  }

  @Test
  fun headerShownAfterMinuteGap() {
    assertTrue(terminalItemHasHeader(msg(2, CIDirection.DirectRcv(), 60), older = msg(1, CIDirection.DirectRcv(), 0)))
  }

  @Test
  fun headerShownForDifferentGroupMember() {
    assertTrue(terminalItemHasHeader(msg(2, CIDirection.GroupRcv(bob), 10), older = msg(1, CIDirection.GroupRcv(alice), 0)))
  }

  @Test
  fun headerShownWhenDirectionChanges() {
    assertTrue(terminalItemHasHeader(msg(2, CIDirection.DirectSnd(), 10), older = msg(1, CIDirection.DirectRcv(), 0)))
    assertTrue(terminalItemHasHeader(msg(2, CIDirection.GroupSnd(), 10), older = msg(1, CIDirection.GroupRcv(alice), 0)))
  }

  @Test
  fun headerShownAfterEventItem() {
    assertTrue(terminalItemHasHeader(msg(2, CIDirection.DirectRcv(), 10), older = featureEvent(1, 0)))
  }

  @Test
  fun headerShownOnDateChangeWithinMinute() {
    val midnight = LocalDate(2026, 10, 13).atStartOfDayIn(TimeZone.currentSystemDefault())
    val older = ChatItem.getSampleData(1, CIDirection.DirectRcv(), Instant.fromEpochSeconds(midnight.epochSeconds - 10))
    val item = ChatItem.getSampleData(2, CIDirection.DirectRcv(), Instant.fromEpochSeconds(midnight.epochSeconds + 10))
    assertTrue(terminalItemHasHeader(item, older))
  }

  @Test
  fun headerShownWhenChannelPostFollowsMemberMessage() {
    assertTrue(terminalItemHasHeader(msg(2, CIDirection.GroupRcv(alice), 10), older = msg(1, CIDirection.ChannelRcv(), 0)))
    assertTrue(terminalItemHasHeader(msg(2, CIDirection.ChannelRcv(), 10), older = msg(1, CIDirection.GroupRcv(alice), 0)))
    assertFalse(terminalItemHasHeader(msg(2, CIDirection.ChannelRcv(), 10), older = msg(1, CIDirection.ChannelRcv(), 0)))
  }

  @Test
  fun headerShownWhenOwnerSwitchesBetweenChannelAndOwnName() {
    val asChannel = msg(1, CIDirection.GroupSnd(), 0).let { it.copy(meta = it.meta.copy(showGroupAsSender = true)) }
    assertTrue(terminalItemHasHeader(msg(2, CIDirection.GroupSnd(), 10), older = asChannel))
    val asChannelToo = msg(2, CIDirection.GroupSnd(), 10).let { it.copy(meta = it.meta.copy(showGroupAsSender = true)) }
    assertFalse(terminalItemHasHeader(asChannelToo, older = asChannel))
  }

  @Test
  fun headerHiddenAfterDeletedMessageFromSameSender() {
    val deleted = ChatItem.getSampleData(1, CIDirection.DirectRcv(), Instant.fromEpochSeconds(noon.epochSeconds), itemDeleted = CIDeleted.Deleted(null))
    assertFalse(terminalItemHasHeader(msg(2, CIDirection.DirectRcv(), 10), older = deleted))
  }

  @Test
  fun hoverBarShownForHoveredMessage() {
    assertTrue(terminalHoverBarVisible(msg(1, CIDirection.DirectRcv(), 0), rowHovered = true, reactionPickerOpen = false, canReact = true, selecting = false))
  }

  @Test
  fun hoverBarHiddenWhenRowNotHovered() {
    assertFalse(terminalHoverBarVisible(msg(1, CIDirection.DirectRcv(), 0), rowHovered = false, reactionPickerOpen = false, canReact = true, selecting = false))
  }

  @Test
  fun hoverBarHiddenInSelectionMode() {
    val message = msg(1, CIDirection.DirectRcv(), 0)
    assertFalse(terminalHoverBarVisible(message, rowHovered = true, reactionPickerOpen = false, canReact = true, selecting = true))
    assertFalse(terminalHoverBarVisible(message, rowHovered = false, reactionPickerOpen = true, canReact = true, selecting = true))
  }

  @Test
  fun hoverBarHiddenForEventAndLiveDummyItems() {
    assertFalse(terminalHoverBarVisible(featureEvent(2, 0), rowHovered = true, reactionPickerOpen = false, canReact = true, selecting = false))
    assertFalse(terminalHoverBarVisible(msg(ChatItem.TEMP_LIVE_CHAT_ITEM_ID, CIDirection.DirectSnd(), 0), rowHovered = true, reactionPickerOpen = false, canReact = true, selecting = false))
  }

  @Test
  fun hoverBarStaysOpenForReactionPickerUntilReactionsUnavailable() {
    val message = msg(1, CIDirection.DirectRcv(), 0)
    assertTrue(terminalHoverBarVisible(message, rowHovered = false, reactionPickerOpen = true, canReact = true, selecting = false))
    assertFalse(terminalHoverBarVisible(message, rowHovered = false, reactionPickerOpen = true, canReact = false, selecting = false))
  }

  @Test
  fun headerGroupsLocalNotes() {
    assertTrue(terminalItemHasHeader(msg(1, CIDirection.LocalSnd(), 0), older = null))
    assertFalse(terminalItemHasHeader(msg(2, CIDirection.LocalSnd(), 10), older = msg(1, CIDirection.LocalSnd(), 0)))
  }

  @Test
  fun metaHiddenForPlainReceivedMessage() {
    val received = ChatItem.getSampleData(1, CIDirection.DirectRcv(), noon, status = CIStatus.RcvRead())
    assertFalse(terminalMetaVisible(received, chatTTL = null, showViaProxy = false, secondaryColor = Color.Gray))
  }

  @Test
  fun metaShownForEditedReceivedMessage() {
    val edited = ChatItem.getSampleData(1, CIDirection.DirectRcv(), noon, status = CIStatus.RcvRead(), itemEdited = true)
    assertTrue(terminalMetaVisible(edited, chatTTL = null, showViaProxy = false, secondaryColor = Color.Gray))
  }

  @Test
  fun metaShownForSentMessage() {
    val sent = ChatItem.getSampleData(1, CIDirection.DirectSnd(), noon, status = CIStatus.SndSent(SndCIStatusProgress.Complete))
    assertTrue(terminalMetaVisible(sent, chatTTL = null, showViaProxy = false, secondaryColor = Color.Gray))
  }

  @Test
  fun noHeaderForEventItem() {
    assertFalse(terminalItemHasHeader(featureEvent(2, 10), older = msg(1, CIDirection.DirectRcv(), 0)))
    assertFalse(terminalItemHasHeader(featureEvent(1, 0), older = null))
  }
}
