package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.views.chat.item.terminalItemShowsSender
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
  fun senderHiddenForSameSenderWithinMinute() {
    assertFalse(terminalItemShowsSender(msg(2, CIDirection.DirectSnd(), 30), older = msg(1, CIDirection.DirectSnd(), 0)))
    assertFalse(terminalItemShowsSender(msg(2, CIDirection.GroupRcv(alice), 59), older = msg(1, CIDirection.GroupRcv(alice), 0)))
  }

  @Test
  fun senderShownAfterMinuteGap() {
    assertTrue(terminalItemShowsSender(msg(2, CIDirection.DirectRcv(), 60), older = msg(1, CIDirection.DirectRcv(), 0)))
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
  fun senderShownOnDateChangeWithinMinute() {
    val midnight = LocalDate(2026, 10, 13).atStartOfDayIn(TimeZone.currentSystemDefault())
    val older = msgAt(1, CIDirection.DirectRcv(), Instant.fromEpochSeconds(midnight.epochSeconds - 10))
    val item = msgAt(2, CIDirection.DirectRcv(), Instant.fromEpochSeconds(midnight.epochSeconds + 10))
    assertTrue(terminalItemShowsSender(item, older))
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
  fun noSenderForEventItem() {
    assertFalse(terminalItemShowsSender(featureEvent(2, 10), older = msg(1, CIDirection.DirectRcv(), 0)))
    assertFalse(terminalItemShowsSender(featureEvent(1, 0), older = null))
  }
}
