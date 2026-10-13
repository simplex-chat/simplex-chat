package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.views.chat.item.flatItemHasHeader
import kotlinx.datetime.Instant
import kotlinx.datetime.LocalDate
import kotlinx.datetime.TimeZone
import kotlinx.datetime.atStartOfDayIn
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class FlatChatItemViewTest {
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
    assertTrue(flatItemHasHeader(msg(1, CIDirection.DirectRcv(), 0), older = null))
  }

  @Test
  fun headerHiddenForSameSenderWithinMinute() {
    assertFalse(flatItemHasHeader(msg(2, CIDirection.DirectSnd(), 30), older = msg(1, CIDirection.DirectSnd(), 0)))
    assertFalse(flatItemHasHeader(msg(2, CIDirection.GroupRcv(alice), 59), older = msg(1, CIDirection.GroupRcv(alice), 0)))
  }

  @Test
  fun headerShownAfterMinuteGap() {
    assertTrue(flatItemHasHeader(msg(2, CIDirection.DirectRcv(), 60), older = msg(1, CIDirection.DirectRcv(), 0)))
  }

  @Test
  fun headerShownForDifferentGroupMember() {
    assertTrue(flatItemHasHeader(msg(2, CIDirection.GroupRcv(bob), 10), older = msg(1, CIDirection.GroupRcv(alice), 0)))
  }

  @Test
  fun headerShownWhenDirectionChanges() {
    assertTrue(flatItemHasHeader(msg(2, CIDirection.DirectSnd(), 10), older = msg(1, CIDirection.DirectRcv(), 0)))
    assertTrue(flatItemHasHeader(msg(2, CIDirection.GroupSnd(), 10), older = msg(1, CIDirection.GroupRcv(alice), 0)))
  }

  @Test
  fun headerShownAfterEventItem() {
    assertTrue(flatItemHasHeader(msg(2, CIDirection.DirectRcv(), 10), older = featureEvent(1, 0)))
  }

  @Test
  fun headerShownOnDateChangeWithinMinute() {
    val midnight = LocalDate(2026, 10, 13).atStartOfDayIn(TimeZone.currentSystemDefault())
    val older = ChatItem.getSampleData(1, CIDirection.DirectRcv(), Instant.fromEpochSeconds(midnight.epochSeconds - 10))
    val item = ChatItem.getSampleData(2, CIDirection.DirectRcv(), Instant.fromEpochSeconds(midnight.epochSeconds + 10))
    assertTrue(flatItemHasHeader(item, older))
  }

  @Test
  fun noHeaderForEventItem() {
    assertFalse(flatItemHasHeader(featureEvent(2, 10), older = msg(1, CIDirection.DirectRcv(), 0)))
    assertFalse(flatItemHasHeader(featureEvent(1, 0), older = null))
  }
}
