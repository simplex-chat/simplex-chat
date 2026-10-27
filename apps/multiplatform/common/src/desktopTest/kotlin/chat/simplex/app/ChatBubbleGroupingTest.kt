package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.views.chat.item.chatMessagesJoined
import kotlinx.datetime.*
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue
import kotlin.time.Duration.Companion.seconds

class ChatBubbleGroupingTest {
  private val timestamp = LocalDateTime(2026, 10, 27, 12, 0).toInstant(TimeZone.currentSystemDefault())
  private fun message(seconds: Int = 0, direction: CIDirection = CIDirection.DirectSnd()) =
    ChatItem.getSampleData(dir = direction, ts = timestamp + seconds.seconds)

  @Test
  fun groupsAcrossMinuteChangesAndStopsAfterFiveMinutes() {
    assertTrue(chatMessagesJoined(message(), message(120)))
    assertTrue(chatMessagesJoined(message(), message(300)))
    assertFalse(chatMessagesJoined(message(), message(301)))
    assertFalse(chatMessagesJoined(message(), null))
  }

  @Test
  fun doesNotJoinDifferentSendersOrDirections() {
    assertFalse(chatMessagesJoined(message(), message(direction = CIDirection.DirectRcv())))
    val member = GroupMember.sampleData
    val first = message(direction = CIDirection.GroupRcv(member))
    assertTrue(chatMessagesJoined(first, message(60, CIDirection.GroupRcv(member))))
    assertFalse(chatMessagesJoined(first, message(60, CIDirection.GroupRcv(member.copy(groupMemberId = member.groupMemberId + 1)))))
    assertFalse(chatMessagesJoined(first, message(direction = CIDirection.DirectRcv())))
  }

  @Test
  fun dateSeparatorsBreakGroupsEvenWithinFiveMinutes() {
    val midnight = LocalDateTime(2026, 10, 28, 0, 0).toInstant(TimeZone.currentSystemDefault())
    val before = ChatItem.getSampleData(ts = midnight - 1.seconds)
    val after = ChatItem.getSampleData(ts = midnight + 1.seconds)
    assertFalse(chatMessagesJoined(before, after))
  }

  @Test
  fun eventsAndDeletedMessagesBreakGroups() {
    assertFalse(chatMessagesJoined(message(), ChatItem.getGroupEventSample()))
    assertFalse(chatMessagesJoined(message(), ChatItem.getDeletedContentSampleData(ts = timestamp)))
    val deleted = message().copy(meta = message().meta.copy(itemDeleted = CIDeleted.Deleted(null)))
    assertFalse(chatMessagesJoined(message(), deleted))
  }
}