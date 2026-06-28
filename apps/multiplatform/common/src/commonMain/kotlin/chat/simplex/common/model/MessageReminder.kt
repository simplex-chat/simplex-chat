package chat.simplex.common.model

import kotlinx.datetime.*
import kotlinx.serialization.Serializable
import java.util.UUID

/** Client-local message reminder (issue #7135). */
@Serializable
data class MessageReminder(
  val id: String,
  val userId: Long,
  val chatId: ChatId,
  val itemId: Long,
  val dueAt: Instant,
  val createdAt: Instant,
  val completedAt: Instant? = null,
  val messagePreview: String = "",
  val chatDisplayName: String = "",
) {
  val isComplete: Boolean get() = completedAt != null
  val isOverdue: Boolean get() = !isComplete && dueAt < Clock.System.now()
}

enum class ReminderPreset {
  In1Hour,
  In3Hours,
  Tomorrow,
  NextWeek,
}

fun ReminderPreset.dueAt(now: Instant = Clock.System.now(), timeZone: TimeZone = TimeZone.currentSystemDefault()): Instant =
  when (this) {
    ReminderPreset.In1Hour -> now + 1.hours
    ReminderPreset.In3Hours -> now + 3.hours
    ReminderPreset.Tomorrow -> {
      val today = now.toLocalDateTime(timeZone).date
      val tomorrowStart = today.plus(1, DateTimeUnit.DAY).atTime(9, 0)
      tomorrowStart.toInstant(timeZone)
    }
    ReminderPreset.NextWeek -> {
      val today = now.toLocalDateTime(timeZone).date
      val daysUntilMonday = (8 - today.dayOfWeek.isoDayNumber) % 7
      val addDays = if (daysUntilMonday == 0) 7 else daysUntilMonday
      today.plus(addDays, DateTimeUnit.DAY).atTime(9, 0).toInstant(timeZone)
    }
  }



fun canSetMessageReminder(cInfo: ChatInfo, cItem: ChatItem, live: Boolean): Boolean {
  if (!cInfo.sendMsgEnabled) return false
  if (cItem.meta.itemDeleted != null || cItem.isReport || cItem.localNote || cItem.isLiveDummy || live || cItem.meta.isLive) return false
  if (cItem.content.msgContent == null && cItem.text(cInfo.isChannel).isEmpty()) return false
  if (cItem.chatDir is CIDirection.ChannelRcv) return false
  return cItem.id >= 0
}

fun newMessageReminderId(): String = UUID.randomUUID().toString()