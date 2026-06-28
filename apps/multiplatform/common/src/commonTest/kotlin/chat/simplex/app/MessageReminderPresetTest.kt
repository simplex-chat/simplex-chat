package chat.simplex.app

import chat.simplex.common.model.ReminderPreset
import chat.simplex.common.model.dueAt
import kotlinx.datetime.Instant
import kotlinx.datetime.TimeZone
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.time.Duration.Companion.hours

class MessageReminderPresetTest {
  @Test
  fun in1HourAddsOneHour() {
    val now = Instant.parse("2026-03-29T12:00:00Z")
    val due = ReminderPreset.In1Hour.dueAt(now, TimeZone.UTC)
    assertEquals(now + 1.hours, due)
  }

  @Test
  fun in3HoursAddsThreeHours() {
    val now = Instant.parse("2026-03-29T12:00:00Z")
    val due = ReminderPreset.In3Hours.dueAt(now, TimeZone.UTC)
    assertEquals(now + 3.hours, due)
  }

  @Test
  fun tomorrowIsNineAmNextCalendarDay() {
    val now = Instant.parse("2026-03-29T15:30:00Z")
    val due = ReminderPreset.Tomorrow.dueAt(now, TimeZone.UTC)
    assertEquals(Instant.parse("2026-03-30T09:00:00Z"), due)
  }

  @Test
  fun nextWeekIsNineAmNextMondayUtc() {
    val now = Instant.parse("2026-03-29T12:00:00Z")
    val due = ReminderPreset.NextWeek.dueAt(now, TimeZone.UTC)
    assertEquals(Instant.parse("2026-03-30T09:00:00Z"), due)
  }
}