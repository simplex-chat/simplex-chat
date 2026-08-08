package chat.simplex.common.platform

import android.content.ActivityNotFoundException
import android.content.Intent
import android.provider.CalendarContract

actual fun addCalendarReminder(title: String, description: String, epochSeconds: Long, remindDaysBefore: Int): Boolean {
  val startMs = epochSeconds * 1000
  val intent = Intent(Intent.ACTION_INSERT).apply {
    data = CalendarContract.Events.CONTENT_URI
    putExtra(CalendarContract.Events.TITLE, title)
    putExtra(CalendarContract.Events.DESCRIPTION, description)
    putExtra(CalendarContract.EXTRA_EVENT_BEGIN_TIME, startMs)
    putExtra(CalendarContract.EXTRA_EVENT_END_TIME, startMs)
    putExtra(CalendarContract.EXTRA_EVENT_ALL_DAY, true)
    // Alert ahead of the date, not on it: by the expiry itself there is no
    // time left to act. Minutes is the unit CalendarContract takes.
    putExtra(CalendarContract.Reminders.MINUTES, remindDaysBefore * 24 * 60)
    putExtra(CalendarContract.Reminders.METHOD, CalendarContract.Reminders.METHOD_ALERT)
    addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
  }
  return try {
    androidAppContext.startActivity(intent)
    true
  } catch (e: ActivityNotFoundException) {
    // No calendar app installed: say so rather than failing silently.
    Log.e(TAG, "addCalendarReminder: no calendar app")
    false
  }
}
