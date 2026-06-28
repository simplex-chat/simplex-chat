package chat.simplex.common.platform

import chat.simplex.common.model.MessageReminder
import chat.simplex.common.model.NtfManager
import chat.simplex.common.views.helpers.withBGApi
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.datetime.Clock

actual object MessageReminderScheduler {
  private val scope = CoroutineScope(Dispatchers.Default)
  private val jobs = mutableMapOf<String, Job>()

  actual fun schedule(reminder: MessageReminder) {
    cancel(reminder.id)
    val delayMs = reminder.dueAt.toEpochMilliseconds() - Clock.System.now().toEpochMilliseconds()
    if (delayMs <= 0) {
      withBGApi { ntfManager.notifyMessageReminder(reminder) }
      return
    }
    jobs[reminder.id] = scope.launch {
      delay(delayMs)
      withBGApi { ntfManager.notifyMessageReminder(reminder) }
    }
  }

  actual fun cancel(reminderId: String) {
    jobs.remove(reminderId)?.cancel()
  }

  actual fun rescheduleAll(reminders: List<MessageReminder>) {
    jobs.keys.toList().forEach { cancel(it) }
    reminders.forEach { schedule(it) }
  }

  actual fun cancelNotification(reminderId: String) {
    NtfManager.cancelReminderNotification(reminderId)
  }
}