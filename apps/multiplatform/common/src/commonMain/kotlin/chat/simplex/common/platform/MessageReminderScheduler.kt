package chat.simplex.common.platform

import chat.simplex.common.model.MessageReminder

expect object MessageReminderScheduler {
  fun schedule(reminder: MessageReminder)
  fun cancel(reminderId: String)
  fun rescheduleAll(reminders: List<MessageReminder>)
  fun cancelNotification(reminderId: String)
}