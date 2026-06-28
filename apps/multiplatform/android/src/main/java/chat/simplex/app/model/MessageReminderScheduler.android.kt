package chat.simplex.app.model

import android.app.AlarmManager
import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import android.os.Build
import chat.simplex.app.SimplexApp
import chat.simplex.common.model.MessageReminder
import chat.simplex.common.platform.MessageReminderScheduler
import chat.simplex.common.platform.ntfManager
import kotlinx.datetime.Clock

actual object MessageReminderScheduler {
  const val ReminderAlarmAction: String = "chat.simplex.app.MESSAGE_REMINDER_ALARM"

  private val context: Context
    get() = SimplexApp.context

  private val alarmManager: AlarmManager
    get() = context.getSystemService(Context.ALARM_SERVICE) as AlarmManager

  actual fun schedule(reminder: MessageReminder) {
    cancel(reminder.id)
    val triggerAt = reminder.dueAt.toEpochMilliseconds()
    val now = Clock.System.now().toEpochMilliseconds()
    if (triggerAt <= now) {
      ntfManager.notifyMessageReminder(reminder)
      return
    }
    val pendingIntent = reminderPendingIntent(reminder)
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
      alarmManager.setExactAndAllowWhileIdle(AlarmManager.RTC_WAKEUP, triggerAt, pendingIntent)
    } else {
      @Suppress("DEPRECATION")
      alarmManager.setExact(AlarmManager.RTC_WAKEUP, triggerAt, pendingIntent)
    }
  }

  actual fun cancel(reminderId: String) {
    alarmManager.cancel(reminderPendingIntent(reminderId))
  }

  actual fun rescheduleAll(reminders: List<MessageReminder>) {
    reminders.forEach { schedule(it) }
  }

  actual fun cancelNotification(reminderId: String) {
    NtfManager.cancelReminderNotification(reminderId)
  }

  private fun reminderPendingIntent(reminder: MessageReminder): PendingIntent =
    reminderPendingIntent(reminder.id, reminder.userId, reminder.chatId, reminder.itemId)

  private fun reminderPendingIntent(reminderId: String): PendingIntent {
    val intent = Intent(context, ReminderNotificationReceiver::class.java)
      .setAction(ReminderAlarmAction)
      .putExtra(NtfManager.ReminderIdKey, reminderId)
    val flags = PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_IMMUTABLE
    return PendingIntent.getBroadcast(context, reminderId.hashCode(), intent, flags)
  }

  private fun reminderPendingIntent(reminderId: String, userId: Long, chatId: String, itemId: Long): PendingIntent {
    val intent = Intent(context, ReminderNotificationReceiver::class.java)
      .setAction(ReminderAlarmAction)
      .putExtra(NtfManager.ReminderIdKey, reminderId)
      .putExtra(NtfManager.UserIdKey, userId)
      .putExtra(NtfManager.ChatIdKey, chatId)
      .putExtra(NtfManager.ItemIdKey, itemId)
    val flags = PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_IMMUTABLE
    return PendingIntent.getBroadcast(context, reminderId.hashCode(), intent, flags)
  }
}