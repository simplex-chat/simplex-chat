package chat.simplex.app.model

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import chat.simplex.app.SimplexApp
import chat.simplex.common.platform.ntfManager

class ReminderNotificationReceiver : BroadcastReceiver() {
  override fun onReceive(context: Context?, intent: Intent?) {
    if (intent?.action != MessageReminderScheduler.ReminderAlarmAction) return
    val reminderId = intent.getStringExtra(NtfManager.ReminderIdKey) ?: return
    val repo = SimplexApp.context.chatModel.reminderRepository
    val reminder = repo.reminderById(reminderId)
    if (reminder != null && !reminder.isComplete) {
      ntfManager.notifyMessageReminder(reminder)
    }
  }
}