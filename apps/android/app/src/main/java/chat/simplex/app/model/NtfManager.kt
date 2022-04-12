package chat.simplex.app.model

import android.app.*
import android.content.Context
import android.content.Intent
import android.util.Log
import androidx.core.app.NotificationCompat
import androidx.core.app.NotificationManagerCompat
import chat.simplex.app.*
import kotlinx.datetime.Clock

class NtfManager(val context: Context) {
  companion object {
    const val MessageChannel: String = "chat.simplex.app.MESSAGE_NOTIFICATION"
    const val MessageGroup: String = "chat.simplex.app.MESSAGE_NOTIFICATION"
    const val OpenChatAction: String = "chat.simplex.app.OPEN_CHAT"
    const val ShowChatsAction: String = "chat.simplex.app.SHOW_CHATS"
  }

  private val manager: NotificationManager = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
  private var prevNtfTime = mutableMapOf<String, Long>()
  private val msgNtfTimeoutMs = 30000L

  init {
    manager.createNotificationChannel(NotificationChannel(
      MessageChannel,
      "SimpleX Chat messages",
      NotificationManager.IMPORTANCE_HIGH
    ))
  }

  fun cancelNotificationsForChat(chatId: String) {
    prevNtfTime.remove(chatId)
    manager.cancel(chatId.hashCode())
    val msgNtfs = manager.activeNotifications.filter {
      ntf -> ntf.notification.channelId == MessageChannel
    }
    if (msgNtfs.count() == 1) {
      // Have a group notification with no children so cancel it
      manager.cancel(0)
    }
  }

  fun notifyMessageReceived(cInfo: ChatInfo, cItem: ChatItem) {
    Log.d(TAG, "notifyMessageReceived ${cInfo.id}")
    val now = Clock.System.now().toEpochMilliseconds()
    val recentNotification = (now - prevNtfTime.getOrDefault(cInfo.id, 0) < msgNtfTimeoutMs)
    prevNtfTime[cInfo.id] = now

    val notification = NotificationCompat.Builder(context, MessageChannel)
      .setContentTitle(cInfo.displayName)
      .setContentText(hideSecrets(cItem))
      .setPriority(NotificationCompat.PRIORITY_HIGH)
      .setGroup(MessageGroup)
      .setGroupAlertBehavior(NotificationCompat.GROUP_ALERT_CHILDREN)
      .setSmallIcon(R.drawable.ntf_icon)
      .setColor(0x88FFFF)
      .setAutoCancel(true)
      .setContentIntent(getMsgPendingIntent(cInfo))
      .setSilent(recentNotification)
      .build()

    val summary = NotificationCompat.Builder(context, MessageChannel)
      .setSmallIcon(R.drawable.ntf_icon)
      .setColor(0x88FFFF)
      .setGroup(MessageGroup)
      .setGroupAlertBehavior(NotificationCompat.GROUP_ALERT_CHILDREN)
      .setGroupSummary(true)
      .setContentIntent(getSummaryNtfIntent())
      .build()

    with(NotificationManagerCompat.from(context)) {
      // using cInfo.id only shows one notification per chat and updates it when the message arrives
      notify(cInfo.id.hashCode(), notification)
      notify(0, summary)
    }
  }

  private fun hideSecrets(cItem: ChatItem) : String {
    val md = cItem.formattedText
    return if (md == null) {
      cItem.content.text
    } else {
      var res = ""
      for (ft in md) {
        res += if (ft.format is Format.Secret) "..." else ft.text
      }
      res
    }
  }

  private fun getMsgPendingIntent(cInfo: ChatInfo) : PendingIntent{
    Log.d(TAG, "getMsgPendingIntent ${cInfo.id}")
    val uniqueInt = (System.currentTimeMillis() and 0xfffffff).toInt()
    val intent = Intent(context, MainActivity::class.java)
      .addFlags(Intent.FLAG_ACTIVITY_NEW_TASK or Intent.FLAG_ACTIVITY_SINGLE_TOP or Intent.FLAG_ACTIVITY_CLEAR_TOP)
      .putExtra("chatId", cInfo.id)
      .setAction(OpenChatAction)
    return TaskStackBuilder.create(context).run {
      addNextIntentWithParentStack(intent)
      getPendingIntent(uniqueInt, PendingIntent.FLAG_IMMUTABLE)
    }
  }

  private fun getSummaryNtfIntent() : PendingIntent{
    Log.d(TAG, "getSummaryNtfIntent")
    val uniqueInt = (System.currentTimeMillis() and 0xfffffff).toInt()
    val intent = Intent(context, MainActivity::class.java)
      .addFlags(Intent.FLAG_ACTIVITY_NEW_TASK or Intent.FLAG_ACTIVITY_SINGLE_TOP or Intent.FLAG_ACTIVITY_CLEAR_TOP)
      .setAction(ShowChatsAction)
    return TaskStackBuilder.create(context).run {
      addNextIntentWithParentStack(intent)
      getPendingIntent(uniqueInt, PendingIntent.FLAG_IMMUTABLE)
    }
  }
}
