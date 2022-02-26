package chat.simplex.app.model

import android.app.*
import android.content.Context
import android.content.Intent
import androidx.core.app.NotificationCompat
import androidx.core.app.NotificationManagerCompat
import chat.simplex.app.MainActivity
import chat.simplex.app.R
import kotlinx.datetime.Clock

class NtfManager(val context: Context) {
  companion object {
    const val MainChannelName: String = "SimpleXNotifications"
    const val SilentChannelName: String = "SilentSimpleXNotifications"
  }
  private val manager: NotificationManager = (
      context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
    )
  private var prevNtfTime = mutableMapOf<Long, Long>()
  private val msgNtfTimeoutMs = 10000L

  fun createNotificationChannel(channelId: String, quiet: Boolean = false) {
    val name = "SimpleX Chat"
    val desc = "Channel for message notifications"
    val importance = NotificationManager.IMPORTANCE_HIGH
    val channel = NotificationChannel(channelId, name, importance)
      .apply {
      description = desc
    }
    if (quiet) {
      channel.enableVibration(false)
      channel.enableLights(false)
      channel.setSound(null, null)
    }
    manager.createNotificationChannel(channel)
  }

  fun notifyMessageReceived(cInfo: ChatInfo, cItem: ChatItem) {
    val now = Clock.System.now().toEpochMilliseconds()
    val recentNotification = (now - prevNtfTime.getOrDefault(cInfo.apiId, 0) < msgNtfTimeoutMs)
    prevNtfTime[cInfo.apiId] = now
    val channelId = if (recentNotification) SilentChannelName else MainChannelName

    val pendingIntent = getMsgPendingIntent(cInfo)

    val notificationId = cItem.hashCode()
    val group = cInfo.id
    val notificationGroupId = group.hashCode()

    val notifications = manager.activeNotifications
    val jointNotifications = notifications.filter { n -> (n.notification.group != null && n.notification.group == group) }
    val rawCount = jointNotifications.count()
    val notificationBuilder = getNotificationBuilder(pendingIntent, NotificationCompat.PRIORITY_HIGH, channelId, cInfo.displayName, cItem.content.text, group)
    val notificationGroupBuilder = getGroupNotificationBuilder(rawCount, pendingIntent, channelId, cInfo.displayName, group)


    with(NotificationManagerCompat.from(context)) {
      if (rawCount != 0) {
        notify(notificationGroupId, notificationGroupBuilder.build())
      }
      if (rawCount == 1) {
        val originalId = jointNotifications[0].id
        manager.cancel(originalId)
        notify(originalId, jointNotifications[0].notification.clone())
      }
      notify(notificationId, notificationBuilder.build())
    }
  }

  private fun getMsgPendingIntent(cInfo: ChatInfo) : PendingIntent{
    val intent = Intent(
      context,
      MainActivity::class.java
    ).apply {
      flags = Intent.FLAG_ACTIVITY_NEW_TASK or Intent.FLAG_ACTIVITY_CLEAR_TASK
    }
      .putExtra("chatId", cInfo.id)
      .putExtra("chatType", cInfo.chatType.chatTypeName)
      .setAction("openChatWithId")
    return TaskStackBuilder.create(context).run {
      addNextIntentWithParentStack(intent)
      getPendingIntent(0, PendingIntent.FLAG_IMMUTABLE)
    }
  }

  private fun getNotificationBuilder(
    pendingIntent: PendingIntent,
    priority: Int = NotificationCompat.PRIORITY_HIGH,
    channelId: String,
    title: String,
    content: String,
    group: String? = null,
  ): NotificationCompat.Builder {
    return NotificationCompat.Builder(context, channelId)
      .setSmallIcon(R.mipmap.icon)
      .setContentTitle(title)
      .setContentText(content)
      .setPriority(priority)
      .setGroup(group)
      .setStyle(NotificationCompat.InboxStyle().addLine(content))
      .setAutoCancel(true)
      .setContentIntent(pendingIntent)
      .setOnlyAlertOnce(true)
  }

  private fun getGroupNotificationBuilder(
    rawNotificationCount: Int,
    pendingIntent: PendingIntent,
    channelId: String,
    title: String,
    group: String? = null
  ): NotificationCompat.Builder {
    // Avoid counting existing group notification
    val msgCount = if (rawNotificationCount <= 1) rawNotificationCount + 1 else rawNotificationCount
    val groupNotificationText = if (msgCount > 1) "$msgCount new messages" else "$msgCount new message"
    return NotificationCompat.Builder(context, channelId)
      .setSmallIcon(R.mipmap.icon)
      .setContentTitle(title)
      .setContentText(groupNotificationText)
      .setStyle(NotificationCompat.InboxStyle()
        .setSummaryText(groupNotificationText)
      )
      .setGroup(group)
      .setGroupSummary(true)
      .setGroupAlertBehavior(NotificationCompat.GROUP_ALERT_CHILDREN)
      .setContentIntent(pendingIntent)
      .setAutoCancel(true)
      .setOnlyAlertOnce(true)
      .setSound(null)
      .setVibrate(null)
  }
}
