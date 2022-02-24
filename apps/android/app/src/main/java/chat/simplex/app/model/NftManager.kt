package chat.simplex.app.model

import android.app.*
import android.content.Context
import android.content.Intent
import androidx.core.app.NotificationCompat
import androidx.core.app.NotificationManagerCompat
import chat.simplex.app.R

fun createNotificationChannel(channelId: String, context: Context) {
  val name = "SimpleX Chat"
  val desc = "Channel for message notifications"
  val importance = NotificationManager.IMPORTANCE_DEFAULT
  val channel = NotificationChannel(channelId, name, importance).apply {
    description = desc
  }
  val manager: NotificationManager = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
  manager.createNotificationChannel(channel)
}

fun notification(
  context: Context,
  channelId: String,
  title: String,
  content: String,
  notificationId: Int,
  priority: Int = NotificationCompat.PRIORITY_DEFAULT
) {
  val intent = Intent().apply {
    flags = Intent.FLAG_ACTIVITY_NEW_TASK or Intent.FLAG_ACTIVITY_CLEAR_TASK
  }
  val pendingIntent = PendingIntent.getActivity(context, 0, intent,  PendingIntent.FLAG_IMMUTABLE)
  val builder = NotificationCompat.Builder(context, channelId)
    .setSmallIcon(R.mipmap.icon)
    .setContentTitle(title)
    .setContentText(content)
    .setContentIntent(pendingIntent)
    .setPriority(priority)
  with(NotificationManagerCompat.from(context)) {
    notify(notificationId, builder.build())
  }
}
