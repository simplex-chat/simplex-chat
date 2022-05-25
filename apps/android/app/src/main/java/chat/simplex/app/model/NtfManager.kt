package chat.simplex.app.model

import android.app.*
import android.content.*
import android.graphics.BitmapFactory
import android.media.AudioAttributes
import android.media.AudioAttributes.*
import android.media.AudioManager
import android.net.Uri
import android.util.Log
import androidx.compose.ui.graphics.asImageBitmap
import androidx.core.app.NotificationCompat
import androidx.core.app.NotificationManagerCompat
import chat.simplex.app.*
import chat.simplex.app.views.call.CallInvitation
import chat.simplex.app.views.call.CallMediaType
import chat.simplex.app.views.helpers.base64ToBitmap
import chat.simplex.app.views.helpers.generalGetString
import kotlinx.datetime.Clock

class NtfManager(val context: Context) {
  companion object {
    const val MessageChannel: String = "chat.simplex.app.MESSAGE_NOTIFICATION"
    const val MessageGroup: String = "chat.simplex.app.MESSAGE_NOTIFICATION"
    const val OpenChatAction: String = "chat.simplex.app.OPEN_CHAT"
    const val ShowChatsAction: String = "chat.simplex.app.SHOW_CHATS"

    const val CallChannel: String = "chat.simplex.app.CALL_NOTIFICATION"
    const val AcceptCallAction: String = "chat.simplex.app.ACCEPT_CALL"
    const val CallNotificationId: Int = -1
  }

  private val manager: NotificationManager = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
  private var prevNtfTime = mutableMapOf<String, Long>()
  private val msgNtfTimeoutMs = 30000L

  init {
    manager.createNotificationChannel(NotificationChannel(MessageChannel, "SimpleX Chat messages", NotificationManager.IMPORTANCE_HIGH))
    manager.createNotificationChannel(callNotificationChannel())
  }

  private fun callNotificationChannel(): NotificationChannel {
    val callChannel = NotificationChannel(CallChannel, "SimpleX Chat calls", NotificationManager.IMPORTANCE_HIGH)
    val attrs = AudioAttributes.Builder()
      .setContentType(AudioAttributes.CONTENT_TYPE_SONIFICATION)
      .setUsage(AudioAttributes.USAGE_NOTIFICATION)
      .build()
    val soundUri = Uri.Builder().scheme(ContentResolver.SCHEME_ANDROID_RESOURCE)
      .authority("chat.simplex.app")
      .path(R.raw.ringtone.toString())
      .build()
    callChannel.setSound(soundUri, attrs)
    return callChannel
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
      .setContentIntent(chatPendingIntent(OpenChatAction, cInfo.id))
      .setSilent(recentNotification)
      .build()

    val summary = NotificationCompat.Builder(context, MessageChannel)
      .setSmallIcon(R.drawable.ntf_icon)
      .setColor(0x88FFFF)
      .setGroup(MessageGroup)
      .setGroupAlertBehavior(NotificationCompat.GROUP_ALERT_CHILDREN)
      .setGroupSummary(true)
      .setContentIntent(chatPendingIntent(ShowChatsAction))
      .build()

    with(NotificationManagerCompat.from(context)) {
      // using cInfo.id only shows one notification per chat and updates it when the message arrives
      notify(cInfo.id.hashCode(), notification)
      notify(0, summary)
    }
  }

  fun notifyCallInvitation(invitation: CallInvitation) {
    if (isAppOnForeground(context)) return
    val contactId = invitation.contact.id
    Log.d(TAG, "notifyCallInvitation $contactId")
    val image = invitation.contact.image
    val notification = NotificationCompat.Builder(context, CallChannel)
      .setContentTitle(invitation.contact.displayName)
      .setContentText("Incoming ${invitation.peerMedia} call (${if (invitation.sharedKey == null) "not e2e encrypted" else "e2e encrypted"})")
      .setPriority(NotificationCompat.PRIORITY_HIGH)
      .setCategory(NotificationCompat.CATEGORY_CALL)
      .setContentIntent(chatPendingIntent(OpenChatAction, invitation.contact.id))
      .addAction(R.drawable.ntf_icon, generalGetString(R.string.accept), chatPendingIntent(AcceptCallAction, contactId))
      .setSmallIcon(R.drawable.ntf_icon)
      .setLargeIcon(if (image == null) BitmapFactory.decodeResource(context.resources, R.drawable.icon) else base64ToBitmap(image))
      .setColor(0x88FFFF)
      .setAutoCancel(true)
      .setSound(Uri.parse( "android.resource://chat.simplex.app/" + R.raw.ringtone), AudioManager.STREAM_NOTIFICATION)
      .build()

    with(NotificationManagerCompat.from(context)) {
      notify(CallNotificationId, notification)
    }
  }

  fun cancelCallNotification() {
    manager.cancel(CallNotificationId)
  }

  private fun hideSecrets(cItem: ChatItem) : String {
    val md = cItem.formattedText
    return if (md == null) {
      if (cItem.content.text != "") {
        cItem.content.text
      } else {
        cItem.file?.fileName ?: ""
      }
    } else {
      var res = ""
      for (ft in md) {
        res += if (ft.format is Format.Secret) "..." else ft.text
      }
      res
    }
  }

  private fun chatPendingIntent(intentAction: String, chatId: String? = null): PendingIntent {
    Log.d(TAG, "chatPendingIntent for $intentAction")
    val uniqueInt = (System.currentTimeMillis() and 0xfffffff).toInt()
    var intent = Intent(context, MainActivity::class.java)
      .addFlags(Intent.FLAG_ACTIVITY_NEW_TASK or Intent.FLAG_ACTIVITY_SINGLE_TOP or Intent.FLAG_ACTIVITY_CLEAR_TOP)
      .setAction(intentAction)
    if (chatId != null) intent = intent.putExtra("chatId", chatId)
    return TaskStackBuilder.create(context).run {
      addNextIntentWithParentStack(intent)
      getPendingIntent(uniqueInt, PendingIntent.FLAG_IMMUTABLE)
    }
  }
}
