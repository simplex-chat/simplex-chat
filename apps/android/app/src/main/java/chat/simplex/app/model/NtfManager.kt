package chat.simplex.app.model

import android.app.*
import android.content.*
import android.graphics.BitmapFactory
import android.media.AudioAttributes
import android.net.Uri
import android.util.Log
import androidx.core.app.NotificationCompat
import androidx.core.app.NotificationManagerCompat
import chat.simplex.app.*
import chat.simplex.app.views.call.*
import chat.simplex.app.views.helpers.base64ToBitmap
import chat.simplex.app.views.helpers.generalGetString
import kotlinx.datetime.Clock

class NtfManager(val context: Context, private val appPreferences: AppPreferences) {
  companion object {
    const val MessageChannel: String = "chat.simplex.app.MESSAGE_NOTIFICATION"
    const val MessageGroup: String = "chat.simplex.app.MESSAGE_NOTIFICATION"
    const val OpenChatAction: String = "chat.simplex.app.OPEN_CHAT"
    const val ShowChatsAction: String = "chat.simplex.app.SHOW_CHATS"

    // DO NOT change notification channel settings / names
    const val CallChannel: String = "chat.simplex.app.CALL_NOTIFICATION"
    const val LockScreenCallChannel: String = "chat.simplex.app.LOCK_SCREEN_CALL_NOTIFICATION"
    const val AcceptCallAction: String = "chat.simplex.app.ACCEPT_CALL"
    const val CallNotificationId: Int = -1
  }

  private val manager: NotificationManager = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
  private var prevNtfTime = mutableMapOf<String, Long>()
  private val msgNtfTimeoutMs = 30000L

  init {
    manager.createNotificationChannel(NotificationChannel(MessageChannel, "SimpleX Chat messages", NotificationManager.IMPORTANCE_HIGH))
    manager.createNotificationChannel(NotificationChannel(LockScreenCallChannel, "SimpleX Chat calls (lock screen)", NotificationManager.IMPORTANCE_HIGH))
    manager.createNotificationChannel(callNotificationChannel())
  }

  private fun callNotificationChannel(): NotificationChannel {
    val callChannel = NotificationChannel(CallChannel, "SimpleX Chat calls", NotificationManager.IMPORTANCE_HIGH)
    val attrs = AudioAttributes.Builder()
      .setContentType(AudioAttributes.CONTENT_TYPE_SONIFICATION)
      .setUsage(AudioAttributes.USAGE_NOTIFICATION)
      .build()
    val soundUri = Uri.parse(ContentResolver.SCHEME_ANDROID_RESOURCE + "://" + context.packageName + "/" + R.raw.ring_once)
    Log.d(TAG,"callNotificationChannel sound: $soundUri")
    callChannel.setSound(soundUri, attrs)
    callChannel.enableVibration(true)
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
    notifyMessageReceived(chatId = cInfo.id, displayName = cInfo.displayName, msgText = hideSecrets(cItem))
  }

  fun notifyMessageReceived(chatId: String, displayName: String, msgText: String) {
    Log.d(TAG, "notifyMessageReceived $chatId")
    val now = Clock.System.now().toEpochMilliseconds()
    val recentNotification = (now - prevNtfTime.getOrDefault(chatId, 0) < msgNtfTimeoutMs)
    prevNtfTime[chatId] = now

    val notification = NotificationCompat.Builder(context, MessageChannel)
      .setContentTitle(displayName)
      .setContentText(msgText)
      .setPriority(NotificationCompat.PRIORITY_HIGH)
      .setGroup(MessageGroup)
      .setGroupAlertBehavior(NotificationCompat.GROUP_ALERT_CHILDREN)
      .setSmallIcon(R.drawable.ntf_icon)
      .setColor(0x88FFFF)
      .setAutoCancel(true)
      .setContentIntent(chatPendingIntent(OpenChatAction, chatId))
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
      notify(chatId.hashCode(), notification)
      notify(0, summary)
    }
  }

  fun notifyCallInvitation(invitation: RcvCallInvitation) {
    if (isAppOnForeground(context)) return
    val contactId = invitation.contact.id
    Log.d(TAG, "notifyCallInvitation $contactId")
    val keyguardManager = getKeyguardManager(context)
    val image = invitation.contact.image
    var ntfBuilder =
      if (keyguardManager.isDeviceLocked && appPreferences.callOnLockScreen.get() != CallOnLockScreen.DISABLE) {
        val fullScreenIntent = Intent(context, IncomingCallActivity::class.java)
        val fullScreenPendingIntent = PendingIntent.getActivity(context, 0, fullScreenIntent, PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_IMMUTABLE)
        NotificationCompat.Builder(context, LockScreenCallChannel)
          .setFullScreenIntent(fullScreenPendingIntent, true)
          .setVisibility(NotificationCompat.VISIBILITY_PUBLIC)
          .setSilent(true)
      } else {
        val soundUri = Uri.parse(ContentResolver.SCHEME_ANDROID_RESOURCE + "://" + context.packageName + "/" + R.raw.ring_once)
        NotificationCompat.Builder(context, CallChannel)
          .setContentIntent(chatPendingIntent(OpenChatAction, invitation.contact.id))
          .addAction(R.drawable.ntf_icon, generalGetString(R.string.accept), chatPendingIntent(AcceptCallAction, contactId))
          .setSound(soundUri)
      }
    val text = generalGetString(
      if (invitation.callType.media == CallMediaType.Video) {
        if (invitation.sharedKey == null) R.string.video_call_no_encryption else R.string.encrypted_video_call
      } else {
        if (invitation.sharedKey == null) R.string.audio_call_no_encryption else R.string.encrypted_audio_call
      }
    )
    ntfBuilder = ntfBuilder
      .setContentTitle(invitation.contact.displayName)
      .setContentText(text)
      .setPriority(NotificationCompat.PRIORITY_HIGH)
      .setCategory(NotificationCompat.CATEGORY_CALL)
      .setSmallIcon(R.drawable.ntf_icon)
      .setLargeIcon(if (image == null) BitmapFactory.decodeResource(context.resources, R.drawable.icon) else base64ToBitmap(image))
      .setColor(0x88FFFF)
      .setAutoCancel(true)
    with(NotificationManagerCompat.from(context)) {
      notify(CallNotificationId, ntfBuilder.build())
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
