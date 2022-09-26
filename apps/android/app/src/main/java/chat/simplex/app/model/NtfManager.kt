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
import chat.simplex.app.views.chatlist.acceptContactRequest
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.usersettings.NotificationPreviewMode
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

    private const val ChatIdKey: String = "chatId"
  }

  private val manager: NotificationManager = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
  private var prevNtfTime = mutableMapOf<String, Long>()
  private val msgNtfTimeoutMs = 30000L

  init {
    manager.createNotificationChannel(NotificationChannel(MessageChannel, generalGetString(R.string.ntf_channel_messages), NotificationManager.IMPORTANCE_HIGH))
    manager.createNotificationChannel(NotificationChannel(LockScreenCallChannel, generalGetString(R.string.ntf_channel_calls_lockscreen), NotificationManager.IMPORTANCE_HIGH))
    manager.createNotificationChannel(callNotificationChannel())
  }

  enum class NotificationAction {
    ACCEPT_CONTACT_REQUEST
  }

  private fun callNotificationChannel(): NotificationChannel {
    val callChannel = NotificationChannel(CallChannel, generalGetString(R.string.ntf_channel_calls), NotificationManager.IMPORTANCE_HIGH)
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

  fun notifyContactRequestReceived(cInfo: ChatInfo.ContactRequest) {
    notifyMessageReceived(
      chatId = cInfo.id,
      displayName = cInfo.displayName,
      msgText = generalGetString(R.string.notification_new_contact_request),
      image = cInfo.image,
      listOf(NotificationAction.ACCEPT_CONTACT_REQUEST)
    )
  }

  fun notifyContactConnected(contact: Contact) {
    notifyMessageReceived(
      chatId = contact.id,
      displayName = contact.displayName,
      msgText = generalGetString(R.string.notification_contact_connected)
    )
  }

  fun notifyMessageReceived(cInfo: ChatInfo, cItem: ChatItem) {
    if (!cInfo.ntfsEnabled) return

    notifyMessageReceived(chatId = cInfo.id, displayName = cInfo.displayName, msgText = hideSecrets(cItem))
  }

  fun notifyMessageReceived(chatId: String, displayName: String, msgText: String, image: String? = null, actions: List<NotificationAction> = emptyList()) {
    Log.d(TAG, "notifyMessageReceived $chatId")
    val now = Clock.System.now().toEpochMilliseconds()
    val recentNotification = (now - prevNtfTime.getOrDefault(chatId, 0) < msgNtfTimeoutMs)
    prevNtfTime[chatId] = now

    val previewMode = appPreferences.notificationPreviewMode.get()
    val title = if (previewMode == NotificationPreviewMode.HIDDEN.name) generalGetString(R.string.notification_preview_somebody) else displayName
    val content = if (previewMode != NotificationPreviewMode.MESSAGE.name) generalGetString(R.string.notification_preview_new_message) else msgText
    val largeIcon = when {
      actions.isEmpty() -> null
      image == null || previewMode == NotificationPreviewMode.HIDDEN.name -> BitmapFactory.decodeResource(context.resources, R.drawable.icon)
      else -> base64ToBitmap(image)
    }
    val builder = NotificationCompat.Builder(context, MessageChannel)
      .setContentTitle(title)
      .setContentText(content)
      .setPriority(NotificationCompat.PRIORITY_HIGH)
      .setGroup(MessageGroup)
      .setGroupAlertBehavior(NotificationCompat.GROUP_ALERT_CHILDREN)
      .setSmallIcon(R.drawable.ntf_icon)
      .setLargeIcon(largeIcon)
      .setColor(0x88FFFF)
      .setAutoCancel(true)
      .setVibrate(if (actions.isEmpty()) null else longArrayOf(0, 250, 250, 250))
      .setContentIntent(chatPendingIntent(OpenChatAction, chatId))
      .setSilent(if (actions.isEmpty()) recentNotification else false)

    for (action in actions) {
      val flags = PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_IMMUTABLE
      val actionIntent = Intent(SimplexApp.context, NtfActionReceiver::class.java)
      actionIntent.action = action.name
      actionIntent.putExtra(ChatIdKey, chatId)
      val actionPendingIntent: PendingIntent = PendingIntent.getBroadcast(SimplexApp.context, 0, actionIntent, flags)
      val actionButton = when (action) {
        NotificationAction.ACCEPT_CONTACT_REQUEST -> generalGetString(R.string.accept)
      }
      builder.addAction(0, actionButton, actionPendingIntent)
    }

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
      notify(chatId.hashCode(), builder.build())
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
    val previewMode = appPreferences.notificationPreviewMode.get()
    val title = if (previewMode == NotificationPreviewMode.HIDDEN.name)
      generalGetString(R.string.notification_preview_somebody)
    else
      invitation.contact.displayName
    val largeIcon = if (image == null || previewMode == NotificationPreviewMode.HIDDEN.name)
      BitmapFactory.decodeResource(context.resources, R.drawable.icon)
    else
      base64ToBitmap(image)

    ntfBuilder = ntfBuilder
      .setContentTitle(title)
      .setContentText(text)
      .setPriority(NotificationCompat.PRIORITY_HIGH)
      .setCategory(NotificationCompat.CATEGORY_CALL)
      .setSmallIcon(R.drawable.ntf_icon)
      .setLargeIcon(largeIcon)
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
    if (chatId != null) intent = intent.putExtra(ChatIdKey, chatId)
    return TaskStackBuilder.create(context).run {
      addNextIntentWithParentStack(intent)
      getPendingIntent(uniqueInt, PendingIntent.FLAG_IMMUTABLE)
    }
  }

  /**
   * Processes every action specified by [NotificationCompat.Builder.addAction] that comes with [NotificationAction]
   * and [ChatInfo.id] as [ChatIdKey] in extra
   * */
  class NtfActionReceiver: BroadcastReceiver() {
    override fun onReceive(context: Context?, intent: Intent?) {
      val chatId = intent?.getStringExtra(ChatIdKey) ?: return
      val cInfo = SimplexApp.context.chatModel.getChat(chatId)?.chatInfo
      when (intent.action) {
        NotificationAction.ACCEPT_CONTACT_REQUEST.name -> {
          if (cInfo !is ChatInfo.ContactRequest) return
          acceptContactRequest(cInfo, SimplexApp.context.chatModel)
          SimplexApp.context.chatModel.controller.ntfManager.cancelNotificationsForChat(chatId)
        }
        else -> {
          Log.e(TAG, "Unknown action. Make sure you provide action from NotificationAction enum")
        }
      }
    }
  }
}
