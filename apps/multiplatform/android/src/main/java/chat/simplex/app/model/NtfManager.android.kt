package chat.simplex.app.model

import android.app.*
import android.app.TaskStackBuilder
import android.content.*
import android.content.pm.PackageManager
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.hardware.display.DisplayManager
import android.media.AudioAttributes
import android.net.Uri
import android.view.Display
import androidx.compose.ui.graphics.asAndroidBitmap
import androidx.core.app.*
import androidx.core.app.Person
import androidx.core.app.RemoteInput
import androidx.core.graphics.drawable.IconCompat
import chat.simplex.app.*
import chat.simplex.app.TAG
import chat.simplex.app.views.call.CallActivity
import chat.simplex.app.views.call.getKeyguardManager
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.call.CallMediaType
import chat.simplex.common.views.call.RcvCallInvitation
import kotlinx.datetime.Clock
import chat.simplex.res.MR

object NtfManager {
  const val MessageChannel: String = "chat.simplex.app.MESSAGE_NOTIFICATION"
  const val MessageGroup: String = "chat.simplex.app.MESSAGE_NOTIFICATION"
  const val OpenChatAction: String = "chat.simplex.app.OPEN_CHAT"
  const val ShowChatsAction: String = "chat.simplex.app.SHOW_CHATS"

  // DO NOT change notification channel settings / names
  const val CallChannel: String = "chat.simplex.app.CALL_NOTIFICATION_2"
  const val AcceptCallAction: String = "chat.simplex.app.ACCEPT_CALL"
  const val RejectCallAction: String = "chat.simplex.app.REJECT_CALL"
  const val EndCallAction: String = "chat.simplex.app.END_CALL"
  const val CallNotificationId: Int = -1
  private const val UserIdKey: String = "userId"
  private const val ChatIdKey: String = "chatId"
  private const val ReplyAction: String = "chat.simplex.app.NTF_REPLY"
  private const val DismissAction: String = "chat.simplex.app.NTF_DISMISS"
  private const val ReplyTextKey: String = "chat.simplex.app.NTF_REPLY_TEXT"
  private val appPreferences: AppPreferences = ChatController.appPrefs
  private val context: Context
    get() = SimplexApp.context

  fun getUserIdFromIntent(intent: Intent?): Long? {
    val userId = intent?.getLongExtra(UserIdKey, -1L)
    return if (userId == -1L || userId == null) null else userId
  }

  private val manager: NotificationManager = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
  // (UserId, ChatId) -> Time
  private var prevNtfTime = mutableMapOf<Pair<Long, ChatId>, Long>()
  private val msgNtfTimeoutMs = 30000L

  // Per-chat conversation history for MessagingStyle re-rendering (also shows the sent reply).
  // API 26-27 can't reliably extract MessagingStyle from a posted notification, so we keep our own.
  private data class NtfMessage(val text: String, val time: Long, val fromSelf: Boolean)
  private class ConversationNtf(val userId: Long, var displayName: String, var image: String?) {
    val messages = ArrayDeque<NtfMessage>()
  }
  private const val maxNtfMessages = 8
  private val conversations = java.util.concurrent.ConcurrentHashMap<ChatId, ConversationNtf>()

  init {
    if (areNotificationsEnabledInSystem()) createNtfChannelsMaybeShowAlert()
  }

  private fun callNotificationChannel(channelId: String, channelName: String): NotificationChannel {
    val callChannel = NotificationChannel(channelId, channelName, NotificationManager.IMPORTANCE_HIGH)
    val attrs = AudioAttributes.Builder()
      .setContentType(AudioAttributes.CONTENT_TYPE_SONIFICATION)
      .setUsage(AudioAttributes.USAGE_NOTIFICATION_RINGTONE)
      .build()
    val soundUri = Uri.parse(ContentResolver.SCHEME_ANDROID_RESOURCE + "://" + context.packageName + "/raw/ring_once")
    Log.d(TAG, "callNotificationChannel sound: $soundUri")
    callChannel.setSound(soundUri, attrs)
    callChannel.enableVibration(true)
    // the numbers below are explained here: https://developer.android.com/reference/android/os/Vibrator
    // (wait, vibration duration, wait till off, wait till on again = ringtone mp3 duration - vibration duration - ~50ms lost somewhere)
    callChannel.vibrationPattern = longArrayOf(250, 250, 0, 2600)
    return callChannel
  }

  fun cancelNotificationsForChat(chatId: String) {
    val key = prevNtfTime.keys.firstOrNull { it.second == chatId }
    prevNtfTime.remove(key)
    conversations.remove(chatId)
    manager.cancel(chatId.hashCode())
    val msgNtfs = manager.activeNotifications.filter { ntf ->
      ntf.notification.channelId == MessageChannel
    }
    if (msgNtfs.size <= 1) {
      // Have a group notification with no children so cancel it
      manager.cancel(0)
    }
  }

  fun cancelNotificationsForUser(userId: Long) {
    prevNtfTime.keys.filter { it.first == userId }.forEach {
      prevNtfTime.remove(it)
      manager.cancel(it.second.hashCode())
    }
    conversations.entries.removeAll { it.value.userId == userId }
    val msgNtfs = manager.activeNotifications.filter { ntf ->
      ntf.notification.channelId == MessageChannel
    }
    if (msgNtfs.size <= 1) {
      // Have a group notification with no children so cancel it
      manager.cancel(0)
    }
  }

  fun displayNotification(user: UserLike, chatId: String, displayName: String, msgText: String, image: String? = null, actions: List<NotificationAction> = emptyList(), canReply: Boolean = false) {
    if (!user.showNotifications) return
    Log.d(TAG, "notifyMessageReceived $chatId")
    val now = Clock.System.now().toEpochMilliseconds()
    val recentNotification = (now - prevNtfTime.getOrDefault(user.userId to chatId, 0) < msgNtfTimeoutMs)
    prevNtfTime[user.userId to chatId] = now
    val previewMode = appPreferences.notificationPreviewMode.get()
    val title = if (previewMode == NotificationPreviewMode.HIDDEN.name) generalGetString(MR.strings.notification_preview_somebody) else displayName
    val content = if (previewMode != NotificationPreviewMode.MESSAGE.name) generalGetString(MR.strings.notification_preview_new_message) else msgText

    val builder = NotificationCompat.Builder(context, MessageChannel)
      .setPriority(NotificationCompat.PRIORITY_HIGH)
      .setGroup(MessageGroup)
      .setGroupAlertBehavior(NotificationCompat.GROUP_ALERT_CHILDREN)
      .setSmallIcon(R.drawable.ntf_icon)
      .setColor(0x88FFFF)
      .setAutoCancel(true)
      .setContentIntent(chatPendingIntent(OpenChatAction, user.userId, chatId))

    if (canReply) {
      // Inline reply: render the conversation as MessagingStyle and attach a RemoteInput reply action.
      val convo = updateConversation(user.userId, chatId, title, image, content, now, fromSelf = false)
      builder
        .setStyle(messagingStyle(user.userId, chatId, convo))
        .addAction(replyAction(user.userId, chatId))
        .setDeleteIntent(dismissPendingIntent(chatId))
        .setSilent(recentNotification)
    } else {
      val largeIcon = when {
        actions.isEmpty() -> null
        image == null || previewMode == NotificationPreviewMode.HIDDEN.name -> BitmapFactory.decodeResource(context.resources, R.drawable.icon)
        else -> base64ToBitmap(image).asAndroidBitmap()
      }
      builder
        .setContentTitle(title)
        .setContentText(content)
        .setLargeIcon(largeIcon)
        .setVibrate(if (actions.isEmpty()) null else longArrayOf(0, 250, 250, 250))
        .setSilent(if (actions.isEmpty()) recentNotification else false)
      for (action in actions) {
        val flags = PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_IMMUTABLE
        val actionIntent = Intent(SimplexApp.context, NtfActionReceiver::class.java)
        actionIntent.action = action.name
        actionIntent.putExtra(UserIdKey, user.userId)
        actionIntent.putExtra(ChatIdKey, chatId)
        val actionPendingIntent: PendingIntent = PendingIntent.getBroadcast(SimplexApp.context, 0, actionIntent, flags)
        val actionButton = when (action) {
          NotificationAction.ACCEPT_CONTACT_REQUEST -> generalGetString(MR.strings.accept)
        }
        builder.addAction(0, actionButton, actionPendingIntent)
      }
    }
    val summary = NotificationCompat.Builder(context, MessageChannel)
      .setSmallIcon(R.drawable.ntf_icon)
      .setColor(0x88FFFF)
      .setGroup(MessageGroup)
      .setGroupAlertBehavior(NotificationCompat.GROUP_ALERT_CHILDREN)
      .setGroupSummary(true)
      .setContentIntent(chatPendingIntent(ShowChatsAction, null))
      .build()

    with(NotificationManagerCompat.from(context)) {
      // using cInfo.id only shows one notification per chat and updates it when the message arrives
      if (ActivityCompat.checkSelfPermission(SimplexApp.context, android.Manifest.permission.POST_NOTIFICATIONS) == PackageManager.PERMISSION_GRANTED) {
        notify(chatId.hashCode(), builder.build())
        notify(0, summary)
      }
    }
  }

  private fun updateConversation(userId: Long, chatId: ChatId, displayName: String, image: String?, text: String, time: Long, fromSelf: Boolean): ConversationNtf {
    val convo = conversations.computeIfAbsent(chatId) { ConversationNtf(userId, displayName, image) }
    synchronized(convo) {
      convo.displayName = displayName
      convo.image = image
      convo.messages.addLast(NtfMessage(text, time, fromSelf))
      while (convo.messages.size > maxNtfMessages) convo.messages.removeFirst()
    }
    return convo
  }

  private fun messagingStyle(userId: Long, chatId: ChatId, convo: ConversationNtf): NotificationCompat.MessagingStyle {
    val self = Person.Builder()
      .setName(generalGetString(MR.strings.notification_reply_you))
      .setKey("self_$userId")
      .build()
    val hidePreview = appPreferences.notificationPreviewMode.get() == NotificationPreviewMode.HIDDEN.name
    val senderIcon = convo.image
      ?.takeIf { !hidePreview }
      ?.let { runCatching { IconCompat.createWithBitmap(base64ToBitmap(it).asAndroidBitmap()) }.getOrNull() }
    val sender = Person.Builder()
      .setName(convo.displayName)
      .setKey(chatId)
      .apply { if (senderIcon != null) setIcon(senderIcon) }
      .build()
    val style = NotificationCompat.MessagingStyle(self)
    synchronized(convo) {
      convo.messages.forEach { m ->
        style.addMessage(NotificationCompat.MessagingStyle.Message(m.text, m.time, if (m.fromSelf) null else sender))
      }
    }
    return style
  }

  private fun replyAction(userId: Long, chatId: ChatId): NotificationCompat.Action {
    val remoteInput = RemoteInput.Builder(ReplyTextKey)
      .setLabel(generalGetString(MR.strings.notification_reply_hint))
      .build()
    val intent = Intent(SimplexApp.context, NtfActionReceiver::class.java)
      .setAction(ReplyAction)
      .putExtra(UserIdKey, userId)
      .putExtra(ChatIdKey, chatId)
    // FLAG_MUTABLE is required so the system can write the RemoteInput reply text into the intent.
    // The target is an explicit, non-exported receiver, so a mutable PendingIntent is safe here.
    val pendingIntent = PendingIntent.getBroadcast(
      SimplexApp.context, chatId.hashCode(), intent,
      PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_MUTABLE
    )
    return NotificationCompat.Action.Builder(R.drawable.ntf_icon, generalGetString(MR.strings.notification_reply_action), pendingIntent)
      .addRemoteInput(remoteInput)
      .setSemanticAction(NotificationCompat.Action.SEMANTIC_ACTION_REPLY)
      .setShowsUserInterface(false)
      .setAllowGeneratedReplies(true)
      // require device unlock before delivering the reply on a locked device (API 31+, no-op below)
      .setAuthenticationRequired(true)
      .build()
  }

  private fun dismissPendingIntent(chatId: ChatId): PendingIntent {
    val intent = Intent(SimplexApp.context, NtfActionReceiver::class.java)
      .setAction(DismissAction)
      .putExtra(ChatIdKey, chatId)
    return PendingIntent.getBroadcast(
      SimplexApp.context, "d:$chatId".hashCode(), intent,
      PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_IMMUTABLE
    )
  }

  // Re-posting the conversation notification under the same id clears the system inline-reply
  // "Sending…" progress state. Built entirely from convo (the per-chat id is user-independent).
  private fun postConversationNotification(chatId: ChatId, convo: ConversationNtf) {
    val builder = NotificationCompat.Builder(context, MessageChannel)
      .setPriority(NotificationCompat.PRIORITY_HIGH)
      .setGroup(MessageGroup)
      .setGroupAlertBehavior(NotificationCompat.GROUP_ALERT_CHILDREN)
      .setSmallIcon(R.drawable.ntf_icon)
      .setColor(0x88FFFF)
      .setAutoCancel(true)
      .setOnlyAlertOnce(true)
      .setSilent(true)
      .setContentIntent(chatPendingIntent(OpenChatAction, convo.userId, chatId))
      .setDeleteIntent(dismissPendingIntent(chatId))
      .setStyle(messagingStyle(convo.userId, chatId, convo))
      .addAction(replyAction(convo.userId, chatId))
    with(NotificationManagerCompat.from(context)) {
      if (ActivityCompat.checkSelfPermission(SimplexApp.context, android.Manifest.permission.POST_NOTIFICATIONS) == PackageManager.PERMISSION_GRANTED) {
        notify(chatId.hashCode(), builder.build())
      }
    }
  }

  fun onReplySent(chatId: ChatId, text: String) {
    val convo = conversations[chatId] ?: return
    val now = Clock.System.now().toEpochMilliseconds()
    updateConversation(convo.userId, chatId, convo.displayName, convo.image, text, now, fromSelf = true)
    postConversationNotification(chatId, convo)
  }

  fun onReplyFailed(chatId: ChatId) {
    Log.e(TAG, "ntf reply failed for $chatId")
    // Re-post without the failed text so the inline-reply progress state is cleared, then notify the user.
    conversations[chatId]?.let { postConversationNotification(chatId, it) }
    showMessage(generalGetString(MR.strings.notification_reply_failed_title), generalGetString(MR.strings.notification_reply_failed_desc))
  }

  fun notifyCallInvitation(invitation: RcvCallInvitation): Boolean {
    val keyguardManager = getKeyguardManager(context)
    Log.d(
      TAG,
      "notifyCallInvitation pre-requests: " +
          "keyguard locked ${keyguardManager.isKeyguardLocked}, " +
          "callOnLockScreen ${appPreferences.callOnLockScreen.get()}, " +
          "onForeground ${isAppOnForeground}"
    )
    if (isAppOnForeground) return false
    val contactId = invitation.contact.id
    Log.d(TAG, "notifyCallInvitation $contactId")
    val image = invitation.contact.image
    val displayManager = context.getSystemService(Context.DISPLAY_SERVICE) as DisplayManager
    val screenOff = displayManager.displays.all { it.state != Display.STATE_ON }
    var ntfBuilder =
      if ((keyguardManager.isKeyguardLocked || screenOff) && appPreferences.callOnLockScreen.get() != CallOnLockScreen.DISABLE) {
        val fullScreenIntent = Intent(context, CallActivity::class.java)
        val fullScreenPendingIntent = PendingIntent.getActivity(context, 0, fullScreenIntent, PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_IMMUTABLE)
        NotificationCompat.Builder(context, CallChannel)
          .setFullScreenIntent(fullScreenPendingIntent, true)
          .setVisibility(NotificationCompat.VISIBILITY_PUBLIC)
      } else {
        val soundUri = Uri.parse(ContentResolver.SCHEME_ANDROID_RESOURCE + "://" + context.packageName + "/raw/ring_once")
        val fullScreenPendingIntent = PendingIntent.getActivity(context, 0, Intent(), PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_IMMUTABLE)
        NotificationCompat.Builder(context, CallChannel)
          .setContentIntent(chatPendingIntent(OpenChatAction, invitation.user.userId, invitation.contact.id))
          .addAction(R.drawable.ntf_icon, generalGetString(MR.strings.accept), chatPendingIntent(AcceptCallAction, invitation.user.userId, contactId))
          .addAction(R.drawable.ntf_icon, generalGetString(MR.strings.reject), chatPendingIntent(RejectCallAction, invitation.user.userId, contactId, true))
          .setFullScreenIntent(fullScreenPendingIntent, true)
          .setSound(soundUri)
      }
    val text = generalGetString(
      if (invitation.callType.media == CallMediaType.Video) {
        if (invitation.sharedKey == null) MR.strings.video_call_no_encryption else MR.strings.encrypted_video_call
      } else {
        if (invitation.sharedKey == null) MR.strings.audio_call_no_encryption else MR.strings.encrypted_audio_call
      }
    )
    val previewMode = appPreferences.notificationPreviewMode.get()
    val title = if (previewMode == NotificationPreviewMode.HIDDEN.name)
      generalGetString(MR.strings.notification_preview_somebody)
    else
      invitation.contact.displayName
    val largeIcon = if (image == null || previewMode == NotificationPreviewMode.HIDDEN.name)
      BitmapFactory.decodeResource(context.resources, R.drawable.icon)
    else
      base64ToBitmap(image).asAndroidBitmap()

    ntfBuilder = ntfBuilder
      .setContentTitle(title)
      .setContentText(text)
      .setPriority(NotificationCompat.PRIORITY_HIGH)
      .setCategory(NotificationCompat.CATEGORY_CALL)
      .setSmallIcon(R.drawable.ntf_icon)
      .setLargeIcon(largeIcon)
      .setColor(0x88FFFF)
      .setAutoCancel(true)
    val notification = ntfBuilder.build()
    // This makes notification sound and vibration repeat endlessly
    notification.flags = notification.flags or NotificationCompat.FLAG_INSISTENT
    with(NotificationManagerCompat.from(context)) {
      if (ActivityCompat.checkSelfPermission(SimplexApp.context, android.Manifest.permission.POST_NOTIFICATIONS) == PackageManager.PERMISSION_GRANTED) {
        notify(CallNotificationId, notification)
      }
    }
    return true
  }

  fun showMessage(title: String, text: String) {
    val builder = NotificationCompat.Builder(context, MessageChannel)
      .setContentTitle(title)
      .setContentText(text)
      .setPriority(NotificationCompat.PRIORITY_HIGH)
      .setGroup(MessageGroup)
      .setGroupAlertBehavior(NotificationCompat.GROUP_ALERT_CHILDREN)
      .setSmallIcon(R.drawable.ntf_icon)
      .setLargeIcon(null as Bitmap?)
      .setColor(0x88FFFF)
      .setAutoCancel(true)
      .setVibrate(null)
      .setContentIntent(chatPendingIntent(ShowChatsAction, null, null))
      .setSilent(false)

    val summary = NotificationCompat.Builder(context, MessageChannel)
      .setSmallIcon(R.drawable.ntf_icon)
      .setColor(0x88FFFF)
      .setGroup(MessageGroup)
      .setGroupAlertBehavior(NotificationCompat.GROUP_ALERT_CHILDREN)
      .setGroupSummary(true)
      .setContentIntent(chatPendingIntent(ShowChatsAction, null))
      .build()

    with(NotificationManagerCompat.from(context)) {
      if (ActivityCompat.checkSelfPermission(SimplexApp.context, android.Manifest.permission.POST_NOTIFICATIONS) == PackageManager.PERMISSION_GRANTED) {
        notify("MESSAGE".hashCode(), builder.build())
        notify(0, summary)
      }
    }
  }

  fun cancelCallNotification() {
    manager.cancel(CallNotificationId)
  }

  fun cancelAllNotifications() {
    manager.cancelAll()
  }

  fun hasNotificationsForChat(chatId: String): Boolean = manager.activeNotifications.any { it.id == chatId.hashCode() }

  private fun chatPendingIntent(intentAction: String, userId: Long?, chatId: String? = null, broadcast: Boolean = false): PendingIntent {
    Log.d(TAG, "chatPendingIntent for $intentAction")
    val uniqueInt = (System.currentTimeMillis() and 0xfffffff).toInt()
    var intent = Intent(context, if (!broadcast) MainActivity::class.java else NtfActionReceiver::class.java)
      .addFlags(Intent.FLAG_ACTIVITY_NEW_TASK or Intent.FLAG_ACTIVITY_SINGLE_TOP or Intent.FLAG_ACTIVITY_CLEAR_TOP)
      .setAction(intentAction)
      .putExtra(UserIdKey, userId)
    if (chatId != null) intent = intent.putExtra(ChatIdKey, chatId)
    return if (!broadcast) {
      TaskStackBuilder.create(context).run {
        addNextIntentWithParentStack(intent)
        getPendingIntent(uniqueInt, PendingIntent.FLAG_IMMUTABLE)
      }
    } else {
      PendingIntent.getBroadcast(SimplexApp.context, uniqueInt, intent, PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_IMMUTABLE)
    }
  }

  fun areNotificationsEnabledInSystem() = manager.areNotificationsEnabled()

  /**
   * This function creates notifications channels. On Android 13+ calling it for the first time will trigger system alert,
   * The alert asks a user to allow or disallow to show notifications for the app. That's why it should be called only when the user
   * already saw such alert or when you want to trigger showing the alert.
   * On the first app launch the channels will be created after user profile is created. Subsequent calls will create new channels and delete
   * old ones if needed
   * */
  fun createNtfChannelsMaybeShowAlert() {
    manager.createNotificationChannel(NotificationChannel(MessageChannel, generalGetString(MR.strings.ntf_channel_messages), NotificationManager.IMPORTANCE_HIGH))
    manager.createNotificationChannel(callNotificationChannel(CallChannel, generalGetString(MR.strings.ntf_channel_calls)))
    // Remove old channels since they can't be edited
    manager.deleteNotificationChannel("chat.simplex.app.CALL_NOTIFICATION")
    manager.deleteNotificationChannel("chat.simplex.app.CALL_NOTIFICATION_1")
    manager.deleteNotificationChannel("chat.simplex.app.LOCK_SCREEN_CALL_NOTIFICATION")
  }

  /**
   * Processes every action specified by [NotificationCompat.Builder.addAction] that comes with [NotificationAction]
   * and [ChatInfo.id] as [ChatIdKey] in extra
   * */
  class NtfActionReceiver: BroadcastReceiver() {
    override fun onReceive(context: Context?, intent: Intent?) {
      val userId = getUserIdFromIntent(intent)
      val chatId = intent?.getStringExtra(ChatIdKey) ?: return
      val m = SimplexApp.context.chatModel
      when (intent.action) {
        ReplyAction -> {
          val text = RemoteInput.getResultsFromIntent(intent)?.getCharSequence(ReplyTextKey)?.toString()
          if (text.isNullOrBlank()) return
          val pending = goAsync()
          withBGApi {
            try {
              if (sendNtfReply(rhId = null, userId = userId, chatId = chatId, text = text)) onReplySent(chatId, text)
              else onReplyFailed(chatId)
            } catch (e: Throwable) {
              Log.e(TAG, "ntf reply error: ${e.stackTraceToString()}")
              onReplyFailed(chatId)
            } finally {
              pending.finish()
            }
          }
        }
        DismissAction -> conversations.remove(chatId)
        NotificationAction.ACCEPT_CONTACT_REQUEST.name -> ntfManager.acceptContactRequestAction(userId, incognito = false, chatId)
        RejectCallAction -> {
          val invitation = m.callInvitations[chatId]
          if (invitation != null) {
            m.callManager.endCall(invitation = invitation)
          }
        }

        else -> {
          Log.e(TAG, "Unknown action. Make sure you provide action from NotificationAction enum")
        }
      }
    }
  }
}
