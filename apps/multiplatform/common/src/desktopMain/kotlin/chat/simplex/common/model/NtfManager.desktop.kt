package chat.simplex.common.model

import androidx.compose.ui.graphics.*
import chat.simplex.common.platform.*
import chat.simplex.common.simplexWindowState
import chat.simplex.common.views.call.CallMediaType
import chat.simplex.common.views.call.RcvCallInvitation
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import com.sshtools.twoslices.*
import kotlinx.coroutines.sync.Mutex
import kotlinx.coroutines.sync.withLock
import java.io.File
import javax.imageio.ImageIO

object NtfManager {
  private val prevNtfs = arrayListOf<Pair<Pair<Long, ChatId>, Slice>>()
  private val prevNtfsMutex: Mutex = Mutex()
  private val nativeRoutes = mutableSetOf<NotificationRoute>()

  fun initializeNativeNotifications() {
    if (desktopPlatform.isMac()) MacOSNotifications.initialize()
  }

  fun notifyCallInvitation(invitation: RcvCallInvitation): Boolean {
    val contactId = invitation.contact.id
    if (
      simplexWindowState.windowFocused.value &&
      chatModel.currentUser.value?.userId == invitation.user.userId &&
      chatModel.remoteHostId() == invitation.remoteHostId &&
      chatModel.chatId.value == contactId
    ) return false
    Log.d(TAG, "notifyCallInvitation $contactId")
    val image = invitation.contact.image
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
      MR.images.icon_foreground_common.image.toComposeImageBitmap()
    else
      base64ToBitmap(image)

    val actions = listOf(
      generalGetString(MR.strings.accept) to { ntfManager.acceptCallAction(invitation.contact.id) },
      generalGetString(MR.strings.reject) to { ChatModel.callManager.endCall(invitation = invitation) }
    )
    if (desktopPlatform.isMac()) {
      val route = NotificationRoute(invitation.user.userId, invitation.remoteHostId, contactId)
      deliverMacNotification(route, title, text, DesktopNotificationCategory.INCOMING_CALL, prepareIconPath(largeIcon))
    } else {
      displayNotificationViaLib(invitation.user.userId, contactId, title, text, prepareIconPath(largeIcon), actions) {
        ntfManager.openChatAction(invitation.user.userId, contactId)
      }
    }
    return true
  }

  fun showMessage(title: String, text: String) {
    if (desktopPlatform.isMac()) {
      deliverMacNotification(NotificationRoute(null, null, "MESSAGE"), title, text, DesktopNotificationCategory.MESSAGE, null)
    } else {
      displayNotificationViaLib(-1, "MESSAGE", title, text, null, emptyList()) {}
    }
  }

  fun hasNotificationsForChat(chatId: ChatId) = if (desktopPlatform.isMac()) synchronized(nativeRoutes) { nativeRoutes.any { it.chatId == chatId } } else false

  fun cancelNotificationsForChat(chatId: ChatId) {
    if (desktopPlatform.isMac()) {
      val matches = synchronized(nativeRoutes) { nativeRoutes.filter { it.chatId == chatId }.also(nativeRoutes::removeAll) }
      matches.forEach { MacOSNotifications.removeChat(it.userId, it.remoteHostId, it.chatId) }
      return
    }
    withBGApi {
      prevNtfsMutex.withLock {
        val ntf = prevNtfs.firstOrNull { (userChat) -> userChat.second == chatId }
        if (ntf != null) {
          prevNtfs.remove(ntf)
          /*try {
            ntf.second.close()
          } catch (e: Exception) {
            // Can be java.lang.UnsupportedOperationException, for example. May do nothing
            Log.e(TAG, "Failed to close notification: ${e.stackTraceToString()}")
          }*/
        }
      }
    }
  }

  fun cancelNotificationsForUser(userId: Long) {
    if (desktopPlatform.isMac()) {
      synchronized(nativeRoutes) { nativeRoutes.removeAll { it.userId == userId } }
      MacOSNotifications.removeUser(userId)
      return
    }
    withBGApi {
      prevNtfsMutex.withLock {
        prevNtfs.filter { (userChat) -> userChat.first == userId }.forEach {
          prevNtfs.remove(it)
        }
      }
    }
  }

  fun cancelAllNotifications() {
    if (desktopPlatform.isMac()) {
      synchronized(nativeRoutes) { nativeRoutes.clear() }
      MacOSNotifications.removeAll()
      return
    }
//    prevNtfs.forEach { try { it.second.close() } catch (e: Exception) { Log.e(TAG, "Failed to close notification: ${e
    //    .stackTraceToString()}") } }
    withBGApi {
      prevNtfsMutex.withLock {
        prevNtfs.clear()
      }
    }
  }

  fun displayNotification(user: UserLike, chatId: String, displayName: String, msgText: String, image: String?, actions: List<Pair<NotificationAction, () -> Unit>>, remoteHostId: Long?, messageId: Long?) {
    if (!user.showNotifications) return
    Log.d(TAG, "notifyMessageReceived $chatId")
    val previewMode = runCatching {
      NotificationPreviewMode.valueOf(appPreferences.notificationPreviewMode.get() ?: NotificationPreviewMode.default.name)
    }.getOrDefault(NotificationPreviewMode.default)
    val preview = desktopNotificationPreview(
      previewMode,
      displayName,
      msgText,
      generalGetString(MR.strings.notification_preview_somebody),
      generalGetString(MR.strings.notification_preview_new_message),
    )
    val largeIcon = when {
      actions.isEmpty() -> null
      image == null || previewMode == NotificationPreviewMode.HIDDEN -> MR.images.icon_foreground_common.image.toComposeImageBitmap()
      else -> base64ToBitmap(image)
    }

    if (desktopPlatform.isMac()) {
      val category = if (actions.any { it.first == NotificationAction.ACCEPT_CONTACT_REQUEST }) DesktopNotificationCategory.CONTACT_REQUEST else DesktopNotificationCategory.MESSAGE
      deliverMacNotification(NotificationRoute(user.userId, remoteHostId, chatId, messageId), preview.title, preview.body, category, prepareIconPath(largeIcon))
    } else {
      displayNotificationViaLib(user.userId, chatId, preview.title, preview.body, prepareIconPath(largeIcon), actions.map { it.first.name to it.second }) {
        ntfManager.openChatAction(user.userId, chatId)
      }
    }
  }

  private fun deliverMacNotification(
    route: NotificationRoute,
    title: String,
    content: String,
    category: DesktopNotificationCategory,
    imagePath: String?,
  ) {
    val identifier = desktopNotificationIdentifier(route, System.currentTimeMillis())
    val payload = DesktopNotificationPayload(
      identifier = identifier,
      route = route,
      title = title,
      preview = content,
      category = category,
      playSound = appPreferences.desktopNotificationSound.get(),
      imagePath = imagePath,
    )
    if (MacOSNotifications.deliver(payload)) synchronized(nativeRoutes) { nativeRoutes.add(route) }
  }

  private fun displayNotificationViaLib(
    userId: Long,
    chatId: String,
    title: String,
    text: String,
    iconPath: String?,
    actions: List<Pair<String, () -> Unit>>,
    defaultAction: (() -> Unit)?
  ) {
    val builder = Toast.builder()
      .title(title)
      .content(text)
    if (iconPath != null) {
      builder.icon(iconPath)
    }
    if (defaultAction != null) {
      builder.defaultAction(defaultAction)
    }
    actions.forEach {
      builder.action(it.first, it.second)
    }
    try {
      withBGApi {
        prevNtfsMutex.withLock {
          prevNtfs.add(Pair(userId, chatId) to builder.toast())
        }
      }
    } catch (e: Throwable) {
      Log.e(TAG, e.stackTraceToString())
      if (e !is Exception) {
        val text = e.stackTraceToString().lines().getOrNull(0) ?: ""
        showToast(generalGetString(MR.strings.error_showing_desktop_notification) + " " + text, 4_000)
      }
    }
  }

  private fun prepareIconPath(icon: ImageBitmap?): String? = if (icon != null) {
    tmpDir.mkdir()
    val newFile = File(tmpDir.absolutePath + File.separator + generateNewFileName("IMG", "png", tmpDir))
    try {
      ImageIO.write(icon.toAwtImage(), "PNG", newFile.outputStream())
      newFile.absolutePath
    } catch (e: Exception) {
      Log.e(TAG, "Failed to write an icon to tmpDir: ${e.stackTraceToString()}")
      null
    }
  } else null

}
