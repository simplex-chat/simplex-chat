package chat.simplex.common.model

import androidx.compose.ui.graphics.*
import chat.simplex.common.platform.*
import chat.simplex.common.showWindow
import chat.simplex.common.simplexWindowState
import chat.simplex.common.views.call.CallMediaType
import chat.simplex.common.views.call.RcvCallInvitation
import chat.simplex.common.views.helpers.*
import chat.simplex.res.*
import com.sshtools.twoslices.*
import com.sshtools.twoslices.impl.AWTToaster
import com.sshtools.twoslices.impl.NotificationCenterToaster
import kotlinx.coroutines.sync.Mutex
import kotlinx.coroutines.sync.withLock
import java.io.File
import javax.imageio.ImageIO

object NtfManager {
  private val prevNtfs = arrayListOf<Pair<Pair<Long, ChatId>, Slice>>()
  private val prevNtfsMutex: Mutex = Mutex()
  private val macToaster by lazy { NotificationCenterToaster(ToasterFactory.getSettings()) }
  private val windowsToaster: Toaster? by lazy {
    try {
      AWTToaster(ToasterFactory.getSettings())
    } catch (e: UnsupportedOperationException) {
      Log.e(TAG, "Windows notifications are unavailable: ${e.message}")
      null
    }
  }

  fun notifyCallInvitation(invitation: RcvCallInvitation): Boolean {
    if (simplexWindowState.windowFocused.value) return false
    val contactId = invitation.contact.id
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
    displayNotificationViaLib(invitation.user.userId, contactId, isCall = true, title, text, largeIcon, actions) {
      ntfManager.openChatAction(invitation.user.userId, contactId)
    }
    return true
  }

  fun showMessage(title: String, text: String) {
    displayNotificationViaLib(-1, "MESSAGE", isCall = false, title, text, null, emptyList()) {}
  }

  fun hasNotificationsForChat(chatId: ChatId) = desktopPlatform.isLinux() && LinuxNotifications.isShown(chatId)

  fun cancelNotificationsForChat(chatId: ChatId) {
    if (desktopPlatform.isLinux()) LinuxNotifications.close { !it.isCall && it.chatId == chatId }
    withBGApi {
      prevNtfsMutex.withLock {
        val ntf = prevNtfs.firstOrNull { (userChat) -> userChat.second == chatId }
        if (ntf != null) {
          prevNtfs.remove(ntf)
        }
      }
    }
  }

  fun cancelNotificationsForUser(userId: Long) {
    if (desktopPlatform.isLinux()) LinuxNotifications.close { it.userId == userId }
    withBGApi {
      prevNtfsMutex.withLock {
        prevNtfs.filter { (userChat) -> userChat.first == userId }.forEach {
          prevNtfs.remove(it)
        }
      }
    }
  }

  fun cancelAllNotifications() {
    if (desktopPlatform.isLinux()) LinuxNotifications.close { true }
    withBGApi {
      prevNtfsMutex.withLock {
        prevNtfs.clear()
      }
    }
  }

  fun cancelCallNotification() {
    if (desktopPlatform.isLinux()) LinuxNotifications.close { it.isCall }
  }

  fun clearUrgency() {
    if (desktopPlatform.isLinux()) LinuxNotifications.clearUrgency()
  }

  fun displayNotification(user: UserLike, chatId: String, displayName: String, msgText: String, image: String?, actions: List<Pair<NotificationAction, () -> Unit>>) {
    if (!user.showNotifications) return
    Log.d(TAG, "notifyMessageReceived $chatId")
    val previewMode = appPreferences.notificationPreviewMode.get()
    val title = if (previewMode == NotificationPreviewMode.HIDDEN.name) generalGetString(MR.strings.notification_preview_somebody) else displayName
    val content = if (previewMode != NotificationPreviewMode.MESSAGE.name) generalGetString(MR.strings.notification_preview_new_message) else msgText
    val largeIcon = when {
      actions.isEmpty() -> null
      image == null || previewMode == NotificationPreviewMode.HIDDEN.name -> MR.images.icon_foreground_common.image.toComposeImageBitmap()
      else -> base64ToBitmap(image)
    }

    displayNotificationViaLib(user.userId, chatId, isCall = false, title, content, largeIcon, actions.map { notificationActionLabel(it.first) to it.second }) {
      ntfManager.openChatAction(user.userId, chatId)
    }
  }

  private fun notificationActionLabel(action: NotificationAction): String = when (action) {
    NotificationAction.ACCEPT_CONTACT_REQUEST -> generalGetString(MR.strings.accept)
  }

  private fun displayNotificationViaLib(
    userId: Long,
    chatId: String,
    isCall: Boolean,
    title: String,
    text: String,
    icon: ImageBitmap?,
    actions: List<Pair<String, () -> Unit>>,
    defaultAction: () -> Unit
  ) {
    if (desktopPlatform.isLinux()) {
      val open = {
        showWindow()
        defaultAction()
      }
      LinuxNotifications.show(NotificationKey(userId, chatId, isCall), LinuxNotificationContent(title, text, icon, actions, open))
      return
    }
    val builder = Toast.builder()
      .title(title)
      .content(text)
    prepareIconPath(icon)?.let { builder.icon(it.absolutePath) }
    builder.defaultAction(defaultAction)
    actions.forEach {
      builder.action(it.first, it.second)
    }
    try {
      if (desktopPlatform.isMac()) {
        builder.toaster(macToaster)
      } else {
        builder.toaster(windowsToaster ?: return)
      }
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
}

internal fun prepareIconPath(icon: ImageBitmap?): File? = if (icon != null) {
  tmpDir.mkdir()
  val newFile = File(tmpDir.absolutePath + File.separator + generateNewFileName("IMG", "png", tmpDir))
  try {
    ImageIO.write(icon.toAwtImage(), "PNG", newFile.outputStream())
    newFile
  } catch (e: Exception) {
    Log.e(TAG, "Failed to write an icon to tmpDir: ${e.stackTraceToString()}")
    null
  }
} else null
