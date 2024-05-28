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
import java.awt.*
import java.awt.TrayIcon.MessageType
import java.io.File
import javax.imageio.ImageIO

object NtfManager {
  private val prevNtfs = arrayListOf<Pair<ChatId, Slice>>()
  private val prevNtfsMutex: Mutex = Mutex()

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
    displayNotificationViaLib(contactId, title, text, prepareIconPath(largeIcon), actions) {
      ntfManager.openChatAction(invitation.user.userId, contactId)
    }
    return true
  }

  fun showMessage(title: String, text: String) {
    displayNotificationViaLib("MESSAGE", title, text, null, emptyList()) {}
  }

  fun hasNotificationsForChat(chatId: ChatId) = false//prevNtfs.any { it.first == chatId }

  fun cancelNotificationsForChat(chatId: ChatId) {
    withBGApi {
      prevNtfsMutex.withLock {
        val ntf = prevNtfs.firstOrNull { it.first == chatId }
        if (ntf != null) {
          prevNtfs.remove(ntf)
          /*try {
            ntf.second.close()
          } catch (e: Exception) {
            // Can be java.lang.UnsupportedOperationException, for example. May do nothing
            println("Failed to close notification: ${e.stackTraceToString()}")
          }*/
        }
      }
    }
  }

  fun cancelAllNotifications() {
//    prevNtfs.forEach { try { it.second.close() } catch (e: Exception) { println("Failed to close notification: ${e.stackTraceToString()}") } }
    withBGApi {
      prevNtfsMutex.withLock {
        prevNtfs.clear()
      }
    }
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

    displayNotificationViaLib(chatId, title, content, prepareIconPath(largeIcon), actions.map { it.first.name to it.second }) {
      ntfManager.openChatAction(user.userId, chatId)
    }
  }

  private fun displayNotificationViaLib(
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
          prevNtfs.add(chatId to builder.toast())
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
      println("Failed to write an icon to tmpDir: ${e.stackTraceToString()}")
      null
    }
  } else null

  private fun displayNotification(title: String, text: String, icon: ImageBitmap?) = when (desktopPlatform) {
    DesktopPlatform.LINUX_X86_64, DesktopPlatform.LINUX_AARCH64 -> linuxDisplayNotification(title, text, prepareIconPath(icon))
    DesktopPlatform.WINDOWS_X86_64 -> windowsDisplayNotification(title, text, icon)
    DesktopPlatform.MAC_X86_64, DesktopPlatform.MAC_AARCH64 -> macDisplayNotification(title, text, prepareIconPath(icon))
  }

  private fun linuxDisplayNotification(title: String, text: String, iconPath: String?) {
    if (iconPath != null) {
      Runtime.getRuntime().exec(arrayOf("notify-send", "-i", iconPath, title, text))
    } else {
      Toast.toast(ToastType.INFO, title, text)
      Runtime.getRuntime().exec(arrayOf("notify-send", title, text))
    }
  }

  private fun windowsDisplayNotification(title: String, text: String, icon: ImageBitmap?) {
    if (SystemTray.isSupported()) {
      val tray = SystemTray.getSystemTray()
      tray.remove(tray.trayIcons.firstOrNull { it.toolTip == "SimpleX" })
      val trayIcon = TrayIcon(icon?.toAwtImage(), "SimpleX")
      trayIcon.isImageAutoSize = true
      tray.add(trayIcon)
      trayIcon.displayMessage(title, text, MessageType.INFO)
    } else {
      Log.e(TAG, "System tray not supported!")
    }
  }

  private fun macDisplayNotification(title: String, text: String, iconPath: String?) {
    Runtime.getRuntime().exec(arrayOf("osascript", "-e", """display notification "${text.replace("\"", "\\\"")}" with title "${title.replace("\"", "\\\"")}""""))
  }
}
