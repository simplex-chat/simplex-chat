package chat.simplex.common.model

import androidx.compose.ui.graphics.*
import chat.simplex.common.platform.*
import chat.simplex.common.simplexWindowState
import chat.simplex.common.views.call.CallMediaType
import chat.simplex.common.views.call.RcvCallInvitation
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.common.views.helpers.generateNewFileName
import chat.simplex.res.MR
import java.awt.*
import java.awt.TrayIcon.MessageType
import java.io.File
import javax.imageio.ImageIO

object NtfManager {
  fun notifyCallInvitation(invitation: RcvCallInvitation) {
    if (simplexWindowState.windowFocused.value) return
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

    displayNotification(title, text, largeIcon)
  }

  fun displayNotification(user: User, chatId: String, displayName: String, msgText: String, image: String?, actions: List<NotificationAction>) {
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

    displayNotification(title, content, largeIcon)
  }

  private fun displayNotification(title: String, text: String, icon: ImageBitmap?) = when (desktopPlatform) {
    DesktopPlatform.LINUX_X86_64, DesktopPlatform.LINUX_AARCH64 -> linuxDisplayNotification(title, text, icon)
    DesktopPlatform.WINDOWS_X86_64 -> windowsDisplayNotification(title, text, icon)
    DesktopPlatform.MAC_X86_64, DesktopPlatform.MAC_AARCH64 -> macDisplayNotification(title, text, icon)
  }

  private fun linuxDisplayNotification(title: String, text: String, icon: ImageBitmap?) {
    val iconPath = if (icon != null) {
      tmpDir.mkdir()
      val newFile = File(tmpDir.absolutePath + File.separator + generateNewFileName("IMG", "png"))
      try {
        ImageIO.write(icon.toAwtImage(), "PNG", newFile.outputStream())
        newFile.absolutePath
      } catch (e: Exception) {
        println("Failed to write an icon to tmpDir: ${e.stackTraceToString()}")
        null
      }
    } else null
    if (iconPath != null) {
      Runtime.getRuntime().exec(arrayOf("notify-send", "-i", iconPath, title, text))
    } else {
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

  private fun macDisplayNotification(title: String, text: String, icon: ImageBitmap?) {
    Runtime.getRuntime().exec(arrayOf("osascript", "-e", """display notification "${text.replace("\"", "\\\"")}" with title "${title.replace("\"", "\\\"")}""""))
  }
}