package chat.simplex.common.model

import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.toAwtImage
import chat.simplex.common.platform.Log
import chat.simplex.common.platform.TAG
import chat.simplex.common.platform.linuxDesktopEntryId
import dev.nucleusframework.freedesktop.icons.FreedesktopIcon
import dev.nucleusframework.launcher.linux.LinuxLauncherEntry
import dev.nucleusframework.notification.linux.CloseReason
import dev.nucleusframework.notification.linux.ImageData
import dev.nucleusframework.notification.linux.LinuxNotificationCenter
import dev.nucleusframework.notification.linux.LinuxNotificationListener
import dev.nucleusframework.notification.linux.Notification
import dev.nucleusframework.notification.linux.NotificationAction
import dev.nucleusframework.notification.linux.NotificationHints
import dev.nucleusframework.notification.linux.Urgency
import kotlinx.coroutines.Dispatchers
import java.awt.image.BufferedImage
import java.io.File
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.Executors
import kotlin.coroutines.EmptyCoroutineContext

private const val APP_NAME = "SimpleX Chat"
private const val BODY_MARKUP = "body-markup"
private const val MESSAGE_CATEGORY = "im.received"
private const val CALL_CATEGORY = "call.incoming"
private const val ACTION_KEY_PREFIX = "action-"
private const val FLATPAK_INFO = "/.flatpak-info"
private const val DESKTOP_FILE_SUFFIX = ".desktop"
private const val RGBA_CHANNELS = 4

internal data class NotificationKey(val userId: Long, val chatId: String, val isCall: Boolean)

internal class LinuxNotificationContent(
  val title: String,
  val body: String,
  val image: ImageBitmap?,
  val actions: List<Pair<String, () -> Unit>>,
  val open: () -> Unit,
)

internal fun escapeNotificationMarkup(text: String): String =
  text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

internal fun notificationActionKeys(count: Int): List<String> = List(count) { "$ACTION_KEY_PREFIX$it" }

internal fun imageData(image: BufferedImage): ImageData {
  val width = image.width
  val height = image.height
  val pixels = ByteArray(width * height * RGBA_CHANNELS)
  var offset = 0
  for (y in 0 until height) {
    for (x in 0 until width) {
      val argb = image.getRGB(x, y)
      pixels[offset++] = (argb shr 16).toByte()
      pixels[offset++] = (argb shr 8).toByte()
      pixels[offset++] = argb.toByte()
      pixels[offset++] = (argb ushr 24).toByte()
    }
  }
  return ImageData(width = width, height = height, rowstride = width * RGBA_CHANNELS, hasAlpha = true, data = pixels)
}

internal object LinuxNotifications : LinuxNotificationListener {
  private val executor = Executors.newSingleThreadExecutor { Thread(it, "simplex-notifications").apply { isDaemon = true } }
  private val shown = ConcurrentHashMap<NotificationKey, Int>()
  private val handlers = ConcurrentHashMap<Int, Map<String, () -> Unit>>()
  private val iconFiles = ConcurrentHashMap<Int, File>()
  private val inSandbox = File(FLATPAK_INFO).exists()
  private val bodyMarkup: Boolean by lazy { BODY_MARKUP in LinuxNotificationCenter.getCapabilities() }
  private val launcherUri: String? by lazy { linuxDesktopEntryId?.let { LinuxLauncherEntry.appUri(it + DESKTOP_FILE_SUFFIX) } }

  init {
    LinuxNotificationCenter.addListener(this)
  }

  fun show(key: NotificationKey, content: LinuxNotificationContent) = executor.execute {
    val replacesId = shown[key] ?: 0
    val actionKeys = notificationActionKeys(content.actions.size)
    val iconFile = if (inSandbox) prepareIconPath(content.image) else null
    val id = LinuxNotificationCenter.notify(
      Notification(
        appName = APP_NAME,
        replacesId = replacesId,
        summary = content.title,
        body = if (bodyMarkup) escapeNotificationMarkup(content.body) else content.body,
        actions = listOf(NotificationAction(NotificationAction.DEFAULT_KEY, "")) +
          actionKeys.zip(content.actions) { actionKey, (label, _) -> NotificationAction(actionKey, label) },
        hints = NotificationHints(
          urgency = if (key.isCall) Urgency.CRITICAL else Urgency.NORMAL,
          category = if (key.isCall) CALL_CATEGORY else MESSAGE_CATEGORY,
          desktopEntry = linuxDesktopEntryId,
          imageData = if (inSandbox) null else content.image?.let { imageData(it.toAwtImage()) },
          imagePath = iconFile?.let { FreedesktopIcon.Custom(it.absolutePath) },
          resident = key.isCall,
        ),
      )
    )
    if (id == 0) {
      Log.e(TAG, "Linux notification was not shown")
      iconFile?.delete()
      return@execute
    }
    if (replacesId != 0 && replacesId != id) forget(replacesId)
    shown[key] = id
    handlers[id] = mapOf(NotificationAction.DEFAULT_KEY to content.open) + actionKeys.zip(content.actions) { actionKey, (_, action) -> actionKey to action }
    iconFile?.let { iconFiles.put(id, it)?.delete() }
    if (!key.isCall) launcherUri?.let { LinuxLauncherEntry.setUrgent(it, true) }
  }

  fun isShown(chatId: String): Boolean = shown.keys.any { !it.isCall && it.chatId == chatId }

  fun close(matches: (NotificationKey) -> Boolean) = executor.execute {
    shown.filterKeys(matches).forEach { (key, id) ->
      LinuxNotificationCenter.closeNotification(id)
      shown.remove(key)
      forget(id)
    }
  }

  fun clearUrgency() = executor.execute {
    launcherUri?.let { LinuxLauncherEntry.setUrgent(it, false) }
  }

  override fun onActionInvoked(notificationId: Int, actionKey: String) {
    val action = handlers[notificationId]?.get(actionKey) ?: return
    Dispatchers.Main.dispatch(EmptyCoroutineContext) { action() }
  }

  override fun onClosed(notificationId: Int, reason: CloseReason) = executor.execute {
    shown.entries.removeIf { it.value == notificationId }
    forget(notificationId)
  }

  private fun forget(id: Int) {
    handlers.remove(id)
    iconFiles.remove(id)?.delete()
  }
}
