package chat.simplex.common

import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import chat.simplex.common.platform.LinuxNative
import chat.simplex.common.platform.Log
import chat.simplex.common.platform.TAG
import chat.simplex.common.platform.tmpDir
import chat.simplex.common.views.helpers.generateNewFileName
import chat.simplex.common.views.helpers.isRunningFromFlatpak
import chat.simplex.res.*
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import dev.nucleusframework.composenativetray.tray.api.NativeTray
import dev.nucleusframework.composenativetray.utils.ComposableIconUtils
import dev.nucleusframework.composenativetray.utils.IconRenderProperties
import dev.nucleusframework.composenativetray.utils.isMenuBarInDarkMode
import dev.nucleusframework.window.tao.ApplicationScope
import kotlinx.coroutines.Dispatchers
import java.io.File
import kotlin.coroutines.EmptyCoroutineContext

private const val TRAY_ICON_PREFIX = "tray"
private const val TRAY_ICON_FORMAT = "png"

private val trayIconFiles = mutableMapOf<ImageResource, File>()

private fun trayIconFile(icon: ImageResource): File? =
  trayIconFiles[icon] ?: try {
    val png = ComposableIconUtils.renderComposableToPngBytes(IconRenderProperties.forCurrentOperatingSystem()) {
      Image(painterResource(icon), null, Modifier.fillMaxSize())
    }
    val file = File(tmpDir, generateNewFileName(TRAY_ICON_PREFIX, TRAY_ICON_FORMAT, tmpDir))
    file.writeBytes(png)
    trayIconFiles[icon] = file
    file
  } catch (e: Exception) {
    Log.e(TAG, "Cannot write the tray icon: ${e.stackTraceToString()}")
    null
  }

private fun onMain(action: () -> Unit) = Dispatchers.Main.dispatch(EmptyCoroutineContext, Runnable(action))

@Composable
fun ApplicationScope.LinuxTray() {
  if (!trayIsAvailable || !trayEnabled()) return
  val unread = trayUnreadCount()
  val iconFile = trayIconFile(trayIcon(unread, isMenuBarInDarkMode())) ?: return
  val tooltip = trayTooltip(unread)
  val showLabel = stringResource(MR.strings.tray_show)
  val quitLabel = stringResource(MR.strings.tray_quit)
  val tray = remember { NativeTray() }
  LaunchedEffect(iconFile, tooltip, showLabel, quitLabel) {
    tray.update(
      iconPath = iconFile.absolutePath,
      tooltip = tooltip,
      primaryAction = { onMain { showWindow() } },
      menuContent = {
        Item(showLabel) { onMain { showWindow() } }
        Divider()
        Item(quitLabel) { onMain(::exitApplication) }
      },
    )
  }
  DisposableEffect(tray) {
    onDispose { tray.dispose() }
  }
  val hidden = !simplexWindowState.windowVisible.value
  val backgroundReason = stringResource(MR.strings.tray_background_reason)
  LaunchedEffect(hidden) {
    if (hidden) requestBackground(backgroundReason)
  }
}

private var backgroundRequested = false

private fun requestBackground(reason: String) {
  if (backgroundRequested || !isRunningFromFlatpak() || !LinuxNative.isLoaded) return
  backgroundRequested = true
  LinuxNative.nativeRequestBackground(reason)
}
