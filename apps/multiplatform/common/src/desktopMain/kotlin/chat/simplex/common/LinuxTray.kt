package chat.simplex.common

import androidx.compose.runtime.*
import chat.simplex.common.platform.Log
import chat.simplex.common.platform.TAG
import chat.simplex.common.platform.tmpDir
import chat.simplex.common.views.helpers.generateNewFileName
import chat.simplex.res.*
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.compose.stringResource
import dev.nucleusframework.composenativetray.tray.api.NativeTray
import dev.nucleusframework.composenativetray.utils.isMenuBarInDarkMode
import dev.nucleusframework.window.tao.ApplicationScope
import kotlinx.coroutines.Dispatchers
import java.io.File
import java.io.IOException
import javax.imageio.ImageIO
import kotlin.coroutines.EmptyCoroutineContext

private const val TRAY_ICON_PREFIX = "tray"
private const val TRAY_ICON_FORMAT = "png"

private val trayIconFiles = mutableMapOf<ImageResource, File>()

private fun trayIconFile(icon: ImageResource): File? =
  trayIconFiles[icon] ?: try {
    val file = File(tmpDir, generateNewFileName(TRAY_ICON_PREFIX, TRAY_ICON_FORMAT, tmpDir))
    ImageIO.write(icon.image, TRAY_ICON_FORMAT, file)
    trayIconFiles[icon] = file
    file
  } catch (e: IOException) {
    Log.e(TAG, "Cannot write the tray icon: ${e.message}")
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
}
