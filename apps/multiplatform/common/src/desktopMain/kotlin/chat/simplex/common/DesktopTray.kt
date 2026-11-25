package chat.simplex.common

import SectionItemView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.window.*
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.CloseBehavior
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.Log
import chat.simplex.common.platform.TAG
import chat.simplex.common.platform.desktopPlatform
import chat.simplex.common.ui.theme.isInDarkTheme
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.*
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.compose.painterResource
import dev.nucleusframework.composenativetray.utils.isLinuxTrayHostRegistered
import dev.icerock.moko.resources.compose.stringResource
import java.awt.AWTException
import java.awt.SystemTray
import java.awt.TrayIcon
import java.awt.image.BufferedImage

// Probed once at startup. False on stock GNOME ≥ JDK 21.0.3 per JDK-8322750, and
// also when SystemTray.add() fails despite isSupported() returning true (an older
// JDK pattern Compose-MP does not catch). When false: the Appearance toggle is
// hidden, the first-close dialog is skipped (Ask migrates silently to Quit), and
// the close handler treats MinimizeToTray as Quit.
val trayIsAvailable: Boolean by lazy {
  if (desktopPlatform.isLinux()) return@lazy isLinuxTrayHostRegistered().also { Log.i(TAG, "Linux tray host registered: $it") }
  if (!SystemTray.isSupported()) return@lazy false
  try {
    val tray = SystemTray.getSystemTray()
    val probe = TrayIcon(BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB))
    tray.add(probe)
    tray.remove(probe)
    true
  } catch (e: AWTException) {
    Log.w(TAG, "SystemTray probe failed: ${e.stackTraceToString()}")
    false
  } catch (e: SecurityException) {
    Log.w(TAG, "SystemTray probe denied: ${e.stackTraceToString()}")
    false
  }
}

@Composable
fun ApplicationScope.SimplexTray() {
  if (!trayIsAvailable) return
  if (!trayEnabled()) return
  val unread = trayUnreadCount()
  Tray(
    icon = painterResource(trayIcon(unread, isInDarkTheme())),
    tooltip = trayTooltip(unread),
    onAction = ::showWindow,
    menu = {
      Item(stringResource(MR.strings.tray_show), onClick = ::showWindow)
      Separator()
      Item(stringResource(MR.strings.tray_quit), onClick = { exitApplication() })
    }
  )
}

@Composable
internal fun trayEnabled(): Boolean = remember { appPrefs.closeBehavior.state }.value == CloseBehavior.MinimizeToTray

// Sum of per-profile unread (UserInfo.unreadCount, the same field UserPicker renders
// per row). Skip muted profiles unless they're the active one.
@Composable
internal fun trayUnreadCount(): Int {
  val unread by remember {
    derivedStateOf {
      ChatModel.users.sumOf {
        if (!it.user.showNtfs && !it.user.activeUser) 0 else it.unreadCount
      }
    }
  }
  return unread
}

internal fun trayIcon(unread: Int, darkPanel: Boolean): ImageResource =
  if (unread > 0) {
    if (darkPanel) MR.images.ic_simplex_tray_dot_light else MR.images.ic_simplex_tray_dot
  } else {
    if (darkPanel) MR.images.ic_simplex_tray_light else MR.images.ic_simplex
  }

@Composable
internal fun trayTooltip(unread: Int): String =
  if (unread > 0) stringResource(MR.strings.tray_tooltip_unread, unread)
  else stringResource(MR.strings.tray_tooltip)

// Renders in the main app window via AlertManager (same surface as e.g. the link
// previews confirmation). Lambdas close over the calling application's exit; if the
// app crashes while the dialog is open, the crash handler's alert replaces it, so
// stale closures never get clicked.
fun requestCloseBehavior(exit: () -> Unit) {
  val pref = appPrefs.closeBehavior
  AlertManager.shared.showAlertDialogButtonsColumn(
    title = generalGetString(MR.strings.close_behavior_dialog_title),
    text = AnnotatedString(generalGetString(MR.strings.close_behavior_dialog_text)),
    buttons = {
      Column {
        SectionItemView({
          AlertManager.shared.hideAlert()
          pref.set(CloseBehavior.Quit)
          exit()
        }) {
          Text(
            stringResource(MR.strings.close_behavior_dialog_close),
            Modifier.fillMaxWidth(),
            textAlign = TextAlign.Center,
            color = Color.Red
          )
        }
        SectionItemView({
          AlertManager.shared.hideAlert()
          pref.set(CloseBehavior.MinimizeToTray)
          simplexWindowState.windowVisible.value = false
        }) {
          Text(
            stringResource(MR.strings.close_behavior_dialog_minimize),
            Modifier.fillMaxWidth(),
            textAlign = TextAlign.Center,
            color = MaterialTheme.colors.primary
          )
        }
      }
    }
  )
}
