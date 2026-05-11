package chat.simplex.common

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.*
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.CloseBehavior
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.Log
import chat.simplex.common.platform.TAG
import chat.simplex.common.ui.theme.SimpleXTheme
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
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

fun showWindow() {
  simplexWindowState.windowVisible.value = true
  simplexWindowState.window?.toFront()
  simplexWindowState.window?.requestFocus()
}

@Composable
fun ApplicationScope.SimplexTray() {
  if (!trayIsAvailable) return
  if (remember { appPrefs.closeBehavior.state }.value != CloseBehavior.MinimizeToTray) return
  // Sum of per-profile unread (UserInfo.unreadCount, the same field UserPicker renders
  // per row). Inactive profiles honor per-chat notification filters
  // (SimpleXAPI.kt:2781-2783); the active profile does not (ChatModel.kt:556-561) —
  // muted-chat unreads in the active profile still count here.
  val unread by remember {
    derivedStateOf { ChatModel.users.sumOf { it.unreadCount } }
  }
  val iconRes = if (unread > 0) MR.images.ic_simplex_tray_dot else MR.images.ic_simplex
  val tooltip =
    if (unread > 0) stringResource(MR.strings.tray_tooltip_unread, unread)
    else stringResource(MR.strings.tray_tooltip)
  Tray(
    icon = painterResource(iconRes),
    tooltip = tooltip,
    onAction = ::showWindow,
    menu = {
      Item(stringResource(MR.strings.tray_show), onClick = ::showWindow)
      Separator()
      Item(stringResource(MR.strings.tray_quit), onClick = { exitApplication() })
    }
  )
}

// Top-level state so handleCloseRequest (non-Composable) can set it. Buttons act on
// the current ApplicationScope, not a captured one — otherwise a crash-recovery
// iteration would resurrect the dialog with closures bound to a torn-down scope.
private val isAskingCloseBehavior = mutableStateOf(false)

fun requestCloseBehavior() {
  isAskingCloseBehavior.value = true
}

// Called at the start of each showApp iteration so a crash that leaves the dialog
// open does not resurface it after recovery.
fun resetAskCloseBehavior() {
  isAskingCloseBehavior.value = false
}

@Composable
fun ApplicationScope.CloseBehaviorDialog() {
  if (!isAskingCloseBehavior.value) return
  val pref = appPrefs.closeBehavior
  DialogWindow(
    onCloseRequest = { /* non-dismissible: ignore X */ },
    state = rememberDialogState(width = 420.dp, height = 220.dp),
    title = stringResource(MR.strings.close_behavior_dialog_title),
    resizable = false,
  ) {
    // DialogWindow opens a separate Compose tree; MaterialTheme must be re-applied
    // so the buttons use the app theme rather than Compose's default lightColors.
    SimpleXTheme {
      Column(Modifier.padding(24.dp)) {
        Text(stringResource(MR.strings.close_behavior_dialog_text))
        Spacer(Modifier.height(24.dp))
        Row(horizontalArrangement = Arrangement.spacedBy(12.dp)) {
          Button(
            onClick = {
              isAskingCloseBehavior.value = false
              pref.set(CloseBehavior.Quit)
              exitApplication()
            },
            colors = ButtonDefaults.buttonColors(backgroundColor = MaterialTheme.colors.error),
          ) { Text(stringResource(MR.strings.close_behavior_dialog_close)) }
          Button(
            onClick = {
              isAskingCloseBehavior.value = false
              pref.set(CloseBehavior.MinimizeToTray)
              simplexWindowState.windowVisible.value = false
            },
          ) { Text(stringResource(MR.strings.close_behavior_dialog_minimize)) }
        }
      }
    }
  }
}
