package chat.simplex.common

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.*
import chat.simplex.common.model.CloseBehavior
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import java.awt.SystemTray

// Probed once at startup. Returns false on stock GNOME ≥ JDK 21.0.3 per JDK-8322750.
// When false the Appearance toggle is hidden, the first-close dialog skips the
// "Minimize to tray" button, and the close handler treats MinimizeToTray as Quit.
val trayIsAvailable: Boolean by lazy { SystemTray.isSupported() }

fun showWindow() {
  simplexWindowState.windowVisible.value = true
  simplexWindowState.window?.toFront()
  simplexWindowState.window?.requestFocus()
}

@Composable
fun ApplicationScope.SimplexTray(closedByError: MutableState<Boolean>) {
  if (!trayIsAvailable) return
  if (appPrefs.closeBehavior.state.value != CloseBehavior.MinimizeToTray) return
  Tray(
    icon = painterResource(MR.images.ic_simplex_tray),
    tooltip = stringResource(MR.strings.tray_tooltip),
    onAction = ::showWindow,
    menu = {
      Item(stringResource(MR.strings.tray_show), onClick = ::showWindow)
      Separator()
      Item(stringResource(MR.strings.tray_quit), onClick = {
        closedByError.value = false
        exitApplication()
      })
    }
  )
}

private data class CloseChoice(val onClose: () -> Unit, val onMinimize: () -> Unit)

private val pendingCloseChoice = mutableStateOf<CloseChoice?>(null)

fun requestCloseBehavior(onClose: () -> Unit, onMinimize: () -> Unit) {
  pendingCloseChoice.value = CloseChoice(onClose, onMinimize)
}

@Composable
fun CloseBehaviorDialog() {
  val choice = pendingCloseChoice.value ?: return
  DialogWindow(
    onCloseRequest = { /* non-dismissible: ignore X */ },
    state = rememberDialogState(width = 420.dp, height = 220.dp),
    title = stringResource(MR.strings.close_behavior_dialog_title),
    resizable = false,
  ) {
    Column(Modifier.padding(24.dp)) {
      Text(stringResource(MR.strings.close_behavior_dialog_text))
      Spacer(Modifier.height(24.dp))
      Row(horizontalArrangement = Arrangement.spacedBy(12.dp)) {
        Button(
          onClick = { pendingCloseChoice.value = null; choice.onClose() },
          colors = ButtonDefaults.buttonColors(backgroundColor = MaterialTheme.colors.error),
        ) { Text(stringResource(MR.strings.close_behavior_dialog_close)) }
        Button(
          onClick = { pendingCloseChoice.value = null; choice.onMinimize() },
          colors = ButtonDefaults.buttonColors(backgroundColor = MaterialTheme.colors.primary),
        ) { Text(stringResource(MR.strings.close_behavior_dialog_minimize)) }
      }
    }
  }
}
