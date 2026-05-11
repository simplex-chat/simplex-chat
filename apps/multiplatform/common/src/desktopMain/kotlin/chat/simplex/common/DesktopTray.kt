package chat.simplex.common

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource

private data class CloseChoice(val onClose: () -> Unit, val onMinimize: () -> Unit)

private val pendingCloseChoice = mutableStateOf<CloseChoice?>(null)

fun requestCloseBehavior(onClose: () -> Unit, onMinimize: () -> Unit) {
  pendingCloseChoice.value = CloseChoice(onClose, onMinimize)
}

@Composable
fun CloseBehaviorDialog() {
  val choice = pendingCloseChoice.value ?: return
  Dialog(
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
