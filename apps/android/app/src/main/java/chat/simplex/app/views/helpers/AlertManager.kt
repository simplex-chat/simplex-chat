package chat.simplex.app.views.helpers

import android.util.Log
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.graphics.Color
import chat.simplex.app.R
import chat.simplex.app.TAG

class AlertManager {
  var alertViews = mutableStateListOf<(@Composable () -> Unit)>()

  fun showAlert(alert: @Composable () -> Unit) {
    Log.d(TAG, "AlertManager.showAlert")
    alertViews.add(alert)
  }

  fun hideAlert() {
    alertViews.removeLastOrNull()
  }

  fun showAlertDialogButtons(
    title: String,
    text: String? = null,
    buttons: @Composable () -> Unit,
  ) {
    val alertText: (@Composable () -> Unit)? = if (text == null) null else { -> Text(text) }
    showAlert {
      AlertDialog(
        onDismissRequest = this::hideAlert,
        title = { Text(title) },
        text = alertText,
        buttons = buttons
      )
    }
  }

  fun showAlertDialog(
    title: String,
    text: String? = null,
    confirmText: String = generalGetString(R.string.ok),
    onConfirm: (() -> Unit)? = null,
    dismissText: String = generalGetString(R.string.cancel_verb),
    onDismiss: (() -> Unit)? = null,
    onDismissRequest: (() -> Unit)? = null,
    destructive: Boolean = false
  ) {
    val alertText: (@Composable () -> Unit)? = if (text == null) null else { -> Text(text) }
    showAlert {
      AlertDialog(
        onDismissRequest = { onDismissRequest?.invoke(); hideAlert() },
        title = { Text(title) },
        text = alertText,
        confirmButton = {
          TextButton(onClick = {
            onConfirm?.invoke()
            hideAlert()
          }) { Text(confirmText, color = if (destructive) MaterialTheme.colors.error else Color.Unspecified) }
        },
        dismissButton = {
          TextButton(onClick = {
            onDismiss?.invoke()
            hideAlert()
          }) { Text(dismissText) }
        }
      )
    }
  }

  fun showAlertMsg(
    title: String, text: String? = null,
    confirmText: String = generalGetString(R.string.ok), onConfirm: (() -> Unit)? = null
  ) {
    val alertText: (@Composable () -> Unit)? = if (text == null) null else { -> Text(text) }
    showAlert {
      AlertDialog(
        onDismissRequest = this::hideAlert,
        title = { Text(title) },
        text = alertText,
        confirmButton = {
          TextButton(onClick = {
            onConfirm?.invoke()
            hideAlert()
          }) { Text(confirmText) }
        }
      )
    }
  }

  fun showAlertMsg(
    title: Int,
    text: Int? = null,
    confirmText: Int = R.string.ok,
    onConfirm: (() -> Unit)? = null
  ) = showAlertMsg(generalGetString(title), if (text != null) generalGetString(text) else null, generalGetString(confirmText), onConfirm)

  @Composable
  fun showInView() {
    remember { alertViews }.lastOrNull()?.invoke()
  }

  companion object {
    val shared = AlertManager()
  }
}