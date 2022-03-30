package chat.simplex.app.views.helpers

import android.util.Log
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.mutableStateOf
import chat.simplex.app.TAG

class AlertManager {
  var alertView = mutableStateOf<(@Composable () -> Unit)?>(null)
  var presentAlert = mutableStateOf<Boolean>(false)

  fun showAlert(alert: @Composable () -> Unit) {
    Log.d(TAG, "AlertManager.showAlert")
    alertView.value = alert
    presentAlert.value = true
  }

  fun hideAlert() {
    presentAlert.value = false
    alertView.value = null
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
    confirmText: String = "Ok",
    onConfirm: (() -> Unit)? = null,
    dismissText: String = "Cancel",
    onDismiss: (() -> Unit)? = null
  ) {
    val alertText: (@Composable () -> Unit)? = if (text == null) null else { -> Text(text) }
    showAlert {
      AlertDialog(
        onDismissRequest = this::hideAlert,
        title = { Text(title) },
        text = alertText,
        confirmButton = {
          Button(onClick = {
            onConfirm?.invoke()
            hideAlert()
          }) { Text(confirmText) }
        },
        dismissButton = {
          Button(onClick = {
            onDismiss?.invoke()
            hideAlert()
          }) { Text(dismissText) }
        }
      )
    }
  }

  fun showAlertMsg(
    title: String, text: String? = null,
    confirmText: String = "Ok", onConfirm: (() -> Unit)? = null
  ) {
    val alertText: (@Composable () -> Unit)? = if (text == null) null else { -> Text(text) }
    showAlert {
      AlertDialog(
        onDismissRequest = this::hideAlert,
        title = { Text(title) },
        text = alertText,
        confirmButton = {
          Button(onClick = {
            onConfirm?.invoke()
            hideAlert()
          }) { Text(confirmText) }
        }
      )
    }
  }

  @Composable
  fun showInView() {
    if (presentAlert.value) alertView.value?.invoke()
  }

  companion object {
    val shared = AlertManager()
  }
}