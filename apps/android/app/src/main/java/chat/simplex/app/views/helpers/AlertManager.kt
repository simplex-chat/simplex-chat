package chat.simplex.app.views.helpers

import android.util.Log
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.ExperimentalComposeUiApi
import androidx.compose.ui.Modifier
import chat.simplex.app.R
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
    confirmText: String = generalGetString(R.string.ok),
    onConfirm: (() -> Unit)? = null,
    dismissText: String = generalGetString(R.string.cancel_verb),
    onDismiss: (() -> Unit)? = null
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

  @OptIn(ExperimentalComposeUiApi::class)
  @Composable
  fun showTextAlertDialog(
    title: String,
    initialValue: String,
    onValueChange: (String) -> Unit,
    confirmText: String,
    dismissText: String?,
    onConfirm: ((String) -> Unit)? = null,
    onDismiss: (() -> Unit)? = null
  ) {
    var value by remember { mutableStateOf(initialValue) }

    showAlert {
      AlertDialog(
        onDismissRequest = { hideAlert(); onDismiss?.invoke() },
        title = { Text(title) },
        text = {
          DefaultBasicTextField(
            Modifier,
            initialValue,
            "",
            selectTextOnFocus = true,
          )
          {
            value = it
            onValueChange(it)
          }
        },
        confirmButton = {
          TextButton(onClick = {
            onConfirm?.invoke(value)
            hideAlert()
          }) { Text(confirmText) }
        },
        dismissButton = if (dismissText != null) {
          {
            TextButton(onClick = {
              onDismiss?.invoke()
              hideAlert()
            }) { Text(dismissText) }
          }
        } else null
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

  @Composable
  fun showInView() {
    if (presentAlert.value) alertView.value?.invoke()
  }

  companion object {
    val shared = AlertManager()
  }
}