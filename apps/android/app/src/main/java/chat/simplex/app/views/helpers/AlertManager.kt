package chat.simplex.app.views.helpers

import android.util.Log
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.*
import androidx.compose.ui.window.Dialog
import chat.simplex.app.R
import chat.simplex.app.TAG
import chat.simplex.app.ui.theme.DEFAULT_PADDING

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

  fun showAlertDialogButtonsColumn(
    title: String,
    text: String? = null,
    buttons: @Composable () -> Unit,
  ) {
    showAlert {
      Dialog(onDismissRequest = this::hideAlert) {
        Column(Modifier.background(MaterialTheme.colors.background)) {
          Text(title, Modifier.padding(DEFAULT_PADDING), fontSize = 18.sp)
          if (text != null) {
            Text(text)
          }
          CompositionLocalProvider(LocalContentAlpha provides ContentAlpha.high) {
            buttons()
          }
        }
      }
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

  fun showAlertDialogStacked(
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
        buttons = {
          Column(
            Modifier.fillMaxWidth().padding(horizontal = 8.dp).padding(top = 16.dp, bottom = 2.dp),
            horizontalAlignment = Alignment.End
          ) {
            TextButton(onClick = {
              onDismiss?.invoke()
              hideAlert()
            }) { Text(dismissText) }
            TextButton(onClick = {
              onConfirm?.invoke()
              hideAlert()
            }) { Text(confirmText, color = if (destructive) MaterialTheme.colors.error else Color.Unspecified) }
          }
        },
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
