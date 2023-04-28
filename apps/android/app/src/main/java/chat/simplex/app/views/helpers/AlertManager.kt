package chat.simplex.app.views.helpers

import android.util.Log
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CornerSize
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.*
import androidx.compose.ui.window.Dialog
import chat.simplex.app.R
import chat.simplex.app.TAG
import chat.simplex.app.ui.theme.*

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
    showAlert {
      AlertDialog(
        onDismissRequest = this::hideAlert,
        title = alertTitle(title),
        text = alertText(text),
        buttons = buttons,
        shape = RoundedCornerShape(corner = CornerSize(25.dp))
      )
    }
  }

  fun showAlertDialogButtonsColumn(
    title: String,
    text: AnnotatedString? = null,
    buttons: @Composable () -> Unit,
  ) {
    showAlert {
      Dialog(onDismissRequest = this::hideAlert) {
        Column(
          Modifier
            .background(MaterialTheme.colors.surface, RoundedCornerShape(corner = CornerSize(25.dp)))
            .padding(bottom = DEFAULT_PADDING)
        ) {
          Text(
            title,
            Modifier.fillMaxWidth().padding(vertical = DEFAULT_PADDING),
            textAlign = TextAlign.Center,
            fontSize = 20.sp
          )
          CompositionLocalProvider(LocalContentAlpha provides ContentAlpha.high) {
            if (text != null) {
              Text(text, Modifier.fillMaxWidth().padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING, bottom = DEFAULT_PADDING * 1.5f), fontSize = 16.sp, textAlign = TextAlign.Center, color = MaterialTheme.colors.secondary)
            }
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
    showAlert {
      AlertDialog(
        onDismissRequest = { onDismissRequest?.invoke(); hideAlert() },
        title = alertTitle(title),
        text = alertText(text),
        buttons = {
          Row (
            Modifier.fillMaxWidth().padding(horizontal = DEFAULT_PADDING).padding(bottom = DEFAULT_PADDING_HALF),
            horizontalArrangement = Arrangement.SpaceBetween
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
        shape = RoundedCornerShape(corner = CornerSize(25.dp))
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
    showAlert {
      AlertDialog(
        onDismissRequest = { onDismissRequest?.invoke(); hideAlert() },
        title = alertTitle(title),
        text = alertText(text),
        buttons = {
          Column(
            Modifier.fillMaxWidth().padding(horizontal = DEFAULT_PADDING_HALF).padding(top = DEFAULT_PADDING, bottom = 2.dp),
            horizontalAlignment = Alignment.CenterHorizontally
          ) {
            TextButton(onClick = {
              onDismiss?.invoke()
              hideAlert()
            }) { Text(dismissText) }
            TextButton(onClick = {
              onConfirm?.invoke()
              hideAlert()
            }) { Text(confirmText, color = if (destructive) Color.Red else Color.Unspecified, textAlign = TextAlign.End) }
          }
        },
        shape = RoundedCornerShape(corner = CornerSize(25.dp))
      )
    }
  }

  fun showAlertMsg(
    title: String, text: String? = null,
    confirmText: String = generalGetString(R.string.ok)
  ) {
    showAlert {
      AlertDialog(
        onDismissRequest = this::hideAlert,
        title = alertTitle(title),
        text = alertText(text),
        buttons = {
          Row(
            Modifier.fillMaxWidth().padding(horizontal = DEFAULT_PADDING).padding(bottom = DEFAULT_PADDING_HALF),
            horizontalArrangement = Arrangement.Center
          ) {
            TextButton(onClick = {
              hideAlert()
            }) { Text(confirmText, color = Color.Unspecified) }
          }
        },
        shape = RoundedCornerShape(corner = CornerSize(25.dp))
      )
    }
  }
  fun showAlertMsg(
    title: Int,
    text: Int? = null,
    confirmText: Int = R.string.ok,
  ) = showAlertMsg(generalGetString(title), if (text != null) generalGetString(text) else null, generalGetString(confirmText))

  @Composable
  fun showInView() {
    remember { alertViews }.lastOrNull()?.invoke()
  }

  companion object {
    val shared = AlertManager()
  }
}

private fun alertTitle(title: String): (@Composable () -> Unit)? {
  return {
    Text(
      title,
      Modifier.fillMaxWidth(),
      textAlign = TextAlign.Center,
      fontSize = 20.sp
    )
  }
}

private fun alertText(text: String?): (@Composable () -> Unit)? {
  return if (text == null) {
    null
  } else {
    ({
      Text(
        text,
        Modifier.fillMaxWidth(),
        textAlign = TextAlign.Center,
        fontSize = 16.sp,
        color = MaterialTheme.colors.secondary
      )
    })
  }
}