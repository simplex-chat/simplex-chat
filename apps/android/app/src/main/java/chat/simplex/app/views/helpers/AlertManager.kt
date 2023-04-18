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
    val alertText: (@Composable () -> Unit)? = if (text == null) null else { -> Text(text) }
    showAlert {
      AlertDialog(
        backgroundColor = if (isInDarkTheme()) Color(0xff222222) else MaterialTheme.colors.background,
        onDismissRequest = this::hideAlert,
        title = { Text(title, Modifier.fillMaxWidth(), textAlign = TextAlign.Center) },
        text = alertText,
        buttons = buttons
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
            .background(if (isInDarkTheme()) Color(0xff222222) else MaterialTheme.colors.background, RoundedCornerShape(corner = CornerSize(25.dp)))
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
              Text(text, Modifier.fillMaxWidth().padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING, bottom = DEFAULT_PADDING * 1.5f), fontSize = 16.sp, textAlign = TextAlign.Center, color = HighOrLowlight)
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
    val alertText: (@Composable () -> Unit)? = if (text == null) null else { -> Text(text, Modifier.fillMaxWidth(), fontSize = 16.sp, textAlign = TextAlign.Center, color = HighOrLowlight) }
    showAlert {
      AlertDialog(
        onDismissRequest = { onDismissRequest?.invoke(); hideAlert() },
        title = {
          Text(
            title,
            Modifier.fillMaxWidth(),
            textAlign = TextAlign.Center,
            fontSize = 20.sp
          )
        },
        text = alertText,
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
        backgroundColor = if (isInDarkTheme()) Color(0xff222222) else MaterialTheme.colors.background,
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
    val alertText: (@Composable () -> Unit)? = if (text == null) null else { -> Text(text) }
    showAlert {
      AlertDialog(
        onDismissRequest = { onDismissRequest?.invoke(); hideAlert() },
        title = { Text(title, Modifier.fillMaxWidth(), textAlign = TextAlign.Center) },
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
            }) { Text(confirmText, color = if (destructive) MaterialTheme.colors.error else Color.Unspecified, textAlign = TextAlign.End) }
          }
        },
        backgroundColor = if (isInDarkTheme()) Color(0xff222222) else MaterialTheme.colors.background,
        shape = RoundedCornerShape(corner = CornerSize(25.dp))
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
        title = { Text(title, Modifier.fillMaxWidth(), textAlign = TextAlign.Center) },
        text = alertText,
        buttons = {
          Row(
            Modifier.fillMaxWidth().padding(horizontal = DEFAULT_PADDING).padding(bottom = DEFAULT_PADDING_HALF),
            horizontalArrangement = Arrangement.Center
          ) {
            TextButton(onClick = {
              onConfirm?.invoke()
              hideAlert()
            }) { Text(confirmText, color = Color.Unspecified) }
          }
        },
        backgroundColor = if (isInDarkTheme()) Color(0xff222222) else MaterialTheme.colors.background,
        shape = RoundedCornerShape(corner = CornerSize(25.dp))
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
