package chat.simplex.common.views.helpers

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.CornerSize
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.*
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.*
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.StringResource
import dev.icerock.moko.resources.compose.painterResource

class AlertManager {
  private var alertViews = mutableStateListOf<(@Composable () -> Unit)>()

  fun showAlert(alert: @Composable () -> Unit) {
    Log.d(TAG, "AlertManager.showAlert")
    alertViews.add(alert)
  }

  fun hideAlert() {
    alertViews.removeLastOrNull()
  }

  fun hideAllAlerts() {
    alertViews.clear()
  }

  fun hasAlertsShown() = alertViews.isNotEmpty()

  fun showAlertDialogButtons(
    title: String,
    text: String? = null,
    buttons: @Composable () -> Unit,
  ) {
    showAlert {
      AlertDialog(
        onDismissRequest = this::hideAlert,
        title = alertTitle(title),
        buttons = {
          AlertContent(text, null, extraPadding = true) {
            buttons()
          }
        },
        shape = RoundedCornerShape(corner = CornerSize(25.dp))
      )
    }
  }

  fun showAlertDialogButtonsColumn(
    title: String,
    text: AnnotatedString? = null,
    onDismissRequest: (() -> Unit)? = null,
    hostDevice: Pair<Long?, String>? = null,
    buttons: @Composable () -> Unit,
  ) {
    showAlert {
      AlertDialog(
        onDismissRequest = { onDismissRequest?.invoke(); hideAlert() },
        title = alertTitle(title),
        buttons = {
          AlertContent(text, hostDevice, extraPadding = true) {
            buttons()
          }
        },
        shape = RoundedCornerShape(corner = CornerSize(25.dp))
      )
    }
  }

  fun showAlertDialog(
    title: String,
    text: String? = null,
    confirmText: String = generalGetString(MR.strings.ok),
    onConfirm: (() -> Unit)? = null,
    dismissText: String = generalGetString(MR.strings.cancel_verb),
    onDismiss: (() -> Unit)? = null,
    onDismissRequest: (() -> Unit)? = null,
    destructive: Boolean = false,
    hostDevice: Pair<Long?, String>? = null,
  ) {
    showAlert {
      AlertDialog(
        onDismissRequest = { onDismissRequest?.invoke(); hideAlert() },
        title = alertTitle(title),
        buttons = {
          AlertContent(text, hostDevice, true) {
            Row(
              Modifier.fillMaxWidth().padding(horizontal = DEFAULT_PADDING),
              horizontalArrangement = Arrangement.SpaceBetween
            ) {
              val focusRequester = remember { FocusRequester() }
              LaunchedEffect(Unit) {
                focusRequester.requestFocus()
              }
              TextButton(onClick = {
                onDismiss?.invoke()
                hideAlert()
              }) { Text(dismissText) }
              TextButton(onClick = {
                onConfirm?.invoke()
                hideAlert()
              }, Modifier.focusRequester(focusRequester)) { Text(confirmText, color = if (destructive) MaterialTheme.colors.error else Color.Unspecified) }
            }
          }
        },
        shape = RoundedCornerShape(corner = CornerSize(25.dp))
      )
    }
  }

  fun showAlertDialogStacked(
    title: String,
    text: String? = null,
    confirmText: String = generalGetString(MR.strings.ok),
    onConfirm: (() -> Unit)? = null,
    dismissText: String = generalGetString(MR.strings.cancel_verb),
    onDismiss: (() -> Unit)? = null,
    onDismissRequest: (() -> Unit)? = null,
    destructive: Boolean = false
  ) {
    showAlert {
      AlertDialog(
        onDismissRequest = { onDismissRequest?.invoke(); hideAlert() },
        title = alertTitle(title),
        buttons = {
          AlertContent(text, null) {
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
          }
        },
        shape = RoundedCornerShape(corner = CornerSize(25.dp))
      )
    }
  }

  fun showAlertMsg(
    title: String, text: String? = null,
    confirmText: String = generalGetString(MR.strings.ok),
    hostDevice: Pair<Long?, String>? = null,
  ) {
    showAlert {
      AlertDialog(
        onDismissRequest = this::hideAlert,
        title = alertTitle(title),
        buttons = {
          AlertContent(text, hostDevice, extraPadding = true) {
            val focusRequester = remember { FocusRequester() }
            LaunchedEffect(Unit) {
              focusRequester.requestFocus()
            }
            Row(
              Modifier.fillMaxWidth().padding(horizontal = DEFAULT_PADDING),
              horizontalArrangement = Arrangement.Center
            ) {
              TextButton(
                onClick = {
                  hideAlert()
                },
                Modifier.focusRequester(focusRequester)
              ) {
                Text(confirmText, color = Color.Unspecified)
              }
            }
          }
        },
        shape = RoundedCornerShape(corner = CornerSize(25.dp))
      )
    }
  }

  fun showAlertMsgWithProgress(
    title: String,
    text: String? = null,
  ) {
    showAlert {
      AlertDialog(
        onDismissRequest = this::hideAlert,
        title = alertTitle(title),
        buttons = {
          AlertContent(text, null) {
            Box(Modifier.fillMaxWidth().height(72.dp).padding(bottom = DEFAULT_PADDING * 2), contentAlignment = Alignment.Center) {
              CircularProgressIndicator(Modifier.size(36.dp).padding(4.dp), color = MaterialTheme.colors.secondary, strokeWidth = 3.dp)
            }
          }
        }
      )
    }
  }

  fun showAlertMsg(
    title: StringResource,
    text: StringResource? = null,
    confirmText: StringResource = MR.strings.ok,
    hostDevice: Pair<Long?, String>? = null,
  ) = showAlertMsg(generalGetString(title), if (text != null) generalGetString(text) else null, generalGetString(confirmText), hostDevice)

  @Composable
  fun showInView() {
    remember { alertViews }.lastOrNull()?.invoke()
  }

  companion object {
    val shared = AlertManager()
    val privacySensitive = AlertManager()
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

@Composable
private fun AlertContent(text: String?, hostDevice: Pair<Long?, String>?, extraPadding: Boolean = false, content: @Composable (() -> Unit)) {
  BoxWithConstraints {
    Column(
      Modifier
        .padding(bottom = if (appPlatform.isDesktop) DEFAULT_PADDING else DEFAULT_PADDING_HALF)
    ) {
      if (appPlatform.isDesktop) {
        HostDeviceTitle(hostDevice, extraPadding = extraPadding)
      } else {
        Spacer(Modifier.size(DEFAULT_PADDING_HALF))
      }
      CompositionLocalProvider(LocalContentAlpha provides ContentAlpha.high) {
        if (text != null) {
          Column(Modifier.heightIn(max = this@BoxWithConstraints.maxHeight * 0.7f)
            .verticalScroll(rememberScrollState())
          ) {
            SelectionContainer {
              Text(
                escapedHtmlToAnnotatedString(text, LocalDensity.current),
                Modifier.fillMaxWidth().padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING, bottom = DEFAULT_PADDING * 1.5f),
                fontSize = 16.sp,
                textAlign = TextAlign.Center,
                color = MaterialTheme.colors.secondary
              )
            }
          }
        }
      }
      content()
    }
  }
}

@Composable
private fun AlertContent(text: AnnotatedString?, hostDevice: Pair<Long?, String>?, extraPadding: Boolean = false, content: @Composable (() -> Unit)) {
  BoxWithConstraints {
    Column(
      Modifier
        .verticalScroll(rememberScrollState())
        .padding(bottom = if (appPlatform.isDesktop) DEFAULT_PADDING else DEFAULT_PADDING_HALF)
    ) {
      if (appPlatform.isDesktop) {
        HostDeviceTitle(hostDevice, extraPadding = extraPadding)
      } else {
        Spacer(Modifier.size(DEFAULT_PADDING_HALF))
      }
      CompositionLocalProvider(LocalContentAlpha provides ContentAlpha.high) {
        if (text != null) {
          Column(
            Modifier.heightIn(max = this@BoxWithConstraints.maxHeight * 0.7f)
              .verticalScroll(rememberScrollState())
          ) {
            SelectionContainer {
              Text(
                text,
                Modifier.fillMaxWidth().padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING, bottom = DEFAULT_PADDING * 1.5f),
                fontSize = 16.sp,
                textAlign = TextAlign.Center,
                color = MaterialTheme.colors.secondary
              )
            }
          }
        }
      }
      content()
    }
  }
}

fun hostDevice(rhId: Long?): Pair<Long?, String>? = if (rhId == null && chatModel.remoteHosts.isNotEmpty()) {
  null to ChatModel.controller.appPrefs.deviceNameForRemoteAccess.get()!!
} else if (rhId == null) {
  null
} else {
  rhId to (chatModel.remoteHosts.firstOrNull { it.remoteHostId == rhId }?.hostDeviceName?.ifEmpty { rhId.toString() } ?: rhId.toString())
}

@Composable
private fun HostDeviceTitle(hostDevice: Pair<Long?, String>?, extraPadding: Boolean = false) {
  if (hostDevice != null) {
    Row(Modifier.fillMaxWidth().padding(top = 5.dp, bottom = if (extraPadding) DEFAULT_PADDING * 2 else DEFAULT_PADDING_HALF), verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.Center) {
      Icon(painterResource(if (hostDevice.first == null) MR.images.ic_desktop else MR.images.ic_smartphone_300), null, Modifier.size(15.dp), tint = MaterialTheme.colors.secondary)
      Spacer(Modifier.width(10.dp))
      Text(hostDevice.second, color = MaterialTheme.colors.secondary)
    }
  } else {
    Spacer(Modifier.height(if (extraPadding) DEFAULT_PADDING * 2 else 0.dp))
  }
}
