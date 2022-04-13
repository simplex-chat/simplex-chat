package chat.simplex.app.views.newchat

import android.content.res.Configuration
import android.net.Uri
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.*

@Composable
fun ConnectContactView(chatModel: ChatModel, close: () -> Unit) {
  BackHandler(onBack = close)
  ConnectContactLayout(
    qrCodeScanner = {
      QRCodeScanner { connReqUri ->
        try {
          val uri = Uri.parse(connReqUri)
          withUriAction(uri) { action ->
            connectViaUri(chatModel, action, uri)
          }
        } catch (e: RuntimeException) {
          AlertManager.shared.showAlertMsg(
            title = generalGetString(R.string.invalid_QR_code),
            text = generalGetString(R.string.this_QR_code_is_not_a_link)
          )
        }
        close()
      }
    },
    close = close
  )
}

fun withUriAction(uri: Uri, run: suspend (String) -> Unit) {
  val action = uri.path?.drop(1)
  if (action == "contact" || action == "invitation") {
    withApi { run(action) }
  } else {
    AlertManager.shared.showAlertMsg(
      title = generalGetString(R.string.invalid_contact_link),
      text = generalGetString(R.string.this_link_is_not_a_valid_connection_link)
    )
  }
}

suspend fun connectViaUri(chatModel: ChatModel, action: String, uri: Uri) {
  val r = chatModel.controller.apiConnect(uri.toString())
  if (r) {
    val whenConnected =
      if (action == "contact") generalGetString(R.string.your_connection_request_is_accepted)
      else generalGetString(R.string.your_contacts_device_is_online)
    AlertManager.shared.showAlertMsg(
      title = generalGetString(R.string.connection_request_sent),
      text = String.format(
        generalGetString(R.string.you_will_be_connected_when_condition_please_wait_or_check_later),
        whenConnected
      )
    )
  }
}

@Composable
fun ConnectContactLayout(qrCodeScanner: @Composable () -> Unit, close: () -> Unit) {
  ModalView(close) {
    Column(
      horizontalAlignment = Alignment.CenterHorizontally,
      verticalArrangement = Arrangement.spacedBy(12.dp)
    ) {
      Text(
        generalGetString(R.string.scan_QR_code),
        style = MaterialTheme.typography.h1.copy(fontWeight = FontWeight.Normal),
      )
      Text(
        generalGetString(R.string.your_chat_profile_will_be_sent_to_your_contact),
        style = MaterialTheme.typography.h3,
        textAlign = TextAlign.Center,
        modifier = Modifier.padding(bottom = 4.dp)
      )
      Box(
        Modifier
          .fillMaxWidth()
          .aspectRatio(ratio = 1F)
      ) { qrCodeScanner() }
      Text(
        annotatedStringResource(R.string.if_you_cannot_meet_in_person_scan_QR_on_video_or_use_invitation_link)
      )
    }
  }
}

@Preview
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewConnectContactLayout() {
  SimpleXTheme {
    ConnectContactLayout(
      qrCodeScanner = { Surface {} },
      close = {},
    )
  }
}
