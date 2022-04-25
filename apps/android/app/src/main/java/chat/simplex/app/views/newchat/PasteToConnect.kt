package chat.simplex.app.views.newchat

import android.content.ClipboardManager
import android.content.res.Configuration
import android.net.Uri
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.core.content.ContextCompat.getSystemService
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.SimpleButton
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.*

@Composable
fun PasteToConnectView(chatModel: ChatModel, close: () -> Unit) {
  val connectionLink = remember { mutableStateOf("")}
  val context = LocalContext.current
  val clipboard = getSystemService(context, ClipboardManager::class.java)
  BackHandler(onBack = close)
  PasteToConnectLayout(
    connectionLink = connectionLink,
    pasteFromClipboard = {
      connectionLink.value = clipboard?.primaryClip?.getItemAt(0)?.coerceToText(context) as String
    },
    connectViaLink = { connReqUri ->
      try {
        val uri = Uri.parse(connReqUri)
        withUriAction(uri) { action ->
          connectViaUri(chatModel, action, uri)
        }
      } catch (e: RuntimeException) {
        AlertManager.shared.showAlertMsg(
          title = generalGetString(R.string.invalid_connection_link),
          text = generalGetString(R.string.this_string_is_not_a_connection_link)
        )
      }
      close()
    },
    close = close
  )
}

@Composable
fun PasteToConnectLayout(
  connectionLink: MutableState<String>,
  pasteFromClipboard: () -> Unit,
  connectViaLink: (String) -> Unit,
  close: () -> Unit
) {
  ModalView(close) {
    Column(
      horizontalAlignment = Alignment.CenterHorizontally,
      verticalArrangement = Arrangement.SpaceBetween,
    ) {
      Text(
        generalGetString(R.string.connect_via_link),
        style = MaterialTheme.typography.h1.copy(fontWeight = FontWeight.Normal),
        modifier = Modifier.padding(bottom = 16.dp)
      )
      Text(generalGetString(R.string.paste_connection_link_below_to_connect))
      Text(generalGetString(R.string.profile_will_be_sent_to_contact_sending_link))

      Box(Modifier.padding(top = 16.dp, bottom = 6.dp)) {
        TextEditor(Modifier.height(180.dp), text = connectionLink)
      }

      Row(
        Modifier.fillMaxWidth().padding(bottom = 6.dp),
        horizontalArrangement = Arrangement.Start,
      ) {
        if (connectionLink.value == "") {
          SimpleButton(text = "Paste", icon = Icons.Outlined.ContentPaste) {
            pasteFromClipboard()
          }
        } else {
          SimpleButton(text = "Clear", icon = Icons.Outlined.Clear) {
            connectionLink.value = ""
          }
        }
        Spacer(Modifier.weight(1f).fillMaxWidth())
        SimpleButton(text = "Connect", icon = Icons.Outlined.Link) {
          connectViaLink(connectionLink.value)
        }
      }

      Text(annotatedStringResource(R.string.you_can_also_connect_by_clicking_the_link))
    }
  }
}


@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  name = "Dark Mode"
)
@Composable
fun PreviewPasteToConnectTextbox() {
  SimpleXTheme {
    PasteToConnectLayout(
      connectionLink = remember { mutableStateOf("") },
      pasteFromClipboard = {},
      connectViaLink = { link ->
        try {
          println(link)
  //        withApi { chatModel.controller.apiConnect(link) }
        } catch (e: Exception) {
          e.printStackTrace()
        }
      },
      close = {}
    )
  }
}
