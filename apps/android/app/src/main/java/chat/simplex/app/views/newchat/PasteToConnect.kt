package chat.simplex.app.views.newchat

import android.content.ClipboardManager
import android.content.res.Configuration
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Check
import androidx.compose.material.icons.outlined.ContentPaste
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.core.content.ContextCompat.getSystemService
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.generalGetString
import chat.simplex.app.views.helpers.withApi

@Composable
fun PasteToConnectTextbox(connectViaLink: (String) -> Unit) {
  val context = LocalContext.current
  val clipboard = getSystemService(context, ClipboardManager::class.java)
  fun pasteFromClipboard() : String {
    return clipboard?.primaryClip?.getItemAt(0)?.coerceToText(context) as String
  }

  val address = remember { mutableStateOf<String?>(null) }
  Row {
    BasicTextField(
      value = address.value ?: "",
      onValueChange = {s-> address.value = s},
      textStyle = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground),
      decorationBox = { innerTextField ->
        Surface(
          shape = RoundedCornerShape(18.dp),
          border = BorderStroke(1.dp, MaterialTheme.colors.secondary)
        ) {
          Row(
            Modifier.background(MaterialTheme.colors.background),
            verticalAlignment = Alignment.Bottom
          ) {
            Box(
              Modifier
                .weight(1f)
                .padding(horizontal = 12.dp)
                .padding(top = 5.dp)
                .padding(bottom = 7.dp)
            ) {
              innerTextField()
            }
            val color = MaterialTheme.colors.primary
            Icon(
              if (address.value == null || address.value == "") Icons.Outlined.ContentPaste else Icons.Outlined.Check,
              generalGetString(if (address.value == null || address.value == "") R.string.icon_paste_from_clipboard else R.string.icon_checkmark_action),
              tint = Color.White,
              modifier = Modifier
                .size(36.dp)
                .padding(4.dp)
                .clip(CircleShape)
                .background(color)
                .clickable {
                  val link = address.value
                  if (link == null) {
                    address.value = pasteFromClipboard()
                  } else {
                    connectViaLink(link)
                    address.value = null
                  }
                }
            )
          }
        }
      }
    )
  }
}

@Composable
fun PasteToConnectView(chatModel: ChatModel) {
  fun connectViaLink(connectionLink: String) {
    try {
        withApi { chatModel.controller.apiConnect(connectionLink) }
    } catch (e: Exception) {
      e.printStackTrace()
    }
  }
  PasteToConnectLayout(
    connectViaLink = { link -> connectViaLink(link) }
  )
}

@Composable
fun PasteToConnectLayout(connectViaLink: (String) -> Unit) {
  BoxWithConstraints {
    Column(
      horizontalAlignment = Alignment.CenterHorizontally,
      verticalArrangement = Arrangement.SpaceBetween,
    ) {
      Text(
        generalGetString(R.string.add_contact),  // TODO new header?
        style = MaterialTheme.typography.h1.copy(fontWeight = FontWeight.Normal),
      )
      Text(
        generalGetString(R.string.paste_connection_link_below_to_connect),
        style = MaterialTheme.typography.h3,
        textAlign = TextAlign.Center,
      )
      PasteToConnectTextbox(connectViaLink)
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
    PasteToConnectTextbox { link ->
      try {
        println(link)
//        withApi { chatModel.controller.apiConnect(link) }
      } catch (e: Exception) {
        e.printStackTrace()
      }
    }
  }
}
