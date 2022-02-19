package chat.simplex.app.views.newchat

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.navigation.NavController
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.CloseSheetBar
import chat.simplex.app.views.helpers.withApi
import kotlinx.coroutines.DelicateCoroutinesApi

@DelicateCoroutinesApi
@Composable
fun ConnectContactView(chatModel: ChatModel, nav: NavController) {
  ConnectContactLayout(
    qrCodeScanner = {
      QRCodeScanner { connReqUri ->
        withApi {
          val res = chatModel.controller.apiConnect(connReqUri)
          // check if it is valid
          nav.popBackStack()
        }
      }
    },
    close = { nav.popBackStack() }
  )
}

@Composable
fun ConnectContactLayout(qrCodeScanner: @Composable () -> Unit, close: () -> Unit) {
  Column(
    modifier = Modifier
      .padding(horizontal = 8.dp)
      .fillMaxSize()
      .background(MaterialTheme.colors.background),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    CloseSheetBar(close)
    Text(
      "Scan QR code",
      style = MaterialTheme.typography.h1,
      modifier = Modifier.padding(bottom = 8.dp)
    )
    Text(
      "Your chat profile will be sent\nto your contact",
      style = MaterialTheme.typography.h2,
      textAlign = TextAlign.Center,
      modifier = Modifier.padding(bottom = 16.dp)
    )
    Box (
      Modifier
        .fillMaxWidth()
        .aspectRatio(ratio = 1F)
    ) { qrCodeScanner() }
    Text(
      buildAnnotatedString {
        append("If you cannot meet in person, you can ")
        withStyle(SpanStyle(fontWeight = FontWeight.Bold)) {
          append("scan QR code in the video call")
        }
        append(", or you can create the invitation link.")
      },
      textAlign = TextAlign.Center,
      style = MaterialTheme.typography.caption,
      modifier = Modifier
        .padding(horizontal = 16.dp)
        .padding(top = 16.dp)
    )
  }
}

@Preview
@Composable
fun PreviewConnectContactLayout() {
  SimpleXTheme {
    ConnectContactLayout(
      qrCodeScanner = { Surface {} },
      close = {},
    )
  }
}
