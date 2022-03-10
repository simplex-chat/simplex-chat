package chat.simplex.app.views.newchat

import android.content.res.Configuration
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Share
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.SimpleButton
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.shareText

@Composable
fun AddContactView(chatModel: ChatModel) {
  val connReq = chatModel.connReqInvitation
  if (connReq != null) {
    val cxt = LocalContext.current
    AddContactLayout(
      connReq = connReq,
      share = { shareText(cxt, connReq) }
    )
  }
}

@Composable
fun AddContactLayout(connReq: String, share: () -> Unit) {
  BoxWithConstraints {
    val screenHeight = maxHeight
    Column(
      horizontalAlignment = Alignment.CenterHorizontally,
      verticalArrangement = Arrangement.SpaceBetween,
    ) {
      Text(
        "Add contact",
        style = MaterialTheme.typography.h1,
      )
      Text(
        "Show QR code to your contact\nto scan from the app",
        style = MaterialTheme.typography.h2.copy(fontSize = if(screenHeight > 600.dp) 26.sp else 20.sp),
        textAlign = TextAlign.Center,
      )
      QRCode(
        connReq, Modifier
          .weight(1f, fill = false)
          .aspectRatio(1f)
          .padding(vertical = 3.dp)
      )
      Text(
        buildAnnotatedString {
          append("If you cannot meet in person, you can ")
          withStyle(SpanStyle(fontWeight = FontWeight.Bold)) {
            append("scan QR code in the video call")
          }
          append(", or you can share the invitation link via any other channel.")
        },
        textAlign = TextAlign.Center,
        style = MaterialTheme.typography.caption.copy(fontSize=if(screenHeight > 600.dp) 20.sp else 16.sp),
        modifier = Modifier
          .padding(horizontal = 16.dp)
          .padding(bottom = if(screenHeight > 600.dp) 16.dp else 8.dp)
      )
      SimpleButton("Share invitation link", icon = Icons.Outlined.Share, click = share)
      Spacer(Modifier.height(10.dp))
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
fun PreviewAddContactView() {
  SimpleXTheme {
    AddContactLayout(
      connReq = "https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D",
      share = {}
    )
  }
}
