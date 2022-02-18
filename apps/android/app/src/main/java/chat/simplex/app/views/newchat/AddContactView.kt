package chat.simplex.app.views.newchat

import android.content.Context
import android.content.Intent
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.material.Button
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.platform.LocalContext
import androidx.navigation.NavController
import chat.simplex.app.Pages
import chat.simplex.app.model.ChatModel

@Composable
fun AddContactView(chatModel: ChatModel, nav: NavController) {
  val connReq = chatModel.connReqInvitation
  if (connReq != null) {
    val cxt = LocalContext.current
    Column {
      Button(onClick = { nav.popBackStack() }) {
        Text("Close")
      }
      Text("Add contact")
//      .font(.title)
//    .padding(.bottom)
      Text("Show QR code to your contact\nto scan from the app")
//      .font(.title2)
//    .multilineTextAlignment(.center)
      QRCode(connReq)
//      .padding()
      Text("If you can't show QR code, you can share the invitation link via any channel")
//      .font(.subheadline)
//    .multilineTextAlignment(.center)
//    .padding(.horizontal)
      Button(onClick = {
        shareText(cxt, connReq)
      }) {
        Row {
          Text("Share invitation link")
        }
      }
    }
  }
}

fun shareText(cxt: Context, text: String) {
  val sendIntent: Intent = Intent().apply {
    action = Intent.ACTION_SEND
    putExtra(Intent.EXTRA_TEXT, text)
    type = "text/plain"
  }
  val shareIntent = Intent.createChooser(sendIntent, null)
  cxt.startActivity(shareIntent)
}