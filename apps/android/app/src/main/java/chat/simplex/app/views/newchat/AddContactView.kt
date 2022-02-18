package chat.simplex.app.views.newchat

import android.content.Context
import android.content.Intent
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.material.Button
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.platform.LocalContext

@Composable
fun AddContactView(connReqInvitation: String) {
  val cxt = LocalContext.current
  Column {
    Text("Add contact")
//      .font(.title)
//    .padding(.bottom)
    Text("Show QR code to your contact\nto scan from the app")
//      .font(.title2)
//    .multilineTextAlignment(.center)
    QRCode(connReqInvitation)
//      .padding()
    Text("If you can't show QR code, you can share the invitation link via any channel")
//      .font(.subheadline)
//    .multilineTextAlignment(.center)
//    .padding(.horizontal)
    Button (onClick = {
      shareText(cxt, connReqInvitation)
    }) {
      Row {
        Text("Share invitation link")
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