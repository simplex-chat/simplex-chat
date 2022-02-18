package chat.simplex.app.views.newchat

import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.Text
import androidx.compose.runtime.Composable

@Composable
fun QRCode(connReq: String) {
  Column {
    Text("QR code will show here, showing link for now")
    SelectionContainer {
      Text(connReq)
    }
  }
}