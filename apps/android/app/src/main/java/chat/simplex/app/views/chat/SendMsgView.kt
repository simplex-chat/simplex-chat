package chat.simplex.app.views.chat

import androidx.compose.foundation.layout.*
import androidx.compose.material.Button
import androidx.compose.material.Text
import androidx.compose.material.TextField
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.ui.theme.SimpleXTheme

@Composable
fun SendMsgView (sendMessage: (String) -> Unit) {
  var cmd by remember { mutableStateOf("") }
  Row {
    TextField(value = cmd, onValueChange = { cmd = it }, modifier = Modifier.height(60.dp))
    Spacer(Modifier.height(10.dp))
    Button(
      onClick = {
        sendMessage(cmd)
        cmd = ""
      },
      modifier = Modifier.width(40.dp).height(60.dp),
      enabled = cmd.isNotEmpty()
    ) {
      Text("Go")
    }
  }
}

@Preview
@Composable
fun PreviewSendMsgView() {
  SimpleXTheme {
    SendMsgView(
      sendMessage = { msg -> println(msg) }
    )
  }
}
