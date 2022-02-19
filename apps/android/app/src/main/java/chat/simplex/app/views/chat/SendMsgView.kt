package chat.simplex.app.views.chat

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.ArrowUpward
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.ui.theme.SimpleXTheme

@Composable
fun SendMsgView(sendMessage: (String) -> Unit) {
  var cmd by remember { mutableStateOf("") }
  Box(
    modifier = Modifier.fillMaxWidth(),
    contentAlignment = Alignment.CenterEnd
  ) {
    TextField(
      value = cmd,
      onValueChange = { cmd = it },
      modifier = Modifier
        .height(60.dp)
        .fillMaxWidth()
        .padding(end = 40.dp)
        .background(MaterialTheme.colors.background)
    )
    val tint = if (cmd.isNotEmpty()) MaterialTheme.colors.primary else Color.Gray
    Icon(
      Icons.Outlined.ArrowUpward,
      "Send Message",
      tint = tint,
      modifier = Modifier
        .width(40.dp)
        .clickable {
          sendMessage(cmd)
          cmd = ""
        }
    )
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
