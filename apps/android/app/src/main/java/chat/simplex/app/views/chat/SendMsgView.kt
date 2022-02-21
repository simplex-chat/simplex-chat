package chat.simplex.app.views.chat

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.ArrowUpward
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.focus.FocusState
import androidx.compose.ui.focus.onFocusChanged
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.input.*
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.ui.theme.*

@Composable
fun SendMsgView(sendMessage: (String) -> Unit) {
  var cmd by remember { mutableStateOf("") }
  BasicTextField(
    value = cmd,
    onValueChange = { cmd = it },
    textStyle = MaterialTheme.typography.body1,
    maxLines = 16,
    keyboardOptions = KeyboardOptions.Default.copy(
      capitalization = KeyboardCapitalization.Sentences,
      autoCorrect = true
    ),
    modifier = Modifier.padding(8.dp),
    decorationBox = { innerTextField ->
      Surface(
        shape = RoundedCornerShape(18.dp),
        border = BorderStroke(1.dp, LightGray)
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
          val color = if (cmd.isNotEmpty()) MaterialTheme.colors.primary else Color.Gray
          Icon(
            Icons.Outlined.ArrowUpward,
            "Send Message",
            tint = Color.White,
            modifier = Modifier
              .size(36.dp)
              .padding(4.dp)
              .clip(CircleShape)
              .background(color)
              .clickable {
                if (cmd.isNotEmpty()) {
                  sendMessage(cmd)
                  cmd = ""
                }
              }
          )
        }
      }
    }
  )
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
