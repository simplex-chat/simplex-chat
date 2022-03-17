package chat.simplex.app.views.chat

import android.content.res.Configuration
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
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.SolidColor
import androidx.compose.ui.text.input.KeyboardCapitalization
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chat.item.*

@Composable
fun SendMsgView(sendMessage: (String) -> Unit) {
  var msg by remember { mutableStateOf("") }
  val smallFont = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground)
  var textStyle by remember { mutableStateOf(smallFont) }
  BasicTextField(
    value = msg,
    onValueChange = {
      msg = it
      textStyle = if(isShortEmoji(it)) {
        if (it.codePoints().count() < 4) largeEmojiFont else mediumEmojiFont
      } else {
        smallFont
      }
    },
    textStyle = textStyle,
    maxLines = 16,
    keyboardOptions = KeyboardOptions.Default.copy(
      capitalization = KeyboardCapitalization.Sentences,
      autoCorrect = true
    ),
    modifier = Modifier.padding(8.dp),
    cursorBrush = SolidColor(HighOrLowlight),
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
          val color = if (msg.isNotEmpty()) MaterialTheme.colors.primary else Color.Gray
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
                if (msg.isNotEmpty()) {
                  sendMessage(msg)
                  msg = ""
                  textStyle = smallFont
                }
              }
          )
        }
      }
    }
  )
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewSendMsgView() {
  SimpleXTheme {
    SendMsgView(
      sendMessage = { msg -> println(msg) }
    )
  }
}
