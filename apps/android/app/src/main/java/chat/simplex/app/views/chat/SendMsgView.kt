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
import androidx.compose.material.icons.filled.Check
import androidx.compose.material.icons.outlined.ArrowUpward
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.SolidColor
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.input.KeyboardCapitalization
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.ChatItem
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme

@Composable
fun SendMsgView(
  composeState: MutableState<ComposeState>,
  sendMessage: () -> Unit,
  onMessageChange: (String) -> Unit,
  textStyle: MutableState<TextStyle>
) {
  val cs = composeState.value
  BasicTextField(
    value = cs.message,
    onValueChange = onMessageChange,
    textStyle = textStyle.value,
    maxLines = 16,
    keyboardOptions = KeyboardOptions.Default.copy(
      capitalization = KeyboardCapitalization.Sentences,
      autoCorrect = true
    ),
    modifier = Modifier.padding(vertical = 8.dp),
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
          val icon = if (cs.editing) Icons.Filled.Check else Icons.Outlined.ArrowUpward
          val color = if (cs.sendEnabled()) MaterialTheme.colors.primary else HighOrLowlight
          if (cs.inProgress
            && (cs.preview is ComposePreview.ImagePreview || cs.preview is ComposePreview.FilePreview)
          ) {
            CircularProgressIndicator(
              Modifier
                .size(36.dp)
                .padding(4.dp),
              color = HighOrLowlight,
              strokeWidth = 3.dp
            )
          } else {
            Icon(
              icon,
              stringResource(R.string.icon_descr_send_message),
              tint = Color.White,
              modifier = Modifier
                .size(36.dp)
                .padding(4.dp)
                .clip(CircleShape)
                .background(color)
                .clickable {
                  if (cs.sendEnabled()) {
                    sendMessage()
                  }
                }
            )
          }
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
  val smallFont = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground)
  val textStyle = remember { mutableStateOf(smallFont) }
  SimpleXTheme {
    SendMsgView(
      composeState = remember { mutableStateOf(ComposeState()) },
      sendMessage = {},
      onMessageChange = { _ -> },
      textStyle = textStyle
    )
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewSendMsgViewEditing() {
  val smallFont = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground)
  val textStyle = remember { mutableStateOf(smallFont) }
  val composeStateEditing = ComposeState(editingItem = ChatItem.getSampleData())
  SimpleXTheme {
    SendMsgView(
      composeState = remember { mutableStateOf(composeStateEditing) },
      sendMessage = {},
      onMessageChange = { _ -> },
      textStyle = textStyle
    )
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewSendMsgViewInProgress() {
  val smallFont = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground)
  val textStyle = remember { mutableStateOf(smallFont) }
  val composeStateInProgress = ComposeState(preview = ComposePreview.FilePreview("test.txt"), inProgress = true)
  SimpleXTheme {
    SendMsgView(
      composeState = remember { mutableStateOf(composeStateInProgress) },
      sendMessage = {},
      onMessageChange = { _ -> },
      textStyle = textStyle
    )
  }
}
