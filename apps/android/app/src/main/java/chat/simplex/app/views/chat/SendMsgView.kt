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
import androidx.compose.ui.text.input.KeyboardCapitalization
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chat.item.*
import chat.simplex.app.views.helpers.getLinkPreview
import chat.simplex.app.views.helpers.withApi
import kotlinx.coroutines.delay

@Composable
fun SendMsgView(
  msg: MutableState<String>,
  linkPreview: MutableState<LinkPreview?>,
  cancelledLinks: MutableSet<String>,
  parseMarkdown: (String) -> List<FormattedText>?,
  sendMessage: (String) -> Unit,
  editing: Boolean = false,
  sendEnabled: Boolean = false
) {
  val smallFont = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground)
  var textStyle by remember { mutableStateOf(smallFont) }
  val linkUrl = remember { mutableStateOf<String?>(null) }
  val prevLinkUrl = remember { mutableStateOf<String?>(null) }
  val pendingLinkUrl = remember { mutableStateOf<String?>(null) }

  fun isSimplexLink(link: String): Boolean =
    link.startsWith("https://simplex.chat",true) || link.startsWith("http://simplex.chat", true)

  fun parseMessage(msg: String): String? {
    val parsedMsg = parseMarkdown(msg)
    val link = parsedMsg?.firstOrNull { ft -> ft.format is Format.Uri && !cancelledLinks.contains(ft.text) && !isSimplexLink(ft.text) }
    return link?.text
  }

  fun loadLinkPreview(url: String, wait: Long? = null) {
    if (pendingLinkUrl.value == url) {
      withApi {
        if (wait != null) delay(wait)
        val lp = getLinkPreview(url)
        if (pendingLinkUrl.value == url) {
          linkPreview.value = lp
          pendingLinkUrl.value = null
        }
      }
    }
  }

  fun showLinkPreview(s: String) {
    prevLinkUrl.value = linkUrl.value
    linkUrl.value = parseMessage(s)
    val url = linkUrl.value
    if (url != null) {
      if (url != linkPreview.value?.uri && url != pendingLinkUrl.value) {
        pendingLinkUrl.value = url
        loadLinkPreview(url, wait = if (prevLinkUrl.value == url) null else 1500L)
      }
    } else {
      linkPreview.value = null
    }
  }

  fun resetLinkPreview() {
    linkUrl.value = null
    prevLinkUrl.value = null
    pendingLinkUrl.value = null
    cancelledLinks.clear()
  }

  BasicTextField(
    value = msg.value,
    onValueChange = { s ->
      msg.value = s
      if (isShortEmoji(s)) {
        textStyle = if (s.codePoints().count() < 4) largeEmojiFont else mediumEmojiFont
      } else {
        textStyle = smallFont
        if (s.isNotEmpty()) showLinkPreview(s)
        else resetLinkPreview()
      }
    },
    textStyle = textStyle,
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
          val color = if (sendEnabled) MaterialTheme.colors.primary else Color.Gray
          Icon(
            if (editing) Icons.Filled.Check else Icons.Outlined.ArrowUpward,
            stringResource(R.string.icon_descr_send_message),
            tint = Color.White,
            modifier = Modifier
              .size(36.dp)
              .padding(4.dp)
              .clip(CircleShape)
              .background(color)
              .clickable {
                if (sendEnabled) {
                  sendMessage(msg.value)
                  msg.value = ""
                  textStyle = smallFont
                  cancelledLinks.clear()
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
      msg = remember { mutableStateOf("") },
      linkPreview = remember {mutableStateOf<LinkPreview?>(null) },
      cancelledLinks = mutableSetOf(),
      parseMarkdown = { null },
      sendMessage = { msg -> println(msg) },
      sendEnabled = true
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
  SimpleXTheme {
    SendMsgView(
      msg = remember { mutableStateOf("") },
      linkPreview = remember {mutableStateOf<LinkPreview?>(null) },
      cancelledLinks = mutableSetOf(),
      sendMessage = { msg -> println(msg) },
      parseMarkdown = { null },
      editing = true,
      sendEnabled = true
    )
  }
}
