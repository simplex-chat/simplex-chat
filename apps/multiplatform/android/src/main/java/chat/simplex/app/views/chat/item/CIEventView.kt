package chat.simplex.app.views.chat.item

import android.content.res.Configuration
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.padding
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.model.ChatItem
import chat.simplex.app.ui.theme.*

@Composable
fun CIEventView(ci: ChatItem) {
  @Composable
  fun chatEventTextView(text: AnnotatedString) {
    Text(text, style = MaterialTheme.typography.body1.copy(lineHeight = 22.sp))
  }
  Row(
    Modifier.padding(horizontal = 6.dp, vertical = 6.dp),
    verticalAlignment = Alignment.CenterVertically
  ) {
    val memberDisplayName = ci.memberDisplayName
    if (memberDisplayName != null) {
      chatEventTextView(
        buildAnnotatedString {
          withStyle(chatEventStyle) { append(memberDisplayName) }
          append(" ")
        }.plus(chatEventText(ci))
      )
    } else {
      chatEventTextView(chatEventText(ci))
    }
  }
}

val chatEventStyle = SpanStyle(fontSize = 12.sp, fontWeight = FontWeight.Light, color = CurrentColors.value.colors.secondary)

fun chatEventText(ci: ChatItem): AnnotatedString =
  buildAnnotatedString {
    withStyle(chatEventStyle) { append(ci.content.text + "  " + ci.timestampText) }
  }

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  name = "Dark Mode"
)
@Composable
fun CIEventViewPreview() {
  SimpleXTheme {
    CIEventView(
      ChatItem.getGroupEventSample()
    )
  }
}
