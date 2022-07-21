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
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme

@Composable
fun CIGroupEventView(ci: ChatItem) {
  val memberDisplayName = ci.memberDisplayName

  fun withGroupEventStyle(builder: AnnotatedString.Builder, text: String) {
    return builder.withStyle(SpanStyle(fontSize = 12.sp, fontWeight = FontWeight.Light, color = HighOrLowlight)) { append(text) }
  }

  Surface {
    Row(
      Modifier.padding(horizontal = 6.dp, vertical = 6.dp),
      verticalAlignment = Alignment.Bottom
    ) {
      Text(
        buildAnnotatedString {
          if (memberDisplayName != null) {
            withGroupEventStyle(this, memberDisplayName)
            append(" ")
          }
          withGroupEventStyle(this, ci.content.text)
          append(" ")
          withGroupEventStyle(this, ci.timestampText)
        },
        style = MaterialTheme.typography.body1.copy(lineHeight = 22.sp)
      )
    }
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  name = "Dark Mode"
)
@Composable
fun CIGroupEventViewPreview() {
  SimpleXTheme {
    CIGroupEventView(
      ChatItem.getGroupEventSample()
    )
  }
}
