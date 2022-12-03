package chat.simplex.app.views.chat.item

import android.content.res.Configuration
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.ChatItem
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.generalGetString

@Composable
fun MarkedDeletedItemView(ci: ChatItem, showMember: Boolean = false) {
  Surface(
    shape = RoundedCornerShape(18.dp),
    color = if (ci.chatDir.sent) SentColorLight else ReceivedColorLight,
  ) {
    Row(
      Modifier.padding(horizontal = 12.dp, vertical = 6.dp),
      verticalAlignment = Alignment.CenterVertically
    ) {
      Text(
        buildAnnotatedString {
          // appendSender(this, if (showMember) ci.memberDisplayName else null, true) // TODO font size
          withStyle(SpanStyle(fontSize = 12.sp, fontStyle = FontStyle.Italic, color = HighOrLowlight)) { append(generalGetString(R.string.marked_deleted_description)) }
        },
        style = MaterialTheme.typography.body1.copy(lineHeight = 22.sp),
        modifier = Modifier.padding(end = 8.dp)
      )
      CIMetaView(ci)
    }
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  name = "Dark Mode"
)
@Composable
fun PreviewMarkedDeletedItemView() {
  SimpleXTheme {
    DeletedItemView(
      ChatItem.getSampleData(itemDeleted = true)
    )
  }
}
