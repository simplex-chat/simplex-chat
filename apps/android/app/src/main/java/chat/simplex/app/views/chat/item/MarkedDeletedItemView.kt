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
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.CIDeleted
import chat.simplex.app.model.ChatItem
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.generalGetString

@Composable
fun MarkedDeletedItemView(ci: ChatItem, timedMessagesTTL: Int?, showMember: Boolean = false) {
  Surface(
    shape = RoundedCornerShape(18.dp),
    color = if (ci.chatDir.sent) SentColorLight else ReceivedColorLight,
  ) {
    Row(
      Modifier.padding(horizontal = 12.dp, vertical = 6.dp),
      verticalAlignment = Alignment.CenterVertically
    ) {
      if (ci.meta.itemDeleted is CIDeleted.Moderated) {
        MarkedDeletedText(String.format(generalGetString(R.string.moderated_item_description), ci.meta.itemDeleted.byGroupMember.chatViewName))
      } else {
        MarkedDeletedText(generalGetString(R.string.marked_deleted_description))
      }
      CIMetaView(ci, timedMessagesTTL)
    }
  }
}

@Composable
private fun MarkedDeletedText(text: String) {
  Text(
    buildAnnotatedString {
      // appendSender(this, if (showMember) ci.memberDisplayName else null, true) // TODO font size
      withStyle(SpanStyle(fontSize = 12.sp, fontStyle = FontStyle.Italic, color = HighOrLowlight)) { append(text) }
    },
    style = MaterialTheme.typography.body1.copy(lineHeight = 22.sp),
    modifier = Modifier.padding(end = 8.dp),
    maxLines = 1,
    overflow = TextOverflow.Ellipsis,
  )
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
      ChatItem.getSampleData(itemDeleted = CIDeleted.Deleted()),
      null
    )
  }
}
