package chat.simplex.common.views.chat.item

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import com.icerockdev.library.MR
import chat.simplex.common.model.CIDeleted
import chat.simplex.common.model.ChatItem
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.generalGetString
import kotlinx.datetime.Clock

@Composable
fun MarkedDeletedItemView(ci: ChatItem, timedMessagesTTL: Int?, showMember: Boolean = false) {
  val sentColor = CurrentColors.collectAsState().value.appColors.sentMessage
  val receivedColor = CurrentColors.collectAsState().value.appColors.receivedMessage
  Surface(
    shape = RoundedCornerShape(18.dp),
    color = if (ci.chatDir.sent) sentColor else receivedColor,
  ) {
    Row(
      Modifier.padding(horizontal = 12.dp, vertical = 6.dp),
      verticalAlignment = Alignment.CenterVertically
    ) {
      Box(Modifier.weight(1f, false)) {
        if (ci.meta.itemDeleted is CIDeleted.Moderated) {
          MarkedDeletedText(String.format(generalGetString(MR.strings.moderated_item_description), ci.meta.itemDeleted.byGroupMember.chatViewName))
        } else {
          MarkedDeletedText(generalGetString(MR.strings.marked_deleted_description))
        }
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
      withStyle(SpanStyle(fontSize = 12.sp, fontStyle = FontStyle.Italic, color = MaterialTheme.colors.secondary)) { append(text) }
    },
    style = MaterialTheme.typography.body1.copy(lineHeight = 22.sp),
    modifier = Modifier.padding(end = 8.dp),
    maxLines = 1,
    overflow = TextOverflow.Ellipsis,
  )
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  name = "Dark Mode"
)*/
@Composable
fun PreviewMarkedDeletedItemView() {
  SimpleXTheme {
    DeletedItemView(
      ChatItem.getSampleData(itemDeleted = CIDeleted.Deleted(Clock.System.now())),
      null
    )
  }
}
