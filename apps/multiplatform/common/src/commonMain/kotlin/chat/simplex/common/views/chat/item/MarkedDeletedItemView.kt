package chat.simplex.common.views.chat.item
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.runtime.*
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatModel.getChatItemIndexOrNull
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.datetime.Clock

@Composable
fun MarkedDeletedItemView(ci: ChatItem, timedMessagesTTL: Int?, revealed: MutableState<Boolean>) {
  val sentColor = MaterialTheme.appColors.sentMessage
  val receivedColor = MaterialTheme.appColors.receivedMessage
  Surface(
    shape = RoundedCornerShape(18.dp),
    color = if (ci.chatDir.sent) sentColor else receivedColor,
    contentColor = LocalContentColor.current
  ) {
    Row(
      Modifier.padding(horizontal = 12.dp, vertical = 6.dp),
      verticalAlignment = Alignment.CenterVertically
    ) {
      Box(Modifier.weight(1f, false)) {
        MergedMarkedDeletedText(ci, revealed)
      }
      CIMetaView(ci, timedMessagesTTL)
    }
  }
}

@Composable
private fun MergedMarkedDeletedText(chatItem: ChatItem, revealed: MutableState<Boolean>) {
  var i = getChatItemIndexOrNull(chatItem)
  val ciCategory = chatItem.mergeCategory
  val text =  if (!revealed.value && ciCategory != null && i != null) {
    val reversedChatItems = ChatModel.chatItems.asReversed()
    var moderated = 0
    var blocked = 0
    var blockedByAdmin = 0
    var deleted = 0
    val moderatedBy: MutableSet<String> = mutableSetOf()
    while (i < reversedChatItems.size) {
      val ci = reversedChatItems.getOrNull(i)
      if (ci?.mergeCategory != ciCategory) break
      when (val itemDeleted = ci.meta.itemDeleted ?: break) {
        is CIDeleted.Moderated -> {
          moderated += 1
          moderatedBy.add(itemDeleted.byGroupMember.displayName)
        }
        is CIDeleted.Blocked -> blocked += 1
        is CIDeleted.BlockedByAdmin -> blockedByAdmin +=1
        is CIDeleted.Deleted -> deleted += 1
      }
      i++
    }
    val total = moderated + blocked + blockedByAdmin + deleted
    if (total <= 1)
      markedDeletedText(chatItem.meta)
    else if (total == moderated)
      stringResource(MR.strings.moderated_items_description).format(total, moderatedBy.joinToString(", "))
    else if (total == blockedByAdmin)
      stringResource(MR.strings.blocked_by_admin_items_description).format(total)
    else if (total == blocked + blockedByAdmin)
      stringResource(MR.strings.blocked_items_description).format(total)
    else
      stringResource(MR.strings.marked_deleted_items_description).format(total)
  } else {
    markedDeletedText(chatItem.meta)
  }

  Text(
    buildAnnotatedString {
      withStyle(SpanStyle(fontSize = 12.sp, fontStyle = FontStyle.Italic, color = MaterialTheme.colors.secondary)) { append(text) }
    },
    style = MaterialTheme.typography.body1.copy(lineHeight = 22.sp),
    modifier = Modifier.padding(end = 8.dp),
    maxLines = 1,
    overflow = TextOverflow.Ellipsis,
  )
}

fun markedDeletedText(meta: CIMeta): String =
  when (meta.itemDeleted) {
    is CIDeleted.Moderated ->
      String.format(generalGetString(MR.strings.moderated_item_description), meta.itemDeleted.byGroupMember.displayName)
    is CIDeleted.Blocked ->
      generalGetString(MR.strings.blocked_item_description)
    is CIDeleted.BlockedByAdmin ->
      generalGetString(MR.strings.blocked_by_admin_item_description)
    is CIDeleted.Deleted, null ->
      generalGetString(MR.strings.marked_deleted_description)
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
