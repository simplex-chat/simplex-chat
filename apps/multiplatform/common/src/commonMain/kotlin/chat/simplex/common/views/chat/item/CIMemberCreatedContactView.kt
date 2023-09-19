package chat.simplex.common.views.chat.item

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.*
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.common.model.*
import chat.simplex.res.MR

@Composable
fun CIMemberCreatedContactView(
  chatItem: ChatItem,
  openDirectChat: (Long) -> Unit
) {
  fun eventText(): AnnotatedString {
    val memberDisplayName = chatItem.memberDisplayName
    return if (memberDisplayName != null) {
      buildAnnotatedString {
        withStyle(chatEventStyle) { append(memberDisplayName) }
        append(" ")
        withStyle(chatEventStyle) { append(chatItem.content.text) }
      }
    } else {
      buildAnnotatedString {
        withStyle(chatEventStyle) { append(chatItem.content.text) }
      }
    }
  }

  Row(
    Modifier.padding(horizontal = 6.dp, vertical = 6.dp),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.spacedBy(4.dp)
  ) {
    if (chatItem.chatDir is CIDirection.GroupRcv && chatItem.chatDir.groupMember.memberContactId != null) {
      val openChatStyle = SpanStyle(color = MaterialTheme.colors.primary, fontSize = 12.sp)
      val annotatedText = buildAnnotatedString {
        append(eventText())
        append("  ")
        withAnnotation(tag = "Open", annotation = "Open") {
          withStyle(openChatStyle) { append(generalGetString(MR.strings.rcv_group_event_open_chat) + "  ") }
        }
        withStyle(chatEventStyle) { append(chatItem.timestampText) }
      }

      fun open(offset: Int): Boolean = annotatedText.getStringAnnotations(tag = "Open", start = offset, end = offset).isNotEmpty()
      ClickableText(
        annotatedText,
        onClick = {
          if (open(it)) {
            openDirectChat(chatItem.chatDir.groupMember.memberContactId)
          }
        },
        shouldConsumeEvent = ::open
      )
    } else {
      val annotatedText = buildAnnotatedString {
        append(eventText())
        append("  ")
        withStyle(chatEventStyle) { append(chatItem.timestampText) }
      }
      Text(annotatedText)
    }
  }
}
