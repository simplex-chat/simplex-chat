package chat.simplex.common.views.chat

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.text.InlineTextContent
import androidx.compose.foundation.text.appendInlineContent
import androidx.compose.runtime.*
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.item.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.getLoadedFilePath
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource
import kotlinx.datetime.Clock

@Composable
fun ContextItemView(
  contextItems: List<ChatItem>,
  contextIcon: Painter,
  showSender: Boolean = true,
  chatInfo: ChatInfo,
  contextIconColor: Color = MaterialTheme.colors.secondary,
  cancelContextItem: () -> Unit,
) {
  val sentColor = MaterialTheme.appColors.sentMessage
  val receivedColor = MaterialTheme.appColors.receivedMessage

  @Composable
  fun MessageText(contextItem: ChatItem, attachment: ImageResource?, lines: Int) {
    val inlineContent: Pair<AnnotatedString.Builder.() -> Unit, Map<String, InlineTextContent>>? = if (attachment != null) {
      remember(contextItem.id) {
        val inlineContentBuilder: AnnotatedString.Builder.() -> Unit = {
          appendInlineContent(id = "attachmentIcon")
          append(" ")
        }
        val inlineContent = mapOf(
          "attachmentIcon" to InlineTextContent(
            Placeholder(20.sp, 20.sp, PlaceholderVerticalAlign.TextCenter)
          ) {
            Icon(painterResource(attachment), null, tint = MaterialTheme.colors.secondary)
          }
        )
        inlineContentBuilder to inlineContent
      }
    } else null
    MarkdownText(
      contextItem.text, contextItem.formattedText,
      sender = null,
      toggleSecrets = false,
      maxLines = lines,
      inlineContent = inlineContent,
      linkMode = SimplexLinkMode.DESCRIPTION,
      modifier = Modifier.fillMaxWidth(),
      mentions = contextItem.mentions,
      userMemberId = when {
        chatInfo is ChatInfo.Group -> chatInfo.groupInfo.membership.memberId
        else -> null
      }
    )
  }

  fun attachment(contextItem: ChatItem): ImageResource? {
    val fileIsLoaded = getLoadedFilePath(contextItem.file) != null

    return when (contextItem.content.msgContent) {
      is MsgContent.MCFile -> if (fileIsLoaded) MR.images.ic_draft_filled else null
      is MsgContent.MCImage -> MR.images.ic_image
      is MsgContent.MCVoice ->  if (fileIsLoaded) MR.images.ic_play_arrow_filled else null
      else -> null
    }
  }

  @Composable
  fun ContextMsgPreview(contextItem: ChatItem, lines: Int) {
    MessageText(contextItem, remember(contextItem.id) { attachment(contextItem) }, lines)
  }

  val sent = contextItems[0].chatDir.sent

  Row(
    Modifier
      .background(if (sent) sentColor else receivedColor),
    verticalAlignment = Alignment.CenterVertically
  ) {
    Row(
      Modifier
        .padding(vertical = 12.dp)
        .fillMaxWidth()
        .weight(1F),
      verticalAlignment = Alignment.CenterVertically
    ) {
      Icon(
        contextIcon,
        modifier = Modifier
          .padding(horizontal = 8.dp)
          .height(20.dp)
          .width(20.dp),
        contentDescription = stringResource(MR.strings.icon_descr_context),
        tint = contextIconColor,
        )

      if (contextItems.count() == 1) {
        val contextItem = contextItems[0]
        val sender = contextItem.memberDisplayName

        if (showSender && sender != null) {
          Column(
            horizontalAlignment = Alignment.Start,
            verticalArrangement = Arrangement.spacedBy(4.dp),
          ) {
            Text(
              sender,
              style = TextStyle(fontSize = 13.5.sp, color = CurrentColors.value.colors.secondary)
            )
            ContextMsgPreview(contextItem, lines = 2)
          }
        } else {
          ContextMsgPreview(contextItem, lines = 3)
        }
      } else if (contextItems.isNotEmpty()) {
        Text(String.format(generalGetString(if (chatInfo.chatType == ChatType.Local) MR.strings.compose_save_messages_n else MR.strings.compose_forward_messages_n), contextItems.count()), fontStyle = FontStyle.Italic)
      }
    }
    IconButton(onClick = cancelContextItem) {
      Icon(
        painterResource(MR.images.ic_close),
        contentDescription = stringResource(MR.strings.cancel_verb),
        tint = MaterialTheme.colors.primary,
        modifier = Modifier.padding(10.dp)
      )
    }
  }
}

@Preview
@Composable
fun PreviewContextItemView() {
  SimpleXTheme {
    ContextItemView(
      contextItems = listOf(ChatItem.getSampleData(1, CIDirection.DirectRcv(), Clock.System.now(), "hello")),
      contextIcon = painterResource(MR.images.ic_edit_filled),
      chatInfo = Chat.sampleData.chatInfo
    ) {}
  }
}
