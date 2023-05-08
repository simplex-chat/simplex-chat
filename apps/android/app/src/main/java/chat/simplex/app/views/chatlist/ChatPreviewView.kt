package chat.simplex.app.views.chatlist

import android.content.res.Configuration
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.text.InlineTextContent
import androidx.compose.foundation.text.appendInlineContent
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.*
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.ComposePreview
import chat.simplex.app.views.chat.ComposeState
import chat.simplex.app.views.chat.item.MarkdownText
import chat.simplex.app.views.helpers.*

@Composable
fun ChatPreviewView(
  chat: Chat,
  chatModelDraft: ComposeState?,
  chatModelDraftChatId: ChatId?,
  chatModelIncognito: Boolean,
  currentUserProfileDisplayName: String?,
  contactNetworkStatus: NetworkStatus?,
  stopped: Boolean,
  linkMode: SimplexLinkMode
) {
  val cInfo = chat.chatInfo

  @Composable
  fun groupInactiveIcon() {
    Icon(
      painterResource(R.drawable.ic_cancel_filled),
      stringResource(R.string.icon_descr_group_inactive),
      Modifier.size(18.dp).background(MaterialTheme.colors.background, CircleShape),
      tint = MaterialTheme.colors.secondary
    )
  }

  @Composable
  fun chatPreviewImageOverlayIcon() {
    if (cInfo is ChatInfo.Group) {
      when (cInfo.groupInfo.membership.memberStatus) {
        GroupMemberStatus.MemLeft -> groupInactiveIcon()
        GroupMemberStatus.MemRemoved -> groupInactiveIcon()
        GroupMemberStatus.MemGroupDeleted -> groupInactiveIcon()
        else -> {}
      }
    }
  }

  @Composable
  fun chatPreviewTitleText(color: Color = Color.Unspecified) {
    Text(
      cInfo.chatViewName,
      maxLines = 1,
      overflow = TextOverflow.Ellipsis,
      style = MaterialTheme.typography.h3,
      fontWeight = FontWeight.Bold,
      color = color
    )
  }

  @Composable
  fun VerifiedIcon() {
    Icon(painterResource(R.drawable.ic_verified_user), null, Modifier.size(19.dp).padding(end = 3.dp, top = 1.dp), tint = MaterialTheme.colors.secondary)
  }

  fun messageDraft(draft: ComposeState): Pair<AnnotatedString, Map<String, InlineTextContent>> {
    fun attachment(): Pair<Int, String?>? =
      when (draft.preview) {
        is ComposePreview.FilePreview -> R.drawable.ic_draft_filled to draft.preview.fileName
        is ComposePreview.MediaPreview -> R.drawable.ic_image to null
        is ComposePreview.VoicePreview -> R.drawable.ic_play_arrow_filled to durationText(draft.preview.durationMs / 1000)
        else -> null
      }

    val attachment = attachment()
    val text = buildAnnotatedString {
      appendInlineContent(id = "editIcon")
      append(" ")
      if (attachment != null) {
        appendInlineContent(id = "attachmentIcon")
        if (attachment.second != null) {
          append(attachment.second as String)
        }
        append(" ")
      }
      append(draft.message)
    }
    val inlineContent: Map<String, InlineTextContent> = mapOf(
      "editIcon" to InlineTextContent(
        Placeholder(20.sp, 20.sp, PlaceholderVerticalAlign.TextCenter)
      ) {
        Icon(painterResource(R.drawable.ic_edit_note), null, tint = MaterialTheme.colors.primary)
      },
      "attachmentIcon" to InlineTextContent(
        Placeholder(20.sp, 20.sp, PlaceholderVerticalAlign.TextCenter)
      ) {
        Icon(if (attachment?.first != null) painterResource(attachment.first) else painterResource(R.drawable.ic_edit_note), null, tint = MaterialTheme.colors.secondary)
      }
    )
    return text to inlineContent
  }

  @Composable
  fun chatPreviewTitle() {
    when (cInfo) {
      is ChatInfo.Direct ->
        Row(verticalAlignment = Alignment.CenterVertically) {
          if (cInfo.contact.verified) {
            VerifiedIcon()
          }
          chatPreviewTitleText(if (cInfo.ready) Color.Unspecified else MaterialTheme.colors.secondary)
        }
      is ChatInfo.Group ->
        when (cInfo.groupInfo.membership.memberStatus) {
          GroupMemberStatus.MemInvited -> chatPreviewTitleText(if (chat.chatInfo.incognito) Indigo else MaterialTheme.colors.primary)
          GroupMemberStatus.MemAccepted -> chatPreviewTitleText(MaterialTheme.colors.secondary)
          else -> chatPreviewTitleText()
        }
      else -> chatPreviewTitleText()
    }
  }

  @Composable
  fun chatPreviewText(chatModelIncognito: Boolean) {
    val ci = chat.chatItems.lastOrNull()
    if (ci != null) {
      val (text: CharSequence, inlineTextContent) = when {
        chatModelDraftChatId == chat.id && chatModelDraft != null -> remember(chatModelDraft) { messageDraft(chatModelDraft) }
        ci.meta.itemDeleted == null -> ci.text to null
        else -> generalGetString(R.string.marked_deleted_description) to null
      }
      val formattedText = when {
        chatModelDraftChatId == chat.id && chatModelDraft != null -> null
        ci.meta.itemDeleted == null -> ci.formattedText
        else -> null
      }
      MarkdownText(
        text,
        formattedText,
        sender = when {
          chatModelDraftChatId == chat.id && chatModelDraft != null -> null
          cInfo is ChatInfo.Group && !ci.chatDir.sent -> ci.memberDisplayName
          else -> null
        },
        linkMode = linkMode,
        senderBold = true,
        maxLines = 2,
        overflow = TextOverflow.Ellipsis,
        style = MaterialTheme.typography.body1.copy(color = if (isInDarkTheme()) MessagePreviewDark else MessagePreviewLight, lineHeight = 22.sp),
        inlineContent = inlineTextContent,
        modifier = Modifier.fillMaxWidth(),
      )
    } else {
      when (cInfo) {
        is ChatInfo.Direct ->
          if (!cInfo.ready) {
            Text(stringResource(R.string.contact_connection_pending), color = MaterialTheme.colors.secondary)
          }
        is ChatInfo.Group ->
          when (cInfo.groupInfo.membership.memberStatus) {
            GroupMemberStatus.MemInvited -> Text(groupInvitationPreviewText(chatModelIncognito, currentUserProfileDisplayName, cInfo.groupInfo))
            GroupMemberStatus.MemAccepted -> Text(stringResource(R.string.group_connection_pending), color = MaterialTheme.colors.secondary)
            else -> {}
          }
        else -> {}
      }
    }
  }

  Row {
    Box(contentAlignment = Alignment.BottomEnd) {
      ChatInfoImage(cInfo, size = 72.dp)
      Box(Modifier.padding(end = 6.dp, bottom = 6.dp)) {
        chatPreviewImageOverlayIcon()
      }
    }
    Column(
      modifier = Modifier
        .padding(horizontal = 8.dp)
        .weight(1F)
    ) {
      chatPreviewTitle()
      val height = with(LocalDensity.current) { 46.sp.toDp() }
      Row(Modifier.heightIn(min = height)) {
        chatPreviewText(chatModelIncognito)
      }
    }
    val ts = chat.chatItems.lastOrNull()?.timestampText ?: getTimestampText(chat.chatInfo.updatedAt)

    Box(
      contentAlignment = Alignment.TopEnd
    ) {
      Text(
        ts,
        color = MaterialTheme.colors.secondary,
        style = MaterialTheme.typography.body2,
        modifier = Modifier.padding(bottom = 5.dp)
      )
      val n = chat.chatStats.unreadCount
      val showNtfsIcon = !chat.chatInfo.ntfsEnabled && (chat.chatInfo is ChatInfo.Direct || chat.chatInfo is ChatInfo.Group)
      if (n > 0 || chat.chatStats.unreadChat) {
        Box(
          Modifier.padding(top = 24.dp),
          contentAlignment = Alignment.Center
        ) {
          Text(
            if (n > 0) unreadCountStr(n) else "",
            color = Color.White,
            fontSize = 11.sp,
            modifier = Modifier
              .background(if (stopped || showNtfsIcon) MaterialTheme.colors.secondary else MaterialTheme.colors.primaryVariant, shape = CircleShape)
              .badgeLayout()
              .padding(horizontal = 3.dp)
              .padding(vertical = 1.dp)
          )
        }
      } else if (showNtfsIcon) {
        Box(
          Modifier.padding(top = 24.dp),
          contentAlignment = Alignment.Center
        ) {
          Icon(
            painterResource(R.drawable.ic_notifications_off_filled),
            contentDescription = generalGetString(R.string.notifications),
            tint = MaterialTheme.colors.secondary,
            modifier = Modifier
              .padding(horizontal = 3.dp)
              .padding(vertical = 1.dp)
              .size(17.dp)
          )
        }
      }
      if (cInfo is ChatInfo.Direct) {
        Box(
          Modifier.padding(top = 52.dp),
          contentAlignment = Alignment.Center
        ) {
          ChatStatusImage(contactNetworkStatus)
        }
      }
    }
  }
}

@Composable
private fun groupInvitationPreviewText(chatModelIncognito: Boolean, currentUserProfileDisplayName: String?, groupInfo: GroupInfo): String {
  return if (groupInfo.membership.memberIncognito)
    String.format(stringResource(R.string.group_preview_join_as), groupInfo.membership.memberProfile.displayName)
  else if (chatModelIncognito)
    String.format(stringResource(R.string.group_preview_join_as), currentUserProfileDisplayName ?: "")
  else
    stringResource(R.string.group_preview_you_are_invited)
}

@Composable
fun unreadCountStr(n: Int): String {
  return if (n < 1000) "$n" else "${n / 1000}" + stringResource(R.string.thousand_abbreviation)
}

@Composable
fun ChatStatusImage(s: NetworkStatus?) {
  val descr = s?.statusString
  if (s is NetworkStatus.Error) {
    Icon(
      painterResource(R.drawable.ic_error),
      contentDescription = descr,
      tint = MaterialTheme.colors.secondary,
      modifier = Modifier
        .size(19.dp)
    )
  } else if (s !is NetworkStatus.Connected) {
    CircularProgressIndicator(
      Modifier
        .padding(horizontal = 2.dp)
        .size(15.dp),
      color = MaterialTheme.colors.secondary,
      strokeWidth = 1.5.dp
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
fun PreviewChatPreviewView() {
  SimpleXTheme {
    ChatPreviewView(Chat.sampleData, null, null, false, "", contactNetworkStatus = NetworkStatus.Connected(), stopped = false, linkMode = SimplexLinkMode.DESCRIPTION)
  }
}
