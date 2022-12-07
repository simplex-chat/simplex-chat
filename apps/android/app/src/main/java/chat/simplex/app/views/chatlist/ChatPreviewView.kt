package chat.simplex.app.views.chatlist

import android.content.res.Configuration
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Cancel
import androidx.compose.material.icons.filled.NotificationsOff
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.*
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.item.MarkdownText
import chat.simplex.app.views.helpers.*

@Composable
fun ChatPreviewView(chat: Chat, chatModelIncognito: Boolean, currentUserProfileDisplayName: String?, stopped: Boolean, linkMode: SimplexLinkMode) {
  val cInfo = chat.chatInfo

  @Composable
  fun groupInactiveIcon() {
    Icon(
      Icons.Filled.Cancel,
      stringResource(R.string.icon_descr_group_inactive),
      Modifier.size(18.dp).background(MaterialTheme.colors.background, CircleShape),
      tint = HighOrLowlight
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
  fun chatPreviewTitle() {
    when (cInfo) {
      is ChatInfo.Direct ->
        chatPreviewTitleText(if (cInfo.ready) Color.Unspecified else HighOrLowlight)
      is ChatInfo.Group ->
        when (cInfo.groupInfo.membership.memberStatus) {
          GroupMemberStatus.MemInvited -> chatPreviewTitleText(if (chat.chatInfo.incognito) Indigo else MaterialTheme.colors.primary)
          GroupMemberStatus.MemAccepted -> chatPreviewTitleText(HighOrLowlight)
          else -> chatPreviewTitleText()
        }
      else -> chatPreviewTitleText()
    }
  }

  @Composable
  fun chatPreviewText(chatModelIncognito: Boolean) {
    val ci = chat.chatItems.lastOrNull()
    if (ci != null) {
      MarkdownText(
        if (!ci.meta.itemDeleted) ci.text else generalGetString(R.string.marked_deleted_description),
        if (!ci.meta.itemDeleted) ci.formattedText else null,
        sender = if (cInfo is ChatInfo.Group && !ci.chatDir.sent) ci.memberDisplayName else null,
        linkMode = linkMode,
        senderBold = true,
        metaText = null,
        maxLines = 2,
        overflow = TextOverflow.Ellipsis,
        style = MaterialTheme.typography.body1.copy(color = if (isInDarkTheme()) MessagePreviewDark else MessagePreviewLight, lineHeight = 22.sp),
        modifier = Modifier.fillMaxWidth(),
      )
    } else {
      when (cInfo) {
        is ChatInfo.Direct ->
          if (!cInfo.ready) {
            Text(stringResource(R.string.contact_connection_pending), color = HighOrLowlight)
          }
        is ChatInfo.Group ->
          when (cInfo.groupInfo.membership.memberStatus) {
            GroupMemberStatus.MemInvited -> Text(groupInvitationPreviewText(chatModelIncognito, currentUserProfileDisplayName, cInfo.groupInfo))
            GroupMemberStatus.MemAccepted -> Text(stringResource(R.string.group_connection_pending), color = HighOrLowlight)
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
        color = HighOrLowlight,
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
            color = MaterialTheme.colors.onPrimary,
            fontSize = 11.sp,
            modifier = Modifier
              .background(if (stopped || showNtfsIcon) HighOrLowlight else MaterialTheme.colors.primary, shape = CircleShape)
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
            Icons.Filled.NotificationsOff,
            contentDescription = generalGetString(R.string.notifications),
            tint = HighOrLowlight,
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
          ChatStatusImage(chat)
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
fun ChatStatusImage(chat: Chat) {
  val s = chat.serverInfo.networkStatus
  val descr = s.statusString
  if (s is Chat.NetworkStatus.Error) {
    Icon(
      Icons.Outlined.ErrorOutline,
      contentDescription = descr,
      tint = HighOrLowlight,
      modifier = Modifier
        .size(19.dp)
    )
  } else if (s !is Chat.NetworkStatus.Connected) {
    CircularProgressIndicator(
      Modifier
        .padding(horizontal = 2.dp)
        .size(15.dp),
      color = HighOrLowlight,
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
    ChatPreviewView(Chat.sampleData, false, "", stopped = false, linkMode = SimplexLinkMode.DESCRIPTION)
  }
}
