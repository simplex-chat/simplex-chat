package chat.simplex.common.views.chatlist

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
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.ui.unit.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.ComposePreview
import chat.simplex.common.views.chat.ComposeState
import chat.simplex.common.views.chat.item.MarkdownText
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.*
import chat.simplex.common.model.GroupInfo
import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.chat.item.markedDeletedText
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource

@Composable
fun ChatPreviewView(
  chat: Chat,
  showChatPreviews: Boolean,
  chatModelDraft: ComposeState?,
  chatModelDraftChatId: ChatId?,
  currentUserProfileDisplayName: String?,
  contactNetworkStatus: NetworkStatus?,
  disabled: Boolean,
  linkMode: SimplexLinkMode,
  inProgress: Boolean,
  progressByTimeout: Boolean
) {
  val cInfo = chat.chatInfo

  @Composable
  fun inactiveIcon() {
    val sp18 = with(LocalDensity.current) { 18.sp.toDp() }
    Icon(
      painterResource(MR.images.ic_cancel_filled),
      stringResource(MR.strings.icon_descr_group_inactive),
      Modifier.size(sp18).background(MaterialTheme.colors.background, CircleShape),
      tint = MaterialTheme.colors.secondary
    )
  }

  @Composable
  fun chatPreviewImageOverlayIcon() {
    when (cInfo) {
      is ChatInfo.Direct ->
        if (!cInfo.contact.active) {
          inactiveIcon()
        }
      is ChatInfo.Group ->
        when (cInfo.groupInfo.membership.memberStatus) {
          GroupMemberStatus.MemLeft -> inactiveIcon()
          GroupMemberStatus.MemRemoved -> inactiveIcon()
          GroupMemberStatus.MemGroupDeleted -> inactiveIcon()
          else -> {}
      }
      else -> {}
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
    val sp19 = with(LocalDensity.current) { 19.sp.toDp() }
    Icon(painterResource(MR.images.ic_verified_user), null, Modifier.size(sp19).padding(end = 3.dp, top = 1.dp), tint = MaterialTheme.colors.secondary)
  }

  fun messageDraft(draft: ComposeState, sp20: Dp): Pair<AnnotatedString.Builder.() -> Unit, Map<String, InlineTextContent>> {
    fun attachment(): Pair<ImageResource, String?>? =
      when (draft.preview) {
        is ComposePreview.FilePreview -> MR.images.ic_draft_filled to draft.preview.fileName
        is ComposePreview.MediaPreview -> MR.images.ic_image to null
        is ComposePreview.VoicePreview -> MR.images.ic_play_arrow_filled to durationText(draft.preview.durationMs / 1000)
        else -> null
      }

    val attachment = attachment()
    val inlineContentBuilder: AnnotatedString.Builder.() -> Unit = {
      appendInlineContent(id = "editIcon")
      append(" ")
      if (attachment != null) {
        appendInlineContent(id = "attachmentIcon")
        if (attachment.second != null) {
          append(attachment.second as String)
        }
        append(" ")
      }
    }
    val inlineContent: Map<String, InlineTextContent> = mapOf(
      "editIcon" to InlineTextContent(
        Placeholder(20.sp, 20.sp, PlaceholderVerticalAlign.TextCenter)
      ) {
        Icon(painterResource(MR.images.ic_edit_note), null, Modifier.size(sp20), tint = MaterialTheme.colors.primary)
      },
      "attachmentIcon" to InlineTextContent(
        Placeholder(20.sp, 20.sp, PlaceholderVerticalAlign.TextCenter)
      ) {
        Icon(if (attachment?.first != null) painterResource(attachment.first) else painterResource(MR.images.ic_edit_note), null, Modifier.size(sp20), tint = MaterialTheme.colors.secondary)
      }
    )
    return inlineContentBuilder to inlineContent
  }

  @Composable
  fun chatPreviewTitle() {
    val deleting by remember(disabled, chat.id) { mutableStateOf(chatModel.deletedChats.value.contains(chat.remoteHostId to chat.chatInfo.id)) }
    when (cInfo) {
      is ChatInfo.Direct ->
        Row(verticalAlignment = Alignment.CenterVertically) {
          if (cInfo.contact.verified) {
            VerifiedIcon()
          }
          chatPreviewTitleText(
            if (deleting)
              MaterialTheme.colors.secondary
            else
              Color.Unspecified
          )
        }
      is ChatInfo.Group ->
        when (cInfo.groupInfo.membership.memberStatus) {
          GroupMemberStatus.MemInvited -> chatPreviewTitleText(
            if (inProgress || deleting)
              MaterialTheme.colors.secondary
            else
              if (chat.chatInfo.incognito) Indigo else MaterialTheme.colors.primary
          )
          GroupMemberStatus.MemAccepted -> chatPreviewTitleText(MaterialTheme.colors.secondary)
          else -> chatPreviewTitleText(
            if (deleting)
              MaterialTheme.colors.secondary
            else
              Color.Unspecified
          )
        }
      else -> chatPreviewTitleText()
    }
  }

  @Composable
  fun chatPreviewText() {
    val ci = chat.chatItems.lastOrNull()
    if (ci != null) {
      if (showChatPreviews || (chatModelDraftChatId == chat.id && chatModelDraft != null)) {
        val sp20 = with(LocalDensity.current) { 20.sp.toDp() }
        val (text: CharSequence, inlineTextContent) = when {
          chatModelDraftChatId == chat.id && chatModelDraft != null -> remember(chatModelDraft) { chatModelDraft.message to messageDraft(chatModelDraft, sp20) }
          ci.meta.itemDeleted == null -> ci.text to null
          else -> markedDeletedText(ci.meta) to null
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
          toggleSecrets = false,
          linkMode = linkMode,
          senderBold = true,
          maxLines = 2,
          overflow = TextOverflow.Ellipsis,
          style = MaterialTheme.typography.body1.copy(color = if (isInDarkTheme()) MessagePreviewDark else MessagePreviewLight, lineHeight = 22.sp),
          inlineContent = inlineTextContent,
          modifier = Modifier.fillMaxWidth(),
        )
      }
    } else {
      when (cInfo) {
        is ChatInfo.Direct ->
          if (cInfo.contact.activeConn == null && cInfo.contact.profile.contactLink != null) {
            Text(stringResource(MR.strings.contact_tap_to_connect), color = MaterialTheme.colors.primary)
          } else if (!cInfo.ready && cInfo.contact.activeConn != null) {
            if (cInfo.contact.nextSendGrpInv) {
              Text(stringResource(MR.strings.member_contact_send_direct_message), color = MaterialTheme.colors.secondary)
            } else if (cInfo.contact.active) {
              Text(stringResource(MR.strings.contact_connection_pending), color = MaterialTheme.colors.secondary)
            }
          }
        is ChatInfo.Group ->
          when (cInfo.groupInfo.membership.memberStatus) {
            GroupMemberStatus.MemInvited -> Text(groupInvitationPreviewText(currentUserProfileDisplayName, cInfo.groupInfo))
            GroupMemberStatus.MemAccepted -> Text(stringResource(MR.strings.group_connection_pending), color = MaterialTheme.colors.secondary)
            else -> {}
          }
        else -> {}
      }
    }
  }

  @Composable
  fun progressView() {
    val sp15 = with(LocalDensity.current) { 15.sp.toDp() }
    CircularProgressIndicator(
      Modifier
        .padding(horizontal = 2.dp)
        .size(sp15),
      color = MaterialTheme.colors.secondary,
      strokeWidth = 1.5.dp
    )
  }

  @Composable
  fun chatStatusImage() {
    val sp19 = with(LocalDensity.current) { 19.sp.toDp() }
    if (cInfo is ChatInfo.Direct) {
      if (cInfo.contact.active && cInfo.contact.activeConn != null) {
        val descr = contactNetworkStatus?.statusString
        when (contactNetworkStatus) {
          is NetworkStatus.Connected ->
            IncognitoIcon(chat.chatInfo.incognito)

          is NetworkStatus.Error ->
            Icon(
              painterResource(MR.images.ic_error),
              contentDescription = descr,
              tint = MaterialTheme.colors.secondary,
              modifier = Modifier
                .size(sp19)
            )

          else ->
            progressView()
        }
      } else {
        IncognitoIcon(chat.chatInfo.incognito)
      }
    } else if (cInfo is ChatInfo.Group) {
      if (progressByTimeout) {
        progressView()
      } else {
        IncognitoIcon(chat.chatInfo.incognito)
      }
    } else {
      IncognitoIcon(chat.chatInfo.incognito)
    }
  }

  Row {
    Box(contentAlignment = Alignment.BottomEnd) {
      ChatInfoImage(cInfo, size = 72.dp * desktopFontSizeSqrtMultiplier)
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
        chatPreviewText()
      }
    }

    Box(
      contentAlignment = Alignment.TopEnd
    ) {
      val ts = chat.chatItems.lastOrNull()?.timestampText ?: getTimestampText(chat.chatInfo.chatTs)
      Text(
        ts,
        color = MaterialTheme.colors.secondary,
        style = MaterialTheme.typography.body2,
        modifier = Modifier.padding(bottom = 5.dp)
      )
      val n = chat.chatStats.unreadCount
      val showNtfsIcon = !chat.chatInfo.ntfsEnabled && (chat.chatInfo is ChatInfo.Direct || chat.chatInfo is ChatInfo.Group)
      val sp17 = with(LocalDensity.current) { 17.sp.toDp() }
      val sp21 = with(LocalDensity.current) { 21.sp.toDp() }
      val sp46 = with(LocalDensity.current) { 46.sp.toDp() }
      if (n > 0 || chat.chatStats.unreadChat) {
        Box(
          Modifier.padding(top = sp21),
          contentAlignment = Alignment.Center
        ) {
          Text(
            if (n > 0) unreadCountStr(n) else "",
            color = Color.White,
            fontSize = 11.sp,
            modifier = Modifier
              .background(if (disabled || showNtfsIcon) MaterialTheme.colors.secondary else MaterialTheme.colors.primaryVariant, shape = CircleShape)
              .badgeLayout()
              .padding(horizontal = 3.dp)
              .padding(vertical = 1.dp)
          )
        }
      } else if (showNtfsIcon) {
        Box(
          Modifier.padding(top = sp21),
          contentAlignment = Alignment.Center
        ) {
          Icon(
            painterResource(MR.images.ic_notifications_off_filled),
            contentDescription = generalGetString(MR.strings.notifications),
            tint = MaterialTheme.colors.secondary,
            modifier = Modifier
              .padding(horizontal = 3.dp)
              .padding(vertical = 1.dp)
              .size(sp17)
          )
        }
      } else if (chat.chatInfo.chatSettings?.favorite == true) {
        Box(
          Modifier.padding(top = sp21),
          contentAlignment = Alignment.Center
        ) {
          Icon(
            painterResource(MR.images.ic_star_filled),
            contentDescription = generalGetString(MR.strings.favorite_chat),
            tint = MaterialTheme.colors.secondary,
            modifier = Modifier
              .padding(horizontal = 3.dp)
              .padding(vertical = 1.dp)
              .size(sp17)
          )
        }
      }
      Box(
        Modifier.padding(top = sp46),
        contentAlignment = Alignment.Center
      ) {
        chatStatusImage()
      }
    }
  }
}

@Composable
fun IncognitoIcon(incognito: Boolean) {
  if (incognito) {
    val sp21 = with(LocalDensity.current) { 21.sp.toDp() }
    Icon(
      painterResource(MR.images.ic_theater_comedy),
      contentDescription = null,
      tint = MaterialTheme.colors.secondary,
      modifier = Modifier
        .size(sp21)
    )
  }
}

@Composable
private fun groupInvitationPreviewText(currentUserProfileDisplayName: String?, groupInfo: GroupInfo): String {
  return if (groupInfo.membership.memberIncognito)
    String.format(stringResource(MR.strings.group_preview_join_as), groupInfo.membership.memberProfile.displayName)
  else
    stringResource(MR.strings.group_preview_you_are_invited)
}

@Composable
fun unreadCountStr(n: Int): String {
  return if (n < 1000) "$n" else "${n / 1000}" + stringResource(MR.strings.thousand_abbreviation)
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
@Composable
fun PreviewChatPreviewView() {
  SimpleXTheme {
    ChatPreviewView(Chat.sampleData, true, null, null, "", contactNetworkStatus = NetworkStatus.Connected(), disabled = false, linkMode = SimplexLinkMode.DESCRIPTION, inProgress = false, progressByTimeout = false)
  }
}
