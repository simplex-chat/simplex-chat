package chat.simplex.common.views.chatlist

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
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.ui.draw.clip
import androidx.compose.ui.input.pointer.pointerHoverIcon
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.text.style.*
import androidx.compose.ui.unit.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.*
import chat.simplex.common.model.GroupInfo
import chat.simplex.common.platform.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.chat.item.*
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
    Icon(
      painterResource(MR.images.ic_cancel_filled),
      stringResource(MR.strings.icon_descr_group_inactive),
      Modifier.size(18.sp.toDp()).background(MaterialTheme.colors.background, CircleShape),
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
    Icon(painterResource(MR.images.ic_verified_user), null, Modifier.size(19.sp.toDp()).padding(end = 3.sp.toDp(), top = 1.sp.toDp()), tint = MaterialTheme.colors.secondary)
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
          style = TextStyle(
            fontFamily = Inter,
            fontSize = 15.sp,
            color = if (isInDarkTheme()) MessagePreviewDark else MessagePreviewLight,
            lineHeight = 21.sp
          ),
          inlineContent = inlineTextContent,
          modifier = Modifier.fillMaxWidth(),
        )
      }
    } else {
      when (cInfo) {
        is ChatInfo.Direct ->
          if (cInfo.contact.activeConn == null && cInfo.contact.profile.contactLink != null) {
            Text(stringResource(MR.strings.contact_tap_to_connect), color = MaterialTheme.colors.primary)
          } else if (!cInfo.contact.sndReady && cInfo.contact.activeConn != null) {
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
  fun chatItemContentPreview(chat: Chat, ci: ChatItem?) {
    val mc = ci?.content?.msgContent
    val provider by remember(chat.id, ci?.id, ci?.file?.fileStatus) {
      mutableStateOf({ providerForGallery(0, chat.chatItems, ci?.id ?: 0) {} })
    }
    val uriHandler = LocalUriHandler.current
    when (mc) {
      is MsgContent.MCLink -> SmallContentPreview {
        IconButton({ uriHandler.openUriCatching(mc.preview.uri) }, Modifier.desktopPointerHoverIconHand()) {
          Image(base64ToBitmap(mc.preview.image), null, contentScale = ContentScale.Crop)
        }
        Box(Modifier.align(Alignment.TopEnd).size(15.sp.toDp()).background(Color.Black.copy(0.25f), CircleShape), contentAlignment = Alignment.Center) {
          Icon(painterResource(MR.images.ic_arrow_outward), null, Modifier.size(13.sp.toDp()), tint = Color.White)
        }
      }
      is MsgContent.MCImage -> SmallContentPreview {
        CIImageView(image = mc.image, file = ci.file, provider, remember { mutableStateOf(false) }, smallView = true) {
          val user = chatModel.currentUser.value ?: return@CIImageView
          withBGApi { chatModel.controller.receiveFile(chat.remoteHostId, user, it) }
        }
      }
      is MsgContent.MCVideo -> SmallContentPreview {
        CIVideoView(image = mc.image, mc.duration, file = ci.file, provider, remember { mutableStateOf(false) }, smallView = true) {
          val user = chatModel.currentUser.value ?: return@CIVideoView
          withBGApi { chatModel.controller.receiveFile(chat.remoteHostId, user, it) }
        }
      }
      is MsgContent.MCVoice -> SmallContentPreviewVoice() {
        CIVoiceView(mc.duration, ci.file, ci.meta.itemEdited, ci.chatDir.sent, hasText = false, ci, cInfo.timedMessagesTTL, showViaProxy = false, smallView = true, longClick = {}) {
          val user = chatModel.currentUser.value ?: return@CIVoiceView
          withBGApi { chatModel.controller.receiveFile(chat.remoteHostId, user, it) }
        }
      }
      is MsgContent.MCFile -> SmallContentPreviewFile {
        CIFileView(ci.file, false, remember { mutableStateOf(false) }, smallView = true) {
          val user = chatModel.currentUser.value ?: return@CIFileView
          withBGApi { chatModel.controller.receiveFile(chat.remoteHostId, user, it) }
        }
      }
      else -> {}
    }
  }

  @Composable
  fun progressView() {
    CircularProgressIndicator(
      Modifier
        .size(15.sp.toDp())
        .offset(y = 2.sp.toDp()),
      color = MaterialTheme.colors.secondary,
      strokeWidth = 1.5.dp
    )
  }

  @Composable
  fun chatStatusImage() {
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
                .size(19.sp.toDp())
                .offset(x = 2.sp.toDp())
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
      ChatInfoImage(cInfo, size = 72.dp * fontSizeSqrtMultiplier)
      Box(Modifier.padding(end = 6.sp.toDp(), bottom = 6.sp.toDp())) {
        chatPreviewImageOverlayIcon()
      }
    }
    Spacer(Modifier.width(8.dp))
    Column(Modifier.weight(1f)) {
      Row {
        Box(Modifier.weight(1f)) {
          chatPreviewTitle()
        }
        Spacer(Modifier.width(8.sp.toDp()))
        val ts = chat.chatItems.lastOrNull()?.timestampText ?: getTimestampText(chat.chatInfo.chatTs)
        ChatListTimestampView(ts)
      }
      Row(Modifier.heightIn(min = 46.sp.toDp()).fillMaxWidth()) {
        Row(Modifier.padding(top = 3.sp.toDp()).weight(1f)) {
          val activeVoicePreview: MutableState<(ActiveVoicePreview)?> = remember(chat.id) { mutableStateOf(null) }
          val chat = activeVoicePreview.value?.chat ?: chat
          val ci = activeVoicePreview.value?.ci ?: chat.chatItems.lastOrNull()
          val mc = ci?.content?.msgContent
          if ((showChatPreviews && chatModelDraftChatId != chat.id) || activeVoicePreview.value != null) {
            chatItemContentPreview(chat, ci)
          }
          if (mc !is MsgContent.MCVoice || mc.text.isNotEmpty() || chatModelDraftChatId == chat.id) {
            Box(Modifier.offset(x = if (mc is MsgContent.MCFile) -15.sp.toDp() else 0.dp)) {
              chatPreviewText()
            }
          }
          LaunchedEffect(AudioPlayer.currentlyPlaying.value, activeVoicePreview.value) {
            val playing = AudioPlayer.currentlyPlaying.value
            when {
              playing == null -> activeVoicePreview.value = null
              activeVoicePreview.value == null -> if (mc is MsgContent.MCVoice && playing.fileSource.filePath == ci.file?.fileSource?.filePath) {
                activeVoicePreview.value = ActiveVoicePreview(chat, ci, mc)
              }
              else -> if (playing.fileSource.filePath != ci?.file?.fileSource?.filePath) {
                activeVoicePreview.value = null
              }
            }
          }
        }

        Spacer(Modifier.width(8.sp.toDp()))

        Box(Modifier.widthIn(min = 34.sp.toDp()), contentAlignment = Alignment.TopEnd) {
          val n = chat.chatStats.unreadCount
          val showNtfsIcon = !chat.chatInfo.ntfsEnabled && (chat.chatInfo is ChatInfo.Direct || chat.chatInfo is ChatInfo.Group)
          if (n > 0 || chat.chatStats.unreadChat) {
            Text(
              if (n > 0) unreadCountStr(n) else "",
              color = Color.White,
              fontSize = 10.sp,
              style = TextStyle(textAlign = TextAlign.Center),
              modifier = Modifier
                .offset(y = 3.sp.toDp())
                .background(if (disabled || showNtfsIcon) MaterialTheme.colors.secondary else MaterialTheme.colors.primaryVariant, shape = CircleShape)
                .badgeLayout()
                .padding(horizontal = 2.sp.toDp())
                .padding(vertical = 1.sp.toDp())
            )
          } else if (showNtfsIcon) {
            Icon(
              painterResource(MR.images.ic_notifications_off_filled),
              contentDescription = generalGetString(MR.strings.notifications),
              tint = MaterialTheme.colors.secondary,
              modifier = Modifier
                .padding(start = 2.sp.toDp())
                .size(18.sp.toDp())
                .offset(x = 2.5.sp.toDp(), y = 2.sp.toDp())
            )
          } else if (chat.chatInfo.chatSettings?.favorite == true) {
            Icon(
              painterResource(MR.images.ic_star_filled),
              contentDescription = generalGetString(MR.strings.favorite_chat),
              tint = MaterialTheme.colors.secondary,
              modifier = Modifier
                .size(20.sp.toDp())
                .offset(x = 2.5.sp.toDp())
            )
          }
          Box(
            Modifier.offset(y = 28.sp.toDp()),
            contentAlignment = Alignment.Center
          ) {
            chatStatusImage()
          }
        }
      }
    }
  }
}

@Composable
private fun SmallContentPreview(content: @Composable BoxScope.() -> Unit) {
  Box(Modifier.padding(top = 2.sp.toDp(), end = 8.sp.toDp()).size(36.sp.toDp()).border(1.dp, MaterialTheme.colors.onSurface.copy(alpha = 0.12f), RoundedCornerShape(22)).clip(RoundedCornerShape(22))) {
    content()
  }
}

@Composable
private fun SmallContentPreviewVoice(content: @Composable () -> Unit) {
  Box(Modifier.padding(top = 2.sp.toDp(), end = 8.sp.toDp()).height(voiceMessageSizeBasedOnSquareSize(36f).sp.toDp())) {
    content()
  }
}

@Composable
private fun SmallContentPreviewFile(content: @Composable () -> Unit) {
  Box(Modifier.padding(top = 3.sp.toDp(), end = 8.sp.toDp()).offset(x = -8.sp.toDp(), y = -4.sp.toDp()).height(41.sp.toDp())) {
    content()
  }
}

@Composable
fun IncognitoIcon(incognito: Boolean) {
  if (incognito) {
    Icon(
      painterResource(MR.images.ic_theater_comedy),
      contentDescription = null,
      tint = MaterialTheme.colors.secondary,
      modifier = Modifier
        .size(21.sp.toDp())
        .offset(x = 1.sp.toDp())
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

@Composable fun ChatListTimestampView(ts: String) {
  Box(contentAlignment = Alignment.BottomStart) {
    // This should be the same font style as in title to make date located on the same line as title
    Text(
      " ",
      style = MaterialTheme.typography.h3,
      fontWeight = FontWeight.Bold,
    )
    Text(
      ts,
      Modifier.padding(bottom = 5.sp.toDp()).offset(x = if (appPlatform.isDesktop) 1.5.sp.toDp() else 0.dp),
      color = MaterialTheme.colors.secondary,
      style = MaterialTheme.typography.body2.copy(fontSize = 13.sp),
    )
  }
}

private data class ActiveVoicePreview(
  val chat: Chat,
  val ci: ChatItem,
  val mc: MsgContent.MCVoice
)

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
