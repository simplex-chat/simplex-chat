package chat.simplex.common.views.chat.item

import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.interaction.collectIsHoveredAsState
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.*
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

private val FLAT_GUTTER_WIDTH = 56.dp
private val FLAT_HEADER_TOP_PADDING = 6.dp
// matches the horizontal text padding in CIMarkdownText
private val FLAT_HEADER_START_PADDING = 12.dp
private const val FLAT_HOVER_ALPHA = 0.04f
private const val FLAT_BAR_BORDER_ALPHA = 0.12f
private val FLAT_BAR_BUTTON_SIZE = 30.dp
private const val FLAT_QUICK_REACTIONS = 3

fun flatItemHasHeader(item: ChatItem, older: ChatItem?): Boolean =
  item.content.hasFlatSender && (
    older == null
      || !older.content.hasFlatSender
      || getTimestampDateText(older.meta.itemTs) != getTimestampDateText(item.meta.itemTs)
      || getItemSeparationLargeGap(item, older)
  )

val CIContent.hasFlatSender: Boolean get() = when (this) {
  is CIContent.SndMsgContent,
  is CIContent.RcvMsgContent,
  is CIContent.SndDeleted,
  is CIContent.RcvDeleted,
  is CIContent.SndCall,
  is CIContent.RcvCall,
  is CIContent.RcvIntegrityError,
  is CIContent.RcvDecryptionError,
  is CIContent.RcvMsgErrorContent,
  is CIContent.RcvGroupInvitation,
  is CIContent.SndGroupInvitation,
  is CIContent.SndModerated,
  is CIContent.RcvModerated,
  is CIContent.RcvBlocked,
  is CIContent.InvalidJSON -> true
  is CIContent.RcvDirectEventContent,
  is CIContent.RcvGroupEventContent,
  is CIContent.SndGroupEventContent,
  is CIContent.RcvConnEventContent,
  is CIContent.SndConnEventContent,
  is CIContent.RcvChatFeature,
  is CIContent.SndChatFeature,
  is CIContent.RcvChatPreference,
  is CIContent.SndChatPreference,
  is CIContent.RcvGroupFeature,
  is CIContent.SndGroupFeature,
  is CIContent.RcvChatFeatureRejected,
  is CIContent.RcvGroupFeatureRejected,
  is CIContent.SndDirectE2EEInfo,
  is CIContent.RcvDirectE2EEInfo,
  is CIContent.SndGroupE2EEInfo,
  is CIContent.RcvGroupE2EEInfo,
  is CIContent.ChatBanner -> false
}

@Composable
fun FlatChatItemRow(
  chatInfo: ChatInfo,
  cItem: ChatItem,
  showHeader: Boolean,
  hoveredItemId: MutableState<Long?>,
  showMemberInfo: (GroupInfo, GroupMember) -> Unit,
  showChatInfo: () -> Unit,
  modifier: Modifier,
  content: @Composable () -> Unit
) {
  val interactionSource = remember { MutableInteractionSource() }
  val hovered = interactionSource.collectIsHoveredAsState().value
  LaunchedEffect(hovered) {
    if (hovered) {
      hoveredItemId.value = cItem.id
    } else if (hoveredItemId.value == cItem.id) {
      hoveredItemId.value = null
    }
  }
  Row(
    modifier
      .fillMaxWidth()
      .hoverable(interactionSource)
      .background(if (hovered) MaterialTheme.colors.onBackground.copy(alpha = FLAT_HOVER_ALPHA) else Color.Transparent)
      .padding(top = if (showHeader) FLAT_HEADER_TOP_PADDING else 0.dp, end = DEFAULT_PADDING_HALF)
  ) {
    Box(Modifier.width(FLAT_GUTTER_WIDTH * fontSizeSqrtMultiplier), contentAlignment = Alignment.TopCenter) {
      if (showHeader) {
        FlatSenderImage(chatInfo, cItem.chatDir, showMemberInfo, showChatInfo)
      } else if (hovered && cItem.content.hasFlatSender) {
        Text(cItem.timestampText, Modifier.padding(top = 6.dp), fontSize = 11.sp, color = MaterialTheme.colors.secondary, maxLines = 1)
      }
    }
    Column(Modifier.weight(1f)) {
      if (showHeader) {
        FlatSenderHeader(chatInfo, cItem)
      }
      content()
    }
  }
}

private data class FlatSender(val name: String, val badge: LocalBadge?, val role: String?)

private fun flatSender(chatInfo: ChatInfo, dir: CIDirection): FlatSender = when (dir) {
  is CIDirection.GroupRcv -> memberSender(dir.groupMember, chatInfo.isChannel)
  is CIDirection.ChannelRcv -> FlatSender(chatInfo.chatViewName, null, generalGetString(MR.strings.channel_role_label))
  is CIDirection.GroupSnd -> if (chatInfo is ChatInfo.Group) memberSender(chatInfo.groupInfo.membership, chatInfo.isChannel) else userSender(chatInfo)
  is CIDirection.DirectRcv -> FlatSender(chatInfo.displayName, chatInfo.nameBadge, null)
  is CIDirection.DirectSnd,
  is CIDirection.LocalSnd,
  is CIDirection.LocalRcv -> userSender(chatInfo)
}

private fun memberSender(member: GroupMember, isChannel: Boolean): FlatSender =
  FlatSender(member.displayName, member.nameBadge, if (member.memberRole > GroupMemberRole.Member) member.memberRole.text(isChannel) else null)

private fun userSender(chatInfo: ChatInfo): FlatSender {
  val user = chatModel.currentUser.value
  return if (chatInfo.incognito) {
    FlatSender(generalGetString(MR.strings.incognito), null, null)
  } else {
    FlatSender(user?.displayName ?: "", user?.profile?.localBadge, null)
  }
}

@Composable
private fun FlatSenderHeader(chatInfo: ChatInfo, cItem: ChatItem) {
  val sender = flatSender(chatInfo, cItem.chatDir)
  Row(Modifier.padding(start = FLAT_HEADER_START_PADDING), verticalAlignment = Alignment.CenterVertically) {
    NameWithBadge(
      sender.name,
      sender.badge,
      Modifier.weight(1f, false),
      fontWeight = FontWeight.SemiBold,
      overflow = TextOverflow.Ellipsis,
      maxLines = 1
    )
    if (sender.role != null) {
      Text(sender.role, Modifier.padding(start = DEFAULT_PADDING_HALF), fontSize = 13.sp, fontWeight = FontWeight.Medium, color = MaterialTheme.colors.secondary, maxLines = 1)
    }
    Text(cItem.timestampText, Modifier.padding(start = DEFAULT_PADDING_HALF), fontSize = 12.sp, color = MaterialTheme.colors.secondary, maxLines = 1)
  }
}

@Composable
private fun FlatSenderImage(chatInfo: ChatInfo, dir: CIDirection, showMemberInfo: (GroupInfo, GroupMember) -> Unit, showChatInfo: () -> Unit) {
  val size = MEMBER_IMAGE_SIZE * fontSizeSqrtMultiplier
  when (dir) {
    is CIDirection.GroupRcv ->
      Box(if (chatInfo is ChatInfo.Group) Modifier.clip(CircleShape).clickable { showMemberInfo(chatInfo.groupInfo, dir.groupMember) } else Modifier) {
        MemberImage(dir.groupMember)
      }
    is CIDirection.ChannelRcv,
    is CIDirection.DirectRcv ->
      Box(Modifier.clip(CircleShape).clickable { showChatInfo() }) {
        ChatInfoImage(chatInfo, size)
      }
    is CIDirection.GroupSnd ->
      if (chatInfo is ChatInfo.Group) MemberImage(chatInfo.groupInfo.membership) else UserImage(chatInfo, size)
    is CIDirection.DirectSnd,
    is CIDirection.LocalSnd,
    is CIDirection.LocalRcv -> UserImage(chatInfo, size)
  }
}

@Composable
private fun UserImage(chatInfo: ChatInfo, size: Dp) {
  if (chatInfo.incognito) {
    IncognitoImage(size)
  } else {
    ProfileImage(size, chatModel.currentUser.value?.image)
  }
}

@Composable
fun FlatHoverBar(
  quickActions: ItemQuickActions,
  cInfo: ChatInfo,
  cItem: ChatItem,
  setReaction: (ChatInfo, ChatItem, Boolean, MsgReaction) -> Unit,
  showReactionPicker: MutableState<Boolean>,
  reply: () -> Unit,
  forward: () -> Unit,
  showMenu: MutableState<Boolean>,
  modifier: Modifier
) {
  Surface(
    modifier,
    shape = RoundedCornerShape(DEFAULT_PADDING_HALF),
    border = BorderStroke(1.dp, MaterialTheme.colors.onBackground.copy(alpha = FLAT_BAR_BORDER_ALPHA)),
    elevation = 2.dp
  ) {
    Row(Modifier.padding(2.dp), verticalAlignment = Alignment.CenterVertically) {
      if (quickActions.react) {
        availableReactions(cItem).take(FLAT_QUICK_REACTIONS).forEach { r ->
          Box(
            Modifier.size(FLAT_BAR_BUTTON_SIZE).clip(CircleShape).clickable { setReaction(cInfo, cItem, true, r) },
            contentAlignment = Alignment.Center
          ) {
            ReactionIcon(r.text, 12.sp)
          }
        }
        Box {
          FlatBarButton(MR.images.ic_add_reaction, stringResource(MR.strings.add_reaction)) { showReactionPicker.value = true }
          DefaultDropdownMenu(showReactionPicker) {
            MsgReactionsMenu(cInfo, cItem, setReaction, showReactionPicker)
          }
        }
      }
      if (quickActions.reply) {
        FlatBarButton(MR.images.ic_reply, stringResource(MR.strings.reply_verb), reply)
      }
      if (quickActions.forward) {
        FlatBarButton(MR.images.ic_forward, stringResource(MR.strings.forward_chat_item), forward)
      }
      FlatBarButton(MR.images.ic_more_horiz, stringResource(MR.strings.more_actions)) { showMenu.value = true }
    }
  }
}

@Composable
private fun FlatBarButton(icon: ImageResource, description: String, onClick: () -> Unit) {
  IconButton(onClick, Modifier.size(FLAT_BAR_BUTTON_SIZE)) {
    Icon(painterResource(icon), description, Modifier.size(18.dp), tint = MaterialTheme.colors.secondary)
  }
}
