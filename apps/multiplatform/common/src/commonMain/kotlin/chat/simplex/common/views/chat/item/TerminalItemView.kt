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
import androidx.compose.ui.layout.layout
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.offset
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
import kotlinx.datetime.TimeZone
import kotlinx.datetime.toLocalDateTime
import kotlin.math.abs

val LocalTerminalLayout = staticCompositionLocalOf { false }

private val TERMINAL_GUTTER_WIDTH = 56.dp
private val TERMINAL_GUTTER_TIME_END_PADDING = 6.dp
private val TERMINAL_HEADER_TOP_PADDING = 8.dp
val TERMINAL_TEXT_START_PADDING = 12.dp
val TERMINAL_TEXT_VERTICAL_PADDING = 2.dp
private val TERMINAL_ROW_INSET = 4.dp
private val TERMINAL_ROW_CORNER = 6.dp
private const val TERMINAL_SUBTLE_FILL_ALPHA = 0.04f
private const val TERMINAL_PLACEHOLDER_CONTRAST = 0.15f
private const val TERMINAL_HIGHLIGHT_ALPHA = 0.12f
private const val TERMINAL_BORDER_ALPHA = 0.12f
private val TERMINAL_BAR_BUTTON_SIZE = 24.dp
private val TERMINAL_BAR_ICON_SIZE = 16.dp
private val TERMINAL_BAR_PADDING = 2.dp
private val TERMINAL_ROW_MIN_HEIGHT = TERMINAL_BAR_BUTTON_SIZE + TERMINAL_BAR_PADDING * 2 + 2.dp
private const val TERMINAL_QUICK_REACTIONS = 3
private const val TERMINAL_GROUP_SECONDS = 60
val TERMINAL_META_BOTTOM_PADDING = 1.dp
val TERMINAL_DELETED_FONT_SIZE = 14.sp
val TERMINAL_REPLY_AVATAR_SIZE = 16.dp
val TERMINAL_REPLY_ICON_SIZE = 14.dp
val TERMINAL_REPLY_FONT_SIZE = 13.sp
val TERMINAL_REPLY_SENDER_MAX_WIDTH = 160.dp
private val TERMINAL_PILL_SHAPE = RoundedCornerShape(50)
val TERMINAL_PILL_HORIZONTAL_PADDING = 6.dp
private const val TERMINAL_REACTED_BACKGROUND_ALPHA = 0.1f
private const val TERMINAL_REACTED_BORDER_ALPHA = 0.4f

fun terminalItemHasHeader(item: ChatItem, older: ChatItem?): Boolean =
  item.content.hasTerminalSender && (
    older == null
      || !older.content.hasTerminalSender
      || !sameTerminalSender(item, older)
      || abs(item.meta.createdAt.epochSeconds - older.meta.createdAt.epochSeconds) >= TERMINAL_GROUP_SECONDS
      || localDate(item) != localDate(older)
  )

private fun sameTerminalSender(item: ChatItem, older: ChatItem): Boolean {
  val o = older.chatDir
  return when (val d = item.chatDir) {
    is CIDirection.GroupRcv -> o is CIDirection.GroupRcv && o.groupMember.groupMemberId == d.groupMember.groupMemberId
    is CIDirection.ChannelRcv -> o is CIDirection.ChannelRcv
    is CIDirection.GroupSnd -> o is CIDirection.GroupSnd && older.meta.showGroupAsSender == item.meta.showGroupAsSender
    is CIDirection.DirectSnd -> o is CIDirection.DirectSnd
    is CIDirection.DirectRcv -> o is CIDirection.DirectRcv
    is CIDirection.LocalSnd -> o is CIDirection.LocalSnd
    is CIDirection.LocalRcv -> o is CIDirection.LocalRcv
  }
}

private fun localDate(item: ChatItem) = item.meta.itemTs.toLocalDateTime(TimeZone.currentSystemDefault()).date

internal fun terminalHoverBarVisible(cItem: ChatItem, rowHovered: Boolean, reactionPickerOpen: Boolean, canReact: Boolean, selecting: Boolean): Boolean =
  !selecting && cItem.content.msgContent != null && !cItem.isLiveDummy && (rowHovered || (reactionPickerOpen && canReact))

private val CIContent.hasTerminalSender: Boolean get() = when (this) {
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
fun TerminalChatItemRow(
  chatInfo: ChatInfo,
  cItem: ChatItem,
  showHeader: Boolean,
  showTime: Boolean,
  highlighted: Boolean,
  showMemberInfo: (GroupInfo, GroupMember) -> Unit,
  showChatInfo: () -> Unit,
  modifier: Modifier = Modifier,
  hoverBar: @Composable BoxScope.(rowHovered: Boolean) -> Unit,
  content: @Composable () -> Unit
) {
  val interactionSource = remember { MutableInteractionSource() }
  val hovered = interactionSource.collectIsHoveredAsState().value
  val background = when {
    highlighted -> MaterialTheme.colors.primary.copy(alpha = TERMINAL_HIGHLIGHT_ALPHA)
    hovered -> MaterialTheme.colors.onBackground.copy(alpha = TERMINAL_SUBTLE_FILL_ALPHA)
    else -> Color.Transparent
  }
  Box(modifier.fillMaxWidth().padding(horizontal = TERMINAL_ROW_INSET).hoverable(interactionSource).background(background, RoundedCornerShape(TERMINAL_ROW_CORNER))) {
    Row(
      Modifier
        .heightIn(min = if (cItem.content.msgContent != null) TERMINAL_ROW_MIN_HEIGHT else 0.dp)
        .padding(top = if (showHeader) TERMINAL_HEADER_TOP_PADDING else 0.dp, end = DEFAULT_PADDING_HALF)
    ) {
      Box(Modifier.width(TERMINAL_GUTTER_WIDTH * fontSizeSqrtMultiplier)) {
        if (showHeader) {
          Box(Modifier.align(Alignment.TopCenter)) {
            TerminalAvatarContrast {
              TerminalSenderImage(chatInfo, cItem, showMemberInfo, showChatInfo)
            }
          }
        } else if ((hovered || showTime) && cItem.content.hasTerminalSender) {
          Text(
            cItem.timestampText,
            Modifier.align(Alignment.TopEnd).padding(top = TERMINAL_TEXT_VERTICAL_PADDING + 3.dp, end = TERMINAL_GUTTER_TIME_END_PADDING),
            fontSize = 10.sp,
            color = MaterialTheme.colors.secondary,
            maxLines = 1,
            softWrap = false
          )
        }
      }
      Column(Modifier.weight(1f)) {
        if (showHeader) {
          TerminalSenderHeader(chatInfo, cItem)
        }
        content()
      }
    }
    hoverBar(hovered)
  }
}

@Composable
fun BoxScope.TerminalHoverBarSlot(
  rowHovered: Boolean,
  cInfo: ChatInfo,
  cItem: ChatItem,
  composeState: MutableState<ComposeState>,
  useLinkPreviews: Boolean,
  selecting: Boolean,
  setReaction: (ChatInfo, ChatItem, Boolean, MsgReaction) -> Unit,
  forwardItem: (ChatInfo, ChatItem) -> Unit,
  showMenu: MutableState<Boolean>
) {
  val showReactionPicker = remember { mutableStateOf(false) }
  if (!rowHovered && !showReactionPicker.value) return
  val live by remember { derivedStateOf { composeState.value.liveMessage != null } }
  val quickActions = itemQuickActions(cInfo, cItem, live)
  val visible = terminalHoverBarVisible(cItem, rowHovered, showReactionPicker.value, quickActions.react, selecting)
  LaunchedEffect(visible, quickActions.react) {
    if (!visible || !quickActions.react) showReactionPicker.value = false
  }
  if (visible) {
    TerminalHoverBar(
      quickActions,
      cInfo,
      cItem,
      setReaction,
      showReactionPicker,
      reply = { replyToItem(composeState, cItem, useLinkPreviews) },
      forward = { forwardItem(cInfo, cItem) },
      showMenu,
      Modifier.align(Alignment.TopEnd).padding(end = DEFAULT_PADDING)
    )
  }
}

@Composable
fun TerminalAvatarContrast(content: @Composable () -> Unit) {
  val colors = MaterialTheme.colors
  MaterialTheme(
    colors = colors.copy(secondaryVariant = colors.secondaryVariant.mixWith(colors.onBackground, 1 - TERMINAL_PLACEHOLDER_CONTRAST)),
    typography = MaterialTheme.typography,
    shapes = MaterialTheme.shapes,
    content = content
  )
}

private data class TerminalSender(val name: String, val badge: LocalBadge?, val role: String?)

private fun terminalSender(chatInfo: ChatInfo, cItem: ChatItem): TerminalSender = when (val dir = cItem.chatDir) {
  is CIDirection.GroupRcv -> memberSender(dir.groupMember, chatInfo.isChannel)
  is CIDirection.ChannelRcv -> channelSender(chatInfo)
  is CIDirection.GroupSnd -> when {
    cItem.meta.showGroupAsSender -> channelSender(chatInfo)
    chatInfo is ChatInfo.Group -> memberSender(chatInfo.groupInfo.membership, chatInfo.isChannel)
    else -> userSender(chatInfo)
  }
  is CIDirection.DirectRcv -> TerminalSender(chatInfo.displayName, chatInfo.nameBadge, null)
  is CIDirection.DirectSnd,
  is CIDirection.LocalSnd,
  is CIDirection.LocalRcv -> userSender(chatInfo)
}

private fun channelSender(chatInfo: ChatInfo): TerminalSender =
  TerminalSender(chatInfo.chatViewName, null, generalGetString(MR.strings.channel_role_label))

private fun memberSender(member: GroupMember, isChannel: Boolean): TerminalSender =
  TerminalSender(member.displayName, member.nameBadge, if (member.memberRole > GroupMemberRole.Member) member.memberRole.text(isChannel) else null)

private fun userSender(chatInfo: ChatInfo): TerminalSender {
  val user = chatModel.currentUser.value
  return if (chatInfo.incognito) {
    TerminalSender(generalGetString(MR.strings.incognito), null, null)
  } else {
    TerminalSender(user?.displayName ?: "", user?.profile?.localBadge, null)
  }
}

@Composable
private fun TerminalSenderHeader(chatInfo: ChatInfo, cItem: ChatItem) {
  val sender = terminalSender(chatInfo, cItem)
  Row(Modifier.padding(start = TERMINAL_TEXT_START_PADDING, bottom = TERMINAL_TEXT_VERTICAL_PADDING), verticalAlignment = Alignment.CenterVertically) {
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
private fun TerminalSenderImage(chatInfo: ChatInfo, cItem: ChatItem, showMemberInfo: (GroupInfo, GroupMember) -> Unit, showChatInfo: () -> Unit) {
  val size = MEMBER_IMAGE_SIZE * fontSizeSqrtMultiplier
  when (val dir = cItem.chatDir) {
    is CIDirection.GroupRcv ->
      Box(if (chatInfo is ChatInfo.Group) Modifier.clip(CircleShape).clickable { showMemberInfo(chatInfo.groupInfo, dir.groupMember) } else Modifier) {
        MemberImage(dir.groupMember)
      }
    is CIDirection.ChannelRcv,
    is CIDirection.DirectRcv ->
      Box(Modifier.clip(CircleShape).clickable { showChatInfo() }) {
        ChatInfoImage(chatInfo, size)
      }
    is CIDirection.GroupSnd -> when {
      cItem.meta.showGroupAsSender ->
        Box(Modifier.clip(CircleShape).clickable { showChatInfo() }) {
          ChatInfoImage(chatInfo, size)
        }
      chatInfo is ChatInfo.Group -> MemberImage(chatInfo.groupInfo.membership)
      else -> TerminalUserImage(chatInfo, size)
    }
    is CIDirection.DirectSnd,
    is CIDirection.LocalSnd,
    is CIDirection.LocalRcv -> TerminalUserImage(chatInfo, size)
  }
}

@Composable
fun TerminalUserImage(chatInfo: ChatInfo, size: Dp) {
  if (chatInfo.incognito) {
    IncognitoImage(size)
  } else {
    ProfileImage(size, chatModel.currentUser.value?.image)
  }
}

@Composable
private fun TerminalHoverBar(
  quickActions: ItemQuickActions,
  cInfo: ChatInfo,
  cItem: ChatItem,
  setReaction: (ChatInfo, ChatItem, Boolean, MsgReaction) -> Unit,
  showReactionPicker: MutableState<Boolean>,
  reply: () -> Unit,
  forward: () -> Unit,
  showMenu: MutableState<Boolean>,
  modifier: Modifier = Modifier
) {
  Surface(
    modifier,
    shape = RoundedCornerShape(DEFAULT_PADDING_HALF),
    border = BorderStroke(1.dp, MaterialTheme.colors.onBackground.copy(alpha = TERMINAL_BORDER_ALPHA)),
    elevation = 2.dp
  ) {
    Row(Modifier.padding(TERMINAL_BAR_PADDING), verticalAlignment = Alignment.CenterVertically) {
      if (quickActions.react) {
        availableReactions(cItem).take(TERMINAL_QUICK_REACTIONS).forEach { r ->
          Box(
            Modifier.size(TERMINAL_BAR_BUTTON_SIZE).clip(CircleShape).clickable { setReaction(cInfo, cItem, true, r) },
            contentAlignment = Alignment.Center
          ) {
            ReactionIcon(r.text, 14.sp)
          }
        }
        Box {
          TerminalBarButton(MR.images.ic_add_reaction, stringResource(MR.strings.add_reaction)) { showReactionPicker.value = true }
          DefaultDropdownMenu(showReactionPicker) {
            MsgReactionsMenu(cInfo, cItem, setReaction, showReactionPicker)
          }
        }
      }
      if (quickActions.reply) {
        TerminalBarButton(MR.images.ic_reply, stringResource(MR.strings.reply_verb), reply)
      }
      if (quickActions.forward) {
        TerminalBarButton(MR.images.ic_forward, stringResource(MR.strings.forward_chat_item), forward)
      }
      TerminalBarButton(MR.images.ic_more_horiz, stringResource(MR.strings.more_actions)) { showMenu.value = true }
    }
  }
}

@Composable
private fun TerminalBarButton(icon: ImageResource, description: String, onClick: () -> Unit) {
  IconButton(onClick, Modifier.size(TERMINAL_BAR_BUTTON_SIZE)) {
    Icon(painterResource(icon), description, Modifier.size(TERMINAL_BAR_ICON_SIZE), tint = MaterialTheme.colors.secondary)
  }
}

fun terminalMetaVisible(ci: ChatItem, chatTTL: Int?, showViaProxy: Boolean, secondaryColor: Color): Boolean =
  reserveSpaceForMeta(
    ci.meta, chatTTL, ci.encryptedFile, secondaryColor,
    showStatus = ci.chatDir.sent, showViaProxy = showViaProxy, showTimestamp = true, signedFileVerified = ci.file?.loaded
  ) != ci.meta.timestampText

fun Modifier.terminalStartOffset(offset: () -> Dp): Modifier = layout { measurable, constraints ->
  val offsetPx = offset().roundToPx()
  val placeable = measurable.measure(constraints.offset(horizontal = -offsetPx))
  layout(placeable.width + offsetPx, placeable.height) { placeable.place(offsetPx, 0) }
}

@Composable
fun Modifier.terminalReactionPill(userReacted: Boolean): Modifier {
  val background = if (userReacted) MaterialTheme.colors.primary.copy(alpha = TERMINAL_REACTED_BACKGROUND_ALPHA) else MaterialTheme.colors.onBackground.copy(alpha = TERMINAL_SUBTLE_FILL_ALPHA)
  val border = if (userReacted) MaterialTheme.colors.primary.copy(alpha = TERMINAL_REACTED_BORDER_ALPHA) else MaterialTheme.colors.onBackground.copy(alpha = TERMINAL_BORDER_ALPHA)
  return clip(TERMINAL_PILL_SHAPE).background(background).border(1.dp, border, TERMINAL_PILL_SHAPE)
}

@Composable
fun TerminalDateSeparator(text: String) {
  Box(Modifier.fillMaxWidth().padding(vertical = DEFAULT_PADDING_HALF), contentAlignment = Alignment.Center) {
    Divider()
    Text(
      text,
      Modifier
        .background(MaterialTheme.colors.background, TERMINAL_PILL_SHAPE)
        .border(1.dp, MaterialTheme.colors.onBackground.copy(alpha = TERMINAL_BORDER_ALPHA), TERMINAL_PILL_SHAPE)
        .padding(horizontal = TERMINAL_TEXT_START_PADDING, vertical = TERMINAL_TEXT_VERTICAL_PADDING * 2),
      fontSize = 13.sp,
      fontWeight = FontWeight.SemiBold,
      color = MaterialTheme.colors.onBackground,
      maxLines = 1
    )
  }
}

private val SELECTION_CHECKBOX_SIZE = 22.dp

@Composable
fun terminalSelectionTopPadding(showHeader: Boolean): Dp =
  if (showHeader) TERMINAL_HEADER_TOP_PADDING + (MEMBER_IMAGE_SIZE * fontSizeSqrtMultiplier - SELECTION_CHECKBOX_SIZE * fontSizeMultiplier) / 2
  else TERMINAL_TEXT_VERTICAL_PADDING + 1.dp

private val TERMINAL_COMPOSE_CORNER = 8.dp
private const val TERMINAL_COMPOSE_FILL_ALPHA = 0.06f

@Composable
fun Modifier.terminalComposeField(): Modifier =
  padding(horizontal = DEFAULT_PADDING_HALF * 1.5f, vertical = DEFAULT_PADDING_HALF)
    .clip(RoundedCornerShape(TERMINAL_COMPOSE_CORNER))
    .background(MaterialTheme.colors.onBackground.copy(alpha = TERMINAL_COMPOSE_FILL_ALPHA))
