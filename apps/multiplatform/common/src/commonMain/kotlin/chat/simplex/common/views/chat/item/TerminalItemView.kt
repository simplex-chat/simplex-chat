package chat.simplex.common.views.chat.item

import androidx.compose.foundation.LocalIndication
import androidx.compose.foundation.clickable
import androidx.compose.foundation.gestures.awaitEachGesture
import androidx.compose.foundation.gestures.awaitFirstDown
import androidx.compose.foundation.gestures.waitForUpOrCancellation
import androidx.compose.foundation.hoverable
import androidx.compose.foundation.indication
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.interaction.PressInteraction
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.*
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.datetime.TimeZone
import kotlinx.datetime.toLocalDateTime
import kotlin.math.abs

val LocalTerminalLayout = staticCompositionLocalOf { false }

fun terminalItemHasHeader(item: ChatItem, older: ChatItem?): Boolean =
  item.content.hasTerminalSender && (
    older == null
      || !older.content.hasTerminalSender
      || !sameTerminalSender(item, older)
      || abs(item.meta.createdAt.epochSeconds - older.meta.createdAt.epochSeconds) >= 60
      || localDate(item) != localDate(older)
  )

private fun sameTerminalSender(item: ChatItem, older: ChatItem): Boolean {
  val d = item.chatDir
  val o = older.chatDir
  return when {
    item.channelIsSender || older.channelIsSender -> item.channelIsSender && older.channelIsSender
    d is CIDirection.GroupRcv -> o is CIDirection.GroupRcv && o.groupMember.groupMemberId == d.groupMember.groupMemberId
    else -> d::class == o::class
  }
}

private val ChatItem.channelIsSender: Boolean get() =
  chatDir is CIDirection.ChannelRcv || (chatDir is CIDirection.GroupSnd && meta.showGroupAsSender)

private fun localDate(item: ChatItem) = item.meta.itemTs.toLocalDateTime(TimeZone.currentSystemDefault()).date

private val CIContent.hasTerminalSender: Boolean get() = when (this) {
  is CIContent.RcvIntegrityError -> false
  is CIContent.SndMsgContent,
  is CIContent.SndDeleted,
  is CIContent.SndCall,
  is CIContent.SndGroupInvitation,
  is CIContent.SndModerated -> true
  else -> showMemberName
}

@Composable
fun Modifier.terminalRowIndication(): Modifier {
  val interactionSource = remember { MutableInteractionSource() }
  return this
    .hoverable(interactionSource)
    .indication(interactionSource, LocalIndication.current)
    .pointerInput(interactionSource) {
      awaitEachGesture {
        val press = PressInteraction.Press(awaitFirstDown(requireUnconsumed = false).position)
        interactionSource.tryEmit(press)
        interactionSource.tryEmit(if (waitForUpOrCancellation() != null) PressInteraction.Release(press) else PressInteraction.Cancel(press))
      }
    }
}

@Composable
fun TerminalChatItemRow(
  chatInfo: ChatInfo,
  cItem: ChatItem,
  showHeader: Boolean,
  range: IntRange?,
  reversedChatItems: List<ChatItem>,
  showMemberInfo: (GroupInfo, GroupMember) -> Unit,
  showChatInfo: () -> Unit,
  modifier: Modifier = Modifier,
  content: @Composable () -> Unit
) {
  Column(
    modifier
      .padding(top = if (showHeader) 8.dp else 0.dp, start = 8.dp, end = 12.dp)
      .fillMaxWidth()
  ) {
    val sender = if (showHeader) terminalSender(chatInfo, cItem, range, reversedChatItems, showMemberInfo, showChatInfo) else null
    if (sender != null) {
      TerminalSenderNameAndRole(sender)
    }
    Row {
      Box(Modifier.width(MEMBER_IMAGE_SIZE * fontSizeSqrtMultiplier)) {
        if (sender != null) {
          sender.image()
        }
      }
      Box(Modifier.padding(top = if (showHeader) 2.dp else 0.dp, start = 4.dp)) {
        content()
      }
    }
  }
}

private class TerminalSender(val name: String, val badge: LocalBadge?, val role: String?, val image: @Composable () -> Unit)

@Composable
private fun terminalSender(
  chatInfo: ChatInfo,
  cItem: ChatItem,
  range: IntRange?,
  reversedChatItems: List<ChatItem>,
  showMemberInfo: (GroupInfo, GroupMember) -> Unit,
  showChatInfo: () -> Unit
): TerminalSender {
  val size = MEMBER_IMAGE_SIZE * fontSizeSqrtMultiplier
  val chatImage: @Composable () -> Unit = {
    Box(Modifier.clickable { showChatInfo() }) {
      ChatInfoImage(chatInfo, size, backgroundColor = MaterialTheme.colors.background)
    }
  }
  val dir = cItem.chatDir
  return when {
    cItem.channelIsSender ->
      TerminalSender(chatInfo.chatViewName, null, stringResource(MR.strings.channel_role_label), chatImage)
    dir is CIDirection.DirectRcv ->
      TerminalSender(chatInfo.displayName, chatInfo.nameBadge, null, chatImage)
    dir is CIDirection.GroupRcv && chatInfo is ChatInfo.Group -> {
      val member = dir.groupMember
      val (prevMember, memCount) = if (range != null) chatModel.getPrevHiddenMember(member, range, reversedChatItems) else null to 1
      TerminalSender(
        memberNames(member, prevMember, memCount),
        if (prevMember == null && memCount == 1) member.nameBadge else null,
        if (memCount == 1) memberRole(member, chatInfo.isChannel) else null
      ) {
        Box(Modifier.clickable { showMemberInfo(chatInfo.groupInfo, member) }) {
          MemberImage(member)
        }
      }
    }
    dir is CIDirection.GroupSnd && chatInfo is ChatInfo.Group -> {
      val membership = chatInfo.groupInfo.membership
      TerminalSender(membership.displayName, membership.nameBadge, memberRole(membership, chatInfo.isChannel)) {
        MemberImage(membership)
      }
    }
    chatInfo.incognito ->
      TerminalSender(stringResource(MR.strings.incognito), null, null) {
        IncognitoImage(size)
      }
    else -> {
      val user = chatModel.currentUser.value
      TerminalSender(user?.displayName ?: "", user?.profile?.localBadge, null) {
        ProfileImage(size, user?.image, backgroundColor = MaterialTheme.colors.background)
      }
    }
  }
}

private fun memberRole(member: GroupMember, isChannel: Boolean): String? =
  if (member.memberRole > GroupMemberRole.Member) member.memberRole.text(isChannel) else null

@Composable
private fun TerminalSenderNameAndRole(sender: TerminalSender) {
  Row(Modifier.padding(start = MEMBER_IMAGE_SIZE * fontSizeSqrtMultiplier + DEFAULT_PADDING_HALF, bottom = 2.dp)) {
    NameWithBadge(
      sender.name,
      sender.badge,
      Modifier.weight(1f, false),
      fontSize = 13.5.sp,
      color = MaterialTheme.colors.secondary,
      overflow = TextOverflow.Ellipsis,
      maxLines = 1
    )
    if (sender.role != null) {
      Text(
        sender.role,
        Modifier.padding(start = DEFAULT_PADDING_HALF * 1.5f),
        fontSize = 13.5.sp,
        fontWeight = FontWeight.Medium,
        color = MaterialTheme.colors.secondary,
        maxLines = 1
      )
    }
  }
}
