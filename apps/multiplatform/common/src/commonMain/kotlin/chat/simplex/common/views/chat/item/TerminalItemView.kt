package chat.simplex.common.views.chat.item

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
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
    dir is CIDirection.ChannelRcv || (dir is CIDirection.GroupSnd && cItem.meta.showGroupAsSender) ->
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
