package chat.simplex.common.views.chat.group

import SectionBottomSpacer
import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chatlist.setGroupMembers
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR

@Composable
fun ChannelRelaysView(
  rhId: Long?,
  groupInfo: GroupInfo,
  chatModel: ChatModel,
  close: () -> Unit,
  showMemberInfo: (GroupMember, GroupRelay?) -> Unit
) {
  BackHandler(onBack = close)
  var groupRelays by remember { mutableStateOf<List<GroupRelay>>(emptyList()) }

  LaunchedEffect(Unit) {
    setGroupMembers(rhId, groupInfo, chatModel)
    if (groupInfo.isOwner) {
      groupRelays = chatModel.controller.apiGetGroupRelays(groupInfo.groupId)
    }
  }

  ChannelRelaysLayout(
    groupInfo = groupInfo,
    chatModel = chatModel,
    groupRelays = groupRelays,
    showMemberInfo = showMemberInfo
  )
}

@Composable
private fun ChannelRelaysLayout(
  groupInfo: GroupInfo,
  chatModel: ChatModel,
  groupRelays: List<GroupRelay>,
  showMemberInfo: (GroupMember, GroupRelay?) -> Unit
) {
  val relayMembers = remember { chatModel.groupMembers }.value
    .filter { it.memberRole == GroupMemberRole.Relay }

  ColumnWithScrollBar {
    AppBarTitle(generalGetString(MR.strings.channel_relays_title))

    if (relayMembers.isEmpty()) {
      SectionView {
        SectionItemView(padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
          Text(
            generalGetString(MR.strings.no_chat_relays),
            color = MaterialTheme.colors.secondary
          )
        }
      }
    } else {
      SectionView {
        relayMembers.forEachIndexed { index, member ->
          if (index > 0) {
            Divider()
          }
          SectionItemView(
            click = { showMemberInfo(member, groupRelays.firstOrNull { it.groupMemberId == member.groupMemberId }) },
            minHeight = 54.dp,
            padding = PaddingValues(horizontal = DEFAULT_PADDING)
          ) {
            val statusText = if (groupInfo.isOwner) {
              ownerRelayStatusText(member, groupRelays)
            } else {
              subscriberRelayStatusText(member)
            }
            RelayMemberRow(member, statusText)
          }
        }
      }
      SectionTextFooter(generalGetString(MR.strings.chat_relays_forward_messages))
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun RelayMemberRow(member: GroupMember, statusText: String) {
  Row(
    Modifier.fillMaxWidth(),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.spacedBy(4.dp)
  ) {
    MemberProfileImage(size = 38.dp, member)
    Spacer(Modifier.width(2.dp))
    Column(Modifier.weight(1f)) {
      Text(
        member.chatViewName,
        maxLines = 1,
        overflow = TextOverflow.Ellipsis,
        color = MaterialTheme.colors.onBackground
      )
      Text(
        statusText,
        maxLines = 1,
        fontSize = 12.sp,
        color = MaterialTheme.colors.secondary
      )
    }
  }
}

private fun subscriberRelayStatusText(member: GroupMember): String {
  return if (member.activeConn?.connDisabled == true) {
    generalGetString(MR.strings.member_info_member_disabled)
  } else if (member.activeConn?.connInactive == true) {
    generalGetString(MR.strings.member_info_member_inactive)
  } else {
    relayConnStatus(member).first
  }
}

private fun ownerRelayStatusText(member: GroupMember, groupRelays: List<GroupRelay>): String {
  return if (member.activeConn?.connStatus is ConnStatus.Failed) {
    generalGetString(MR.strings.relay_conn_status_failed)
  } else if (member.activeConn?.connDisabled == true) {
    generalGetString(MR.strings.member_info_member_disabled)
  } else if (member.activeConn?.connInactive == true) {
    generalGetString(MR.strings.member_info_member_inactive)
  } else {
    groupRelays.firstOrNull { it.groupMemberId == member.groupMemberId }?.relayStatus?.text
      ?: relayConnStatus(member).first
  }
}

fun relayConnStatus(member: GroupMember): Pair<String, Color> {
  return when (member.activeConn?.connStatus) {
    is ConnStatus.Ready -> generalGetString(MR.strings.relay_conn_status_connected) to Color.Green
    is ConnStatus.Deleted -> generalGetString(MR.strings.relay_conn_status_deleted) to Color.Red
    is ConnStatus.Failed -> generalGetString(MR.strings.relay_conn_status_failed) to Color.Red
    else -> generalGetString(MR.strings.relay_conn_status_connecting) to WarningOrange
  }
}

fun hostFromRelayLink(link: String): String {
  val ft = parseToMarkdown(link)
  if (ft != null) {
    for (f in ft) {
      val format = f.format
      if (format is Format.SimplexLink) {
        val host = format.smpHosts.firstOrNull()
        if (host != null) return host
      }
    }
  }
  return link
}
