package chat.simplex.common.views.chat.group

import SectionBottomSpacer
import SectionItemView
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.subscriberCountStr
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR

@Composable
fun ChannelMembersView(
  rhId: Long?,
  groupInfo: GroupInfo,
  chatModel: ChatModel,
  close: () -> Unit,
  showMemberInfo: (GroupMember) -> Unit
) {
  BackHandler(onBack = close)
  val members = remember { chatModel.groupMembers }.value
    .filter { m ->
      m.memberStatus != GroupMemberStatus.MemLeft
          && m.memberStatus != GroupMemberStatus.MemRemoved
          && m.memberRole != GroupMemberRole.Relay
    }

  ColumnWithScrollBar {
    val title = if (groupInfo.isOwner) {
      generalGetString(MR.strings.channel_members_title_subscribers)
    } else {
      generalGetString(MR.strings.channel_members_section_owners)
    }
    AppBarTitle(title)

    if (groupInfo.isOwner) {
      val subscriberCount = groupInfo.groupSummary.publicMemberCount ?: (members.size + 1).toLong()
      SectionView(title = subscriberCountStr(subscriberCount).uppercase()) {
        SectionItemView(minHeight = 54.dp, padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
          ChannelMemberRow(groupInfo.membership, user = true, showRole = true)
        }
        members.forEachIndexed { index, member ->
          Divider()
          SectionItemView(
            click = { showMemberInfo(member) },
            minHeight = 54.dp,
            padding = PaddingValues(horizontal = DEFAULT_PADDING)
          ) {
            ChannelMemberRow(member, user = false, showRole = member.memberRole >= GroupMemberRole.Owner)
          }
        }
      }
    } else {
      val owners = members.filter { it.memberRole >= GroupMemberRole.Owner }
      SectionView(title = generalGetString(MR.strings.channel_members_section_owners)) {
        owners.forEachIndexed { index, member ->
          if (index > 0) {
            Divider()
          }
          SectionItemView(
            click = { showMemberInfo(member) },
            minHeight = 54.dp,
            padding = PaddingValues(horizontal = DEFAULT_PADDING)
          ) {
            ChannelMemberRow(member, user = false, showRole = false)
          }
        }
      }
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun ChannelMemberRow(member: GroupMember, user: Boolean, showRole: Boolean) {
  Row(
    Modifier.fillMaxWidth(),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.spacedBy(4.dp)
  ) {
    MemberProfileImage(size = 38.dp, member)
    Spacer(Modifier.width(2.dp))
    Column(Modifier.weight(1f)) {
      Row(verticalAlignment = Alignment.CenterVertically) {
        if (member.verified) {
          MemberVerifiedShield()
        }
        Text(
          member.chatViewName,
          maxLines = 1,
          overflow = TextOverflow.Ellipsis,
          color = if (member.memberIncognito) Indigo else Color.Unspecified
        )
      }
      if (user) {
        Text(
          generalGetString(MR.strings.channel_member_you),
          style = MaterialTheme.typography.body2,
          color = MaterialTheme.colors.secondary
        )
      }
    }
    if (showRole) {
      Text(
        member.memberRole.text,
        color = MaterialTheme.colors.secondary
      )
    }
  }
}
