package chat.simplex.common.views.chat.group

import SectionBottomSpacer
import SectionDividerSpaced
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
  val allMembers = remember { chatModel.groupMembers }.value
    .filter { m ->
      m.memberStatus != GroupMemberStatus.MemLeft
          && m.memberStatus != GroupMemberStatus.MemRemoved
          && m.groupMemberId != groupInfo.membership.groupMemberId
    }
  val owners = allMembers.filter { it.memberRole >= GroupMemberRole.Owner }
  // TODO [relays] subscriber/owner counts require backend support for accurate totals
  val subscribers = allMembers.filter { it.memberRole < GroupMemberRole.Owner && it.memberRole != GroupMemberRole.Relay }

  ColumnWithScrollBar {
    val title = if (groupInfo.isOwner) {
      generalGetString(MR.strings.channel_members_title_owners_and_subscribers)
    } else {
      generalGetString(MR.strings.channel_members_section_owners)
    }
    AppBarTitle(title)

    SectionView(title = generalGetString(MR.strings.channel_members_section_owners)) {
      if (groupInfo.membership.memberRole >= GroupMemberRole.Owner) {
        SectionItemView(minHeight = 54.dp, padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
          ChannelMemberRow(groupInfo.membership)
        }
      }
      owners.forEachIndexed { index, member ->
        if (index > 0 || groupInfo.membership.memberRole >= GroupMemberRole.Owner) {
          Divider()
        }
        SectionItemView(
          click = { showMemberInfo(member) },
          minHeight = 54.dp,
          padding = PaddingValues(horizontal = DEFAULT_PADDING)
        ) {
          ChannelMemberRow(member)
        }
      }
    }

    if (groupInfo.isOwner) {
      SectionDividerSpaced(maxTopPadding = true, maxBottomPadding = false)
      SectionView(title = String.format(generalGetString(MR.strings.channel_members_num_subscribers), subscribers.size)) {
        if (subscribers.isEmpty()) {
          SectionItemView(padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
            Text(
              generalGetString(MR.strings.channel_members_no_subscribers),
              color = MaterialTheme.colors.secondary
            )
          }
        } else {
          subscribers.forEachIndexed { index, member ->
            if (index > 0) {
              Divider()
            }
            SectionItemView(
              click = { showMemberInfo(member) },
              minHeight = 54.dp,
              padding = PaddingValues(horizontal = DEFAULT_PADDING)
            ) {
              ChannelMemberRow(member)
            }
          }
        }
      }
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun ChannelMemberRow(member: GroupMember) {
  Row(
    Modifier.fillMaxWidth(),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.spacedBy(4.dp)
  ) {
    MemberProfileImage(size = 38.dp, member)
    Spacer(Modifier.width(2.dp))
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
  }
}
