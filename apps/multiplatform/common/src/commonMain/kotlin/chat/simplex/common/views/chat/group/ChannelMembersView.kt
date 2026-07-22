package chat.simplex.common.views.chat.group

import SectionBottomSpacer
import SectionItemView
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyListScope
import androidx.compose.foundation.lazy.itemsIndexed
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.ownersContributorsCountStr
import chat.simplex.common.views.chat.subscriberCountStr
import chat.simplex.common.views.chat.topPaddingToContent
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR

@Composable
fun ModalData.ChannelMembersView(
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
    .sortedByDescending { it.memberRole }

  val searchText = remember { stateGetOrPut("searchText") { TextFieldValue() } }
  val s = searchText.value.text.trim().lowercase()
  val subscriberCount = groupInfo.groupSummary.publicMemberCount ?: (members.size + 1).toLong()
  val oneHandUI = remember { ChatController.appPrefs.oneHandUI.state }
  val title = if (groupInfo.isOwner) {
    generalGetString(MR.strings.channel_members_title_subscribers)
  } else {
    generalGetString(MR.strings.channel_members_section_owners)
  }
  LazyColumnWithScrollBar(
    contentPadding = PaddingValues(
      top = if (oneHandUI.value) WindowInsets.statusBars.asPaddingValues().calculateTopPadding() + DEFAULT_PADDING + 5.dp else topPaddingToContent(false)
    )
  ) {
    item {
      AppBarTitle(title)
    }
    if (groupInfo.isOwner) {
      val showSearch = members.size > 8
      item {
        SectionView(title = subscriberCountStr(subscriberCount)) {
          if (showSearch) {
            SectionItemView(padding = PaddingValues(start = 14.dp, end = DEFAULT_PADDING_HALF)) {
              MemberListSearchRowView(searchText)
            }
            Divider()
          }
          SectionItemView(minHeight = 54.dp, padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
            ChannelMemberRow(groupInfo.membership, user = true, showRole = true, isChannel = groupInfo.isChannel)
          }
        }
      }
      val filtered = if (s.isEmpty()) members else members.filter { it.anyNameContains(s) }
      channelMemberItems(filtered, groupInfo, GroupMemberRole.Member, dividerAboveFirst = true, showMemberInfo)
    } else {
      val contributors = members.filter { it.memberRole >= GroupMemberRole.Member && it.memberStatus != GroupMemberStatus.MemUnknown }
      val contributorCount = contributors.size + if (groupInfo.membership.memberRole >= GroupMemberRole.Member) 1 else 0
      val withContributors = contributors.any { it.memberRole < GroupMemberRole.Owner } ||
          groupInfo.membership.memberRole >= GroupMemberRole.Member
      val showSearch = contributors.size > 8
      val showUserRow = groupInfo.membership.memberRole >= GroupMemberRole.Member
      item {
        SectionView(title = ownersContributorsCountStr(contributorCount, withContributors)) {
          if (showSearch) {
            SectionItemView(padding = PaddingValues(start = 14.dp, end = DEFAULT_PADDING_HALF)) {
              MemberListSearchRowView(searchText)
            }
            if (showUserRow) Divider()
          }
          if (showUserRow) {
            SectionItemView(minHeight = 54.dp, padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
              ChannelMemberRow(groupInfo.membership, user = true, showRole = true, isChannel = groupInfo.isChannel)
            }
          }
        }
      }
      val filtered = if (s.isEmpty()) contributors else contributors.filter { it.anyNameContains(s) }
      channelMemberItems(filtered, groupInfo, GroupMemberRole.Moderator, dividerAboveFirst = showSearch || showUserRow, showMemberInfo)
    }
    item {
      SectionBottomSpacer()
    }
  }
}

private fun LazyListScope.channelMemberItems(
  members: List<GroupMember>,
  groupInfo: GroupInfo,
  showRoleFrom: GroupMemberRole,
  dividerAboveFirst: Boolean,
  showMemberInfo: (GroupMember) -> Unit
) {
  itemsIndexed(members, key = { _, m -> m.groupMemberId }) { index, member ->
    if (index > 0 || dividerAboveFirst) {
      Divider()
    }
    SectionItemView(
      click = { showMemberInfo(member) },
      minHeight = 54.dp,
      padding = PaddingValues(horizontal = DEFAULT_PADDING)
    ) {
      ChannelMemberRow(member, user = false, showRole = member.memberRole >= showRoleFrom, isChannel = groupInfo.isChannel)
    }
  }
}

@Composable
private fun ChannelMemberRow(member: GroupMember, user: Boolean, showRole: Boolean, isChannel: Boolean) {
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
        NameWithBadge(
          member.chatViewName,
          member.nameBadge,
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
        member.memberRole.text(isChannel = isChannel),
        color = MaterialTheme.colors.secondary
      )
    }
  }
}
