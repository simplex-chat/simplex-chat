package chat.simplex.common.views.chat.group

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.max
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.theme.WarningOrange
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource

@Composable
fun SelectedItemsMembersToolbar(
  selectedItems: MutableState<Set<Long>?>,
  activeMembers: State<List<GroupMember>>,
  groupInfo: GroupInfo,
  delete: () -> Unit,
  blockForAll: (Boolean) -> Unit, // Boolean - block or unlock
  changeRole: (GroupMemberRole) -> Unit,
) {
  val deleteEnabled = remember { mutableStateOf(false) }
  val blockForAllEnabled = remember { mutableStateOf(false) }
  val unblockForAllEnabled = remember { mutableStateOf(false) }
  val blockForAllButtonEnabled = remember { derivedStateOf { (blockForAllEnabled.value && !unblockForAllEnabled.value) || (!blockForAllEnabled.value && unblockForAllEnabled.value) } }

  val roleToMemberEnabled = remember { mutableStateOf(false) }
  val roleToObserverEnabled = remember { mutableStateOf(false) }
  val roleButtonEnabled = remember { derivedStateOf { (roleToMemberEnabled.value && !roleToObserverEnabled.value) || (!roleToMemberEnabled.value && roleToObserverEnabled.value) } }
  Box(
    Modifier
      .background(MaterialTheme.colors.background)
      .navigationBarsPadding()
      .imePadding()
  ) {
    // It's hard to measure exact height of ComposeView with different fontSizes. Better to depend on actual ComposeView, even empty
    Box(Modifier.alpha(0f)) {
      ComposeView(chatModel = chatModel, Chat.sampleData, remember { mutableStateOf(ComposeState(useLinkPreviews = false)) }, remember { mutableStateOf(null) }, {}, remember { FocusRequester() })
    }
    Row(
      Modifier
        .matchParentSize()
        .padding(horizontal = 2.dp)
        .height(AppBarHeight * fontSizeSqrtMultiplier)
        .pointerInput(Unit) {
          detectGesture {
            true
          }
        },
      horizontalArrangement = Arrangement.SpaceBetween,
      verticalAlignment = Alignment.CenterVertically
    ) {
      IconButton(delete, enabled = deleteEnabled.value) {
        Icon(
          painterResource(MR.images.ic_delete),
          null,
          Modifier.size(22.dp),
          tint = if (!deleteEnabled.value) MaterialTheme.colors.secondary else MaterialTheme.colors.error
        )
      }

      IconButton({ blockForAll(blockForAllEnabled.value) }, enabled = blockForAllButtonEnabled.value) {
        Icon(
          painterResource(if (unblockForAllEnabled.value && blockForAllButtonEnabled.value) MR.images.ic_do_not_touch else MR.images.ic_back_hand),
          null,
          Modifier.size(22.dp),
          tint = if (!blockForAllButtonEnabled.value) MaterialTheme.colors.secondary else if (blockForAllEnabled.value) MaterialTheme.colors.error else WarningOrange
        )
      }

      IconButton({ changeRole(if (roleToMemberEnabled.value) GroupMemberRole.Member else GroupMemberRole.Observer) }, enabled = roleButtonEnabled.value) {
        Icon(
          painterResource(if (roleToObserverEnabled.value || !roleButtonEnabled.value) MR.images.ic_person else MR.images.ic_person_edit),
          null,
          Modifier.size(22.dp),
          tint = if (!roleButtonEnabled.value) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
        )
      }
    }
    Divider(Modifier.align(Alignment.TopStart))
  }
  LaunchedEffect(groupInfo, activeMembers.value.toList(), selectedItems.value) {
    recheckItems(groupInfo, selectedItems, activeMembers.value, deleteEnabled, blockForAllEnabled, unblockForAllEnabled, roleToMemberEnabled, roleToObserverEnabled)
  }
}

private fun recheckItems(
  groupInfo: GroupInfo,
  selectedItems: MutableState<Set<Long>?>,
  activeMembers: List<GroupMember>,
  deleteEnabled: MutableState<Boolean>,
  blockForAllEnabled: MutableState<Boolean>,
  unblockForAllEnabled: MutableState<Boolean>,
  roleToMemberEnabled: MutableState<Boolean>,
  roleToObserverEnabled: MutableState<Boolean>,
) {
  val selected = selectedItems.value ?: return
  var rDeleteEnabled = true
  var rBlockForAllEnabled = true
  var rUnblockForAllEnabled = true
  var rRoleToMemberEnabled = true
  var rRoleToObserverEnabled = true
  val rSelectedItems = mutableSetOf<Long>()
  for (mem in activeMembers) {
    if (selected.contains(mem.groupMemberId) && groupInfo.membership.memberRole >= mem.memberRole && mem.memberRole < GroupMemberRole.Moderator && groupInfo.membership.memberActive) {
      rDeleteEnabled = rDeleteEnabled && mem.memberStatus != GroupMemberStatus.MemRemoved && mem.memberStatus != GroupMemberStatus.MemLeft
      rBlockForAllEnabled = rBlockForAllEnabled && !mem.blockedByAdmin
      rUnblockForAllEnabled = rUnblockForAllEnabled && mem.blockedByAdmin
      rRoleToMemberEnabled = rRoleToMemberEnabled && mem.memberRole != GroupMemberRole.Member
      rRoleToObserverEnabled = rRoleToObserverEnabled && mem.memberRole != GroupMemberRole.Observer
      rSelectedItems.add(mem.groupMemberId) // we are collecting new selected items here to account for any changes in members list
    }
  }
  deleteEnabled.value = rDeleteEnabled
  blockForAllEnabled.value = rBlockForAllEnabled
  unblockForAllEnabled.value = rUnblockForAllEnabled
  roleToMemberEnabled.value = rRoleToMemberEnabled
  roleToObserverEnabled.value = rRoleToObserverEnabled
  selectedItems.value = rSelectedItems
}
