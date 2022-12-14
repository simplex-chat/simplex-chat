package chat.simplex.app.views.chat.group

import InfoRow
import SectionDivider
import SectionItemView
import SectionSpacer
import SectionTextFooter
import SectionView
import android.util.Log
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.TAG
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.*
import chat.simplex.app.views.chatlist.cantInviteIncognitoAlert
import chat.simplex.app.views.chatlist.setGroupMembers
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.usersettings.*

@Composable
fun GroupChatInfoView(chatModel: ChatModel, groupLink: String?, onGroupLinkUpdated: (String?) -> Unit, close: () -> Unit) {
  BackHandler(onBack = close)
  val chat = chatModel.chats.firstOrNull { it.id == chatModel.chatId.value }
  val developerTools = chatModel.controller.appPrefs.developerTools.get()
  if (chat != null && chat.chatInfo is ChatInfo.Group) {
    val groupInfo = chat.chatInfo.groupInfo
    GroupChatInfoLayout(
      chat,
      groupInfo,
      members = chatModel.groupMembers
        .filter { it.memberStatus != GroupMemberStatus.MemLeft && it.memberStatus != GroupMemberStatus.MemRemoved }
        .sortedBy { it.displayName.lowercase() },
      developerTools,
      groupLink,
      addMembers = {
        withApi {
          setGroupMembers(groupInfo, chatModel)
          ModalManager.shared.showModalCloseable(true) { close ->
            AddGroupMembersView(groupInfo, false, chatModel, close)
          }
        }
      },
      showMemberInfo = { member ->
        withApi {
          val stats = chatModel.controller.apiGroupMemberInfo(groupInfo.groupId, member.groupMemberId)
          val (_, code) = if (member.memberActive) {
            try {
              chatModel.controller.apiGetGroupMemberCode(groupInfo.apiId, member.groupMemberId)
            } catch (e: Exception) {
              Log.e(TAG, e.stackTraceToString())
              member to null
            }
          } else {
            member to null
          }
          ModalManager.shared.showModalCloseable(true) { closeCurrent ->
            remember { derivedStateOf { chatModel.groupMembers.firstOrNull { it.memberId == member.memberId } } }.value?.let { mem ->
              GroupMemberInfoView(groupInfo, mem, stats, code, chatModel, closeCurrent) {
                closeCurrent()
                close()
              }
            }
          }
        }
      },
      editGroupProfile = {
        ModalManager.shared.showCustomModal { close -> GroupProfileView(groupInfo, chatModel, close) }
      },
      openPreferences = {
        ModalManager.shared.showCustomModal { close ->
          GroupPreferencesView(
            chatModel,
            chat.id,
            close
          )
        }
      },
      deleteGroup = { deleteGroupDialog(chat.chatInfo, groupInfo, chatModel, close) },
      clearChat = { clearChatDialog(chat.chatInfo, chatModel, close) },
      leaveGroup = { leaveGroupDialog(groupInfo, chatModel, close) },
      manageGroupLink = {
        withApi {
          ModalManager.shared.showModal { GroupLinkView(chatModel, groupInfo, groupLink, onGroupLinkUpdated) }
        }
      }
    )
  }
}

fun deleteGroupDialog(chatInfo: ChatInfo, groupInfo: GroupInfo, chatModel: ChatModel, close: (() -> Unit)? = null) {
  val alertTextKey =
    if (groupInfo.membership.memberCurrent) R.string.delete_group_for_all_members_cannot_undo_warning
    else R.string.delete_group_for_self_cannot_undo_warning
  AlertManager.shared.showAlertMsg(
    title = generalGetString(R.string.delete_group_question),
    text = generalGetString(alertTextKey),
    confirmText = generalGetString(R.string.delete_verb),
    onConfirm = {
      withApi {
        val r = chatModel.controller.apiDeleteChat(chatInfo.chatType, chatInfo.apiId)
        if (r) {
          chatModel.removeChat(chatInfo.id)
          chatModel.chatId.value = null
          chatModel.controller.ntfManager.cancelNotificationsForChat(chatInfo.id)
          close?.invoke()
        }
      }
    }
  )
}

fun leaveGroupDialog(groupInfo: GroupInfo, chatModel: ChatModel, close: (() -> Unit)? = null) {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(R.string.leave_group_question),
    text = generalGetString(R.string.you_will_stop_receiving_messages_from_this_group_chat_history_will_be_preserved),
    confirmText = generalGetString(R.string.leave_group_button),
    onConfirm = {
      withApi {
        chatModel.controller.leaveGroup(groupInfo.groupId)
        close?.invoke()
      }
    }
  )
}

@Composable
fun GroupChatInfoLayout(
  chat: Chat,
  groupInfo: GroupInfo,
  members: List<GroupMember>,
  developerTools: Boolean,
  groupLink: String?,
  addMembers: () -> Unit,
  showMemberInfo: (GroupMember) -> Unit,
  editGroupProfile: () -> Unit,
  openPreferences: () -> Unit,
  deleteGroup: () -> Unit,
  clearChat: () -> Unit,
  leaveGroup: () -> Unit,
  manageGroupLink: () -> Unit,
) {
  Column(
    Modifier
      .fillMaxWidth()
      .verticalScroll(rememberScrollState()),
    horizontalAlignment = Alignment.Start
  ) {
    Row(
      Modifier.fillMaxWidth(),
      horizontalArrangement = Arrangement.Center
    ) {
      GroupChatInfoHeader(chat.chatInfo)
    }
    SectionSpacer()

    SectionView {
      if (groupInfo.canEdit) {
        SectionItemView(editGroupProfile) { EditGroupProfileButton() }
        SectionDivider()
      }
      GroupPreferencesButton(openPreferences)
    }
    SectionTextFooter(stringResource(R.string.only_group_owners_can_change_prefs))
    SectionSpacer()

    SectionView(title = String.format(generalGetString(R.string.group_info_section_title_num_members), members.count() + 1)) {
      if (groupInfo.canAddMembers) {
        SectionItemView(manageGroupLink) {
          if (groupLink == null) {
            CreateGroupLinkButton()
          } else {
            GroupLinkButton()
          }
        }
        SectionDivider()
        val onAddMembersClick = if (chat.chatInfo.incognito) ::cantInviteIncognitoAlert else addMembers
        SectionItemView(onAddMembersClick) {
          val tint = if (chat.chatInfo.incognito) HighOrLowlight else MaterialTheme.colors.primary
          AddMembersButton(tint)
        }
        SectionDivider()
      }
      SectionItemView(minHeight = 50.dp) {
        MemberRow(groupInfo.membership, user = true)
      }
      if (members.isNotEmpty()) {
        SectionDivider()
      }
      MembersList(members, showMemberInfo)
    }
    SectionSpacer()
    SectionView {
      ClearChatButton(clearChat)
      if (groupInfo.canDelete) {
        SectionDivider()
        SectionItemView(deleteGroup) { DeleteGroupButton() }
      }
      if (groupInfo.membership.memberCurrent) {
        SectionDivider()
        SectionItemView(leaveGroup) { LeaveGroupButton() }
      }
    }
    SectionSpacer()

    if (developerTools) {
      SectionView(title = stringResource(R.string.section_title_for_console)) {
        InfoRow(stringResource(R.string.info_row_local_name), groupInfo.localDisplayName)
        SectionDivider()
        InfoRow(stringResource(R.string.info_row_database_id), groupInfo.apiId.toString())
      }
      SectionSpacer()
    }
  }
}

@Composable
private fun GroupChatInfoHeader(cInfo: ChatInfo) {
  Column(
    Modifier.padding(horizontal = 8.dp),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    ChatInfoImage(cInfo, size = 192.dp, iconColor = if (isInDarkTheme()) GroupDark else SettingsSecondaryLight)
    Text(
      cInfo.displayName, style = MaterialTheme.typography.h1.copy(fontWeight = FontWeight.Normal),
      color = MaterialTheme.colors.onBackground,
      maxLines = 1,
      overflow = TextOverflow.Ellipsis
    )
    if (cInfo.fullName != "" && cInfo.fullName != cInfo.displayName) {
      Text(
        cInfo.fullName, style = MaterialTheme.typography.h2,
        color = MaterialTheme.colors.onBackground,
        maxLines = 2,
        overflow = TextOverflow.Ellipsis
      )
    }
  }
}

@Composable
private fun GroupPreferencesButton(onClick: () -> Unit) {
  SettingsActionItem(
    Icons.Outlined.ToggleOn,
    stringResource(R.string.group_preferences),
    click = onClick
  )
}

@Composable
private fun AddMembersButton(tint: Color = MaterialTheme.colors.primary) {
  Row(
    Modifier.fillMaxSize(),
    verticalAlignment = Alignment.CenterVertically
  ) {
    Icon(
      Icons.Outlined.Add,
      stringResource(R.string.button_add_members),
      tint = tint
    )
    Spacer(Modifier.size(8.dp))
    Text(stringResource(R.string.button_add_members), color = tint)
  }
}

@Composable
private fun MembersList(members: List<GroupMember>, showMemberInfo: (GroupMember) -> Unit) {
  Column {
    members.forEachIndexed { index, member ->
      SectionItemView({ showMemberInfo(member) }, minHeight = 50.dp) {
        MemberRow(member)
      }
      if (index < members.lastIndex) {
        SectionDivider()
      }
    }
  }
}

@Composable
private fun MemberRow(member: GroupMember, user: Boolean = false) {
  Row(
    Modifier.fillMaxWidth(),
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    Row(
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.spacedBy(4.dp)
    ) {
      ProfileImage(size = 46.dp, member.image)
      Column {
        Row(verticalAlignment = Alignment.CenterVertically) {
          if (member.verified) {
            MemberVerifiedShield()
          }
          Text(
            member.chatViewName, maxLines = 1, overflow = TextOverflow.Ellipsis,
            color = if (member.memberIncognito) Indigo else Color.Unspecified
          )
        }
        val s = member.memberStatus.shortText
        val statusDescr = if (user) String.format(generalGetString(R.string.group_info_member_you), s) else s
        Text(
          statusDescr,
          color = HighOrLowlight,
          fontSize = 12.sp,
          maxLines = 1,
          overflow = TextOverflow.Ellipsis
        )
      }
    }
    val role = member.memberRole
    if (role == GroupMemberRole.Owner || role == GroupMemberRole.Admin) {
      Text(role.text, color = HighOrLowlight)
    }
  }
}

@Composable
private fun MemberVerifiedShield() {
  Icon(Icons.Outlined.VerifiedUser, null, Modifier.padding(end = 3.dp).size(16.dp), tint = HighOrLowlight)
}

@Composable
private fun GroupLinkButton() {
  Row(
    Modifier
      .fillMaxSize(),
    verticalAlignment = Alignment.CenterVertically
  ) {
    Icon(
      Icons.Outlined.Link,
      stringResource(R.string.group_link),
      tint = HighOrLowlight
    )
    Spacer(Modifier.size(8.dp))
    Text(stringResource(R.string.group_link))
  }
}

@Composable
private fun CreateGroupLinkButton() {
  Row(
    Modifier
      .fillMaxSize(),
    verticalAlignment = Alignment.CenterVertically
  ) {
    Icon(
      Icons.Outlined.AddLink,
      stringResource(R.string.create_group_link),
      tint = HighOrLowlight
    )
    Spacer(Modifier.size(8.dp))
    Text(stringResource(R.string.create_group_link))
  }
}

@Composable
fun EditGroupProfileButton() {
  Row(
    Modifier
      .fillMaxSize(),
    verticalAlignment = Alignment.CenterVertically
  ) {
    Icon(
      Icons.Outlined.Edit,
      stringResource(R.string.button_edit_group_profile),
      tint = HighOrLowlight
    )
    Spacer(Modifier.size(8.dp))
    Text(stringResource(R.string.button_edit_group_profile))
  }
}

@Composable
private fun LeaveGroupButton() {
  Row(
    Modifier.fillMaxSize(),
    verticalAlignment = Alignment.CenterVertically
  ) {
    Icon(
      Icons.Outlined.Logout,
      stringResource(R.string.button_leave_group),
      tint = Color.Red
    )
    Spacer(Modifier.size(8.dp))
    Text(stringResource(R.string.button_leave_group), color = Color.Red)
  }
}

@Composable
private fun DeleteGroupButton() {
  Row(
    Modifier.fillMaxSize(),
    verticalAlignment = Alignment.CenterVertically
  ) {
    Icon(
      Icons.Outlined.Delete,
      stringResource(R.string.button_delete_group),
      tint = Color.Red
    )
    Spacer(Modifier.size(8.dp))
    Text(stringResource(R.string.button_delete_group), color = Color.Red)
  }
}

@Preview
@Composable
fun PreviewGroupChatInfoLayout() {
  SimpleXTheme {
    GroupChatInfoLayout(
      chat = Chat(
        chatInfo = ChatInfo.Direct.sampleData,
        chatItems = arrayListOf(),
        serverInfo = Chat.ServerInfo(Chat.NetworkStatus.Error("agent BROKER TIMEOUT"))
      ),
      groupInfo = GroupInfo.sampleData,
      members = listOf(GroupMember.sampleData, GroupMember.sampleData, GroupMember.sampleData),
      developerTools = false,
      groupLink = null,
      addMembers = {}, showMemberInfo = {}, editGroupProfile = {}, openPreferences = {}, deleteGroup = {}, clearChat = {}, leaveGroup = {}, manageGroupLink = {},
    )
  }
}
