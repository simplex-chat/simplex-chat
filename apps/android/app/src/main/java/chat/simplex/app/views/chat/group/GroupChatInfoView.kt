package chat.simplex.app.views.chat.group

import InfoRow
import SectionDivider
import SectionItemView
import SectionSpacer
import SectionView
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.*
import androidx.compose.foundation.gestures.Orientation
import androidx.compose.foundation.gestures.scrollable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.itemsIndexed
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.clearChatDialog
import chat.simplex.app.views.helpers.*

@Composable
fun GroupChatInfoView(groupInfo: GroupInfo, chatModel: ChatModel, close: () -> Unit) {
  BackHandler(onBack = close)
  val chat = chatModel.chats.firstOrNull { it.id == chatModel.chatId.value }
  if (chat != null) {
    GroupChatInfoLayout(
      chat,
      groupInfo,
      members = chatModel.groupMembers.sortedBy { it.displayName.lowercase() },
      deleteGroup = { deleteGroupDialog(chat.chatInfo, chatModel, close) },
      clearChat = { clearChatDialog(chat.chatInfo, chatModel, close) },
      leaveGroup = { leaveGroupDialog(groupInfo, chatModel, close) }
    )
  }
}

fun deleteGroupDialog(chatInfo: ChatInfo, chatModel: ChatModel, close: (() -> Unit)? = null) {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(R.string.delete_group_question),
    text = generalGetString(R.string.delete_group_for_all_members_cannot_undo_warning),
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
  deleteGroup: () -> Unit,
  clearChat: () -> Unit,
  leaveGroup: () -> Unit,
) {
//  val scrollState = rememberScrollState()
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
      GroupInfoHeader(chat.chatInfo)
    }
    SectionSpacer()

    SectionView(title = String.format(generalGetString(R.string.group_info_section_title_num_members), members.count())) {
      MembersList(members)
    }
    SectionSpacer()

    SectionView {
      SectionItemView {
        ClearChatButton(clearChat)
      }
      SectionDivider()
      SectionItemView {
        LeaveGroupButton(leaveGroup)
      }
      SectionDivider()
      SectionItemView() {
        DeleteGroupButton(deleteGroup)
      }
    }
    SectionSpacer()

    SectionView(title = stringResource(R.string.section_title_for_console)) {
      InfoRow(stringResource(R.string.info_row_local_name), groupInfo.localDisplayName)
      SectionDivider()
      InfoRow(stringResource(R.string.info_row_database_id), groupInfo.apiId.toString())
    }
    SectionSpacer()
  }
}

@Composable
fun GroupInfoHeader(cInfo: ChatInfo) {
  Column(
    Modifier.padding(horizontal = 8.dp),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    ChatInfoImage(cInfo, size = 192.dp, iconColor = HighOrLowlight)
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
fun MembersList(members: List<GroupMember>) {
//  LazyColumn {
//    itemsIndexed(members) { index, member ->
//      SectionItemView {
//        MemberRow(member)
//      }
//      if (index < members.lastIndex) {
//        SectionDivider()
//      }
//    }
//  }
  Column {
    members.forEach { member ->
      SectionItemView {
        MemberRow(member)
      }
//      if (index < members.lastIndex) {
//        SectionDivider()
//      }
    }
  }
}

@Composable
fun MemberRow(member: GroupMember) {
  Text(member.chatViewName)
}

@Composable
fun ClearChatButton(clearChat: () -> Unit) {
  Row(
    Modifier
      .fillMaxSize()
      .clickable { clearChat() },
    verticalAlignment = Alignment.CenterVertically
  ) {
    Icon(
      Icons.Outlined.Restore,
      stringResource(R.string.clear_chat_button),
      tint = WarningOrange
    )
    Spacer(Modifier.size(8.dp))
    Text(stringResource(R.string.clear_chat_button), color = WarningOrange)
  }
}

@Composable
fun LeaveGroupButton(leaveGroup: () -> Unit) {
  Row(
    Modifier
      .fillMaxSize()
      .clickable { leaveGroup() },
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
fun DeleteGroupButton(deleteGroup: () -> Unit) {
  Row(
    Modifier
      .fillMaxSize()
      .clickable { deleteGroup() },
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
      deleteGroup = {}, clearChat = {}, leaveGroup = {}
    )
  }
}
