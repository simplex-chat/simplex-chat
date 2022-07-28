package chat.simplex.app.views.chat.group

import SectionDivider
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.background
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
      members = chatModel.groupMembers.sortedBy { it.displayName.lowercase() },
      close = close,
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
  members: List<GroupMember>,
  close: () -> Unit,
  deleteGroup: () -> Unit,
  clearChat: () -> Unit,
  leaveGroup: () -> Unit,
) {
  Column(
    Modifier
      .fillMaxSize()
      .background(MaterialTheme.colors.background)
      .padding(horizontal = 8.dp),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    CloseSheetBar(close)
    Spacer(Modifier.size(48.dp))
    val cInfo = chat.chatInfo
    ChatInfoImage(cInfo, size = 192.dp)
    Text(
      cInfo.displayName, style = MaterialTheme.typography.h1.copy(fontWeight = FontWeight.Normal),
      color = MaterialTheme.colors.onBackground,
      modifier = Modifier
        .padding(top = 32.dp)
        .padding(bottom = 8.dp)
    )
    Text(
      cInfo.fullName, style = MaterialTheme.typography.h2,
      color = MaterialTheme.colors.onBackground,
      modifier = Modifier.padding(bottom = 16.dp)
    )

    Spacer(modifier = Modifier.size(30.dp))

    MembersList(members)

    Spacer(Modifier.weight(1F))

    Box(Modifier.padding(4.dp)) {
      SimpleButton(
        stringResource(R.string.clear_chat_button),
        icon = Icons.Outlined.Restore,
        color = WarningOrange,
        click = clearChat
      )
    }
    Box(Modifier.padding(4.dp)) {
      SimpleButton(
        stringResource(R.string.button_leave_group),
        icon = Icons.Outlined.Logout,
        color = Color.Red,
        click = leaveGroup
      )
    }
    Box(
      Modifier
        .padding(4.dp)
        .padding(bottom = 32.dp)
    ) {
      SimpleButton(
        stringResource(R.string.button_delete_group),
        icon = Icons.Outlined.Delete,
        color = Color.Red,
        click = deleteGroup
      )
    }
  }
}

@Composable
fun MembersList(members: List<GroupMember>) {
  LazyColumn {
    itemsIndexed(members) { index, member ->
      MemberRow(member)
      if (index < members.lastIndex) {
        SectionDivider()
      }
    }
  }
}

@Composable
fun MemberRow(member: GroupMember) {
  Text(member.chatViewName)
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
      members = listOf(GroupMember.sampleData, GroupMember.sampleData, GroupMember.sampleData),
      close = {}, deleteGroup = {}, clearChat = {}, leaveGroup = {}
    )
  }
}
