package chat.simplex.app.views.chat.group

import InfoRow
import SectionDivider
import SectionItemView
import SectionSpacer
import SectionView
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
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
import chat.simplex.app.views.chat.SimplexServers
import chat.simplex.app.views.chatlist.openChat
import chat.simplex.app.views.helpers.*

@Composable
fun GroupMemberInfoView(
  groupInfo: GroupInfo,
  member: GroupMember,
  connStats: ConnectionStats?,
  chatModel: ChatModel,
  close: () -> Unit,
  closeAll: () -> Unit, // Close all open windows up to ChatView
) {
  BackHandler(onBack = close)
  val chat = chatModel.chats.firstOrNull { it.id == chatModel.chatId.value }
  val developerTools = chatModel.controller.appPrefs.developerTools.get()
  if (chat != null) {
    GroupMemberInfoLayout(
      groupInfo,
      member,
      connStats,
      developerTools,
      openDirectChat = {
        withApi {
          val oldChat = chatModel.getContactChat(member.memberContactId ?: return@withApi)
          if (oldChat != null) {
            openChat(oldChat.chatInfo, chatModel)
          } else {
            var newChat = chatModel.controller.apiGetChat(ChatType.Direct, member.memberContactId) ?: return@withApi
            // TODO it's not correct to blindly set network status to connected - we should manage network status in model / backend
            newChat = newChat.copy(serverInfo = Chat.ServerInfo(networkStatus = Chat.NetworkStatus.Connected()))
            chatModel.addChat(newChat)
            chatModel.chatItems.clear()
            chatModel.chatId.value = newChat.id
          }
          closeAll()
        }
      },
      removeMember = { removeMemberDialog(groupInfo, member, chatModel, close) }
    )
  }
}

fun removeMemberDialog(groupInfo: GroupInfo, member: GroupMember, chatModel: ChatModel, close: (() -> Unit)? = null) {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(R.string.button_remove_member),
    text = generalGetString(R.string.member_will_be_removed_from_group_cannot_be_undone),
    confirmText = generalGetString(R.string.remove_member_confirmation),
    onConfirm = {
      withApi {
        val removedMember = chatModel.controller.apiRemoveMember(member.groupId, member.groupMemberId)
        if (removedMember != null) {
          chatModel.upsertGroupMember(groupInfo, removedMember)
        }
        close?.invoke()
      }
    }
  )
}

@Composable
fun GroupMemberInfoLayout(
  groupInfo: GroupInfo,
  member: GroupMember,
  connStats: ConnectionStats?,
  developerTools: Boolean,
  openDirectChat: () -> Unit,
  removeMember: () -> Unit,
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
      GroupMemberInfoHeader(member)
    }
    SectionSpacer()

    SectionView {
      SectionItemView {
        OpenChatButton(openDirectChat)
      }
    }
    SectionSpacer()

    SectionView(title = stringResource(R.string.member_info_section_title_member)) {
      InfoRow(stringResource(R.string.info_row_group), groupInfo.displayName)
      val conn = member.activeConn
      if (conn != null) {
        SectionDivider()
        val connLevelDesc =
          if (conn.connLevel == 0) stringResource(R.string.conn_level_desc_direct)
          else String.format(generalGetString(R.string.conn_level_desc_indirect), conn.connLevel)
        InfoRow(stringResource(R.string.info_row_connection), connLevelDesc)
      }
    }
    SectionSpacer()

    if (connStats != null) {
      val rcvServers = connStats.rcvServers
      val sndServers = connStats.sndServers
      if ((rcvServers != null && rcvServers.isNotEmpty()) || (sndServers != null && sndServers.isNotEmpty())) {
        SectionView(title = stringResource(R.string.conn_stats_section_title_servers)) {
          if (rcvServers != null && rcvServers.isNotEmpty()) {
            SimplexServers(stringResource(R.string.receiving_via), rcvServers)
            if (sndServers != null && sndServers.isNotEmpty()) {
              SectionDivider()
              SimplexServers(stringResource(R.string.sending_via), sndServers)
            }
          } else if (sndServers != null && sndServers.isNotEmpty()) {
            SimplexServers(stringResource(R.string.sending_via), sndServers)
          }
        }
        SectionSpacer()
      }
    }

    if (member.canBeRemoved(groupInfo.membership)) {
      SectionView {
        SectionItemView {
          RemoveMemberButton(removeMember)
        }
      }
      SectionSpacer()
    }

    if (developerTools) {
      SectionView(title = stringResource(R.string.section_title_for_console)) {
        InfoRow(stringResource(R.string.info_row_local_name), member.localDisplayName)
        SectionDivider()
        InfoRow(stringResource(R.string.info_row_database_id), member.groupMemberId.toString())
      }
      SectionSpacer()
    }
  }
}

@Composable
fun GroupMemberInfoHeader(member: GroupMember) {
  Column(
    Modifier.padding(horizontal = 8.dp),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    ProfileImage(size = 192.dp, member.image, color = if (isInDarkTheme()) GroupDark else SettingsSecondaryLight)
    Text(
      member.displayName, style = MaterialTheme.typography.h1.copy(fontWeight = FontWeight.Normal),
      color = MaterialTheme.colors.onBackground,
      maxLines = 1,
      overflow = TextOverflow.Ellipsis
    )
    if (member.fullName != "" && member.fullName != member.displayName) {
      Text(
        member.fullName, style = MaterialTheme.typography.h2,
        color = MaterialTheme.colors.onBackground,
        maxLines = 2,
        overflow = TextOverflow.Ellipsis
      )
    }
  }
}

@Composable
fun RemoveMemberButton(removeMember: () -> Unit) {
  Row(
    Modifier
      .fillMaxSize()
      .clickable { removeMember() },
    verticalAlignment = Alignment.CenterVertically
  ) {
    Icon(
      Icons.Outlined.Delete,
      stringResource(R.string.button_remove_member),
      tint = Color.Red
    )
    Spacer(Modifier.size(8.dp))
    Text(stringResource(R.string.button_remove_member), color = Color.Red)
  }
}

@Composable
fun OpenChatButton(onClick: () -> Unit) {
  Row(
    Modifier
      .fillMaxSize()
      .clickable { onClick() },
    verticalAlignment = Alignment.CenterVertically
  ) {
    Icon(
      Icons.Outlined.Message,
      stringResource(R.string.button_send_direct_message),
      Modifier.padding(top = 5.dp),
      tint = MaterialTheme.colors.primary
    )
    Spacer(Modifier.size(8.dp))
    Text(stringResource(R.string.button_send_direct_message), color = MaterialTheme.colors.primary)
  }
}

@Preview
@Composable
fun PreviewGroupMemberInfoLayout() {
  SimpleXTheme {
    GroupMemberInfoLayout(
      groupInfo = GroupInfo.sampleData,
      member = GroupMember.sampleData,
      connStats = null,
      developerTools = false,
      openDirectChat = {},
      removeMember = {}
    )
  }
}
