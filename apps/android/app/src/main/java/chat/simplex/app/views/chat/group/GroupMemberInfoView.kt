package chat.simplex.app.views.chat.group

import InfoRow
import SectionBottomSpacer
import SectionDividerSpaced
import SectionSpacer
import SectionTextFooter
import SectionView
import android.net.Uri
import android.util.Log
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.*
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.newchat.*
import chat.simplex.app.views.usersettings.SettingsActionItem
import kotlinx.datetime.Clock

@Composable
fun GroupMemberInfoView(
  groupInfo: GroupInfo,
  member: GroupMember,
  connStats: ConnectionStats?,
  connectionCode: String?,
  chatModel: ChatModel,
  close: () -> Unit,
  closeAll: () -> Unit, // Close all open windows up to ChatView
) {
  BackHandler(onBack = close)
  val chat = chatModel.chats.firstOrNull { it.id == chatModel.chatId.value }
  val developerTools = chatModel.controller.appPrefs.developerTools.get()
  if (chat != null) {
    val newRole = remember { mutableStateOf(member.memberRole) }
    GroupMemberInfoLayout(
      groupInfo,
      member,
      connStats,
      newRole,
      developerTools,
      connectionCode,
      getContactChat = { chatModel.getContactChat(it) },
      openDirectChat = {
        withApi {
          val c = chatModel.controller.apiGetChat(ChatType.Direct, it)
          if (c != null) {
            if (chatModel.getContactChat(it) == null) {
              chatModel.addChat(c)
            }
            chatModel.chatItems.clear()
            chatModel.chatItems.addAll(c.chatItems)
            chatModel.chatId.value = c.id
            closeAll()
          }
        }
      },
      connectViaAddress = { connReqUri ->
        val uri = Uri.parse(connReqUri)
        withUriAction(uri) { linkType ->
          withApi {
            Log.d(TAG, "connectViaUri: connecting")
            connectViaUri(chatModel, linkType, uri)
          }
        }
      },
      removeMember = { removeMemberDialog(groupInfo, member, chatModel, close) },
      onRoleSelected = {
        if (it == newRole.value) return@GroupMemberInfoLayout
        val prevValue = newRole.value
        newRole.value = it
        updateMemberRoleDialog(it, member, onDismiss = {
          newRole.value = prevValue
        }) {
          withApi {
            kotlin.runCatching {
              val mem = chatModel.controller.apiMemberRole(groupInfo.groupId, member.groupMemberId, it)
              chatModel.upsertGroupMember(groupInfo, mem)
            }.onFailure {
              newRole.value = prevValue
            }
          }
        }
      },
      switchMemberAddress = {
        switchMemberAddress(chatModel, groupInfo, member)
      },
      verifyClicked = {
        ModalManager.shared.showModalCloseable { close ->
          remember { derivedStateOf { chatModel.groupMembers.firstOrNull { it.memberId == member.memberId } } }.value?.let { mem ->
            VerifyCodeView(
              mem.displayName,
              connectionCode,
              mem.verified,
              verify = { code ->
                chatModel.controller.apiVerifyGroupMember(mem.groupId, mem.groupMemberId, code)?.let { r ->
                  val (verified, existingCode) = r
                  chatModel.upsertGroupMember(
                    groupInfo,
                    mem.copy(
                      activeConn = mem.activeConn?.copy(
                        connectionCode = if (verified) SecurityCode(existingCode, Clock.System.now()) else null
                      )
                    )
                  )
                  r
                }
              },
              close,
            )
          }
        }
      }
    )
  }
}

fun removeMemberDialog(groupInfo: GroupInfo, member: GroupMember, chatModel: ChatModel, close: (() -> Unit)? = null) {
  AlertManager.shared.showAlertDialog(
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
    },
    destructive = true,
  )
}

@Composable
fun GroupMemberInfoLayout(
  groupInfo: GroupInfo,
  member: GroupMember,
  connStats: ConnectionStats?,
  newRole: MutableState<GroupMemberRole>,
  developerTools: Boolean,
  connectionCode: String?,
  getContactChat: (Long) -> Chat?,
  openDirectChat: (Long) -> Unit,
  connectViaAddress: (String) -> Unit,
  removeMember: () -> Unit,
  onRoleSelected: (GroupMemberRole) -> Unit,
  switchMemberAddress: () -> Unit,
  verifyClicked: () -> Unit,
) {
  fun knownDirectChat(contactId: Long): Chat? {
    val chat = getContactChat(contactId)
    return if (chat != null && chat.chatInfo is ChatInfo.Direct && chat.chatInfo.contact.directOrUsed) {
      chat
    } else {
      null
    }
  }

  Column(
    Modifier
      .fillMaxWidth()
      .verticalScroll(rememberScrollState()),
  ) {
    Row(
      Modifier.fillMaxWidth(),
      horizontalArrangement = Arrangement.Center
    ) {
      GroupMemberInfoHeader(member)
    }
    SectionSpacer()

    val contactId = member.memberContactId

    if (member.memberActive) {
      if (contactId != null) {
        SectionView {
          if (knownDirectChat(contactId) != null || groupInfo.fullGroupPreferences.directMessages.on) {
            OpenChatButton(onClick = { openDirectChat(contactId) })
          }
          if (connectionCode != null) {
            VerifyCodeButton(member.verified, verifyClicked)
          }
        }
        SectionDividerSpaced()
      }
    }

    if (member.contactLink != null) {
      val context = LocalContext.current
      SectionView(stringResource(R.string.address_section_title).uppercase()) {
        QRCode(member.contactLink, Modifier.padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF).aspectRatio(1f))
        ShareAddressButton { shareText(context, member.contactLink) }
        if (contactId != null) {
          if (knownDirectChat(contactId) == null && !groupInfo.fullGroupPreferences.directMessages.on) {
            ConnectViaAddressButton(onClick = { connectViaAddress(member.contactLink) })
          }
        } else {
          ConnectViaAddressButton(onClick = { connectViaAddress(member.contactLink) })
        }
        SectionTextFooter(stringResource(R.string.you_can_share_this_address_with_your_contacts).format(member.displayName))
      }
      SectionDividerSpaced()
    }

    SectionView(title = stringResource(R.string.member_info_section_title_member)) {
      InfoRow(stringResource(R.string.info_row_group), groupInfo.displayName)
      val roles = remember { member.canChangeRoleTo(groupInfo) }
      if (roles != null) {
        RoleSelectionRow(roles, newRole, onRoleSelected)
      } else {
        InfoRow(stringResource(R.string.role_in_group), member.memberRole.text)
      }
      val conn = member.activeConn
      if (conn != null) {
        val connLevelDesc =
          if (conn.connLevel == 0) stringResource(R.string.conn_level_desc_direct)
          else String.format(generalGetString(R.string.conn_level_desc_indirect), conn.connLevel)
        InfoRow(stringResource(R.string.info_row_connection), connLevelDesc)
      }
    }
    if (connStats != null) {
      SectionDividerSpaced()
      SectionView(title = stringResource(R.string.conn_stats_section_title_servers)) {
      SwitchAddressButton(switchMemberAddress)
        val rcvServers = connStats.rcvServers
        val sndServers = connStats.sndServers
        if ((rcvServers != null && rcvServers.isNotEmpty()) || (sndServers != null && sndServers.isNotEmpty())) {
          if (rcvServers != null && rcvServers.isNotEmpty()) {
            SimplexServers(stringResource(R.string.receiving_via), rcvServers)
            if (sndServers != null && sndServers.isNotEmpty()) {
              SimplexServers(stringResource(R.string.sending_via), sndServers)
            }
          } else if (sndServers != null && sndServers.isNotEmpty()) {
            SimplexServers(stringResource(R.string.sending_via), sndServers)
          }
        }
      }
    }

    if (member.canBeRemoved(groupInfo)) {
      SectionDividerSpaced(maxBottomPadding = false)
      SectionView {
        RemoveMemberButton(removeMember)
      }
    }

    if (developerTools) {
      SectionDividerSpaced()
      SectionView(title = stringResource(R.string.section_title_for_console)) {
        InfoRow(stringResource(R.string.info_row_local_name), member.localDisplayName)
        InfoRow(stringResource(R.string.info_row_database_id), member.groupMemberId.toString())
      }
    }
    SectionBottomSpacer()
  }
}

@Composable
fun GroupMemberInfoHeader(member: GroupMember) {
  Column(
    Modifier.padding(horizontal = 8.dp),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    ProfileImage(size = 192.dp, member.image, color = if (isInDarkTheme()) GroupDark else SettingsSecondaryLight)
    Row(verticalAlignment = Alignment.CenterVertically) {
      if (member.verified) {
        Icon(painterResource(R.drawable.ic_verified_user), null, Modifier.padding(end = 6.dp, top = 4.dp).size(24.dp), tint = MaterialTheme.colors.secondary)
      }
      Text(
        member.displayName, style = MaterialTheme.typography.h1.copy(fontWeight = FontWeight.Normal),
        color = MaterialTheme.colors.onBackground,
        maxLines = 1,
        overflow = TextOverflow.Ellipsis
      )
    }
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
fun RemoveMemberButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(R.drawable.ic_delete),
    stringResource(R.string.button_remove_member),
    click = onClick,
    textColor = Color.Red,
    iconColor = Color.Red,
  )
}

@Composable
fun OpenChatButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(R.drawable.ic_chat),
    stringResource(R.string.button_send_direct_message),
    click = onClick,
    textColor = MaterialTheme.colors.primary,
    iconColor = MaterialTheme.colors.primary,
  )
}

@Composable
fun ConnectViaAddressButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(R.drawable.ic_link),
    stringResource(R.string.connect_button),
    click = onClick,
    textColor = MaterialTheme.colors.primary,
    iconColor = MaterialTheme.colors.primary,
  )
}

@Composable
private fun RoleSelectionRow(
  roles: List<GroupMemberRole>,
  selectedRole: MutableState<GroupMemberRole>,
  onSelected: (GroupMemberRole) -> Unit
) {
  Row(
    Modifier.fillMaxWidth(),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.SpaceBetween
  ) {
    val values = remember { roles.map { it to it.text } }
    ExposedDropDownSettingRow(
      generalGetString(R.string.change_role),
      values,
      selectedRole,
      icon = null,
      enabled = remember { mutableStateOf(true) },
      onSelected = onSelected
    )
  }
}

private fun updateMemberRoleDialog(
  newRole: GroupMemberRole,
  member: GroupMember,
  onDismiss: () -> Unit,
  onConfirm: () -> Unit
) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.change_member_role_question),
    text = if (member.memberCurrent)
      String.format(generalGetString(R.string.member_role_will_be_changed_with_notification), newRole.text)
    else
      String.format(generalGetString(R.string.member_role_will_be_changed_with_invitation), newRole.text),
    confirmText = generalGetString(R.string.change_verb),
    onDismiss = onDismiss,
    onConfirm = onConfirm,
    onDismissRequest = onDismiss
  )
}

private fun switchMemberAddress(m: ChatModel, groupInfo: GroupInfo, member: GroupMember) = withApi {
  m.controller.apiSwitchGroupMember(groupInfo.apiId, member.groupMemberId)
}

@Preview
@Composable
fun PreviewGroupMemberInfoLayout() {
  SimpleXTheme {
    GroupMemberInfoLayout(
      groupInfo = GroupInfo.sampleData,
      member = GroupMember.sampleData,
      connStats = null,
      newRole = remember { mutableStateOf(GroupMemberRole.Member) },
      developerTools = false,
      connectionCode = "123",
      getContactChat = { Chat.sampleData },
      openDirectChat = {},
      connectViaAddress = {},
      removeMember = {},
      onRoleSelected = {},
      switchMemberAddress = {},
      verifyClicked = {},
    )
  }
}
