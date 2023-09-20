package chat.simplex.common.views.chat.group

import InfoRow
import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionSpacer
import SectionTextFooter
import SectionView
import androidx.compose.desktop.ui.tooling.preview.Preview
import java.net.URI
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.InlineTextContent
import androidx.compose.foundation.text.appendInlineContent
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.text.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.*
import chat.simplex.common.views.usersettings.SettingsActionItem
import chat.simplex.common.model.GroupInfo
import chat.simplex.common.platform.*
import chat.simplex.common.views.chatlist.openChat
import chat.simplex.res.MR
import kotlinx.datetime.Clock

@Composable
fun GroupMemberInfoView(
  groupInfo: GroupInfo,
  member: GroupMember,
  connectionStats: ConnectionStats?,
  connectionCode: String?,
  chatModel: ChatModel,
  close: () -> Unit,
  closeAll: () -> Unit, // Close all open windows up to ChatView
) {
  BackHandler(onBack = close)
  val chat = chatModel.chats.firstOrNull { it.id == chatModel.chatId.value }
  val connStats = remember { mutableStateOf(connectionStats) }
  val developerTools = chatModel.controller.appPrefs.developerTools.get()
  var progressIndicator by remember { mutableStateOf(false) }

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
      createMemberContact = {
        withApi {
          progressIndicator = true
          val memberContact = chatModel.controller.apiCreateMemberContact(groupInfo.apiId, member.groupMemberId)
          if (memberContact != null) {
            val memberChat = Chat(ChatInfo.Direct(memberContact), chatItems = arrayListOf())
            chatModel.addChat(memberChat)
            openChat(memberChat, chatModel)
            closeAll()
            chatModel.setContactNetworkStatus(memberContact, NetworkStatus.Connected())
          }
          progressIndicator = false
        }
      },
      connectViaAddress = { connReqUri ->
        connectViaMemberAddressAlert(connReqUri)
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
        showSwitchAddressAlert(switchAddress = {
          withApi {
            val r = chatModel.controller.apiSwitchGroupMember(groupInfo.apiId, member.groupMemberId)
            if (r != null) {
              connStats.value = r.second
              chatModel.updateGroupMemberConnectionStats(groupInfo, r.first, r.second)
              close.invoke()
            }
          }
        })
      },
      abortSwitchMemberAddress = {
        showAbortSwitchAddressAlert(abortSwitchAddress = {
          withApi {
            val r = chatModel.controller.apiAbortSwitchGroupMember(groupInfo.apiId, member.groupMemberId)
            if (r != null) {
              connStats.value = r.second
              chatModel.updateGroupMemberConnectionStats(groupInfo, r.first, r.second)
              close.invoke()
            }
          }
        })
      },
      syncMemberConnection = {
        withApi {
          val r = chatModel.controller.apiSyncGroupMemberRatchet(groupInfo.apiId, member.groupMemberId, force = false)
          if (r != null) {
            connStats.value = r.second
            chatModel.updateGroupMemberConnectionStats(groupInfo, r.first, r.second)
            close.invoke()
          }
        }
      },
      syncMemberConnectionForce = {
        showSyncConnectionForceAlert(syncConnectionForce = {
          withApi {
            val r = chatModel.controller.apiSyncGroupMemberRatchet(groupInfo.apiId, member.groupMemberId, force = true)
            if (r != null) {
              connStats.value = r.second
              chatModel.updateGroupMemberConnectionStats(groupInfo, r.first, r.second)
              close.invoke()
            }
          }
        })
      },
      verifyClicked = {
        ModalManager.end.showModalCloseable { close ->
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

    if (progressIndicator) {
      ProgressIndicator()
    }
  }
}

fun removeMemberDialog(groupInfo: GroupInfo, member: GroupMember, chatModel: ChatModel, close: (() -> Unit)? = null) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.button_remove_member),
    text = generalGetString(MR.strings.member_will_be_removed_from_group_cannot_be_undone),
    confirmText = generalGetString(MR.strings.remove_member_confirmation),
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
  connStats: MutableState<ConnectionStats?>,
  newRole: MutableState<GroupMemberRole>,
  developerTools: Boolean,
  connectionCode: String?,
  getContactChat: (Long) -> Chat?,
  openDirectChat: (Long) -> Unit,
  createMemberContact: () -> Unit,
  connectViaAddress: (String) -> Unit,
  removeMember: () -> Unit,
  onRoleSelected: (GroupMemberRole) -> Unit,
  switchMemberAddress: () -> Unit,
  abortSwitchMemberAddress: () -> Unit,
  syncMemberConnection: () -> Unit,
  syncMemberConnectionForce: () -> Unit,
  verifyClicked: () -> Unit,
) {
  val cStats = connStats.value
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
      SectionView {
        if (contactId != null && knownDirectChat(contactId) != null) {
          OpenChatButton(onClick = { openDirectChat(contactId) })
        } else if (groupInfo.fullGroupPreferences.directMessages.on) {
          if (contactId != null) {
            OpenChatButton(onClick = { openDirectChat(contactId) })
          } else if (member.activeConn?.peerChatVRange?.isCompatibleRange(CREATE_MEMBER_CONTACT_VRANGE) == true) {
            OpenChatButton(onClick = { createMemberContact() })
          }
        }
        if (connectionCode != null) {
          VerifyCodeButton(member.verified, verifyClicked)
        }
        if (cStats != null && cStats.ratchetSyncAllowed) {
          SynchronizeConnectionButton(syncMemberConnection)
        }
//        } else if (developerTools) {
//          SynchronizeConnectionButtonForce(syncMemberConnectionForce)
//        }
      }
      SectionDividerSpaced()
    }

    if (member.contactLink != null) {
      SectionView(stringResource(MR.strings.address_section_title).uppercase()) {
        QRCode(member.contactLink, Modifier.padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF).aspectRatio(1f))
        val clipboard = LocalClipboardManager.current
        ShareAddressButton { clipboard.shareText(member.contactLink) }
        if (contactId != null) {
          if (knownDirectChat(contactId) == null && !groupInfo.fullGroupPreferences.directMessages.on) {
            ConnectViaAddressButton(onClick = { connectViaAddress(member.contactLink) })
          }
        } else {
          ConnectViaAddressButton(onClick = { connectViaAddress(member.contactLink) })
        }
        SectionTextFooter(stringResource(MR.strings.you_can_share_this_address_with_your_contacts).format(member.displayName))
      }
      SectionDividerSpaced()
    }

    SectionView(title = stringResource(MR.strings.member_info_section_title_member)) {
      InfoRow(stringResource(MR.strings.info_row_group), groupInfo.displayName)
      val roles = remember { member.canChangeRoleTo(groupInfo) }
      if (roles != null) {
        RoleSelectionRow(roles, newRole, onRoleSelected)
      } else {
        InfoRow(stringResource(MR.strings.role_in_group), member.memberRole.text)
      }
      val conn = member.activeConn
      if (conn != null) {
        val connLevelDesc =
          if (conn.connLevel == 0) stringResource(MR.strings.conn_level_desc_direct)
          else String.format(generalGetString(MR.strings.conn_level_desc_indirect), conn.connLevel)
        InfoRow(stringResource(MR.strings.info_row_connection), connLevelDesc)
      }
    }
    if (cStats != null) {
      SectionDividerSpaced()
      SectionView(title = stringResource(MR.strings.conn_stats_section_title_servers)) {
        SwitchAddressButton(
          disabled = cStats.rcvQueuesInfo.any { it.rcvSwitchStatus != null } || cStats.ratchetSyncSendProhibited,
          switchAddress = switchMemberAddress
        )
        if (cStats.rcvQueuesInfo.any { it.rcvSwitchStatus != null }) {
          AbortSwitchAddressButton(
            disabled = cStats.rcvQueuesInfo.any { it.rcvSwitchStatus != null && !it.canAbortSwitch } || cStats.ratchetSyncSendProhibited,
            abortSwitchAddress = abortSwitchMemberAddress
          )
        }
        val rcvServers = cStats.rcvQueuesInfo.map { it.rcvServer }
        if (rcvServers.isNotEmpty()) {
          SimplexServers(stringResource(MR.strings.receiving_via), rcvServers)
        }
        val sndServers = cStats.sndQueuesInfo.map { it.sndServer }
        if (sndServers.isNotEmpty()) {
          SimplexServers(stringResource(MR.strings.sending_via), sndServers)
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
      SectionView(title = stringResource(MR.strings.section_title_for_console)) {
        InfoRow(stringResource(MR.strings.info_row_local_name), member.localDisplayName)
        InfoRow(stringResource(MR.strings.info_row_database_id), member.groupMemberId.toString())
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
    val text = buildAnnotatedString {
      if (member.verified) {
        appendInlineContent(id = "shieldIcon")
      }
      append(member.displayName)
    }
    val inlineContent: Map<String, InlineTextContent> = mapOf(
      "shieldIcon" to InlineTextContent(
        Placeholder(24.sp, 24.sp, PlaceholderVerticalAlign.TextCenter)
      ) {
        Icon(painterResource(MR.images.ic_verified_user), null, tint = MaterialTheme.colors.secondary)
      }
    )
    Text(
      text,
      inlineContent = inlineContent,
      style = MaterialTheme.typography.h1.copy(fontWeight = FontWeight.Normal),
      textAlign = TextAlign.Center,
      maxLines = 3,
      overflow = TextOverflow.Ellipsis
    )
    if (member.fullName != "" && member.fullName != member.displayName) {
      Text(
        member.fullName, style = MaterialTheme.typography.h2,
        color = MaterialTheme.colors.onBackground,
        textAlign = TextAlign.Center,
        maxLines = 4,
        overflow = TextOverflow.Ellipsis
      )
    }
  }
}

@Composable
fun RemoveMemberButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_delete),
    stringResource(MR.strings.button_remove_member),
    click = onClick,
    textColor = Color.Red,
    iconColor = Color.Red,
  )
}

@Composable
fun OpenChatButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_chat),
    stringResource(MR.strings.button_send_direct_message),
    click = onClick,
    textColor = MaterialTheme.colors.primary,
    iconColor = MaterialTheme.colors.primary,
  )
}

@Composable
fun ConnectViaAddressButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_link),
    stringResource(MR.strings.connect_button),
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
      generalGetString(MR.strings.change_role),
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
    title = generalGetString(MR.strings.change_member_role_question),
    text = if (member.memberCurrent)
      String.format(generalGetString(MR.strings.member_role_will_be_changed_with_notification), newRole.text)
    else
      String.format(generalGetString(MR.strings.member_role_will_be_changed_with_invitation), newRole.text),
    confirmText = generalGetString(MR.strings.change_verb),
    onDismiss = onDismiss,
    onConfirm = onConfirm,
    onDismissRequest = onDismiss
  )
}

fun connectViaMemberAddressAlert(connReqUri: String) {
  AlertManager.shared.showAlertDialogButtonsColumn(
    title = generalGetString(MR.strings.connect_via_member_address_alert_title),
    text = AnnotatedString(generalGetString(MR.strings.connect_via_member_address_alert_desc)),
    buttons = {
      Column {
        SectionItemView({
          AlertManager.shared.hideAlert()
          val uri = URI(connReqUri)
          withUriAction(uri) { linkType ->
            withApi {
              Log.d(TAG, "connectViaUri: connecting")
              connectViaUri(chatModel, linkType, uri, incognito = false)
            }
          }
        }) {
          Text(generalGetString(MR.strings.connect_use_current_profile), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
        SectionItemView({
          AlertManager.shared.hideAlert()
          val uri = URI(connReqUri)
          withUriAction(uri) { linkType ->
            withApi {
              Log.d(TAG, "connectViaUri: connecting incognito")
              connectViaUri(chatModel, linkType, uri, incognito = true)
            }
          }
        }) {
          Text(generalGetString(MR.strings.connect_use_new_incognito_profile), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
        SectionItemView({
          AlertManager.shared.hideAlert()
        }) {
          Text(stringResource(MR.strings.cancel_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
      }
    }
  )
}

@Preview
@Composable
fun PreviewGroupMemberInfoLayout() {
  SimpleXTheme {
    GroupMemberInfoLayout(
      groupInfo = GroupInfo.sampleData,
      member = GroupMember.sampleData,
      connStats = remember { mutableStateOf(null) },
      newRole = remember { mutableStateOf(GroupMemberRole.Member) },
      developerTools = false,
      connectionCode = "123",
      getContactChat = { Chat.sampleData },
      openDirectChat = {},
      createMemberContact = {},
      connectViaAddress = {},
      removeMember = {},
      onRoleSelected = {},
      switchMemberAddress = {},
      abortSwitchMemberAddress = {},
      syncMemberConnection = {},
      syncMemberConnectionForce = {},
      verifyClicked = {},
    )
  }
}
