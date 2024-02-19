package chat.simplex.common.views.chat.group

import InfoRow
import SectionBottomSpacer
import SectionDividerSpaced
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
import chat.simplex.common.views.chatlist.openLoadedChat
import chat.simplex.res.MR
import kotlinx.datetime.Clock

@Composable
fun GroupMemberInfoView(
  rhId: Long?,
  groupInfo: GroupInfo,
  member: GroupMember,
  connectionStats: ConnectionStats?,
  connectionCode: String?,
  chatModel: ChatModel,
  close: () -> Unit,
  closeAll: () -> Unit, // Close all open windows up to ChatView
) {
  BackHandler(onBack = close)
  val chat = chatModel.chats.firstOrNull { ch -> ch.id == chatModel.chatId.value && ch.remoteHostId == rhId }
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
        withBGApi {
          val c = chatModel.controller.apiGetChat(rhId, ChatType.Direct, it)
          if (c != null) {
            if (chatModel.getContactChat(it) == null) {
              chatModel.addChat(c)
            }
            chatModel.chatItemStatuses.clear()
            chatModel.chatItems.replaceAll(c.chatItems)
            chatModel.chatId.value = c.id
            closeAll()
          }
        }
      },
      createMemberContact = {
        withBGApi {
          progressIndicator = true
          val memberContact = chatModel.controller.apiCreateMemberContact(rhId, groupInfo.apiId, member.groupMemberId)
          if (memberContact != null) {
            val memberChat = Chat(remoteHostId = rhId, ChatInfo.Direct(memberContact), chatItems = arrayListOf())
            chatModel.addChat(memberChat)
            openLoadedChat(memberChat, chatModel)
            closeAll()
            chatModel.setContactNetworkStatus(memberContact, NetworkStatus.Connected())
          }
          progressIndicator = false
        }
      },
      connectViaAddress = { connReqUri ->
        connectViaMemberAddressAlert(rhId, connReqUri)
      },
      blockMember = { blockMemberAlert(rhId, groupInfo, member) },
      unblockMember = { unblockMemberAlert(rhId, groupInfo, member) },
      blockForAll = { blockForAllAlert(rhId, groupInfo, member) },
      unblockForAll = { unblockForAllAlert(rhId, groupInfo, member) },
      removeMember = { removeMemberDialog(rhId, groupInfo, member, chatModel, close) },
      onRoleSelected = {
        if (it == newRole.value) return@GroupMemberInfoLayout
        val prevValue = newRole.value
        newRole.value = it
        updateMemberRoleDialog(it, member, onDismiss = {
          newRole.value = prevValue
        }) {
          withBGApi {
            kotlin.runCatching {
              val mem = chatModel.controller.apiMemberRole(rhId, groupInfo.groupId, member.groupMemberId, it)
              chatModel.upsertGroupMember(rhId, groupInfo, mem)
            }.onFailure {
              newRole.value = prevValue
            }
          }
        }
      },
      switchMemberAddress = {
        showSwitchAddressAlert(switchAddress = {
          withBGApi {
            val r = chatModel.controller.apiSwitchGroupMember(rhId, groupInfo.apiId, member.groupMemberId)
            if (r != null) {
              connStats.value = r.second
              chatModel.updateGroupMemberConnectionStats(rhId, groupInfo, r.first, r.second)
              close.invoke()
            }
          }
        })
      },
      abortSwitchMemberAddress = {
        showAbortSwitchAddressAlert(abortSwitchAddress = {
          withBGApi {
            val r = chatModel.controller.apiAbortSwitchGroupMember(rhId, groupInfo.apiId, member.groupMemberId)
            if (r != null) {
              connStats.value = r.second
              chatModel.updateGroupMemberConnectionStats(rhId, groupInfo, r.first, r.second)
              close.invoke()
            }
          }
        })
      },
      syncMemberConnection = {
        withBGApi {
          val r = chatModel.controller.apiSyncGroupMemberRatchet(rhId, groupInfo.apiId, member.groupMemberId, force = false)
          if (r != null) {
            connStats.value = r.second
            chatModel.updateGroupMemberConnectionStats(rhId, groupInfo, r.first, r.second)
            close.invoke()
          }
        }
      },
      syncMemberConnectionForce = {
        showSyncConnectionForceAlert(syncConnectionForce = {
          withBGApi {
            val r = chatModel.controller.apiSyncGroupMemberRatchet(rhId, groupInfo.apiId, member.groupMemberId, force = true)
            if (r != null) {
              connStats.value = r.second
              chatModel.updateGroupMemberConnectionStats(rhId, groupInfo, r.first, r.second)
              close.invoke()
            }
          }
        })
      },
      verifyClicked = {
        ModalManager.end.showModalCloseable { close ->
          remember { derivedStateOf { chatModel.getGroupMember(member.groupMemberId) } }.value?.let { mem ->
            VerifyCodeView(
              mem.displayName,
              connectionCode,
              mem.verified,
              verify = { code ->
                chatModel.controller.apiVerifyGroupMember(rhId, mem.groupId, mem.groupMemberId, code)?.let { r ->
                  val (verified, existingCode) = r
                  chatModel.upsertGroupMember(
                    rhId,
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

fun removeMemberDialog(rhId: Long?, groupInfo: GroupInfo, member: GroupMember, chatModel: ChatModel, close: (() -> Unit)? = null) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.button_remove_member),
    text = generalGetString(MR.strings.member_will_be_removed_from_group_cannot_be_undone),
    confirmText = generalGetString(MR.strings.remove_member_confirmation),
    onConfirm = {
      withBGApi {
        val removedMember = chatModel.controller.apiRemoveMember(rhId, member.groupId, member.groupMemberId)
        if (removedMember != null) {
          chatModel.upsertGroupMember(rhId, groupInfo, removedMember)
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
  blockMember: () -> Unit,
  unblockMember: () -> Unit,
  blockForAll: () -> Unit,
  unblockForAll: () -> Unit,
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

  @Composable
  fun AdminDestructiveSection() {
    val canBlockForAll = member.canBlockForAll(groupInfo)
    val canRemove = member.canBeRemoved(groupInfo)
    if (canBlockForAll || canRemove) {
      SectionDividerSpaced(maxBottomPadding = false)
      SectionView {
        if (canBlockForAll) {
          if (member.blockedByAdmin) {
            UnblockForAllButton(unblockForAll)
          } else {
            BlockForAllButton(blockForAll)
          }
        }
        if (canRemove) {
          RemoveMemberButton(removeMember)
        }
      }
    }
  }

  @Composable
  fun NonAdminBlockSection() {
    SectionDividerSpaced(maxBottomPadding = false)
    SectionView {
      if (member.blockedByAdmin) {
        SettingsActionItem(
          painterResource(MR.images.ic_back_hand),
          stringResource(MR.strings.member_blocked_by_admin),
          click = null,
          disabled = true
        )
      } else if (member.memberSettings.showMessages) {
        BlockMemberButton(blockMember)
      } else {
        UnblockMemberButton(unblockMember)
      }
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
        SimpleXLinkQRCode(member.contactLink)
        val clipboard = LocalClipboardManager.current
        ShareAddressButton { clipboard.shareText(simplexChatLink(member.contactLink)) }
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

    if (groupInfo.membership.memberRole >= GroupMemberRole.Admin) {
      AdminDestructiveSection()
    } else {
      NonAdminBlockSection()
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
    Modifier.padding(horizontal = 16.dp),
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
fun BlockMemberButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_back_hand),
    stringResource(MR.strings.block_member_button),
    click = onClick,
    textColor = Color.Red,
    iconColor = Color.Red,
  )
}

@Composable
fun UnblockMemberButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_do_not_touch),
    stringResource(MR.strings.unblock_member_button),
    click = onClick
  )
}

@Composable
fun BlockForAllButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_back_hand),
    stringResource(MR.strings.block_for_all),
    click = onClick,
    textColor = Color.Red,
    iconColor = Color.Red,
  )
}

@Composable
fun UnblockForAllButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_do_not_touch),
    stringResource(MR.strings.unblock_for_all),
    click = onClick
  )
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

fun connectViaMemberAddressAlert(rhId: Long?, connReqUri: String) {
  try {
    val uri = URI(connReqUri)
    withBGApi {
      planAndConnect(rhId, uri, incognito = null, close = { ModalManager.closeAllModalsEverywhere() })
    }
  } catch (e: RuntimeException) {
    AlertManager.shared.showAlertMsg(
      title = generalGetString(MR.strings.invalid_connection_link),
      text = generalGetString(MR.strings.this_string_is_not_a_connection_link)
    )
  }
}

fun blockMemberAlert(rhId: Long?, gInfo: GroupInfo, mem: GroupMember) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.block_member_question),
    text = generalGetString(MR.strings.block_member_desc).format(mem.chatViewName),
    confirmText = generalGetString(MR.strings.block_member_confirmation),
    onConfirm = {
      toggleShowMemberMessages(rhId, gInfo, mem, false)
    },
    destructive = true,
  )
}

fun unblockMemberAlert(rhId: Long?, gInfo: GroupInfo, mem: GroupMember) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.unblock_member_question),
    text = generalGetString(MR.strings.unblock_member_desc).format(mem.chatViewName),
    confirmText = generalGetString(MR.strings.unblock_member_confirmation),
    onConfirm = {
      toggleShowMemberMessages(rhId, gInfo, mem, true)
    },
  )
}

fun toggleShowMemberMessages(rhId: Long?, gInfo: GroupInfo, member: GroupMember, showMessages: Boolean) {
  val updatedMemberSettings = member.memberSettings.copy(showMessages = showMessages)
  updateMemberSettings(rhId, gInfo, member, updatedMemberSettings)
}

fun updateMemberSettings(rhId: Long?, gInfo: GroupInfo, member: GroupMember, memberSettings: GroupMemberSettings) {
  withBGApi {
    val success = ChatController.apiSetMemberSettings(rhId, gInfo.groupId, member.groupMemberId, memberSettings)
    if (success) {
      ChatModel.upsertGroupMember(rhId, gInfo, member.copy(memberSettings = memberSettings))
    }
  }
}

fun blockForAllAlert(rhId: Long?, gInfo: GroupInfo, mem: GroupMember) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.block_for_all_question),
    text = generalGetString(MR.strings.block_member_desc).format(mem.chatViewName),
    confirmText = generalGetString(MR.strings.block_for_all),
    onConfirm = {
      blockMemberForAll(rhId, gInfo, mem, true)
    },
    destructive = true,
  )
}

fun unblockForAllAlert(rhId: Long?, gInfo: GroupInfo, mem: GroupMember) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.unblock_for_all_question),
    text = generalGetString(MR.strings.unblock_member_desc).format(mem.chatViewName),
    confirmText = generalGetString(MR.strings.unblock_for_all),
    onConfirm = {
      blockMemberForAll(rhId, gInfo, mem, false)
    },
  )
}

fun blockMemberForAll(rhId: Long?, gInfo: GroupInfo, member: GroupMember, blocked: Boolean) {
  withBGApi {
    val updatedMember = ChatController.apiBlockMemberForAll(rhId, gInfo.groupId, member.groupMemberId, blocked)
    chatModel.upsertGroupMember(rhId, gInfo, updatedMember)
  }
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
      blockMember = {},
      unblockMember = {},
      blockForAll = {},
      unblockForAll = {},
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
