package chat.simplex.common.views.chat.group

import InfoRow
import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionItemViewLongClickable
import SectionSpacer
import SectionTextFooter
import SectionView
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatModel.withChats
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.common.model.GroupInfo
import chat.simplex.common.platform.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.chatlist.*
import chat.simplex.res.MR
import kotlinx.coroutines.launch

const val SMALL_GROUPS_RCPS_MEM_LIMIT: Int = 20

@Composable
fun GroupChatInfoView(chatModel: ChatModel, rhId: Long?, chatId: String, groupLink: String?, groupLinkMemberRole: GroupMemberRole?, onGroupLinkUpdated: (Pair<String, GroupMemberRole>?) -> Unit, close: () -> Unit, onSearchClicked: () -> Unit) {
  BackHandler(onBack = close)
  // TODO derivedStateOf?
  val chat = chatModel.chats.value.firstOrNull { ch -> ch.id == chatId && ch.remoteHostId == rhId }
  val currentUser = chatModel.currentUser.value
  val developerTools = chatModel.controller.appPrefs.developerTools.get()
  if (chat != null && chat.chatInfo is ChatInfo.Group && currentUser != null) {
    val groupInfo = chat.chatInfo.groupInfo
    val sendReceipts = remember { mutableStateOf(SendReceipts.fromBool(groupInfo.chatSettings.sendRcpts, currentUser.sendRcptsSmallGroups)) }
    GroupChatInfoLayout(
      chat,
      groupInfo,
      currentUser,
      sendReceipts = sendReceipts,
      setSendReceipts = { sendRcpts ->
        val chatSettings = (chat.chatInfo.chatSettings ?: ChatSettings.defaults).copy(sendRcpts = sendRcpts.bool)
        updateChatSettings(chat.remoteHostId, chat.chatInfo, chatSettings, chatModel)
        sendReceipts.value = sendRcpts
      },
      members = chatModel.groupMembers
        .filter { it.memberStatus != GroupMemberStatus.MemLeft && it.memberStatus != GroupMemberStatus.MemRemoved }
        .sortedByDescending { it.memberRole },
      developerTools,
      groupLink,
      addMembers = {
        withBGApi {
          setGroupMembers(rhId, groupInfo, chatModel)
          ModalManager.end.showModalCloseable(true) { close ->
            AddGroupMembersView(rhId, groupInfo, false, chatModel, close)
          }
        }
      },
      showMemberInfo = { member ->
        withBGApi {
          val r = chatModel.controller.apiGroupMemberInfo(rhId, groupInfo.groupId, member.groupMemberId)
          val stats = r?.second
          val (_, code) = if (member.memberActive) {
            val memCode = chatModel.controller.apiGetGroupMemberCode(rhId, groupInfo.apiId, member.groupMemberId)
            member to memCode?.second
          } else {
            member to null
          }
          ModalManager.end.showModalCloseable(true) { closeCurrent ->
            remember { derivedStateOf { chatModel.getGroupMember(member.groupMemberId) } }.value?.let { mem ->
              GroupMemberInfoView(rhId, groupInfo, mem, stats, code, chatModel, closeCurrent) {
                closeCurrent()
                close()
              }
            }
          }
        }
      },
      editGroupProfile = {
        ModalManager.end.showCustomModal { close -> GroupProfileView(rhId, groupInfo, chatModel, close) }
      },
      addOrEditWelcomeMessage = {
        ModalManager.end.showCustomModal { close -> GroupWelcomeView(chatModel, rhId, groupInfo, close) }
      },
      openPreferences = {
        ModalManager.end.showCustomModal { close ->
          GroupPreferencesView(
            chatModel,
            rhId,
            chat.id,
            close
          )
        }
      },
      deleteGroup = { deleteGroupDialog(chat, groupInfo, chatModel, close) },
      clearChat = { clearChatDialog(chat, close) },
      leaveGroup = { leaveGroupDialog(rhId, groupInfo, chatModel, close) },
      manageGroupLink = {
          ModalManager.end.showModal { GroupLinkView(chatModel, rhId, groupInfo, groupLink, groupLinkMemberRole, onGroupLinkUpdated) }
      },
      onSearchClicked = onSearchClicked
    )
  }
}

fun deleteGroupDialog(chat: Chat, groupInfo: GroupInfo, chatModel: ChatModel, close: (() -> Unit)? = null) {
  val chatInfo = chat.chatInfo
  val alertTextKey =
    if (groupInfo.membership.memberCurrent) MR.strings.delete_group_for_all_members_cannot_undo_warning
    else MR.strings.delete_group_for_self_cannot_undo_warning
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.delete_group_question),
    text = generalGetString(alertTextKey),
    confirmText = generalGetString(MR.strings.delete_verb),
    onConfirm = {
      withBGApi {
        val r = chatModel.controller.apiDeleteChat(chat.remoteHostId, chatInfo.chatType, chatInfo.apiId)
        if (r) {
          withChats {
            removeChat(chat.remoteHostId, chatInfo.id)
            if (chatModel.chatId.value == chatInfo.id) {
              chatModel.chatId.value = null
              ModalManager.end.closeModals()
            }
            ntfManager.cancelNotificationsForChat(chatInfo.id)
            close?.invoke()
          }
        }
      }
    },
    destructive = true,
  )
}

fun leaveGroupDialog(rhId: Long?, groupInfo: GroupInfo, chatModel: ChatModel, close: (() -> Unit)? = null) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.leave_group_question),
    text = generalGetString(MR.strings.you_will_stop_receiving_messages_from_this_group_chat_history_will_be_preserved),
    confirmText = generalGetString(MR.strings.leave_group_button),
    onConfirm = {
      withLongRunningApi(60_000) {
        chatModel.controller.leaveGroup(rhId, groupInfo.groupId)
        close?.invoke()
      }
    },
    destructive = true,
  )
}

private fun removeMemberAlert(rhId: Long?, groupInfo: GroupInfo, mem: GroupMember) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.button_remove_member_question),
    text = generalGetString(MR.strings.member_will_be_removed_from_group_cannot_be_undone),
    confirmText = generalGetString(MR.strings.remove_member_confirmation),
    onConfirm = {
      withBGApi {
        val updatedMember = chatModel.controller.apiRemoveMember(rhId, groupInfo.groupId, mem.groupMemberId)
        if (updatedMember != null) {
          withChats {
            upsertGroupMember(rhId, groupInfo, updatedMember)
          }
        }
      }
    },
    destructive = true,
  )
}

@Composable
fun SearchButton(chat: Chat, group: GroupInfo, close: () -> Unit, onSearchClicked: () -> Unit) {
  val disabled = !group.ready || chat.chatItems.isEmpty()

  InfoViewActionButton(
    icon = painterResource(MR.images.ic_search),
    title = generalGetString(MR.strings.info_view_search_button),
    disabled = disabled,
    disabledLook = disabled,
    onClick = {
      if (appPlatform.isAndroid) {
        close.invoke()
      }
      onSearchClicked()
    }
  )
}

@Composable
fun MuteButton(chat: Chat, groupInfo: GroupInfo) {
  val ntfsEnabled = remember { mutableStateOf(chat.chatInfo.ntfsEnabled) }

  InfoViewActionButton(
    icon =  if (ntfsEnabled.value) painterResource(MR.images.ic_notifications_off) else painterResource(MR.images.ic_notifications),
    title = if (ntfsEnabled.value) stringResource(MR.strings.mute_chat) else stringResource(MR.strings.unmute_chat),
    disabled = !groupInfo.ready,
    disabledLook = !groupInfo.ready,
    onClick = {
      toggleNotifications(chat.remoteHostId, chat.chatInfo, !ntfsEnabled.value, chatModel, ntfsEnabled)
    }
  )
}

@Composable
fun AddGroupMembersButton(chat: Chat, groupInfo: GroupInfo) {
  InfoViewActionButton(
    icon =  if (groupInfo.incognito) painterResource(MR.images.ic_add_link) else painterResource(MR.images.ic_person_add_500),
    title = stringResource(MR.strings.action_button_add_members),
    disabled = !groupInfo.ready,
    disabledLook = !groupInfo.ready,
    onClick = {
      if (groupInfo.incognito) {
        openGroupLink(groupInfo = groupInfo, rhId = chat.remoteHostId)
      } else {
        addGroupMembers(groupInfo = groupInfo, rhId = chat.remoteHostId)
      }
    }
  )
}


@Composable
fun GroupChatInfoLayout(
  chat: Chat,
  groupInfo: GroupInfo,
  currentUser: User,
  sendReceipts: State<SendReceipts>,
  setSendReceipts: (SendReceipts) -> Unit,
  members: List<GroupMember>,
  developerTools: Boolean,
  groupLink: String?,
  addMembers: () -> Unit,
  showMemberInfo: (GroupMember) -> Unit,
  editGroupProfile: () -> Unit,
  addOrEditWelcomeMessage: () -> Unit,
  openPreferences: () -> Unit,
  deleteGroup: () -> Unit,
  clearChat: () -> Unit,
  leaveGroup: () -> Unit,
  manageGroupLink: () -> Unit,
  close: () -> Unit = { ModalManager.closeAllModalsEverywhere()},
  onSearchClicked: () -> Unit
) {
  val listState = rememberLazyListState()
  val scope = rememberCoroutineScope()
  KeyChangeEffect(chat.id) {
    scope.launch { listState.scrollToItem(0) }
  }
  val searchText = rememberSaveable(stateSaver = TextFieldValue.Saver) { mutableStateOf(TextFieldValue()) }
  val filteredMembers = remember(members) { derivedStateOf { members.filter { it.chatViewName.lowercase().contains(searchText.value.text.trim().lowercase()) } } }
  // LALAL strange scrolling
  LazyColumnWithScrollBar(
    Modifier
      .fillMaxWidth(),
    state = listState
  ) {
    item {
      Row(
        Modifier.fillMaxWidth(),
        horizontalArrangement = Arrangement.Center
      ) {
        GroupChatInfoHeader(chat.chatInfo)
      }
      SectionSpacer()

      Row(
        Modifier
          .fillMaxWidth()
          .padding(horizontal = DEFAULT_PADDING),
        horizontalArrangement = Arrangement.Center,
        verticalAlignment = Alignment.CenterVertically
      ) {
        SearchButton(chat, groupInfo, close, onSearchClicked)
        if (groupInfo.canAddMembers) {
          Spacer(Modifier.width(INFO_VIEW_BUTTONS_PADDING))
          AddGroupMembersButton(chat, groupInfo)
        }
        Spacer(Modifier.width(INFO_VIEW_BUTTONS_PADDING))
        MuteButton(chat, groupInfo)
      }

      SectionSpacer()

      SectionView {
        if (groupInfo.canEdit) {
          EditGroupProfileButton(editGroupProfile)
        }
        if (groupInfo.groupProfile.description != null || groupInfo.canEdit) {
          AddOrEditWelcomeMessage(groupInfo.groupProfile.description, addOrEditWelcomeMessage)
        }
        GroupPreferencesButton(openPreferences)
        if (members.filter { it.memberCurrent }.size <= SMALL_GROUPS_RCPS_MEM_LIMIT) {
          SendReceiptsOption(currentUser, sendReceipts, setSendReceipts)
        } else {
          SendReceiptsOptionDisabled()
        }

        WallpaperButton {
          ModalManager.end.showModal {
            val chat = remember { derivedStateOf { chatModel.chats.value.firstOrNull { it.id == chat.id } } }
            val c = chat.value
            if (c != null) {
              ChatWallpaperEditorModal(c)
            }
          }
        }
      }
      SectionTextFooter(stringResource(MR.strings.only_group_owners_can_change_prefs))
      SectionDividerSpaced(maxTopPadding = true)

      SectionView(title = String.format(generalGetString(MR.strings.group_info_section_title_num_members), members.count() + 1)) {
        if (groupInfo.canAddMembers) {
          if (groupLink == null) {
            CreateGroupLinkButton(manageGroupLink)
          } else {
            GroupLinkButton(manageGroupLink)
          }
          val onAddMembersClick = if (chat.chatInfo.incognito) ::cantInviteIncognitoAlert else addMembers
          val tint = if (chat.chatInfo.incognito) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
          AddMembersButton(tint, onAddMembersClick)
        }
        if (members.size > 8) {
          SectionItemView(padding = PaddingValues(start = 14.dp, end = DEFAULT_PADDING_HALF)) {
            SearchRowView(searchText)
          }
        }
        SectionItemView(minHeight = 54.dp) {
          MemberRow(groupInfo.membership, user = true)
        }
      }
    }
    items(filteredMembers.value) { member ->
      Divider()
      val showMenu = remember { mutableStateOf(false) }
      SectionItemViewLongClickable({ showMemberInfo(member) }, { showMenu.value = true }, minHeight = 54.dp) {
        DropDownMenuForMember(chat.remoteHostId, member, groupInfo, showMenu)
        MemberRow(member, onClick = { showMemberInfo(member) })
      }
    }
    item {
      SectionDividerSpaced(maxTopPadding = true, maxBottomPadding = false)
      SectionView {
        ClearChatButton(clearChat)
        if (groupInfo.canDelete) {
          DeleteGroupButton(deleteGroup)
        }
        if (groupInfo.membership.memberCurrent) {
          LeaveGroupButton(leaveGroup)
        }
      }

      if (developerTools) {
        SectionDividerSpaced()
        SectionView(title = stringResource(MR.strings.section_title_for_console)) {
          InfoRow(stringResource(MR.strings.info_row_local_name), groupInfo.localDisplayName)
          InfoRow(stringResource(MR.strings.info_row_database_id), groupInfo.apiId.toString())
        }
      }
      SectionBottomSpacer()
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
      textAlign = TextAlign.Center,
      maxLines = 4,
      overflow = TextOverflow.Ellipsis
    )
    if (cInfo.fullName != "" && cInfo.fullName != cInfo.displayName) {
      Text(
        cInfo.fullName, style = MaterialTheme.typography.h2,
        color = MaterialTheme.colors.onBackground,
        textAlign = TextAlign.Center,
        maxLines = 8,
        overflow = TextOverflow.Ellipsis
      )
    }
  }
}

@Composable
private fun GroupPreferencesButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_toggle_on),
    stringResource(MR.strings.group_preferences),
    click = onClick
  )
}

@Composable
private fun SendReceiptsOption(currentUser: User, state: State<SendReceipts>, onSelected: (SendReceipts) -> Unit) {
  val values = remember {
    mutableListOf(SendReceipts.Yes, SendReceipts.No, SendReceipts.UserDefault(currentUser.sendRcptsSmallGroups)).map { it to it.text }
  }
  ExposedDropDownSettingRow(
    generalGetString(MR.strings.send_receipts),
    values,
    state,
    icon = painterResource(MR.images.ic_double_check),
    enabled = remember { mutableStateOf(true) },
    onSelected = onSelected
  )
}

@Composable
fun SendReceiptsOptionDisabled() {
  SettingsActionItemWithContent(
    icon = painterResource(MR.images.ic_double_check),
    text = generalGetString(MR.strings.send_receipts),
    click = {
      AlertManager.shared.showAlertMsg(
        title = generalGetString(MR.strings.send_receipts_disabled_alert_title),
        text = String.format(generalGetString(MR.strings.send_receipts_disabled_alert_msg), SMALL_GROUPS_RCPS_MEM_LIMIT)
      )
    }
  ) {
    Text(generalGetString(MR.strings.send_receipts_disabled), color = MaterialTheme.colors.secondary)
  }
}

@Composable
private fun AddMembersButton(tint: Color = MaterialTheme.colors.primary, onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_add),
    stringResource(MR.strings.button_add_members),
    onClick,
    iconColor = tint,
    textColor = tint
  )
}

@Composable
private fun MemberRow(member: GroupMember, user: Boolean = false, onClick: (() -> Unit)? = null) {
  @Composable
  fun MemberInfo() {
    if (member.blocked) {
      Text(stringResource(MR.strings.member_info_member_blocked), color = MaterialTheme.colors.secondary)
    } else {
      val role = member.memberRole
      if (role in listOf(GroupMemberRole.Owner, GroupMemberRole.Admin, GroupMemberRole.Observer)) {
        Text(role.text, color = MaterialTheme.colors.secondary)
      }
    }
  }

  fun memberConnStatus(): String {
    return if (member.activeConn?.connDisabled == true) {
      generalGetString(MR.strings.member_info_member_disabled)
    } else if (member.activeConn?.connDisabled == true) {
      generalGetString(MR.strings.member_info_member_inactive)
    } else {
      member.memberStatus.shortText
    }
  }

  Row(
    Modifier.fillMaxWidth(),
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    Row(
      Modifier.weight(1f).padding(end = DEFAULT_PADDING),
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.spacedBy(4.dp)
    ) {
      ProfileImage(size = 46.dp, member.image)
      Spacer(Modifier.width(DEFAULT_PADDING_HALF))
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
        val statusDescr =
          if (user) String.format(generalGetString(MR.strings.group_info_member_you), member.memberStatus.shortText) else memberConnStatus()
        Text(
          statusDescr,
          color = MaterialTheme.colors.secondary,
          fontSize = 12.sp,
          maxLines = 1,
          overflow = TextOverflow.Ellipsis
        )
      }
    }
    MemberInfo()
  }
}

@Composable
private fun MemberVerifiedShield() {
  Icon(painterResource(MR.images.ic_verified_user), null, Modifier.padding(end = 3.dp).size(16.dp), tint = MaterialTheme.colors.secondary)
}

@Composable
private fun DropDownMenuForMember(rhId: Long?, member: GroupMember, groupInfo: GroupInfo, showMenu: MutableState<Boolean>) {
  if (groupInfo.membership.memberRole >= GroupMemberRole.Admin) {
    val canBlockForAll = member.canBlockForAll(groupInfo)
    val canRemove = member.canBeRemoved(groupInfo)
    if (canBlockForAll || canRemove) {
      DefaultDropdownMenu(showMenu) {
        if (canBlockForAll) {
          if (member.blockedByAdmin) {
            ItemAction(stringResource(MR.strings.unblock_for_all), painterResource(MR.images.ic_do_not_touch), onClick = {
              unblockForAllAlert(rhId, groupInfo, member)
              showMenu.value = false
            })
          } else {
            ItemAction(stringResource(MR.strings.block_for_all), painterResource(MR.images.ic_back_hand), color = MaterialTheme.colors.error, onClick = {
              blockForAllAlert(rhId, groupInfo, member)
              showMenu.value = false
            })
          }
        }
        if (canRemove) {
          ItemAction(stringResource(MR.strings.remove_member_button), painterResource(MR.images.ic_delete), color = MaterialTheme.colors.error, onClick = {
            removeMemberAlert(rhId, groupInfo, member)
            showMenu.value = false
          })
        }
      }
    }
  } else if (!member.blockedByAdmin) {
    DefaultDropdownMenu(showMenu) {
      if (member.memberSettings.showMessages) {
        ItemAction(stringResource(MR.strings.block_member_button), painterResource(MR.images.ic_back_hand), color = MaterialTheme.colors.error, onClick = {
          blockMemberAlert(rhId, groupInfo, member)
          showMenu.value = false
        })
      } else {
        ItemAction(stringResource(MR.strings.unblock_member_button), painterResource(MR.images.ic_do_not_touch), onClick = {
          unblockMemberAlert(rhId, groupInfo, member)
          showMenu.value = false
        })
      }
    }
  }
}

@Composable
private fun GroupLinkButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_link),
    stringResource(MR.strings.group_link),
    onClick,
    iconColor = MaterialTheme.colors.secondary
  )
}

@Composable
private fun CreateGroupLinkButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_add_link),
    stringResource(MR.strings.create_group_link),
    onClick,
    iconColor = MaterialTheme.colors.secondary
  )
}

@Composable
fun EditGroupProfileButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_edit),
    stringResource(MR.strings.button_edit_group_profile),
    onClick,
    iconColor = MaterialTheme.colors.secondary
  )
}

@Composable
private fun AddOrEditWelcomeMessage(welcomeMessage: String?, onClick: () -> Unit) {
  val text = if (welcomeMessage == null) {
    stringResource(MR.strings.button_add_welcome_message)
  } else {
    stringResource(MR.strings.button_welcome_message)
  }
  SettingsActionItem(
    painterResource(MR.images.ic_maps_ugc),
    text,
    onClick,
    iconColor = MaterialTheme.colors.secondary
  )
}

@Composable
private fun LeaveGroupButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_logout),
    stringResource(MR.strings.button_leave_group),
    onClick,
    iconColor = Color.Red,
    textColor = Color.Red
  )
}

@Composable
private fun DeleteGroupButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_delete),
    stringResource(MR.strings.button_delete_group),
    onClick,
    iconColor = Color.Red,
    textColor = Color.Red
  )
}

@Composable
private fun SearchRowView(
  searchText: MutableState<TextFieldValue> = rememberSaveable(stateSaver = TextFieldValue.Saver) { mutableStateOf(TextFieldValue()) }
) {
  Box(Modifier.width(36.dp), contentAlignment = Alignment.Center) {
    Icon(painterResource(MR.images.ic_search), stringResource(MR.strings.search_verb), tint = MaterialTheme.colors.secondary)
  }
  Spacer(Modifier.width(14.dp))
  SearchTextField(Modifier.fillMaxWidth(), searchText = searchText, alwaysVisible = true) {
    searchText.value = searchText.value.copy(it)
  }
}

@Preview
@Composable
fun PreviewGroupChatInfoLayout() {
  SimpleXTheme {
    GroupChatInfoLayout(
      chat = Chat(
        remoteHostId = null,
        chatInfo = ChatInfo.Direct.sampleData,
        chatItems = arrayListOf()
      ),
      groupInfo = GroupInfo.sampleData,
      User.sampleData,
      sendReceipts = remember { mutableStateOf(SendReceipts.Yes) },
      setSendReceipts = {},
      members = listOf(GroupMember.sampleData, GroupMember.sampleData, GroupMember.sampleData),
      developerTools = false,
      groupLink = null,
      addMembers = {}, showMemberInfo = {}, editGroupProfile = {}, addOrEditWelcomeMessage = {}, openPreferences = {}, deleteGroup = {}, clearChat = {}, leaveGroup = {}, manageGroupLink = {}, onSearchClicked = {},
    )
  }
}
