package chat.simplex.common.views.chat.group

import InfoRow
import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionItemViewLongClickable
import SectionSpacer
import SectionTextFooter
import SectionView
import androidx.compose.animation.*
import androidx.compose.animation.core.animateDpAsState
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.text.AnnotatedString
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.common.model.GroupInfo
import chat.simplex.common.platform.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.chat.item.*
import chat.simplex.common.views.chatlist.*
import chat.simplex.common.views.database.TtlOptions
import chat.simplex.res.MR
import dev.icerock.moko.resources.StringResource
import kotlinx.coroutines.*

const val SMALL_GROUPS_RCPS_MEM_LIMIT: Int = 20
val MEMBER_ROW_AVATAR_SIZE = 42.dp
val MEMBER_ROW_VERTICAL_PADDING = 8.dp

@Composable
fun ModalData.GroupChatInfoView(
  chatsCtx: ChatModel.ChatsContext,
  rhId: Long?,
  chatId: String,
  groupLink: CreatedConnLink?,
  groupLinkMemberRole: GroupMemberRole?,
  selectedItems: MutableState<Set<Long>?>,
  appBar: MutableState<@Composable (BoxScope.() -> Unit)?>,
  scrollToItemId: MutableState<Long?>,
  onGroupLinkUpdated: (Pair<CreatedConnLink, GroupMemberRole>?) -> Unit,
  close: () -> Unit,
  onSearchClicked: () -> Unit
) {
  BackHandler(onBack = close)
  // TODO derivedStateOf?
  val chat = chatModel.chats.value.firstOrNull { ch -> ch.id == chatId && ch.remoteHostId == rhId }
  val currentUser = chatModel.currentUser.value
  val developerTools = chatModel.controller.appPrefs.developerTools.get()
  if (chat != null && chat.chatInfo is ChatInfo.Group && currentUser != null) {
    val groupInfo = chat.chatInfo.groupInfo
    val sendReceipts = remember { mutableStateOf(SendReceipts.fromBool(groupInfo.chatSettings.sendRcpts, currentUser.sendRcptsSmallGroups)) }
    val chatItemTTL = remember(groupInfo.id) { mutableStateOf(if (groupInfo.chatItemTTL != null) ChatItemTTL.fromSeconds(groupInfo.chatItemTTL) else null) }
    val deletingItems = rememberSaveable(groupInfo.id) { mutableStateOf(false) }
    val scope = rememberCoroutineScope()

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
      chatItemTTL = chatItemTTL,
      setChatItemTTL = {
        if (it == chatItemTTL.value) {
          return@GroupChatInfoLayout
        }
        val previousChatTTL = chatItemTTL.value
        chatItemTTL.value = it

        setChatTTLAlert(chatsCtx, chat.remoteHostId, chat.chatInfo, chatItemTTL, previousChatTTL, deletingItems)
      },
      activeSortedMembers = remember { chatModel.groupMembers }.value
        .filter { it.memberStatus != GroupMemberStatus.MemLeft && it.memberStatus != GroupMemberStatus.MemRemoved }
        .sortedByDescending { it.memberRole },
      developerTools,
      onLocalAliasChanged = { setGroupAlias(chat, it, chatModel) },
      groupLink,
      selectedItems,
      appBar,
      scrollToItemId,
      addMembers = {
        scope.launch(Dispatchers.Default) {
          setGroupMembers(rhId, groupInfo, chatModel)
          if (!isActive) return@launch

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
      onSearchClicked = onSearchClicked,
      deletingItems = deletingItems
    )
  }
}

fun deleteGroupDialog(chat: Chat, groupInfo: GroupInfo, chatModel: ChatModel, close: (() -> Unit)? = null) {
  val chatInfo = chat.chatInfo
  val titleId = if (groupInfo.businessChat == null) MR.strings.delete_group_question else MR.strings.delete_chat_question
  val messageId =
    if (groupInfo.businessChat == null) {
      if (groupInfo.membership.memberCurrent) MR.strings.delete_group_for_all_members_cannot_undo_warning
      else MR.strings.delete_group_for_self_cannot_undo_warning
    } else {
      if (groupInfo.membership.memberCurrent) MR.strings.delete_chat_for_all_members_cannot_undo_warning
      else MR.strings.delete_chat_for_self_cannot_undo_warning
    }
  AlertManager.shared.showAlertDialog(
    title = generalGetString(titleId),
    text = generalGetString(messageId),
    confirmText = generalGetString(MR.strings.delete_verb),
    onConfirm = {
      withBGApi {
        val r = chatModel.controller.apiDeleteChat(chat.remoteHostId, chatInfo.chatType, chatInfo.apiId)
        if (r) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.removeChat(chat.remoteHostId, chatInfo.id)
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
  val titleId = if (groupInfo.businessChat == null) MR.strings.leave_group_question else MR.strings.leave_chat_question
  val messageId = if (groupInfo.businessChat == null)
    MR.strings.you_will_stop_receiving_messages_from_this_group_chat_history_will_be_preserved
  else
    MR.strings.you_will_stop_receiving_messages_from_this_chat_chat_history_will_be_preserved
  AlertManager.shared.showAlertDialog(
    title = generalGetString(titleId),
    text = generalGetString(messageId),
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
  val messageId = if (groupInfo.businessChat == null)
    MR.strings.member_will_be_removed_from_group_cannot_be_undone
  else
    MR.strings.member_will_be_removed_from_chat_cannot_be_undone
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.button_remove_member_question),
    text = generalGetString(messageId),
    confirmText = generalGetString(MR.strings.remove_member_confirmation),
    onConfirm = {
      removeMembers(rhId, groupInfo, listOf(mem.groupMemberId))
    },
    destructive = true,
  )
}

private fun removeMembersAlert(rhId: Long?, groupInfo: GroupInfo, memberIds: List<Long>, onSuccess: () -> Unit = {}) {
  val messageId = if (groupInfo.businessChat == null)
    MR.strings.members_will_be_removed_from_group_cannot_be_undone
  else
    MR.strings.members_will_be_removed_from_chat_cannot_be_undone
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.button_remove_members_question),
    text = generalGetString(messageId),
    confirmText = generalGetString(MR.strings.remove_member_confirmation),
    onConfirm = {
      removeMembers(rhId, groupInfo, memberIds, onSuccess)
    },
    destructive = true,
  )
}

@Composable
fun SearchButton(
  modifier: Modifier,
  chat: Chat,
  group: GroupInfo,
  close: () -> Unit,
  onSearchClicked: () -> Unit
) {
  val disabled = !group.ready || chat.chatItems.isEmpty()

  InfoViewActionButton(
    modifier = modifier,
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
fun MuteButton(
  modifier: Modifier,
  chat: Chat,
  groupInfo: GroupInfo
) {
  val notificationMode = remember { mutableStateOf(groupInfo.chatSettings.enableNtfs) }
  val nextNotificationMode by remember { derivedStateOf { notificationMode.value.nextMode(true) } }

  InfoViewActionButton(
    modifier = modifier,
    icon =  painterResource(nextNotificationMode.icon),
    title = generalGetString(nextNotificationMode.text(true)),
    disabled = !groupInfo.ready,
    disabledLook = !groupInfo.ready,
    onClick = {
      toggleNotifications(chat.remoteHostId, chat.chatInfo, nextNotificationMode, chatModel, notificationMode)
    }
  )
}

@Composable
fun AddGroupMembersButton(
  modifier: Modifier,
  chat: Chat,
  groupInfo: GroupInfo
) {
  InfoViewActionButton(
    modifier = modifier,
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
fun ModalData.GroupChatInfoLayout(
  chat: Chat,
  groupInfo: GroupInfo,
  currentUser: User,
  sendReceipts: State<SendReceipts>,
  setSendReceipts: (SendReceipts) -> Unit,
  chatItemTTL: MutableState<ChatItemTTL?>,
  setChatItemTTL: (ChatItemTTL?) -> Unit,
  activeSortedMembers: List<GroupMember>,
  developerTools: Boolean,
  onLocalAliasChanged: (String) -> Unit,
  groupLink: CreatedConnLink?,
  selectedItems: MutableState<Set<Long>?>,
  appBar: MutableState<@Composable (BoxScope.() -> Unit)?>,
  scrollToItemId: MutableState<Long?>,
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
  onSearchClicked: () -> Unit,
  deletingItems: State<Boolean>
) {
  val listState = remember { appBarHandler.listState }
  val scope = rememberCoroutineScope()
  KeyChangeEffect(chat.id) {
    scope.launch { listState.scrollToItem(0) }
  }
  val searchText = remember { stateGetOrPut("searchText") { TextFieldValue() } }
  val filteredMembers = remember(activeSortedMembers) {
    derivedStateOf {
      val s = searchText.value.text.trim().lowercase()
      if (s.isEmpty()) activeSortedMembers else activeSortedMembers.filter { m -> m.anyNameContains(s) }
    }
  }
  Box {
    val oneHandUI = remember { appPrefs.oneHandUI.state }
    val selectedItemsBarHeight = if (selectedItems.value != null) AppBarHeight * fontSizeSqrtMultiplier else 0.dp
    val navBarPadding = WindowInsets.navigationBars.asPaddingValues().calculateBottomPadding()
    val imePadding = WindowInsets.ime.asPaddingValues().calculateBottomPadding()
  LazyColumnWithScrollBar(
    state = listState,
    contentPadding = if (oneHandUI.value) {
      PaddingValues(
        top = WindowInsets.statusBars.asPaddingValues().calculateTopPadding() + DEFAULT_PADDING + 5.dp,
        bottom = navBarPadding +
            imePadding +
            selectedItemsBarHeight +
            // TODO: that's workaround but works. Actually, something in the codebase doesn't consume padding for AppBar and it produce
            // different padding when the user has NavigationBar and doesn't have it with ime shown (developer options helps to test it nav bars)
            (if (navBarPadding > 0.dp && imePadding > 0.dp) 0.dp else AppBarHeight * fontSizeSqrtMultiplier)
      )
    } else {
      PaddingValues(
        top = topPaddingToContent(false),
        bottom = if (imePadding > 0.dp) {
          imePadding + selectedItemsBarHeight
        } else {
          navBarPadding + selectedItemsBarHeight
        }
      )
    }
  ) {
    item {
      Row(
        Modifier.fillMaxWidth(),
        horizontalArrangement = Arrangement.Center
      ) {
        GroupChatInfoHeader(chat.chatInfo, groupInfo)
      }

      LocalAliasEditor(chat.id, groupInfo.localAlias, isContact = false, updateValue = onLocalAliasChanged)

      SectionSpacer()

      Box(
        Modifier.fillMaxWidth(),
        contentAlignment = Alignment.Center
      ) {
        Row(
          Modifier
            .widthIn(max = if (groupInfo.canAddMembers) 320.dp else 230.dp)
            .padding(horizontal = DEFAULT_PADDING),
          horizontalArrangement = Arrangement.SpaceEvenly,
          verticalAlignment = Alignment.CenterVertically
        ) {
          if (groupInfo.canAddMembers) {
            SearchButton(modifier = Modifier.fillMaxWidth(0.33f), chat, groupInfo, close, onSearchClicked)
            AddGroupMembersButton(modifier = Modifier.fillMaxWidth(0.5f), chat, groupInfo)
            MuteButton(modifier = Modifier.fillMaxWidth(1f), chat, groupInfo)
          } else {
            SearchButton(modifier = Modifier.fillMaxWidth(0.5f), chat, groupInfo, close, onSearchClicked)
            MuteButton(modifier = Modifier.fillMaxWidth(1f), chat, groupInfo)
          }
        }
      }

      SectionSpacer()

      SectionView {
        if (groupInfo.isOwner && groupInfo.businessChat?.chatType == null) {
          EditGroupProfileButton(editGroupProfile)
        }
        if (groupInfo.groupProfile.description != null || (groupInfo.isOwner && groupInfo.businessChat?.chatType == null)) {
          AddOrEditWelcomeMessage(groupInfo.groupProfile.description, addOrEditWelcomeMessage)
        }
        val prefsTitleId = if (groupInfo.businessChat == null) MR.strings.group_preferences else MR.strings.chat_preferences
        GroupPreferencesButton(prefsTitleId, openPreferences)
        if (groupInfo.canModerate) {
          GroupReportsButton {
            scope.launch {
              showGroupReportsView(chatModel.chatId, scrollToItemId, chat.chatInfo)
            }
          }
        }
        if (activeSortedMembers.filter { it.memberCurrent }.size <= SMALL_GROUPS_RCPS_MEM_LIMIT) {
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
      val footerId = if (groupInfo.businessChat == null) MR.strings.only_group_owners_can_change_prefs else MR.strings.only_chat_owners_can_change_prefs
      SectionTextFooter(stringResource(footerId))
      SectionDividerSpaced(maxTopPadding = true, maxBottomPadding = false)

      ChatTTLSection(chatItemTTL, setChatItemTTL, deletingItems)
      SectionDividerSpaced(maxTopPadding = true, maxBottomPadding = true)

      SectionView(title = String.format(generalGetString(MR.strings.group_info_section_title_num_members), activeSortedMembers.count() + 1)) {
        if (groupInfo.canAddMembers) {
          if (groupInfo.businessChat == null) {
            if (groupLink == null) {
              CreateGroupLinkButton(manageGroupLink)
            } else {
              GroupLinkButton(manageGroupLink)
            }
          }
          val onAddMembersClick = if (chat.chatInfo.incognito) ::cantInviteIncognitoAlert else addMembers
          val tint = if (chat.chatInfo.incognito) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
          val addMembersTitleId = when (groupInfo.businessChat?.chatType) {
            BusinessChatType.Customer -> MR.strings.button_add_team_members
            BusinessChatType.Business -> MR.strings.button_add_friends
            null -> MR.strings.button_add_members
          }
          AddMembersButton(addMembersTitleId, tint, onAddMembersClick)
        }
        if (activeSortedMembers.size > 8) {
          SectionItemView(padding = PaddingValues(start = 14.dp, end = DEFAULT_PADDING_HALF)) {
            SearchRowView(searchText)
          }
        }
        SectionItemView(minHeight = 54.dp, padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
          MemberRow(groupInfo.membership, user = true)
        }
      }
    }
    items(filteredMembers.value, key = { it.groupMemberId }) { member ->
      Divider()
      val showMenu = remember { mutableStateOf(false) }
      val canBeSelected = groupInfo.membership.memberRole >= member.memberRole && member.memberRole < GroupMemberRole.Moderator
      SectionItemViewLongClickable(
        click = {
          if (selectedItems.value != null) {
            if (canBeSelected) {
              toggleItemSelection(member.groupMemberId, selectedItems)
            }
          } else {
            showMemberInfo(member)
          }
        },
        longClick = { showMenu.value = true },
        minHeight = 54.dp,
        padding = PaddingValues(horizontal = DEFAULT_PADDING)
      ) {
        Box(contentAlignment = Alignment.CenterStart) {
          androidx.compose.animation.AnimatedVisibility(selectedItems.value != null, enter = fadeIn(), exit = fadeOut()) {
            SelectedListItem(Modifier.alpha(if (canBeSelected) 1f else 0f).padding(start = 2.dp), member.groupMemberId, selectedItems)
          }
          val selectionOffset by animateDpAsState(if (selectedItems.value != null) 20.dp + 22.dp * fontSizeMultiplier else 0.dp)
          DropDownMenuForMember(chat.remoteHostId, member, groupInfo, selectedItems, showMenu)
          Box(Modifier.padding(start = selectionOffset)) {
            MemberRow(member)
          }
        }
      }
    }
    item {
      SectionDividerSpaced(maxTopPadding = true, maxBottomPadding = false)
      SectionView {
        ClearChatButton(clearChat)
        if (groupInfo.canDelete) {
          val titleId = if (groupInfo.businessChat == null) MR.strings.button_delete_group else MR.strings.button_delete_chat
          DeleteGroupButton(titleId, deleteGroup)
        }
        if (groupInfo.membership.memberCurrent) {
          val titleId = if (groupInfo.businessChat == null) MR.strings.button_leave_group else MR.strings.button_leave_chat
          LeaveGroupButton(titleId, leaveGroup)
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
    if (!oneHandUI.value) {
      NavigationBarBackground(oneHandUI.value, oneHandUI.value)
    }
    SelectedItemsButtonsToolbar(chat, groupInfo, selectedItems, rememberUpdatedState(activeSortedMembers))
    SelectedItemsCounterToolbarSetter(groupInfo, selectedItems, filteredMembers, appBar)
  }
}

@Composable
private fun BoxScope.SelectedItemsButtonsToolbar(chat: Chat, groupInfo: GroupInfo, selectedItems: MutableState<Set<Long>?>, activeMembers: State<List<GroupMember>>) {
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  Column(Modifier.align(Alignment.BottomCenter)) {
    AnimatedVisibility(selectedItems.value != null) {
      SelectedItemsMembersToolbar(
        selectedItems = selectedItems,
        activeMembers = activeMembers,
        groupInfo = groupInfo,
        delete = {
          removeMembersAlert(chat.remoteHostId, groupInfo, selectedItems.value!!.sorted()) {
            selectedItems.value = null
          }
        },
        blockForAll = { block ->
          if (block) {
            blockForAllAlert(chat.remoteHostId, groupInfo, selectedItems.value!!.sorted()) {
              selectedItems.value = null
            }
          } else {
            unblockForAllAlert(chat.remoteHostId, groupInfo, selectedItems.value!!.sorted()) {
              selectedItems.value = null
            }
          }
        },
        changeRole = { toRole ->
          updateMembersRoleDialog(toRole, groupInfo) {
            updateMembersRole(toRole, chat.remoteHostId, groupInfo, selectedItems.value!!.sorted()) {
              selectedItems.value = null
            }
          }
        }
      )
    }
    if (oneHandUI.value) {
      // That's placeholder to take some space for bottom app bar in oneHandUI
      Box(Modifier.height(AppBarHeight * fontSizeSqrtMultiplier))
    }
  }
}

@Composable
private fun SelectedItemsCounterToolbarSetter(
  groupInfo: GroupInfo,
  selectedItems: MutableState<Set<Long>?>,
  filteredMembers: State<List<GroupMember>>,
  appBar: MutableState<@Composable (BoxScope.() -> Unit)?>
) {
  LaunchedEffect(
    groupInfo,
    /* variable, not value - intentionally - to reduce work but handle variable change because it changes in remember(members) { derivedState {} } */
    filteredMembers
  ) {
    snapshotFlow { selectedItems.value == null }
      .collect { nullItems ->
        if (!nullItems) {
          appBar.value = {
            SelectedItemsCounterToolbar(selectedItems, !remember { appPrefs.oneHandUI.state }.value) {
              if (!groupInfo.membership.memberActive) return@SelectedItemsCounterToolbar
              val ids: MutableSet<Long> = mutableSetOf()
              for (mem in filteredMembers.value) {
                if (groupInfo.membership.memberActive && groupInfo.membership.memberRole >= mem.memberRole && mem.memberRole < GroupMemberRole.Moderator) {
                  ids.add(mem.groupMemberId)
                }
              }
              if (ids.isNotEmpty() && (selectedItems.value ?: setOf()).containsAll(ids)) {
                selectedItems.value = (selectedItems.value ?: setOf()).minus(ids)
              } else {
                selectedItems.value = (selectedItems.value ?: setOf()).union(ids)
              }
            }
          }
        } else {
          appBar.value = null
        }
      }
  }
}

@Composable
fun ChatTTLSection(chatItemTTL: State<ChatItemTTL?>, setChatItemTTL: (ChatItemTTL?) -> Unit, deletingItems: State<Boolean>) {
  Box {
    SectionView {
      TtlOptions(
        chatItemTTL,
        enabled = remember { derivedStateOf { !deletingItems.value } },
        onSelected = setChatItemTTL,
        default = chatModel.chatItemTTL
      )
      SectionTextFooter(stringResource(MR.strings.chat_ttl_options_footer))
    }
    if (deletingItems.value) {
      Box(Modifier.matchParentSize()) {
        ProgressIndicator()
      }
    }
  }
}

@Composable
private fun GroupChatInfoHeader(cInfo: ChatInfo, groupInfo: GroupInfo) {
  Column(
    Modifier.padding(horizontal = 8.dp),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    ChatInfoImage(cInfo, size = 192.dp, iconColor = if (isInDarkTheme()) GroupDark else SettingsSecondaryLight)
    val clipboard = LocalClipboardManager.current
    val copyNameToClipboard = {
      clipboard.setText(AnnotatedString(groupInfo.groupProfile.displayName))
      showToast(generalGetString(MR.strings.copied))
    }
    Text(
      groupInfo.groupProfile.displayName, style = MaterialTheme.typography.h1.copy(fontWeight = FontWeight.Normal),
      color = MaterialTheme.colors.onBackground,
      textAlign = TextAlign.Center,
      maxLines = 4,
      overflow = TextOverflow.Ellipsis,
      modifier = Modifier.combinedClickable(onClick = copyNameToClipboard, onLongClick = copyNameToClipboard).onRightClick(copyNameToClipboard)
    )
    if (cInfo.fullName != "" && cInfo.fullName != cInfo.displayName && cInfo.fullName != groupInfo.groupProfile.displayName) {
      Text(
        cInfo.fullName, style = MaterialTheme.typography.h2,
        color = MaterialTheme.colors.onBackground,
        textAlign = TextAlign.Center,
        maxLines = 8,
        overflow = TextOverflow.Ellipsis,
        modifier = Modifier.combinedClickable(onClick = copyNameToClipboard, onLongClick = copyNameToClipboard).onRightClick(copyNameToClipboard)
      )
    }
  }
}

@Composable
private fun GroupPreferencesButton(titleId: StringResource, onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_toggle_on),
    stringResource(titleId),
    click = onClick
  )
}

@Composable
private fun GroupReportsButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_flag),
    stringResource(MR.strings.group_reports_member_reports),
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
private fun AddMembersButton(titleId: StringResource, tint: Color = MaterialTheme.colors.primary, onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_add),
    stringResource(titleId),
    onClick,
    iconColor = tint,
    textColor = tint
  )
}

@Composable
fun MemberRow(member: GroupMember, user: Boolean = false, infoPage: Boolean = true, showlocalAliasAndFullName: Boolean = false, selected: Boolean = false) {
  @Composable
  fun MemberInfo() {
    if (member.blocked) {
      Text(stringResource(MR.strings.member_info_member_blocked), color = MaterialTheme.colors.secondary)
    } else {
      val role = member.memberRole
      if (role in listOf(GroupMemberRole.Owner, GroupMemberRole.Admin, GroupMemberRole.Moderator, GroupMemberRole.Observer)) {
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
      Modifier.weight(1f).padding(top = MEMBER_ROW_VERTICAL_PADDING, end = DEFAULT_PADDING, bottom = MEMBER_ROW_VERTICAL_PADDING),
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.spacedBy(4.dp)
    ) {
      MemberProfileImage(size = MEMBER_ROW_AVATAR_SIZE, member)
      Spacer(Modifier.width(DEFAULT_PADDING_HALF))
      Column {
        Row(verticalAlignment = Alignment.CenterVertically) {
          if (member.verified) {
            MemberVerifiedShield()
          }
          Text(
            if (showlocalAliasAndFullName) member.localAliasAndFullName else member.chatViewName, maxLines = 1, overflow = TextOverflow.Ellipsis,
            color = if (member.memberIncognito) Indigo else Color.Unspecified
          )
        }

        if (infoPage) {
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
    }
    if (infoPage) {
      MemberInfo()
    }
    if (selected) {
      Icon(
        painterResource(
          MR.images.ic_check
        ),
        null,
        Modifier.size(20.dp),
        tint = MaterialTheme.colors.primary,
      )
    }
  }
}

@Composable
private fun MemberVerifiedShield() {
  Icon(painterResource(MR.images.ic_verified_user), null, Modifier.padding(end = 3.dp).size(16.dp), tint = MaterialTheme.colors.secondary)
}

@Composable
private fun DropDownMenuForMember(rhId: Long?, member: GroupMember, groupInfo: GroupInfo, selectedItems: MutableState<Set<Long>?>, showMenu: MutableState<Boolean>) {
  if (groupInfo.membership.memberRole >= GroupMemberRole.Moderator) {
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
        if (selectedItems.value == null && member.memberRole < GroupMemberRole.Moderator) {
          Divider()
          SelectItemAction(showMenu) { toggleItemSelection(member.groupMemberId, selectedItems) }
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
private fun LeaveGroupButton(titleId: StringResource, onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_logout),
    stringResource(titleId),
    onClick,
    iconColor = Color.Red,
    textColor = Color.Red
  )
}

@Composable
private fun DeleteGroupButton(titleId: StringResource, onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_delete),
    stringResource(titleId),
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

private fun setGroupAlias(chat: Chat, localAlias: String, chatModel: ChatModel) = withBGApi {
  val chatRh = chat.remoteHostId
  chatModel.controller.apiSetGroupAlias(chatRh, chat.chatInfo.apiId, localAlias)?.let {
    withContext(Dispatchers.Main) {
      chatModel.chatsContext.updateGroup(chatRh, it)
    }
  }
}

fun removeMembers(rhId: Long?, groupInfo: GroupInfo, memberIds: List<Long>, onSuccess: () -> Unit = {}) {
  withBGApi {
    val updatedMembers = chatModel.controller.apiRemoveMembers(rhId, groupInfo.groupId, memberIds)
    if (updatedMembers != null) {
      withContext(Dispatchers.Main) {
        updatedMembers.forEach { updatedMember ->
          chatModel.chatsContext.upsertGroupMember(rhId, groupInfo, updatedMember)
        }
      }
      withContext(Dispatchers.Main) {
        updatedMembers.forEach { updatedMember ->
          chatModel.secondaryChatsContext.value?.upsertGroupMember(rhId, groupInfo, updatedMember)
        }
      }
      onSuccess()
    }
  }
}

fun <T> toggleItemSelection(itemId: T, selectedItems: MutableState<Set<T>?>) {
  val select = selectedItems.value?.contains(itemId) != true
  if (select) {
    val sel = selectedItems.value ?: setOf()
    selectedItems.value = sel + itemId
  } else {
    val sel = (selectedItems.value ?: setOf()).toMutableSet()
    sel.remove(itemId)
    selectedItems.value = sel
  }
}

@Preview
@Composable
fun PreviewGroupChatInfoLayout() {
  SimpleXTheme {
    ModalData().GroupChatInfoLayout(
      chat = Chat(
        remoteHostId = null,
        chatInfo = ChatInfo.Direct.sampleData,
        chatItems = arrayListOf()
      ),
      groupInfo = GroupInfo.sampleData,
      User.sampleData,
      sendReceipts = remember { mutableStateOf(SendReceipts.Yes) },
      setSendReceipts = {},
      chatItemTTL = remember { mutableStateOf(ChatItemTTL.fromSeconds(0)) },
      setChatItemTTL = {},
      activeSortedMembers = listOf(GroupMember.sampleData, GroupMember.sampleData, GroupMember.sampleData),
      developerTools = false,
      onLocalAliasChanged = {},
      groupLink = null,
      selectedItems = remember { mutableStateOf(null) },
      appBar = remember { mutableStateOf(null) },
      scrollToItemId = remember { mutableStateOf(null) },
      addMembers = {}, showMemberInfo = {}, editGroupProfile = {}, addOrEditWelcomeMessage = {}, openPreferences = {}, deleteGroup = {}, clearChat = {}, leaveGroup = {}, manageGroupLink = {}, onSearchClicked = {}, deletingItems = remember { mutableStateOf(true) }
    )
  }
}
