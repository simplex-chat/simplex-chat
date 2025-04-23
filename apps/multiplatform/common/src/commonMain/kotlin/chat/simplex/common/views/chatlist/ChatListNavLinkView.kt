package chat.simplex.common.views.chatlist

import SectionItemView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.platform.LocalDensity
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.*
import androidx.compose.ui.graphics.*
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.chat.group.*
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.contacts.onRequestAccepted
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.*
import chat.simplex.res.MR
import kotlinx.coroutines.*
import kotlinx.datetime.Clock

@Composable
fun ChatListNavLinkView(chat: Chat, nextChatSelected: State<Boolean>) {
  val showMenu = remember { mutableStateOf(false) }
  val showMarkRead = remember(chat.chatStats.unreadCount, chat.chatStats.unreadChat) {
    chat.chatStats.unreadCount > 0 || chat.chatStats.unreadChat
  }
  val disabled = chatModel.chatRunning.value == false || chatModel.deletedChats.value.contains(chat.remoteHostId to chat.chatInfo.id)
  val linkMode by remember { chatModel.controller.appPrefs.simplexLinkMode.state }
  LaunchedEffect(chat.id) {
    showMenu.value = false
    delay(500L)
  }
  val selectedChat = remember(chat.id) { derivedStateOf { chat.id == chatModel.chatId.value } }
  val showChatPreviews = chatModel.showChatPreviews.value
  val inProgress = remember { mutableStateOf(false) }
  var progressByTimeout by rememberSaveable { mutableStateOf(false) }

  LaunchedEffect(inProgress.value) {
    progressByTimeout = if (inProgress.value) {
      delay(1000)
      inProgress.value
    } else {
      false
    }
  }

  val scope = rememberCoroutineScope()

  when (chat.chatInfo) {
    is ChatInfo.Direct -> {
      val contactNetworkStatus = chatModel.contactNetworkStatus(chat.chatInfo.contact)
      val defaultClickAction = { if (chatModel.chatId.value != chat.id) scope.launch { directChatAction(chat.remoteHostId, chat.chatInfo.contact, chatModel) } }
      ChatListNavLinkLayout(
        chatLinkPreview = {
          tryOrShowError("${chat.id}ChatListNavLink", error = { ErrorChatListItem() }) {
            ChatPreviewView(chat, showChatPreviews, chatModel.draft.value, chatModel.draftChatId.value, chatModel.currentUser.value?.profile?.displayName, contactNetworkStatus, disabled, linkMode, inProgress = false, progressByTimeout = false, defaultClickAction)
          }
        },
        click = defaultClickAction,
        dropdownMenuItems = {
          tryOrShowError("${chat.id}ChatListNavLinkDropdown", error = {}) {
            ContactMenuItems(chat, chat.chatInfo.contact, chatModel, showMenu, showMarkRead)
          }
        },
        showMenu,
        disabled,
        selectedChat,
        nextChatSelected,
      )
    }
    is ChatInfo.Group -> {
      val defaultClickAction = { if (!inProgress.value && chatModel.chatId.value != chat.id) scope.launch { groupChatAction(chat.remoteHostId, chat.chatInfo.groupInfo, chatModel, inProgress) } }
      ChatListNavLinkLayout(
        chatLinkPreview = {
          tryOrShowError("${chat.id}ChatListNavLink", error = { ErrorChatListItem() }) {
            ChatPreviewView(chat, showChatPreviews, chatModel.draft.value, chatModel.draftChatId.value, chatModel.currentUser.value?.profile?.displayName, null, disabled, linkMode, inProgress.value, progressByTimeout, defaultClickAction)
          }
        },
        click = defaultClickAction,
        dropdownMenuItems = {
          tryOrShowError("${chat.id}ChatListNavLinkDropdown", error = {}) {
            GroupMenuItems(chat, chat.chatInfo.groupInfo, chatModel, showMenu, inProgress, showMarkRead)
          }
        },
        showMenu,
        disabled,
        selectedChat,
        nextChatSelected,
      )
    }
    is ChatInfo.Local -> {
      val defaultClickAction = { if (chatModel.chatId.value != chat.id) scope.launch { noteFolderChatAction(chat.remoteHostId, chat.chatInfo.noteFolder) } }
      ChatListNavLinkLayout(
        chatLinkPreview = {
          tryOrShowError("${chat.id}ChatListNavLink", error = { ErrorChatListItem() }) {
            ChatPreviewView(chat, showChatPreviews, chatModel.draft.value, chatModel.draftChatId.value, chatModel.currentUser.value?.profile?.displayName, null, disabled, linkMode, inProgress = false, progressByTimeout = false, defaultClickAction)
          }
        },
        click = defaultClickAction,
        dropdownMenuItems = {
          tryOrShowError("${chat.id}ChatListNavLinkDropdown", error = {}) {
            NoteFolderMenuItems(chat, showMenu, showMarkRead)
          }
        },
        showMenu,
        disabled,
        selectedChat,
        nextChatSelected,
      )
    }
    is ChatInfo.ContactRequest ->
      ChatListNavLinkLayout(
        chatLinkPreview = {
          tryOrShowError("${chat.id}ChatListNavLink", error = { ErrorChatListItem() }) {
            ContactRequestView(chat.chatInfo)
          }
        },
        click = { contactRequestAlertDialog(chat.remoteHostId, chat.chatInfo, chatModel) { onRequestAccepted(it) } },
        dropdownMenuItems = {
          tryOrShowError("${chat.id}ChatListNavLinkDropdown", error = {}) {
            ContactRequestMenuItems(chat.remoteHostId, chat.chatInfo, chatModel, showMenu)
          }
        },
        showMenu,
        disabled,
        selectedChat,
        nextChatSelected,
      )
    is ChatInfo.ContactConnection ->
      ChatListNavLinkLayout(
        chatLinkPreview = {
          tryOrShowError("${chat.id}ChatListNavLink", error = { ErrorChatListItem() }) {
            ContactConnectionView(chat.chatInfo.contactConnection)
          }
        },
        click = {
          chatModel.chatId.value = chat.id
        },
        dropdownMenuItems = {
          tryOrShowError("${chat.id}ChatListNavLinkDropdown", error = {}) {
            ContactConnectionMenuItems(chat.remoteHostId, chat.chatInfo, chatModel, showMenu)
          }
        },
        showMenu,
        disabled,
        selectedChat,
        nextChatSelected,
      )
    is ChatInfo.InvalidJSON ->
      ChatListNavLinkLayout(
        chatLinkPreview = {
          tryOrShowError("${chat.id}ChatListNavLink", error = { ErrorChatListItem() }) {
            InvalidDataView()
          }
        },
        click = {
          chatModel.chatId.value = chat.id
        },
        dropdownMenuItems = null,
        showMenu,
        disabled,
        selectedChat,
        nextChatSelected,
      )
  }
}

@Composable
fun ErrorChatListItem() {
  Box(Modifier.fillMaxWidth().padding(horizontal = 10.dp, vertical = 6.dp)) {
    Text(stringResource(MR.strings.error_showing_content), color = MaterialTheme.colors.error, fontStyle = FontStyle.Italic)
  }
}

suspend fun directChatAction(rhId: Long?, contact: Contact, chatModel: ChatModel) {
  when {
    contact.activeConn == null && contact.profile.contactLink != null && contact.active -> askCurrentOrIncognitoProfileConnectContactViaAddress(chatModel, rhId, contact, close = null, openChat = true)
    else -> openDirectChat(rhId, contact.contactId)
  }
}

suspend fun groupChatAction(rhId: Long?, groupInfo: GroupInfo, chatModel: ChatModel, inProgress: MutableState<Boolean>? = null) {
  when (groupInfo.membership.memberStatus) {
    GroupMemberStatus.MemInvited -> acceptGroupInvitationAlertDialog(rhId, groupInfo, chatModel, inProgress)
    GroupMemberStatus.MemAccepted -> groupInvitationAcceptedAlert(rhId)
    else -> openGroupChat(rhId, groupInfo.groupId)
  }
}

suspend fun noteFolderChatAction(rhId: Long?, noteFolder: NoteFolder) = openChat(secondaryChatsCtx = null, rhId, ChatInfo.Local(noteFolder))

suspend fun openDirectChat(rhId: Long?, contactId: Long) = openChat(secondaryChatsCtx = null, rhId, ChatType.Direct, contactId)

suspend fun openGroupChat(rhId: Long?, groupId: Long) = openChat(secondaryChatsCtx = null, rhId, ChatType.Group, groupId)

suspend fun openChat(secondaryChatsCtx: ChatModel.ChatsContext?, rhId: Long?, chatInfo: ChatInfo) = openChat(secondaryChatsCtx, rhId, chatInfo.chatType, chatInfo.apiId)

suspend fun openChat(
  secondaryChatsCtx: ChatModel.ChatsContext?,
  rhId: Long?,
  chatType: ChatType,
  apiId: Long,
  openAroundItemId: Long? = null
) {
  if (secondaryChatsCtx != null) {
    chatModel.secondaryChatsContext.value = secondaryChatsCtx
  }
  apiLoadMessages(
    chatsCtx = secondaryChatsCtx ?: chatModel.chatsContext,
    rhId,
    chatType,
    apiId,
    if (openAroundItemId != null) {
      ChatPagination.Around(openAroundItemId, ChatPagination.INITIAL_COUNT)
    } else {
      ChatPagination.Initial(ChatPagination.INITIAL_COUNT)
    },
    "",
    openAroundItemId
  )
}

suspend fun openLoadedChat(chat: Chat) {
  withContext(Dispatchers.Main) {
    chatModel.chatsContext.chatItemStatuses.clear()
    chatModel.chatsContext.chatItems.replaceAll(chat.chatItems)
    chatModel.chatId.value = chat.chatInfo.id
    chatModel.chatsContext.chatState.clear()
  }
}

suspend fun apiFindMessages(chatsCtx: ChatModel.ChatsContext, ch: Chat, search: String) {
  withContext(Dispatchers.Main) {
    chatsCtx.chatItems.clearAndNotify()
  }
  apiLoadMessages(chatsCtx, ch.remoteHostId, ch.chatInfo.chatType, ch.chatInfo.apiId, pagination = if (search.isNotEmpty()) ChatPagination.Last(ChatPagination.INITIAL_COUNT) else ChatPagination.Initial(ChatPagination.INITIAL_COUNT), search = search)
}

suspend fun setGroupMembers(rhId: Long?, groupInfo: GroupInfo, chatModel: ChatModel) = coroutineScope {
  // groupMembers loading can take a long time and if the user already closed the screen, coroutine may be canceled
  val groupMembers = chatModel.controller.apiListMembers(rhId, groupInfo.groupId)
  val currentMembers = chatModel.groupMembers.value
  val newMembers = groupMembers.map { newMember ->
    val currentMember = currentMembers.find { it.id == newMember.id }
    val currentMemberStats = currentMember?.activeConn?.connectionStats
    val newMemberConn = newMember.activeConn
    if (currentMemberStats != null && newMemberConn != null && newMemberConn.connectionStats == null) {
      newMember.copy(activeConn = newMemberConn.copy(connectionStats = currentMemberStats))
    } else {
      newMember
    }
  }
  chatModel.groupMembersIndexes.value = emptyMap()
  chatModel.groupMembers.value = newMembers
  chatModel.membersLoaded.value = true
  chatModel.populateGroupMembersIndexes()
}

@Composable
fun ContactMenuItems(chat: Chat, contact: Contact, chatModel: ChatModel, showMenu: MutableState<Boolean>, showMarkRead: Boolean) {
  if (contact.activeConn != null) {
    if (showMarkRead) {
      MarkReadChatAction(chat, showMenu)
    } else {
      MarkUnreadChatAction(chat, chatModel, showMenu)
    }
    ToggleFavoritesChatAction(chat, chatModel, chat.chatInfo.chatSettings?.favorite == true, showMenu)
    ToggleNotificationsChatAction(chat, chatModel, contact.chatSettings.enableNtfs.nextMode(false), showMenu)
    TagListAction(chat, showMenu)
    ClearChatAction(chat, showMenu)
  }
  DeleteContactAction(chat, chatModel, showMenu)
}

@Composable
fun GroupMenuItems(
  chat: Chat,
  groupInfo: GroupInfo,
  chatModel: ChatModel,
  showMenu: MutableState<Boolean>,
  inProgress: MutableState<Boolean>,
  showMarkRead: Boolean
) {
  when (groupInfo.membership.memberStatus) {
    GroupMemberStatus.MemInvited -> {
      if (!inProgress.value) {
        JoinGroupAction(chat, groupInfo, chatModel, showMenu, inProgress)
      }
      if (groupInfo.canDelete) {
        DeleteGroupAction(chat, groupInfo, chatModel, showMenu)
      }
    }
    GroupMemberStatus.MemAccepted -> {
      if (groupInfo.membership.memberCurrent) {
        LeaveGroupAction(chat.remoteHostId, groupInfo, chatModel, showMenu)
      }
      if (groupInfo.canDelete) {
        DeleteGroupAction(chat, groupInfo, chatModel, showMenu)
      }
    }
    else -> {
      if (showMarkRead) {
        MarkReadChatAction(chat, showMenu)
      } else {
        MarkUnreadChatAction(chat, chatModel, showMenu)
      }
      ToggleFavoritesChatAction(chat, chatModel, chat.chatInfo.chatSettings?.favorite == true, showMenu)
      ToggleNotificationsChatAction(chat, chatModel, groupInfo.chatSettings.enableNtfs.nextMode(true), showMenu)
      TagListAction(chat, showMenu)
      if (chat.chatStats.reportsCount > 0 && groupInfo.membership.memberRole >= GroupMemberRole.Moderator) {
        ArchiveAllReportsItemAction(showMenu) {
          archiveAllReportsForMe(chat.remoteHostId, chat.chatInfo.apiId)
        }
      }
      ClearChatAction(chat, showMenu)
      if (groupInfo.membership.memberCurrent) {
        LeaveGroupAction(chat.remoteHostId, groupInfo, chatModel, showMenu)
      }
      if (groupInfo.canDelete) {
        DeleteGroupAction(chat, groupInfo, chatModel, showMenu)
      }
    }
  }
}

@Composable
fun NoteFolderMenuItems(chat: Chat, showMenu: MutableState<Boolean>, showMarkRead: Boolean) {
  if (showMarkRead) {
    MarkReadChatAction(chat, showMenu)
  } else {
    MarkUnreadChatAction(chat, chatModel, showMenu)
  }
  ClearNoteFolderAction(chat, showMenu)
}

@Composable
fun MarkReadChatAction(chat: Chat, showMenu: MutableState<Boolean>) {
  ItemAction(
    stringResource(MR.strings.mark_read),
    painterResource(MR.images.ic_check),
    onClick = {
      markChatRead(chat)
      ntfManager.cancelNotificationsForChat(chat.id)
      showMenu.value = false
    }
  )
}

@Composable
fun MarkUnreadChatAction(chat: Chat, chatModel: ChatModel, showMenu: MutableState<Boolean>) {
  ItemAction(
    stringResource(MR.strings.mark_unread),
    painterResource(MR.images.ic_mark_chat_unread),
    onClick = {
      markChatUnread(chat, chatModel)
      showMenu.value = false
    }
  )
}

@Composable
fun TagListAction(
  chat: Chat,
  showMenu: MutableState<Boolean>
) {
  val userTags = remember { chatModel.userTags }
  ItemAction(
    stringResource(if (chat.chatInfo.chatTags.isNullOrEmpty()) MR.strings.add_to_list else MR.strings.change_list),
    painterResource(MR.images.ic_label),
    onClick = {
      ModalManager.start.showModalCloseable { close ->
        if (userTags.value.isEmpty()) {
          TagListEditor(rhId = chat.remoteHostId, chat = chat, close = close)
        } else {
          TagListView(rhId = chat.remoteHostId, chat = chat, close = close, reorderMode = false)
        }
      }
      showMenu.value = false
    }
  )
}

@Composable
fun ToggleFavoritesChatAction(chat: Chat, chatModel: ChatModel, favorite: Boolean, showMenu: MutableState<Boolean>) {
  ItemAction(
    if (favorite) stringResource(MR.strings.unfavorite_chat) else stringResource(MR.strings.favorite_chat),
    if (favorite) painterResource(MR.images.ic_star_off) else painterResource(MR.images.ic_star),
    onClick = {
      toggleChatFavorite(chat.remoteHostId, chat.chatInfo, !favorite, chatModel)
      showMenu.value = false
    }
  )
}

@Composable
fun ToggleNotificationsChatAction(chat: Chat, chatModel: ChatModel, nextMsgFilter: MsgFilter, showMenu: MutableState<Boolean>) {
  ItemAction(
    generalGetString(nextMsgFilter.text(chat.chatInfo.hasMentions)),
    painterResource(nextMsgFilter.icon),
    onClick = {
      toggleNotifications(chat.remoteHostId, chat.chatInfo, nextMsgFilter, chatModel)
      showMenu.value = false
    }
  )
}

@Composable
fun ClearChatAction(chat: Chat, showMenu: MutableState<Boolean>) {
  ItemAction(
    stringResource(MR.strings.clear_chat_menu_action),
    painterResource(MR.images.ic_settings_backup_restore),
    onClick = {
      clearChatDialog(chat)
      showMenu.value = false
    },
    color = WarningOrange
  )
}

@Composable
fun ClearNoteFolderAction(chat: Chat, showMenu: MutableState<Boolean>) {
  ItemAction(
    stringResource(MR.strings.clear_chat_menu_action),
    painterResource(MR.images.ic_settings_backup_restore),
    onClick = {
      clearNoteFolderDialog(chat)
      showMenu.value = false
    },
    color = WarningOrange
  )
}

@Composable
fun DeleteContactAction(chat: Chat, chatModel: ChatModel, showMenu: MutableState<Boolean>) {
  ItemAction(
    stringResource(MR.strings.delete_contact_menu_action),
    painterResource(MR.images.ic_delete),
    onClick = {
      deleteContactDialog(chat, chatModel)
      showMenu.value = false
    },
    color = Color.Red
  )
}

@Composable
fun DeleteGroupAction(chat: Chat, groupInfo: GroupInfo, chatModel: ChatModel, showMenu: MutableState<Boolean>) {
  ItemAction(
    stringResource(MR.strings.delete_group_menu_action),
    painterResource(MR.images.ic_delete),
    onClick = {
      deleteGroupDialog(chat, groupInfo, chatModel)
      showMenu.value = false
    },
    color = Color.Red
  )
}

@Composable
fun JoinGroupAction(
  chat: Chat,
  groupInfo: GroupInfo,
  chatModel: ChatModel,
  showMenu: MutableState<Boolean>,
  inProgress: MutableState<Boolean>
) {
  val joinGroup: () -> Unit = {
    withBGApi {
      inProgress.value = true
      chatModel.controller.apiJoinGroup(chat.remoteHostId, groupInfo.groupId)
      inProgress.value = false
    }
  }
  ItemAction(
    if (chat.chatInfo.incognito) stringResource(MR.strings.join_group_incognito_button) else stringResource(MR.strings.join_group_button),
    if (chat.chatInfo.incognito) painterResource(MR.images.ic_theater_comedy_filled) else painterResource(MR.images.ic_login),
    color = if (chat.chatInfo.incognito) Indigo else MaterialTheme.colors.onBackground,
    onClick = {
      joinGroup()
      showMenu.value = false
    }
  )
}

@Composable
fun LeaveGroupAction(rhId: Long?, groupInfo: GroupInfo, chatModel: ChatModel, showMenu: MutableState<Boolean>) {
  ItemAction(
    stringResource(MR.strings.leave_group_button),
    painterResource(MR.images.ic_logout),
    onClick = {
      leaveGroupDialog(rhId, groupInfo, chatModel)
      showMenu.value = false
    },
    color = Color.Red
  )
}

@Composable
fun ContactRequestMenuItems(rhId: Long?, chatInfo: ChatInfo.ContactRequest, chatModel: ChatModel, showMenu: MutableState<Boolean>, onSuccess: ((chat: Chat) -> Unit)? = null) {
  ItemAction(
    stringResource(MR.strings.accept_contact_button),
    painterResource(MR.images.ic_check),
    color = MaterialTheme.colors.onBackground,
    onClick = {
      acceptContactRequest(rhId, incognito = false, chatInfo.apiId, chatInfo, true, chatModel, onSuccess)
      showMenu.value = false
    }
  )
  ItemAction(
    stringResource(MR.strings.accept_contact_incognito_button),
    painterResource(MR.images.ic_theater_comedy),
    color = MaterialTheme.colors.onBackground,
    onClick = {
      acceptContactRequest(rhId, incognito = true, chatInfo.apiId, chatInfo, true, chatModel, onSuccess)
      showMenu.value = false
    }
  )
  ItemAction(
    stringResource(MR.strings.reject_contact_button),
    painterResource(MR.images.ic_close),
    onClick = {
      rejectContactRequest(rhId, chatInfo, chatModel)
      showMenu.value = false
    },
    color = Color.Red
  )
}

@Composable
fun ContactConnectionMenuItems(rhId: Long?, chatInfo: ChatInfo.ContactConnection, chatModel: ChatModel, showMenu: MutableState<Boolean>) {
  ItemAction(
    stringResource(MR.strings.set_contact_name),
    painterResource(MR.images.ic_edit),
    onClick = {
      ModalManager.center.closeModals()
      ModalManager.end.closeModals()
      ModalManager.center.showModalCloseable(true, showClose = appPlatform.isAndroid) { close ->
        ContactConnectionInfoView(chatModel, rhId, chatInfo.contactConnection.connLinkInv, chatInfo.contactConnection, true, close)
      }
      showMenu.value = false
    },
  )
  ItemAction(
    stringResource(MR.strings.delete_verb),
    painterResource(MR.images.ic_delete),
    onClick = {
      deleteContactConnectionAlert(rhId, chatInfo.contactConnection, chatModel) {
        chatModel.dismissConnReqView(chatInfo.contactConnection.id)
      }
      showMenu.value = false
    },
    color = Color.Red
  )
}

@Composable
private fun InvalidDataView() {
  Row {
    ProfileImage(72.dp, null, MR.images.ic_account_circle_filled, MaterialTheme.colors.secondary)
    Column(
      modifier = Modifier
        .padding(horizontal = 8.dp)
        .weight(1F)
    ) {
      Text(
        stringResource(MR.strings.invalid_data),
        maxLines = 1,
        overflow = TextOverflow.Ellipsis,
        style = MaterialTheme.typography.h3,
        fontWeight = FontWeight.Bold,
        color = Color.Red
      )
      val height = with(LocalDensity.current) { 46.sp.toDp() }
      Spacer(Modifier.height(height))
    }
  }
}

@Composable
private fun ArchiveAllReportsItemAction(showMenu: MutableState<Boolean>, archiveReports: () -> Unit) {
  ItemAction(
    stringResource(MR.strings.archive_reports),
    painterResource(MR.images.ic_inventory_2),
    onClick = {
      showArchiveAllReportsForMeAlert(archiveReports)
      showMenu.value = false
    }
  )
}

fun markChatRead(c: Chat) {
  var chat = c
  withApi {
    if (chat.chatStats.unreadCount > 0) {
      withContext(Dispatchers.Main) {
        chatModel.chatsContext.markChatItemsRead(chat.remoteHostId, chat.chatInfo.id)
      }
      withContext(Dispatchers.Main) {
        chatModel.secondaryChatsContext.value?.markChatItemsRead(chat.remoteHostId, chat.chatInfo.id)
      }
      chatModel.controller.apiChatRead(
        chat.remoteHostId,
        chat.chatInfo.chatType,
        chat.chatInfo.apiId
      )
      chat = chatModel.getChat(chat.id) ?: return@withApi
    }
    if (chat.chatStats.unreadChat) {
      val success = chatModel.controller.apiChatUnread(
        chat.remoteHostId,
        chat.chatInfo.chatType,
        chat.chatInfo.apiId,
        false
      )
      if (success) {
        withContext(Dispatchers.Main) {
          chatModel.chatsContext.replaceChat(chat.remoteHostId, chat.id, chat.copy(chatStats = chat.chatStats.copy(unreadChat = false)))
          chatModel.chatsContext.markChatTagRead(chat)
        }
      }
    }
  }
}

fun markChatUnread(chat: Chat, chatModel: ChatModel) {
  // Just to be sure
  if (chat.chatStats.unreadChat) return

  withApi {
    val wasUnread = chat.unreadTag
    val success = chatModel.controller.apiChatUnread(
      chat.remoteHostId,
      chat.chatInfo.chatType,
      chat.chatInfo.apiId,
      true
    )
    if (success) {
      withContext(Dispatchers.Main) {
        chatModel.chatsContext.replaceChat(chat.remoteHostId, chat.id, chat.copy(chatStats = chat.chatStats.copy(unreadChat = true)))
        chatModel.chatsContext.updateChatTagReadNoContentTag(chat, wasUnread)
      }
    }
  }
}

fun contactRequestAlertDialog(rhId: Long?, contactRequest: ChatInfo.ContactRequest, chatModel: ChatModel, onSucess: ((chat: Chat) -> Unit)? = null) {
  AlertManager.shared.showAlertDialogButtonsColumn(
    title = generalGetString(MR.strings.accept_connection_request__question),
    text = AnnotatedString(generalGetString(MR.strings.if_you_choose_to_reject_the_sender_will_not_be_notified)),
    buttons = {
      Column {
        SectionItemView({
          AlertManager.shared.hideAlert()
          acceptContactRequest(rhId, incognito = false, contactRequest.apiId, contactRequest, true, chatModel, onSucess)
        }) {
          Text(generalGetString(MR.strings.accept_contact_button), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
        SectionItemView({
          AlertManager.shared.hideAlert()
          acceptContactRequest(rhId, incognito = true, contactRequest.apiId, contactRequest, true, chatModel, onSucess)
        }) {
          Text(generalGetString(MR.strings.accept_contact_incognito_button), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
        SectionItemView({
          AlertManager.shared.hideAlert()
          rejectContactRequest(rhId, contactRequest, chatModel)
        }) {
          Text(generalGetString(MR.strings.reject_contact_button), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = Color.Red)
        }
      }
    },
    hostDevice = hostDevice(rhId),
  )
}

fun acceptContactRequest(rhId: Long?, incognito: Boolean, apiId: Long, contactRequest: ChatInfo.ContactRequest?, isCurrentUser: Boolean, chatModel: ChatModel, close: ((chat: Chat) -> Unit)? = null ) {
  withBGApi {
    val contact = chatModel.controller.apiAcceptContactRequest(rhId, incognito, apiId)
    if (contact != null && isCurrentUser && contactRequest != null) {
      val chat = Chat(remoteHostId = rhId, ChatInfo.Direct(contact), listOf())
      withContext(Dispatchers.Main) {
        chatModel.chatsContext.replaceChat(rhId, contactRequest.id, chat)
      }
      chatModel.setContactNetworkStatus(contact, NetworkStatus.Connected())
      close?.invoke(chat)
    }
  }
}

fun rejectContactRequest(rhId: Long?, contactRequest: ChatInfo.ContactRequest, chatModel: ChatModel) {
  withBGApi {
    chatModel.controller.apiRejectContactRequest(rhId, contactRequest.apiId)
    withContext(Dispatchers.Main) {
      chatModel.chatsContext.removeChat(rhId, contactRequest.id)
    }
  }
}

fun deleteContactConnectionAlert(rhId: Long?, connection: PendingContactConnection, chatModel: ChatModel, onSuccess: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.delete_pending_connection__question),
    text = generalGetString(
      if (connection.initiated) MR.strings.contact_you_shared_link_with_wont_be_able_to_connect
      else MR.strings.connection_you_accepted_will_be_cancelled
    ),
    confirmText = generalGetString(MR.strings.delete_verb),
    onConfirm = {
      withBGApi {
        AlertManager.shared.hideAlert()
        if (chatModel.controller.apiDeleteChat(rhId, ChatType.ContactConnection, connection.apiId)) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.removeChat(rhId, connection.id)
          }
          onSuccess()
        }
      }
    },
    destructive = true,
  )
}

// TODO why is it not used
fun pendingContactAlertDialog(rhId: Long?, chatInfo: ChatInfo, chatModel: ChatModel) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.alert_title_contact_connection_pending),
    text = generalGetString(MR.strings.alert_text_connection_pending_they_need_to_be_online_can_delete_and_retry),
    confirmText = generalGetString(MR.strings.button_delete_contact),
    onConfirm = {
      withBGApi {
        val r = chatModel.controller.apiDeleteChat(rhId, chatInfo.chatType, chatInfo.apiId)
        if (r) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.removeChat(rhId, chatInfo.id)
          }
          if (chatModel.chatId.value == chatInfo.id) {
            chatModel.chatId.value = null
            ModalManager.end.closeModals()
          }
        }
      }
    },
    destructive = true,
    dismissText = generalGetString(MR.strings.cancel_verb),
  )
}

fun askCurrentOrIncognitoProfileConnectContactViaAddress(
  chatModel: ChatModel,
  rhId: Long?,
  contact: Contact,
  close: (() -> Unit)?,
  openChat: Boolean
) {
  AlertManager.privacySensitive.showAlertDialogButtonsColumn(
    title = String.format(generalGetString(MR.strings.connect_with_contact_name_question), contact.chatViewName),
    buttons = {
      Column {
        SectionItemView({
          AlertManager.privacySensitive.hideAlert()
          withBGApi {
            close?.invoke()
            val ok = connectContactViaAddress(chatModel, rhId, contact.contactId, incognito = false)
            if (ok && openChat) {
              openDirectChat(rhId, contact.contactId)
            }
          }
        }) {
          Text(generalGetString(MR.strings.connect_use_current_profile), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
        SectionItemView({
          AlertManager.privacySensitive.hideAlert()
          withBGApi {
            close?.invoke()
            val ok = connectContactViaAddress(chatModel, rhId, contact.contactId, incognito = true)
            if (ok && openChat) {
              openDirectChat(rhId, contact.contactId)
            }
          }
        }) {
          Text(generalGetString(MR.strings.connect_use_new_incognito_profile), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
        SectionItemView({
          AlertManager.privacySensitive.hideAlert()
        }) {
          Text(stringResource(MR.strings.cancel_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
      }
    },
    hostDevice = hostDevice(rhId),
  )
}

suspend fun connectContactViaAddress(chatModel: ChatModel, rhId: Long?, contactId: Long, incognito: Boolean): Boolean {
  val contact = chatModel.controller.apiConnectContactViaAddress(rhId, incognito, contactId)
  if (contact != null) {
    withContext(Dispatchers.Main) {
      chatModel.chatsContext.updateContact(rhId, contact)
    }
    AlertManager.privacySensitive.showAlertMsg(
      title = generalGetString(MR.strings.connection_request_sent),
      text = generalGetString(MR.strings.you_will_be_connected_when_your_connection_request_is_accepted),
      hostDevice = hostDevice(rhId),
    )
    return true
  }
  return false
}

fun acceptGroupInvitationAlertDialog(rhId: Long?, groupInfo: GroupInfo, chatModel: ChatModel, inProgress: MutableState<Boolean>? = null) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.join_group_question),
    text = generalGetString(MR.strings.you_are_invited_to_group_join_to_connect_with_group_members),
    confirmText = if (groupInfo.membership.memberIncognito) generalGetString(MR.strings.join_group_incognito_button) else generalGetString(MR.strings.join_group_button),
    onConfirm = {
      withBGApi {
        inProgress?.value = true
        chatModel.controller.apiJoinGroup(rhId, groupInfo.groupId)
        inProgress?.value = false
      }
    },
    dismissText = generalGetString(MR.strings.delete_verb),
    onDismiss = { deleteGroup(rhId, groupInfo, chatModel) },
    hostDevice = hostDevice(rhId),
  )
}

fun cantInviteIncognitoAlert() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.alert_title_cant_invite_contacts),
    text = generalGetString(MR.strings.alert_title_cant_invite_contacts_descr),
    confirmText = generalGetString(MR.strings.ok),
  )
}

fun deleteGroup(rhId: Long?, groupInfo: GroupInfo, chatModel: ChatModel) {
  withBGApi {
    val r = chatModel.controller.apiDeleteChat(rhId, ChatType.Group, groupInfo.apiId)
    if (r) {
      withContext(Dispatchers.Main) {
        chatModel.chatsContext.removeChat(rhId, groupInfo.id)
      }
      if (chatModel.chatId.value == groupInfo.id) {
        chatModel.chatId.value = null
        ModalManager.end.closeModals()
      }
      ntfManager.cancelNotificationsForChat(groupInfo.id)
    }
  }
}

fun groupInvitationAcceptedAlert(rhId: Long?) {
  AlertManager.shared.showAlertMsg(
    generalGetString(MR.strings.joining_group),
    generalGetString(MR.strings.youve_accepted_group_invitation_connecting_to_inviting_group_member),
    hostDevice = hostDevice(rhId),
  )
}

fun toggleNotifications(remoteHostId: Long?, chatInfo: ChatInfo, filter: MsgFilter, chatModel: ChatModel, currentState: MutableState<MsgFilter>? = null) {
  val chatSettings = (chatInfo.chatSettings ?: ChatSettings.defaults).copy(enableNtfs = filter)
  updateChatSettings(remoteHostId, chatInfo, chatSettings, chatModel, currentState)
}

fun toggleChatFavorite(remoteHostId: Long?, chatInfo: ChatInfo, favorite: Boolean, chatModel: ChatModel) {
  val chatSettings = (chatInfo.chatSettings ?: ChatSettings.defaults).copy(favorite = favorite)
  updateChatSettings(remoteHostId, chatInfo, chatSettings, chatModel)
}

fun updateChatSettings(remoteHostId: Long?, chatInfo: ChatInfo, chatSettings: ChatSettings, chatModel: ChatModel, currentState: MutableState<MsgFilter>? = null) {
  val newChatInfo = when(chatInfo) {
    is ChatInfo.Direct -> with (chatInfo) {
      ChatInfo.Direct(contact.copy(chatSettings = chatSettings))
    }
    is ChatInfo.Group -> with(chatInfo) {
      ChatInfo.Group(groupInfo.copy(chatSettings = chatSettings))
    }
    else -> null
  }
  withBGApi {
    val res = when (newChatInfo) {
      is ChatInfo.Direct -> with(newChatInfo) {
        chatModel.controller.apiSetSettings(remoteHostId, chatType, apiId, contact.chatSettings)
      }
      is ChatInfo.Group -> with(newChatInfo) {
        chatModel.controller.apiSetSettings(remoteHostId, chatType, apiId, groupInfo.chatSettings)
      }
      else -> false
    }
    if (res && newChatInfo != null) {
      val chat = chatModel.getChat(chatInfo.id)
      val wasUnread = chat?.unreadTag ?: false
      val wasFavorite = chatInfo.chatSettings?.favorite ?: false
      chatModel.updateChatFavorite(favorite = chatSettings.favorite, wasFavorite)
      withContext(Dispatchers.Main) {
        chatModel.chatsContext.updateChatInfo(remoteHostId, newChatInfo)
      }
      if (chatSettings.enableNtfs == MsgFilter.None) {
        ntfManager.cancelNotificationsForChat(chatInfo.id)
      }
      val updatedChat = chatModel.getChat(chatInfo.id)
      if (updatedChat != null) {
        withContext(Dispatchers.Main) {
          chatModel.chatsContext.updateChatTagReadNoContentTag(updatedChat, wasUnread)
        }
      }
      val current = currentState?.value
      if (current != null) {
        currentState.value = chatSettings.enableNtfs
      }
    }
  }
}

private fun showArchiveAllReportsForMeAlert(archiveReports: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.report_archive_alert_title_all),
    text = generalGetString(MR.strings.report_archive_alert_desc_all),
    onConfirm = archiveReports,
    destructive = true,
    confirmText = generalGetString(MR.strings.archive_verb),
  )
}

private fun archiveAllReportsForMe(chatRh: Long?, apiId: Long) {
  withBGApi {
    val r = chatModel.controller.apiArchiveReceivedReports(chatRh, apiId)
    if (r != null) {
      controller.groupChatItemsDeleted(chatRh, r)
    }
  }
}

@Composable
expect fun ChatListNavLinkLayout(
  chatLinkPreview: @Composable () -> Unit,
  click: () -> Unit,
  dropdownMenuItems: (@Composable () -> Unit)?,
  showMenu: MutableState<Boolean>,
  disabled: Boolean,
  selectedChat: State<Boolean>,
  nextChatSelected: State<Boolean>,
)

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
@Composable
fun PreviewChatListNavLinkDirect() {
  SimpleXTheme {
    ChatListNavLinkLayout(
      chatLinkPreview = {
        ChatPreviewView(
          chat = Chat(
            remoteHostId = null,
            chatInfo = ChatInfo.Direct.sampleData,
            chatItems = listOf(
              ChatItem.getSampleData(
                1,
                CIDirection.DirectSnd(),
                Clock.System.now(),
                "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
              )
            ),
            chatStats = Chat.ChatStats()
          ),
          true,
          null,
          null,
          null,
          null,
          disabled = false,
          linkMode = SimplexLinkMode.DESCRIPTION,
          inProgress = false,
          progressByTimeout = false,
          {}
        )
      },
      click = {},
      dropdownMenuItems = null,
      showMenu = remember { mutableStateOf(false) },
      disabled = false,
      selectedChat = remember { mutableStateOf(false) },
      nextChatSelected = remember { mutableStateOf(false) },
    )
  }
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
@Composable
fun PreviewChatListNavLinkGroup() {
  SimpleXTheme {
    ChatListNavLinkLayout(
      chatLinkPreview = {
        ChatPreviewView(
          Chat(
            remoteHostId = null,
            chatInfo = ChatInfo.Group.sampleData,
            chatItems = listOf(
              ChatItem.getSampleData(
                1,
                CIDirection.DirectSnd(),
                Clock.System.now(),
                "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
              )
            ),
            chatStats = Chat.ChatStats()
          ),
          true,
          null,
          null,
          null,
          null,
          disabled = false,
          linkMode = SimplexLinkMode.DESCRIPTION,
          inProgress = false,
          progressByTimeout = false,
          {}
        )
      },
      click = {},
      dropdownMenuItems = null,
      showMenu = remember { mutableStateOf(false) },
      disabled = false,
      selectedChat = remember { mutableStateOf(false) },
      nextChatSelected = remember { mutableStateOf(false) },
    )
  }
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
@Composable
fun PreviewChatListNavLinkContactRequest() {
  SimpleXTheme {
    ChatListNavLinkLayout(
      chatLinkPreview = {
        ContactRequestView(ChatInfo.ContactRequest.sampleData)
      },
      click = {},
      dropdownMenuItems = null,
      showMenu = remember { mutableStateOf(false) },
      disabled = false,
      selectedChat = remember { mutableStateOf(false) },
      nextChatSelected = remember { mutableStateOf(false) },
    )
  }
}
