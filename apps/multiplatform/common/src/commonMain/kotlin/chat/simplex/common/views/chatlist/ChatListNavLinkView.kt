package chat.simplex.common.views.chatlist

import SectionItemView
import androidx.compose.animation.core.animateFloatAsState
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalDensity
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatModel.withChats
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.chat.group.deleteGroupDialog
import chat.simplex.common.views.chat.group.leaveGroupDialog
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.contacts.onRequestAccepted
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.*
import chat.simplex.res.MR
import kotlinx.coroutines.*
import kotlinx.datetime.Clock

@Composable
fun ChatListNavLinkView(chat: Chat, nextChatSelected: State<Boolean>, reachableChatToolbar: State<Boolean>) {
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
      ChatListNavLinkLayout(
        chatLinkPreview = {
          tryOrShowError("${chat.id}ChatListNavLink", error = { ErrorChatListItem() }) {
            ChatPreviewView(chat, showChatPreviews, chatModel.draft.value, chatModel.draftChatId.value, chatModel.currentUser.value?.profile?.displayName, contactNetworkStatus, disabled, linkMode, inProgress = false, progressByTimeout = false)
          }
        },
        click = { scope.launch { directChatAction(chat.remoteHostId, chat.chatInfo.contact, chatModel) } },
        dropdownMenuItems = {
          tryOrShowError("${chat.id}ChatListNavLinkDropdown", error = {}) {
            ContactMenuItems(chat, chat.chatInfo.contact, chatModel, showMenu, showMarkRead)
          }
        },
        showMenu,
        disabled,
        selectedChat,
        nextChatSelected,
        reachableChatToolbar
      )
    }
    is ChatInfo.Group ->
      ChatListNavLinkLayout(
        chatLinkPreview = {
          tryOrShowError("${chat.id}ChatListNavLink", error = { ErrorChatListItem() }) {
            ChatPreviewView(chat, showChatPreviews, chatModel.draft.value, chatModel.draftChatId.value, chatModel.currentUser.value?.profile?.displayName, null, disabled, linkMode, inProgress.value, progressByTimeout)
          }
        },
        click = { if (!inProgress.value) scope.launch { groupChatAction(chat.remoteHostId, chat.chatInfo.groupInfo, chatModel, inProgress) } },
        dropdownMenuItems = {
          tryOrShowError("${chat.id}ChatListNavLinkDropdown", error = {}) {
            GroupMenuItems(chat, chat.chatInfo.groupInfo, chatModel, showMenu, inProgress, showMarkRead)
          }
        },
        showMenu,
        disabled,
        selectedChat,
        nextChatSelected,
        reachableChatToolbar
      )
    is ChatInfo.Local -> {
      ChatListNavLinkLayout(
        chatLinkPreview = {
          tryOrShowError("${chat.id}ChatListNavLink", error = { ErrorChatListItem() }) {
            ChatPreviewView(chat, showChatPreviews, chatModel.draft.value, chatModel.draftChatId.value, chatModel.currentUser.value?.profile?.displayName, null, disabled, linkMode, inProgress = false, progressByTimeout = false)
          }
        },
        click = { scope.launch { noteFolderChatAction(chat.remoteHostId, chat.chatInfo.noteFolder) } },
        dropdownMenuItems = {
          tryOrShowError("${chat.id}ChatListNavLinkDropdown", error = {}) {
            NoteFolderMenuItems(chat, showMenu, showMarkRead)
          }
        },
        showMenu,
        disabled,
        selectedChat,
        nextChatSelected,
        reachableChatToolbar
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
        reachableChatToolbar
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
        reachableChatToolbar
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
        reachableChatToolbar
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
    contact.activeConn == null && contact.profile.contactLink != null -> askCurrentOrIncognitoProfileConnectContactViaAddress(chatModel, rhId, contact, close = null, openChat = true)
    else -> openChat(rhId, ChatInfo.Direct(contact), chatModel)
  }
}

suspend fun groupChatAction(rhId: Long?, groupInfo: GroupInfo, chatModel: ChatModel, inProgress: MutableState<Boolean>? = null) {
  when (groupInfo.membership.memberStatus) {
    GroupMemberStatus.MemInvited -> acceptGroupInvitationAlertDialog(rhId, groupInfo, chatModel, inProgress)
    GroupMemberStatus.MemAccepted -> groupInvitationAcceptedAlert(rhId)
    else -> openChat(rhId, ChatInfo.Group(groupInfo), chatModel)
  }
}

suspend fun noteFolderChatAction(rhId: Long?, noteFolder: NoteFolder) {
  openChat(rhId, ChatInfo.Local(noteFolder), chatModel)
}

suspend fun openDirectChat(rhId: Long?, contactId: Long, chatModel: ChatModel) = coroutineScope {
  val chat = chatModel.controller.apiGetChat(rhId, ChatType.Direct, contactId)
  if (chat != null && isActive) {
    openLoadedChat(chat, chatModel)
  }
}

suspend fun openGroupChat(rhId: Long?, groupId: Long, chatModel: ChatModel) = coroutineScope {
  val chat = chatModel.controller.apiGetChat(rhId, ChatType.Group, groupId)
  if (chat != null && isActive) {
    openLoadedChat(chat, chatModel)
  }
}

suspend fun openChat(rhId: Long?, chatInfo: ChatInfo, chatModel: ChatModel) = coroutineScope {
  val chat = chatModel.controller.apiGetChat(rhId, chatInfo.chatType, chatInfo.apiId)
  if (chat != null && isActive) {
    openLoadedChat(chat, chatModel)
  }
}

fun openLoadedChat(chat: Chat, chatModel: ChatModel) {
  chatModel.chatItemStatuses.clear()
  chatModel.chatItems.replaceAll(chat.chatItems)
  chatModel.chatId.value = chat.chatInfo.id
}

suspend fun apiLoadPrevMessages(ch: Chat, chatModel: ChatModel, beforeChatItemId: Long, search: String) {
  val chatInfo = ch.chatInfo
  val pagination = ChatPagination.Before(beforeChatItemId, ChatPagination.PRELOAD_COUNT)
  val chat = chatModel.controller.apiGetChat(ch.remoteHostId, chatInfo.chatType, chatInfo.apiId, pagination, search) ?: return
  if (chatModel.chatId.value != chat.id) return
  chatModel.chatItems.addAll(0, chat.chatItems)
}

suspend fun apiFindMessages(ch: Chat, chatModel: ChatModel, search: String) {
  val chatInfo = ch.chatInfo
  val chat = chatModel.controller.apiGetChat(ch.remoteHostId, chatInfo.chatType, chatInfo.apiId, search = search) ?: return
  if (chatModel.chatId.value != chat.id) return
  chatModel.chatItems.replaceAll(chat.chatItems)
}

suspend fun setGroupMembers(rhId: Long?, groupInfo: GroupInfo, chatModel: ChatModel) {
  val groupMembers = chatModel.controller.apiListMembers(rhId, groupInfo.groupId)
  val currentMembers = chatModel.groupMembers
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
  chatModel.groupMembers.clear()
  chatModel.groupMembersIndexes.clear()
  chatModel.groupMembers.addAll(newMembers)
  chatModel.populateGroupMembersIndexes()
}

@Composable
fun ContactMenuItems(chat: Chat, contact: Contact, chatModel: ChatModel, showMenu: MutableState<Boolean>, showMarkRead: Boolean) {
  if (contact.activeConn != null) {
    if (showMarkRead) {
      MarkReadChatAction(chat, chatModel, showMenu)
    } else {
      MarkUnreadChatAction(chat, chatModel, showMenu)
    }
    ToggleFavoritesChatAction(chat, chatModel, chat.chatInfo.chatSettings?.favorite == true, showMenu)
    ToggleNotificationsChatAction(chat, chatModel, chat.chatInfo.ntfsEnabled, showMenu)
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
        MarkReadChatAction(chat, chatModel, showMenu)
      } else {
        MarkUnreadChatAction(chat, chatModel, showMenu)
      }
      ToggleFavoritesChatAction(chat, chatModel, chat.chatInfo.chatSettings?.favorite == true, showMenu)
      ToggleNotificationsChatAction(chat, chatModel, chat.chatInfo.ntfsEnabled, showMenu)
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
    MarkReadChatAction(chat, chatModel, showMenu)
  } else {
    MarkUnreadChatAction(chat, chatModel, showMenu)
  }
  ClearNoteFolderAction(chat, showMenu)
}

@Composable
fun MarkReadChatAction(chat: Chat, chatModel: ChatModel, showMenu: MutableState<Boolean>) {
  ItemAction(
    stringResource(MR.strings.mark_read),
    painterResource(MR.images.ic_check),
    onClick = {
      markChatRead(chat, chatModel)
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
fun ToggleNotificationsChatAction(chat: Chat, chatModel: ChatModel, ntfsEnabled: Boolean, showMenu: MutableState<Boolean>) {
  ItemAction(
    if (ntfsEnabled) stringResource(MR.strings.mute_chat) else stringResource(MR.strings.unmute_chat),
    if (ntfsEnabled) painterResource(MR.images.ic_notifications_off) else painterResource(MR.images.ic_notifications),
    onClick = {
      toggleNotifications(chat.remoteHostId, chat.chatInfo, !ntfsEnabled, chatModel)
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
        ContactConnectionInfoView(chatModel, rhId, chatInfo.contactConnection.connReqInv, chatInfo.contactConnection, true, close)
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

fun markChatRead(c: Chat, chatModel: ChatModel) {
  var chat = c
  withApi {
    if (chat.chatStats.unreadCount > 0) {
      val minUnreadItemId = chat.chatStats.minUnreadItemId
      withChats {
        markChatItemsRead(chat.remoteHostId, chat.chatInfo)
      }
      chatModel.controller.apiChatRead(
        chat.remoteHostId,
        chat.chatInfo.chatType,
        chat.chatInfo.apiId,
        CC.ItemRange(minUnreadItemId, chat.chatItems.last().id)
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
        withChats {
          replaceChat(chat.remoteHostId, chat.id, chat.copy(chatStats = chat.chatStats.copy(unreadChat = false)))
        }
      }
    }
  }
}

fun markChatUnread(chat: Chat, chatModel: ChatModel) {
  // Just to be sure
  if (chat.chatStats.unreadChat) return

  withApi {
    val success = chatModel.controller.apiChatUnread(
      chat.remoteHostId,
      chat.chatInfo.chatType,
      chat.chatInfo.apiId,
      true
    )
    if (success) {
      withChats {
        replaceChat(chat.remoteHostId, chat.id, chat.copy(chatStats = chat.chatStats.copy(unreadChat = true)))
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
      withChats {
        replaceChat(rhId, contactRequest.id, chat)
      }
      chatModel.setContactNetworkStatus(contact, NetworkStatus.Connected())
      close?.invoke(chat)
    }
  }
}

fun rejectContactRequest(rhId: Long?, contactRequest: ChatInfo.ContactRequest, chatModel: ChatModel) {
  withBGApi {
    chatModel.controller.apiRejectContactRequest(rhId, contactRequest.apiId)
    withChats {
      removeChat(rhId, contactRequest.id)
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
          withChats {
            removeChat(rhId, connection.id)
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
          withChats {
            removeChat(rhId, chatInfo.id)
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
              openDirectChat(rhId, contact.contactId, chatModel)
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
              openDirectChat(rhId, contact.contactId, chatModel)
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
    withChats {
      updateContact(rhId, contact)
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
      withChats {
        removeChat(rhId, groupInfo.id)
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

fun toggleNotifications(remoteHostId: Long?, chatInfo: ChatInfo, enableAllNtfs: Boolean, chatModel: ChatModel, currentState: MutableState<Boolean>? = null) {
  val chatSettings = (chatInfo.chatSettings ?: ChatSettings.defaults).copy(enableNtfs = if (enableAllNtfs) MsgFilter.All else MsgFilter.None)
  updateChatSettings(remoteHostId, chatInfo, chatSettings, chatModel, currentState)
}

fun toggleChatFavorite(remoteHostId: Long?, chatInfo: ChatInfo, favorite: Boolean, chatModel: ChatModel) {
  val chatSettings = (chatInfo.chatSettings ?: ChatSettings.defaults).copy(favorite = favorite)
  updateChatSettings(remoteHostId, chatInfo, chatSettings, chatModel)
}

fun updateChatSettings(remoteHostId: Long?, chatInfo: ChatInfo, chatSettings: ChatSettings, chatModel: ChatModel, currentState: MutableState<Boolean>? = null) {
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
      withChats {
        updateChatInfo(remoteHostId, newChatInfo)
      }
      if (chatSettings.enableNtfs != MsgFilter.All) {
        ntfManager.cancelNotificationsForChat(chatInfo.id)
      }
      val current = currentState?.value
      if (current != null) {
        currentState.value = !current
      }
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
  reachableChatToolbar: State<Boolean>
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
          progressByTimeout = false
        )
      },
      click = {},
      dropdownMenuItems = null,
      showMenu = remember { mutableStateOf(false) },
      disabled = false,
      selectedChat = remember { mutableStateOf(false) },
      nextChatSelected = remember { mutableStateOf(false) },
      reachableChatToolbar = remember { mutableStateOf(false) }
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
          progressByTimeout = false
        )
      },
      click = {},
      dropdownMenuItems = null,
      showMenu = remember { mutableStateOf(false) },
      disabled = false,
      selectedChat = remember { mutableStateOf(false) },
      nextChatSelected = remember { mutableStateOf(false) },
      reachableChatToolbar = remember { mutableStateOf(false) }
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
      reachableChatToolbar = remember { mutableStateOf(false) }
    )
  }
}
