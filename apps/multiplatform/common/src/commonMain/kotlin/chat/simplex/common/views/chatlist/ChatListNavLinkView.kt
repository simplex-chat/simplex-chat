package chat.simplex.common.views.chatlist

import SectionItemView
import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalDensity
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.chat.group.deleteGroupDialog
import chat.simplex.common.views.chat.group.leaveGroupDialog
import chat.simplex.common.views.chat.item.InvalidJSONView
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.*
import chat.simplex.res.MR
import kotlinx.coroutines.delay
import kotlinx.datetime.Clock

@Composable
fun ChatListNavLinkView(chat: Chat, chatModel: ChatModel) {
  val showMenu = remember { mutableStateOf(false) }
  val showMarkRead = remember(chat.chatStats.unreadCount, chat.chatStats.unreadChat) {
    chat.chatStats.unreadCount > 0 || chat.chatStats.unreadChat
  }
  val stopped = chatModel.chatRunning.value == false
  val linkMode by remember { chatModel.controller.appPrefs.simplexLinkMode.state }
  LaunchedEffect(chat.id) {
    showMenu.value = false
    delay(500L)
  }
  val showChatPreviews = chatModel.showChatPreviews.value
  when (chat.chatInfo) {
    is ChatInfo.Direct -> {
      val contactNetworkStatus = chatModel.contactNetworkStatus(chat.chatInfo.contact)
      ChatListNavLinkLayout(
        chatLinkPreview = { ChatPreviewView(chat, showChatPreviews, chatModel.draft.value, chatModel.draftChatId.value, chatModel.currentUser.value?.profile?.displayName, contactNetworkStatus, stopped, linkMode) },
        click = { directChatAction(chat.chatInfo, chatModel) },
        dropdownMenuItems = { ContactMenuItems(chat, chatModel, showMenu, showMarkRead) },
        showMenu,
        stopped
      )
    }
    is ChatInfo.Group ->
      ChatListNavLinkLayout(
        chatLinkPreview = { ChatPreviewView(chat, showChatPreviews, chatModel.draft.value, chatModel.draftChatId.value, chatModel.currentUser.value?.profile?.displayName, null, stopped, linkMode) },
        click = { groupChatAction(chat.chatInfo.groupInfo, chatModel) },
        dropdownMenuItems = { GroupMenuItems(chat, chat.chatInfo.groupInfo, chatModel, showMenu, showMarkRead) },
        showMenu,
        stopped
      )
    is ChatInfo.ContactRequest ->
      ChatListNavLinkLayout(
        chatLinkPreview = { ContactRequestView(chat.chatInfo) },
        click = { contactRequestAlertDialog(chat.chatInfo, chatModel) },
        dropdownMenuItems = { ContactRequestMenuItems(chat.chatInfo, chatModel, showMenu) },
        showMenu,
        stopped
      )
    is ChatInfo.ContactConnection ->
      ChatListNavLinkLayout(
        chatLinkPreview = { ContactConnectionView(chat.chatInfo.contactConnection) },
        click = {
          ModalManager.center.closeModals()
          ModalManager.end.closeModals()
          ModalManager.center.showModalCloseable(true, showClose = appPlatform.isAndroid) { close ->
            ContactConnectionInfoView(chatModel, chat.chatInfo.contactConnection.connReqInv, chat.chatInfo.contactConnection, false, close)
          }
        },
        dropdownMenuItems = { ContactConnectionMenuItems(chat.chatInfo, chatModel, showMenu) },
        showMenu,
        stopped
      )
    is ChatInfo.InvalidJSON ->
      ChatListNavLinkLayout(
        chatLinkPreview = {
          InvalidDataView()
        },
        click = {
          ModalManager.end.closeModals()
          ModalManager.center.showModal(true) { InvalidJSONView(chat.chatInfo.json) }
        },
        dropdownMenuItems = null,
        showMenu,
        stopped
      )
  }
}

fun directChatAction(chatInfo: ChatInfo, chatModel: ChatModel) {
  withBGApi { openChat(chatInfo, chatModel) }
}

fun groupChatAction(groupInfo: GroupInfo, chatModel: ChatModel) {
  when (groupInfo.membership.memberStatus) {
    GroupMemberStatus.MemInvited -> acceptGroupInvitationAlertDialog(groupInfo, chatModel)
    GroupMemberStatus.MemAccepted -> groupInvitationAcceptedAlert()
    else -> withBGApi { openChat(ChatInfo.Group(groupInfo), chatModel) }
  }
}

suspend fun openDirectChat(contactId: Long, chatModel: ChatModel) {
  val chat = chatModel.controller.apiGetChat(ChatType.Direct, contactId)
  if (chat != null) {
    openChat(chat, chatModel)
  }
}

suspend fun openChat(chatInfo: ChatInfo, chatModel: ChatModel) {
  val chat = chatModel.controller.apiGetChat(chatInfo.chatType, chatInfo.apiId)
  if (chat != null) {
    openChat(chat, chatModel)
  }
}

suspend fun openChat(chat: Chat, chatModel: ChatModel) {
  chatModel.chatItems.clear()
  chatModel.chatItems.addAll(chat.chatItems)
  chatModel.chatId.value = chat.chatInfo.id
}

suspend fun apiLoadPrevMessages(chatInfo: ChatInfo, chatModel: ChatModel, beforeChatItemId: Long, search: String) {
  val pagination = ChatPagination.Before(beforeChatItemId, ChatPagination.PRELOAD_COUNT)
  val chat = chatModel.controller.apiGetChat(chatInfo.chatType, chatInfo.apiId, pagination, search) ?: return
  if (chatModel.chatId.value != chat.id) return
  chatModel.chatItems.addAll(0, chat.chatItems)
}

suspend fun apiFindMessages(chatInfo: ChatInfo, chatModel: ChatModel, search: String) {
  val chat = chatModel.controller.apiGetChat(chatInfo.chatType, chatInfo.apiId, search = search) ?: return
  if (chatModel.chatId.value != chat.id) return
  chatModel.chatItems.clear()
  chatModel.chatItems.addAll(0, chat.chatItems)
}

suspend fun setGroupMembers(groupInfo: GroupInfo, chatModel: ChatModel) {
  val groupMembers = chatModel.controller.apiListMembers(groupInfo.groupId)
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
  chatModel.groupMembers.addAll(newMembers)
}

@Composable
fun ContactMenuItems(chat: Chat, chatModel: ChatModel, showMenu: MutableState<Boolean>, showMarkRead: Boolean) {
  if (showMarkRead) {
    MarkReadChatAction(chat, chatModel, showMenu)
  } else {
    MarkUnreadChatAction(chat, chatModel, showMenu)
  }
  ToggleFavoritesChatAction(chat, chatModel, chat.chatInfo.chatSettings?.favorite == true, showMenu)
  ToggleNotificationsChatAction(chat, chatModel, chat.chatInfo.ntfsEnabled, showMenu)
  ClearChatAction(chat, chatModel, showMenu)
  DeleteContactAction(chat, chatModel, showMenu)
}

@Composable
fun GroupMenuItems(chat: Chat, groupInfo: GroupInfo, chatModel: ChatModel, showMenu: MutableState<Boolean>, showMarkRead: Boolean) {
  when (groupInfo.membership.memberStatus) {
    GroupMemberStatus.MemInvited -> {
      JoinGroupAction(chat, groupInfo, chatModel, showMenu)
      if (groupInfo.canDelete) {
        DeleteGroupAction(chat, groupInfo, chatModel, showMenu)
      }
    }
    GroupMemberStatus.MemAccepted -> {
      if (groupInfo.membership.memberCurrent) {
        LeaveGroupAction(groupInfo, chatModel, showMenu)
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
      ClearChatAction(chat, chatModel, showMenu)
      if (groupInfo.membership.memberCurrent) {
        LeaveGroupAction(groupInfo, chatModel, showMenu)
      }
      if (groupInfo.canDelete) {
        DeleteGroupAction(chat, groupInfo, chatModel, showMenu)
      }
    }
  }
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
      toggleChatFavorite(chat, !favorite, chatModel)
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
      toggleNotifications(chat, !ntfsEnabled, chatModel)
      showMenu.value = false
    }
  )
}

@Composable
fun ClearChatAction(chat: Chat, chatModel: ChatModel, showMenu: MutableState<Boolean>) {
  ItemAction(
    stringResource(MR.strings.clear_chat_menu_action),
    painterResource(MR.images.ic_settings_backup_restore),
    onClick = {
      clearChatDialog(chat.chatInfo, chatModel)
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
      deleteContactDialog(chat.chatInfo, chatModel)
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
      deleteGroupDialog(chat.chatInfo, groupInfo, chatModel)
      showMenu.value = false
    },
    color = Color.Red
  )
}

@Composable
fun JoinGroupAction(chat: Chat, groupInfo: GroupInfo, chatModel: ChatModel, showMenu: MutableState<Boolean>) {
  val joinGroup: () -> Unit = { withApi { chatModel.controller.apiJoinGroup(groupInfo.groupId) } }
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
fun LeaveGroupAction(groupInfo: GroupInfo, chatModel: ChatModel, showMenu: MutableState<Boolean>) {
  ItemAction(
    stringResource(MR.strings.leave_group_button),
    painterResource(MR.images.ic_logout),
    onClick = {
      leaveGroupDialog(groupInfo, chatModel)
      showMenu.value = false
    },
    color = Color.Red
  )
}

@Composable
fun ContactRequestMenuItems(chatInfo: ChatInfo.ContactRequest, chatModel: ChatModel, showMenu: MutableState<Boolean>) {
  ItemAction(
    stringResource(MR.strings.accept_contact_button),
    painterResource(MR.images.ic_check),
    color = MaterialTheme.colors.onBackground,
    onClick = {
      acceptContactRequest(incognito = false, chatInfo.apiId, chatInfo, true, chatModel)
      showMenu.value = false
    }
  )
  ItemAction(
    stringResource(MR.strings.accept_contact_incognito_button),
    painterResource(MR.images.ic_theater_comedy),
    color = MaterialTheme.colors.onBackground,
    onClick = {
      acceptContactRequest(incognito = true, chatInfo.apiId, chatInfo, true, chatModel)
      showMenu.value = false
    }
  )
  ItemAction(
    stringResource(MR.strings.reject_contact_button),
    painterResource(MR.images.ic_close),
    onClick = {
      rejectContactRequest(chatInfo, chatModel)
      showMenu.value = false
    },
    color = Color.Red
  )
}

@Composable
fun ContactConnectionMenuItems(chatInfo: ChatInfo.ContactConnection, chatModel: ChatModel, showMenu: MutableState<Boolean>) {
  ItemAction(
    stringResource(MR.strings.set_contact_name),
    painterResource(MR.images.ic_edit),
    onClick = {
      ModalManager.center.closeModals()
      ModalManager.end.closeModals()
      ModalManager.center.showModalCloseable(true, showClose = appPlatform.isAndroid) { close ->
        ContactConnectionInfoView(chatModel, chatInfo.contactConnection.connReqInv, chatInfo.contactConnection, true, close)
      }
      showMenu.value = false
    },
  )
  ItemAction(
    stringResource(MR.strings.delete_verb),
    painterResource(MR.images.ic_delete),
    onClick = {
      deleteContactConnectionAlert(chatInfo.contactConnection, chatModel) {
        if (chatModel.chatId.value == null) {
          ModalManager.center.closeModals()
          ModalManager.end.closeModals()
        }
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
      chatModel.markChatItemsRead(chat.chatInfo)
      chatModel.controller.apiChatRead(
        chat.chatInfo.chatType,
        chat.chatInfo.apiId,
        CC.ItemRange(minUnreadItemId, chat.chatItems.last().id)
      )
      chat = chatModel.getChat(chat.id) ?: return@withApi
    }
    if (chat.chatStats.unreadChat) {
      val success = chatModel.controller.apiChatUnread(
        chat.chatInfo.chatType,
        chat.chatInfo.apiId,
        false
      )
      if (success) {
        chatModel.replaceChat(chat.id, chat.copy(chatStats = chat.chatStats.copy(unreadChat = false)))
      }
    }
  }
}

fun markChatUnread(chat: Chat, chatModel: ChatModel) {
  // Just to be sure
  if (chat.chatStats.unreadChat) return

  withApi {
    val success = chatModel.controller.apiChatUnread(
      chat.chatInfo.chatType,
      chat.chatInfo.apiId,
      true
    )
    if (success) {
      chatModel.replaceChat(chat.id, chat.copy(chatStats = chat.chatStats.copy(unreadChat = true)))
    }
  }
}

fun contactRequestAlertDialog(contactRequest: ChatInfo.ContactRequest, chatModel: ChatModel) {
  AlertManager.shared.showAlertDialogButtonsColumn(
    title = generalGetString(MR.strings.accept_connection_request__question),
    text = AnnotatedString(generalGetString(MR.strings.if_you_choose_to_reject_the_sender_will_not_be_notified)),
    buttons = {
      Column {
        SectionItemView({
          AlertManager.shared.hideAlert()
          acceptContactRequest(incognito = false, contactRequest.apiId, contactRequest, true, chatModel)
        }) {
          Text(generalGetString(MR.strings.accept_contact_button), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
        SectionItemView({
          AlertManager.shared.hideAlert()
          acceptContactRequest(incognito = true, contactRequest.apiId, contactRequest, true, chatModel)
        }) {
          Text(generalGetString(MR.strings.accept_contact_incognito_button), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
        SectionItemView({
          AlertManager.shared.hideAlert()
          rejectContactRequest(contactRequest, chatModel)
        }) {
          Text(generalGetString(MR.strings.reject_contact_button), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = Color.Red)
        }
      }
    }
  )
}

fun acceptContactRequest(incognito: Boolean, apiId: Long, contactRequest: ChatInfo.ContactRequest?, isCurrentUser: Boolean, chatModel: ChatModel) {
  withApi {
    val contact = chatModel.controller.apiAcceptContactRequest(incognito, apiId)
    if (contact != null && isCurrentUser && contactRequest != null) {
      val chat = Chat(ChatInfo.Direct(contact), listOf())
      chatModel.replaceChat(contactRequest.id, chat)
    }
  }
}

fun rejectContactRequest(contactRequest: ChatInfo.ContactRequest, chatModel: ChatModel) {
  withApi {
    chatModel.controller.apiRejectContactRequest(contactRequest.apiId)
    chatModel.removeChat(contactRequest.id)
  }
}

fun deleteContactConnectionAlert(connection: PendingContactConnection, chatModel: ChatModel, onSuccess: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.delete_pending_connection__question),
    text = generalGetString(
      if (connection.initiated) MR.strings.contact_you_shared_link_with_wont_be_able_to_connect
      else MR.strings.connection_you_accepted_will_be_cancelled
    ),
    confirmText = generalGetString(MR.strings.delete_verb),
    onConfirm = {
      withApi {
        AlertManager.shared.hideAlert()
        if (chatModel.controller.apiDeleteChat(ChatType.ContactConnection, connection.apiId)) {
          chatModel.removeChat(connection.id)
          onSuccess()
        }
      }
    },
    destructive = true,
  )
}

fun pendingContactAlertDialog(chatInfo: ChatInfo, chatModel: ChatModel) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.alert_title_contact_connection_pending),
    text = generalGetString(MR.strings.alert_text_connection_pending_they_need_to_be_online_can_delete_and_retry),
    confirmText = generalGetString(MR.strings.button_delete_contact),
    onConfirm = {
      withApi {
        val r = chatModel.controller.apiDeleteChat(chatInfo.chatType, chatInfo.apiId)
        if (r) {
          chatModel.removeChat(chatInfo.id)
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

fun acceptGroupInvitationAlertDialog(groupInfo: GroupInfo, chatModel: ChatModel) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.join_group_question),
    text = generalGetString(MR.strings.you_are_invited_to_group_join_to_connect_with_group_members),
    confirmText = if (groupInfo.membership.memberIncognito) generalGetString(MR.strings.join_group_incognito_button) else generalGetString(MR.strings.join_group_button),
    onConfirm = { withApi { chatModel.controller.apiJoinGroup(groupInfo.groupId) } },
    dismissText = generalGetString(MR.strings.delete_verb),
    onDismiss = { deleteGroup(groupInfo, chatModel) }
  )
}

fun cantInviteIncognitoAlert() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.alert_title_cant_invite_contacts),
    text = generalGetString(MR.strings.alert_title_cant_invite_contacts_descr),
    confirmText = generalGetString(MR.strings.ok),
  )
}

fun deleteGroup(groupInfo: GroupInfo, chatModel: ChatModel) {
  withApi {
    val r = chatModel.controller.apiDeleteChat(ChatType.Group, groupInfo.apiId)
    if (r) {
      chatModel.removeChat(groupInfo.id)
      if (chatModel.chatId.value == groupInfo.id) {
        chatModel.chatId.value = null
        ModalManager.end.closeModals()
      }
      ntfManager.cancelNotificationsForChat(groupInfo.id)
    }
  }
}

fun groupInvitationAcceptedAlert() {
  AlertManager.shared.showAlertMsg(
    generalGetString(MR.strings.joining_group),
    generalGetString(MR.strings.youve_accepted_group_invitation_connecting_to_inviting_group_member)
  )
}

fun toggleNotifications(chat: Chat, enableNtfs: Boolean, chatModel: ChatModel, currentState: MutableState<Boolean>? = null) {
  val chatSettings = (chat.chatInfo.chatSettings ?: ChatSettings.defaults).copy(enableNtfs = enableNtfs)
  updateChatSettings(chat, chatSettings, chatModel, currentState)
}

fun toggleChatFavorite(chat: Chat, favorite: Boolean, chatModel: ChatModel) {
  val chatSettings = (chat.chatInfo.chatSettings ?: ChatSettings.defaults).copy(favorite = favorite)
  updateChatSettings(chat, chatSettings, chatModel)
}

fun updateChatSettings(chat: Chat, chatSettings: ChatSettings, chatModel: ChatModel, currentState: MutableState<Boolean>? = null) {
  val newChatInfo = when(chat.chatInfo) {
    is ChatInfo.Direct -> with (chat.chatInfo) {
      ChatInfo.Direct(contact.copy(chatSettings = chatSettings))
    }
    is ChatInfo.Group -> with(chat.chatInfo) {
      ChatInfo.Group(groupInfo.copy(chatSettings = chatSettings))
    }
    else -> null
  }
  withApi {
    val res = when (newChatInfo) {
      is ChatInfo.Direct -> with(newChatInfo) {
        chatModel.controller.apiSetSettings(chatType, apiId, contact.chatSettings)
      }
      is ChatInfo.Group -> with(newChatInfo) {
        chatModel.controller.apiSetSettings(chatType, apiId, groupInfo.chatSettings)
      }
      else -> false
    }
    if (res && newChatInfo != null) {
      chatModel.updateChatInfo(newChatInfo)
      if (!chatSettings.enableNtfs) {
        ntfManager.cancelNotificationsForChat(chat.id)
      }
      val current = currentState?.value
      if (current != null) {
        currentState.value = !current
      }
    }
  }
}

@Composable
fun ChatListNavLinkLayout(
  chatLinkPreview: @Composable () -> Unit,
  click: () -> Unit,
  dropdownMenuItems: (@Composable () -> Unit)?,
  showMenu: MutableState<Boolean>,
  stopped: Boolean
) {
  var modifier = Modifier.fillMaxWidth()
  if (!stopped) modifier = modifier
    .combinedClickable(onClick = click, onLongClick = { showMenu.value = true })
    .onRightClick { showMenu.value = true }
  Box(modifier) {
    Row(
      modifier = Modifier
        .fillMaxWidth()
        .padding(start = 8.dp, top = 8.dp, end = 12.dp, bottom = 8.dp),
      verticalAlignment = Alignment.Top
    ) {
      chatLinkPreview()
    }
    if (dropdownMenuItems != null) {
      DefaultDropdownMenu(showMenu, dropdownMenuItems = dropdownMenuItems)
    }
  }
  Divider(Modifier.padding(horizontal = 8.dp))
}

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
          stopped = false,
          linkMode = SimplexLinkMode.DESCRIPTION
        )
      },
      click = {},
      dropdownMenuItems = null,
      showMenu = remember { mutableStateOf(false) },
      stopped = false
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
          stopped = false,
          linkMode = SimplexLinkMode.DESCRIPTION
        )
      },
      click = {},
      dropdownMenuItems = null,
      showMenu = remember { mutableStateOf(false) },
      stopped = false
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
      stopped = false
    )
  }
}
