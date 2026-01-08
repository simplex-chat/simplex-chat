package chat.simplex.common.views.chat.group

import SectionBottomSpacer
import SectionDividerSpaced
import SectionTextFooter
import SectionView
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.graphics.Color
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.chatlist.updateChatSettings
import chat.simplex.common.views.usersettings.SettingsActionItem
import chat.simplex.common.views.usersettings.SettingsActionItemWithContent
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.StringResource
import kotlinx.coroutines.*

@Composable
fun GroupSettingsView(
  m: ChatModel,
  rhId: Long?,
  chatId: String,
  close: () -> Unit
) {
  val chat = remember { derivedStateOf { m.getChat(chatId) } }
  val c = chat.value
  if (c == null || c.chatInfo !is ChatInfo.Group) return
  
  val groupInfo = c.chatInfo.groupInfo
  val currentUser = m.currentUser.value ?: return
  
  val sendReceipts = remember { mutableStateOf(SendReceipts.fromBool(groupInfo.chatSettings.sendRcpts, currentUser.sendRcptsSmallGroups)) }
  val chatItemTTL = rememberSaveable(groupInfo.id) { mutableStateOf(if (groupInfo.chatItemTTL != null) ChatItemTTL.fromSeconds(groupInfo.chatItemTTL) else null) }
  val deletingItems = rememberSaveable(groupInfo.id) { mutableStateOf(false) }

  fun setSendReceipts(sendRcpts: SendReceipts) {
    val chatSettings = (c.chatInfo.chatSettings ?: ChatSettings.defaults).copy(sendRcpts = sendRcpts.bool)
    updateChatSettings(c.remoteHostId, c.chatInfo, chatSettings, m)
    sendReceipts.value = sendRcpts
  }

  fun setChatItemTTL(ttl: ChatItemTTL?) {
    if (ttl == chatItemTTL.value) return
    val previousChatTTL = chatItemTTL.value
    chatItemTTL.value = ttl
    setChatTTLAlert(m.chatsContext, c.remoteHostId, c.chatInfo, chatItemTTL, previousChatTTL, deletingItems)
  }

  ModalView(close = close) {
    GroupSettingsLayout(
      chat = c,
      groupInfo = groupInfo,
      currentUser = currentUser,
      sendReceipts = sendReceipts,
      setSendReceipts = ::setSendReceipts,
      chatItemTTL = chatItemTTL,
      setChatItemTTL = ::setChatItemTTL,
      deletingItems = deletingItems,
      editGroupProfile = {
        ModalManager.end.showCustomModal { closeModal -> 
          GroupProfileView(rhId, groupInfo, m, closeModal)
        }
      },
      addOrEditWelcomeMessage = {
        ModalManager.end.showCustomModal { closeModal -> 
          GroupWelcomeView(m, rhId, groupInfo, closeModal)
        }
      },
      openPreferences = {
        ModalManager.end.showCustomModal { closeModal ->
          GroupPreferencesView(m, rhId, chatId, closeModal)
        }
      },
      clearChat = { clearChatDialog(c, close) },
      deleteGroup = { deleteGroupDialog(c, groupInfo, m, close) },
      leaveGroup = { leaveGroupDialog(rhId, groupInfo, m, close) },
      close = close
    )
  }
}

@Composable
private fun GroupSettingsLayout(
  chat: Chat,
  groupInfo: GroupInfo,
  currentUser: User,
  sendReceipts: MutableState<SendReceipts>,
  setSendReceipts: (SendReceipts) -> Unit,
  chatItemTTL: MutableState<ChatItemTTL?>,
  setChatItemTTL: (ChatItemTTL?) -> Unit,
  deletingItems: MutableState<Boolean>,
  editGroupProfile: () -> Unit,
  addOrEditWelcomeMessage: () -> Unit,
  openPreferences: () -> Unit,
  clearChat: () -> Unit,
  deleteGroup: () -> Unit,
  leaveGroup: () -> Unit,
  close: () -> Unit
) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.icon_descr_settings))

    SectionView {
      if (groupInfo.isOwner && groupInfo.businessChat?.chatType == null) {
        EditGroupProfileButton(editGroupProfile)
      }
      if (groupInfo.groupProfile.description != null || (groupInfo.isOwner && groupInfo.businessChat?.chatType == null)) {
        AddOrEditWelcomeMessage(groupInfo.groupProfile.description, addOrEditWelcomeMessage)
      }
      val prefsTitleId = if (groupInfo.businessChat == null) MR.strings.group_preferences else MR.strings.chat_preferences
      GroupPreferencesButton(prefsTitleId, openPreferences)
    }
    val footerId = if (groupInfo.businessChat == null) MR.strings.only_group_owners_can_change_prefs else MR.strings.only_chat_owners_can_change_prefs
    SectionTextFooter(stringResource(footerId))
    SectionDividerSpaced(maxTopPadding = true, maxBottomPadding = false)

    SectionView {
      val activeSortedMembers = remember { chatModel.groupMembers }.value
        .filter { it.memberStatus != GroupMemberStatus.MemLeft && it.memberStatus != GroupMemberStatus.MemRemoved }
      
      if (activeSortedMembers.filter { it.memberCurrent }.size <= SMALL_GROUPS_RCPS_MEM_LIMIT) {
        SendReceiptsOption(currentUser, sendReceipts, setSendReceipts)
      } else {
        SendReceiptsOptionDisabled()
      }
      WallpaperButton {
        ModalManager.end.showModal {
          val chatState = remember { derivedStateOf { chatModel.chats.value.firstOrNull { it.id == chat.id } } }
          val currentChat = chatState.value
          if (currentChat != null) {
            ChatWallpaperEditorModal(currentChat)
          }
        }
      }
    }
    SectionDividerSpaced(maxTopPadding = true, maxBottomPadding = false)

    SectionView {
      ChatTTLOption(chatItemTTL, setChatItemTTL, deletingItems)
      SectionTextFooter(stringResource(MR.strings.chat_ttl_options_footer))
    }
    SectionDividerSpaced(maxTopPadding = true, maxBottomPadding = false)

    SectionView {
      ClearChatButton(clearChat)
      if (groupInfo.canDelete) {
        val titleId = if (groupInfo.businessChat == null) MR.strings.button_delete_group else MR.strings.button_delete_chat
        DeleteGroupButton(titleId, deleteGroup)
      }
      if (groupInfo.membership.memberCurrentOrPending) {
        val titleId = if (groupInfo.businessChat == null) MR.strings.button_leave_group else MR.strings.button_leave_chat
        LeaveGroupButton(titleId, leaveGroup)
      }
    }

    SectionBottomSpacer()
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
private fun SendReceiptsOptionDisabled() {
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
    androidx.compose.material.Text(generalGetString(MR.strings.send_receipts_disabled), color = MaterialTheme.colors.secondary)
  }
}

@Composable
private fun WallpaperButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_image),
    stringResource(MR.strings.settings_section_title_chat_theme),
    click = onClick
  )
}

@Composable
private fun ClearChatButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_settings_backup_restore),
    stringResource(MR.strings.clear_chat_button),
    click = onClick,
    textColor = WarningOrange,
    iconColor = WarningOrange,
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

