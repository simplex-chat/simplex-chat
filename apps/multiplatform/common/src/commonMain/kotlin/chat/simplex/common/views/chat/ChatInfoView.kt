package chat.simplex.common.views.chat

import InfoRow
import InfoRowEllipsis
import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionItemViewSpaceBetween
import SectionSpacer
import SectionTextFooter
import SectionView
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.*
import androidx.compose.ui.text.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.ThemeManager.colorFromReadableHex
import chat.simplex.common.views.chatlist.updateChatSettings
import chat.simplex.common.views.newchat.*
import chat.simplex.res.MR
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.*
import kotlinx.coroutines.launch
import kotlinx.datetime.Clock
import java.util.*

@Composable
fun ChatInfoView(
  chatModel: ChatModel,
  contact: Contact,
  connectionStats: ConnectionStats?,
  customUserProfile: Profile?,
  localAlias: String,
  connectionCode: String?,
  close: () -> Unit,
) {
  BackHandler(onBack = close)
  val contact = rememberUpdatedState(contact).value
  val chat = remember(contact.id) { chatModel.chats.firstOrNull { it.id == contact.id } }
  val currentUser = remember { chatModel.currentUser }.value
  val connStats = remember(contact.id, connectionStats) { mutableStateOf(connectionStats) }
  val developerTools = chatModel.controller.appPrefs.developerTools.get()
  if (chat != null && currentUser != null) {
    val contactNetworkStatus = remember(chatModel.networkStatuses.toMap(), contact) {
      mutableStateOf(chatModel.contactNetworkStatus(contact))
    }
    val chatRh = chat.remoteHostId
    val sendReceipts = remember(contact.id) { mutableStateOf(SendReceipts.fromBool(contact.chatSettings.sendRcpts, currentUser.sendRcptsContacts)) }
    ChatInfoLayout(
      chat,
      contact,
      currentUser,
      sendReceipts = sendReceipts,
      setSendReceipts = { sendRcpts ->
        val chatSettings = (chat.chatInfo.chatSettings ?: ChatSettings.defaults).copy(sendRcpts = sendRcpts.bool)
        updateChatSettings(chat, chatSettings, chatModel)
        sendReceipts.value = sendRcpts
      },
      connStats = connStats,
      contactNetworkStatus.value,
      customUserProfile,
      localAlias,
      connectionCode,
      developerTools,
      onLocalAliasChanged = {
        setContactAlias(chat, it, chatModel)
      },
      openPreferences = {
        ModalManager.end.showCustomModal { close ->
          val user = chatModel.currentUser.value
          if (user != null) {
            ContactPreferencesView(chatModel, user, chatRh, contact.contactId, close)
          }
        }
      },
      deleteContact = { deleteContactDialog(chat, chatModel, close) },
      clearChat = { clearChatDialog(chat, close) },
      switchContactAddress = {
        showSwitchAddressAlert(switchAddress = {
          withBGApi {
            val cStats = chatModel.controller.apiSwitchContact(chatRh, contact.contactId)
            connStats.value = cStats
            if (cStats != null) {
              chatModel.updateContactConnectionStats(chatRh, contact, cStats)
            }
            close.invoke()
          }
        })
      },
      abortSwitchContactAddress = {
        showAbortSwitchAddressAlert(abortSwitchAddress = {
          withBGApi {
            val cStats = chatModel.controller.apiAbortSwitchContact(chatRh, contact.contactId)
            connStats.value = cStats
            if (cStats != null) {
              chatModel.updateContactConnectionStats(chatRh, contact, cStats)
            }
          }
        })
      },
      syncContactConnection = {
        withBGApi {
          val cStats = chatModel.controller.apiSyncContactRatchet(chatRh, contact.contactId, force = false)
          connStats.value = cStats
          if (cStats != null) {
            chatModel.updateContactConnectionStats(chatRh, contact, cStats)
          }
          close.invoke()
        }
      },
      syncContactConnectionForce = {
        showSyncConnectionForceAlert(syncConnectionForce = {
          withBGApi {
            val cStats = chatModel.controller.apiSyncContactRatchet(chatRh, contact.contactId, force = true)
            connStats.value = cStats
            if (cStats != null) {
              chatModel.updateContactConnectionStats(chatRh, contact, cStats)
            }
            close.invoke()
          }
        })
      },
      verifyClicked = {
        ModalManager.end.showModalCloseable { close ->
          remember { derivedStateOf { (chatModel.getContactChat(contact.contactId)?.chatInfo as? ChatInfo.Direct)?.contact } }.value?.let { ct ->
            VerifyCodeView(
              ct.displayName,
              connectionCode,
              ct.verified,
              verify = { code ->
                chatModel.controller.apiVerifyContact(chatRh, ct.contactId, code)?.let { r ->
                  val (verified, existingCode) = r
                  chatModel.updateContact(
                    chatRh,
                    ct.copy(
                      activeConn = ct.activeConn?.copy(
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

sealed class SendReceipts {
  object Yes: SendReceipts()
  object No: SendReceipts()
  data class UserDefault(val enable: Boolean): SendReceipts()

  val text: String get() = when (this) {
    is Yes -> generalGetString(MR.strings.chat_preferences_yes)
    is No -> generalGetString(MR.strings.chat_preferences_no)
    is UserDefault -> String.format(
      generalGetString(MR.strings.chat_preferences_default),
      generalGetString(if (enable) MR.strings.chat_preferences_yes else MR.strings.chat_preferences_no)
    )
  }

  val bool: Boolean? get() = when (this) {
    is Yes -> true
    is No -> false
    is UserDefault -> null
  }

  companion object {
    fun fromBool(enable: Boolean?, userDefault: Boolean): SendReceipts {
      return if (enable == null) UserDefault(userDefault)
      else if (enable) Yes else No
    }
  }
}

fun deleteContactDialog(chat: Chat, chatModel: ChatModel, close: (() -> Unit)? = null) {
  val chatInfo = chat.chatInfo
  AlertManager.shared.showAlertDialogButtonsColumn(
    title = generalGetString(MR.strings.delete_contact_question),
    text = AnnotatedString(generalGetString(MR.strings.delete_contact_all_messages_deleted_cannot_undo_warning)),
    buttons = {
      Column {
        if (chatInfo is ChatInfo.Direct && chatInfo.contact.ready && chatInfo.contact.active) {
          // Delete and notify contact
          SectionItemView({
            AlertManager.shared.hideAlert()
            deleteContact(chat, chatModel, close, notify = true)
          }) {
            Text(generalGetString(MR.strings.delete_and_notify_contact), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.error)
          }
          // Delete
          SectionItemView({
            AlertManager.shared.hideAlert()
            deleteContact(chat, chatModel, close, notify = false)
          }) {
            Text(generalGetString(MR.strings.delete_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.error)
          }
        } else {
          // Delete
          SectionItemView({
            AlertManager.shared.hideAlert()
            deleteContact(chat, chatModel, close)
          }) {
            Text(generalGetString(MR.strings.delete_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.error)
          }
        }
        // Cancel
        SectionItemView({
          AlertManager.shared.hideAlert()
        }) {
          Text(stringResource(MR.strings.cancel_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
      }
    }
  )
}

fun deleteContact(chat: Chat, chatModel: ChatModel, close: (() -> Unit)?, notify: Boolean? = null) {
  val chatInfo = chat.chatInfo
  withBGApi {
    val chatRh = chat.remoteHostId
    val r = chatModel.controller.apiDeleteChat(chatRh, chatInfo.chatType, chatInfo.apiId, notify)
    if (r) {
      chatModel.removeChat(chatRh, chatInfo.id)
      if (chatModel.chatId.value == chatInfo.id) {
        chatModel.chatId.value = null
        ModalManager.end.closeModals()
      }
      ntfManager.cancelNotificationsForChat(chatInfo.id)
      close?.invoke()
    }
  }
}

fun clearChatDialog(chat: Chat, close: (() -> Unit)? = null) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.clear_chat_question),
    text = generalGetString(MR.strings.clear_chat_warning),
    confirmText = generalGetString(MR.strings.clear_verb),
    onConfirm = { controller.clearChat(chat, close) },
    destructive = true,
  )
}

fun clearNoteFolderDialog(chat: Chat, close: (() -> Unit)? = null) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.clear_note_folder_question),
    text = generalGetString(MR.strings.clear_note_folder_warning),
    confirmText = generalGetString(MR.strings.clear_verb),
    onConfirm = { controller.clearChat(chat, close) },
    destructive = true,
  )
}

@Composable
fun ChatInfoLayout(
  chat: Chat,
  contact: Contact,
  currentUser: User,
  sendReceipts: State<SendReceipts>,
  setSendReceipts: (SendReceipts) -> Unit,
  connStats: State<ConnectionStats?>,
  contactNetworkStatus: NetworkStatus,
  customUserProfile: Profile?,
  localAlias: String,
  connectionCode: String?,
  developerTools: Boolean,
  onLocalAliasChanged: (String) -> Unit,
  openPreferences: () -> Unit,
  deleteContact: () -> Unit,
  clearChat: () -> Unit,
  switchContactAddress: () -> Unit,
  abortSwitchContactAddress: () -> Unit,
  syncContactConnection: () -> Unit,
  syncContactConnectionForce: () -> Unit,
  verifyClicked: () -> Unit,
) {
  val cStats = connStats.value
  val scrollState = rememberScrollState()
  val scope = rememberCoroutineScope()
  KeyChangeEffect(chat.id) {
    scope.launch { scrollState.scrollTo(0) }
  }
  ColumnWithScrollBar(
    Modifier
      .fillMaxWidth()
  ) {
    Row(
      Modifier.fillMaxWidth(),
      horizontalArrangement = Arrangement.Center
    ) {
      ChatInfoHeader(chat.chatInfo, contact)
    }

    LocalAliasEditor(chat.id, localAlias, updateValue = onLocalAliasChanged)
    SectionSpacer()
    if (customUserProfile != null) {
      SectionView(generalGetString(MR.strings.incognito).uppercase()) {
        SectionItemViewSpaceBetween {
          Text(generalGetString(MR.strings.incognito_random_profile))
          Text(customUserProfile.chatViewName, color = Indigo)
        }
      }
      SectionDividerSpaced()
    }

    if (contact.ready && contact.active) {
      SectionView {
        if (connectionCode != null) {
          VerifyCodeButton(contact.verified, verifyClicked)
        }
        ContactPreferencesButton(openPreferences)
        SendReceiptsOption(currentUser, sendReceipts, setSendReceipts)
        if (cStats != null && cStats.ratchetSyncAllowed) {
          SynchronizeConnectionButton(syncContactConnection)
        }

        WallpaperButton {
          ModalManager.end.showModal {
            WallpaperEditorModal(chat)
          }
        }
        //      } else if (developerTools) {
        //        SynchronizeConnectionButtonForce(syncContactConnectionForce)
        //      }
      }
      SectionDividerSpaced()
    }

    val conn = contact.activeConn
    if (conn != null) {
      SectionView {
        InfoRow("E2E encryption", if (conn.connPQEnabled) "Quantum resistant" else "Standard")
        SectionDividerSpaced()
      }
    }

    if (contact.contactLink != null) {
      SectionView(stringResource(MR.strings.address_section_title).uppercase()) {
        SimpleXLinkQRCode(contact.contactLink)
        val clipboard = LocalClipboardManager.current
        ShareAddressButton { clipboard.shareText(simplexChatLink(contact.contactLink)) }
        SectionTextFooter(stringResource(MR.strings.you_can_share_this_address_with_your_contacts).format(contact.displayName))
      }
      SectionDividerSpaced()
    }

    if (contact.ready && contact.active) {
      SectionView(title = stringResource(MR.strings.conn_stats_section_title_servers)) {
        SectionItemView({
          AlertManager.shared.showAlertMsg(
            generalGetString(MR.strings.network_status),
            contactNetworkStatus.statusExplanation
          )
        }) {
          NetworkStatusRow(contactNetworkStatus)
        }
        if (cStats != null) {
          SwitchAddressButton(
            disabled = cStats.rcvQueuesInfo.any { it.rcvSwitchStatus != null } || cStats.ratchetSyncSendProhibited,
            switchAddress = switchContactAddress
          )
          if (cStats.rcvQueuesInfo.any { it.rcvSwitchStatus != null }) {
            AbortSwitchAddressButton(
              disabled = cStats.rcvQueuesInfo.any { it.rcvSwitchStatus != null && !it.canAbortSwitch } || cStats.ratchetSyncSendProhibited,
              abortSwitchAddress = abortSwitchContactAddress
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
      SectionDividerSpaced()
    }

    SectionView {
      ClearChatButton(clearChat)
      DeleteContactButton(deleteContact)
    }

    if (developerTools) {
      SectionDividerSpaced()
      SectionView(title = stringResource(MR.strings.section_title_for_console)) {
        InfoRow(stringResource(MR.strings.info_row_local_name), chat.chatInfo.localDisplayName)
        InfoRow(stringResource(MR.strings.info_row_database_id), chat.chatInfo.apiId.toString())
      }
    }
    SectionBottomSpacer()
  }
}

@Composable
fun ChatInfoHeader(cInfo: ChatInfo, contact: Contact) {
  Column(
    Modifier.padding(horizontal = 8.dp),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    ChatInfoImage(cInfo, size = 192.dp, iconColor = if (isInDarkTheme()) GroupDark else SettingsSecondaryLight)
    val text = buildAnnotatedString {
      if (contact.verified) {
        appendInlineContent(id = "shieldIcon")
      }
      append(contact.profile.displayName)
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
    if (cInfo.fullName != "" && cInfo.fullName != cInfo.displayName && cInfo.fullName != contact.profile.displayName) {
      Text(
        cInfo.fullName, style = MaterialTheme.typography.h2,
        color = MaterialTheme.colors.onBackground,
        textAlign = TextAlign.Center,
        maxLines = 4,
        overflow = TextOverflow.Ellipsis
      )
    }
  }
}

@Composable
fun LocalAliasEditor(
  chatId: String,
  initialValue: String,
  center: Boolean = true,
  leadingIcon: Boolean = false,
  focus: Boolean = false,
  updateValue: (String) -> Unit
) {
  val state = remember(chatId) {
    mutableStateOf(TextFieldValue(initialValue))
  }
  var updatedValueAtLeastOnce = remember { false }
  val modifier = if (center)
    Modifier.padding(horizontal = if (!leadingIcon) DEFAULT_PADDING else 0.dp).widthIn(min = 100.dp)
  else
    Modifier.padding(horizontal = if (!leadingIcon) DEFAULT_PADDING else 0.dp).fillMaxWidth()
  Row(Modifier.fillMaxWidth(), horizontalArrangement = if (center) Arrangement.Center else Arrangement.Start) {
    DefaultBasicTextField(
      modifier,
      state,
      {
        Text(
          generalGetString(MR.strings.text_field_set_contact_placeholder),
          textAlign = if (center) TextAlign.Center else TextAlign.Start,
          color = MaterialTheme.colors.secondary
        )
      },
      leadingIcon = if (leadingIcon) {
        { Icon(painterResource(MR.images.ic_edit_filled), null, Modifier.padding(start = 7.dp)) }
      } else null,
      color = MaterialTheme.colors.secondary,
      focus = focus,
      textStyle = TextStyle.Default.copy(textAlign = if (state.value.text.isEmpty() || !center) TextAlign.Start else TextAlign.Center),
      keyboardActions = KeyboardActions(onDone = { updateValue(state.value.text) })
    ) {
      state.value = it
      updatedValueAtLeastOnce = true
    }
  }
  LaunchedEffect(chatId) {
    var prevValue = state.value
    snapshotFlow { state.value }
      .distinctUntilChanged()
      .onEach { delay(500) } // wait a little after every new character, don't emit until user stops typing
      .conflate() // get the latest value
      .filter { it == state.value && it != prevValue } // don't process old ones
      .collect {
        updateValue(it.text)
        prevValue = it
      }
  }
  DisposableEffect(chatId) {
    onDispose { if (updatedValueAtLeastOnce) updateValue(state.value.text) } // just in case snapshotFlow will be canceled when user presses Back too fast
  }
}

@Composable
private fun NetworkStatusRow(networkStatus: NetworkStatus) {
  Row(
    Modifier.fillMaxSize(),
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    Row(
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.spacedBy(4.dp)
    ) {
      Text(stringResource(MR.strings.network_status))
      Icon(
        painterResource(MR.images.ic_info),
        stringResource(MR.strings.network_status),
        tint = MaterialTheme.colors.primary
      )
    }

    Row(
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.spacedBy(4.dp)
    ) {
      Text(
        networkStatus.statusString,
        color = MaterialTheme.colors.secondary
      )
      ServerImage(networkStatus)
    }
  }
}

@Composable
private fun ServerImage(networkStatus: NetworkStatus) {
  Box(Modifier.size(18.dp)) {
    when (networkStatus) {
      is NetworkStatus.Connected ->
        Icon(painterResource(MR.images.ic_circle_filled), stringResource(MR.strings.icon_descr_server_status_connected), tint = Color.Green)
      is NetworkStatus.Disconnected ->
        Icon(painterResource(MR.images.ic_pending_filled), stringResource(MR.strings.icon_descr_server_status_disconnected), tint = MaterialTheme.colors.secondary)
      is NetworkStatus.Error ->
        Icon(painterResource(MR.images.ic_error_filled), stringResource(MR.strings.icon_descr_server_status_error), tint = MaterialTheme.colors.secondary)
      else -> Icon(painterResource(MR.images.ic_circle), stringResource(MR.strings.icon_descr_server_status_pending), tint = MaterialTheme.colors.secondary)
    }
  }
}

@Composable
fun SimplexServers(text: String, servers: List<String>) {
  val info = servers.joinToString(separator = ", ") { it.substringAfter("@") }
  val clipboardManager: ClipboardManager = LocalClipboardManager.current
  InfoRowEllipsis(text, info) {
    clipboardManager.setText(AnnotatedString(servers.joinToString(separator = ",")))
    showToast(generalGetString(MR.strings.copied))
  }
}

@Composable
fun SwitchAddressButton(disabled: Boolean, switchAddress: () -> Unit) {
  SectionItemView(switchAddress, disabled = disabled) {
    Text(
      stringResource(MR.strings.switch_receiving_address),
      color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
    )
  }
}

@Composable
fun AbortSwitchAddressButton(disabled: Boolean, abortSwitchAddress: () -> Unit) {
  SectionItemView(abortSwitchAddress, disabled = disabled) {
    Text(
      stringResource(MR.strings.abort_switch_receiving_address),
      color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
    )
  }
}

@Composable
fun SynchronizeConnectionButton(syncConnection: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_sync_problem),
    stringResource(MR.strings.fix_connection),
    click = syncConnection,
    textColor = WarningOrange,
    iconColor = WarningOrange
  )
}

@Composable
fun SynchronizeConnectionButtonForce(syncConnectionForce: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_warning),
    stringResource(MR.strings.renegotiate_encryption),
    click = syncConnectionForce,
    textColor = Color.Red,
    iconColor = Color.Red
  )
}

@Composable
fun VerifyCodeButton(contactVerified: Boolean, onClick: () -> Unit) {
  SettingsActionItem(
    if (contactVerified) painterResource(MR.images.ic_verified_user) else painterResource(MR.images.ic_shield),
    stringResource(if (contactVerified) MR.strings.view_security_code else MR.strings.verify_security_code),
    click = onClick,
    iconColor = MaterialTheme.colors.secondary,
  )
}

@Composable
private fun ContactPreferencesButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_toggle_on),
    stringResource(MR.strings.contact_preferences),
    click = onClick
  )
}

@Composable
private fun SendReceiptsOption(currentUser: User, state: State<SendReceipts>, onSelected: (SendReceipts) -> Unit) {
  val values = remember {
    mutableListOf(SendReceipts.Yes, SendReceipts.No, SendReceipts.UserDefault(currentUser.sendRcptsContacts)).map { it to it.text }
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
fun WallpaperButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_image),
    stringResource(MR.strings.settings_section_title_wallpaper),
    click = onClick
  )
}

@Composable
fun ClearChatButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_settings_backup_restore),
    stringResource(MR.strings.clear_chat_button),
    click = onClick,
    textColor = WarningOrange,
    iconColor = WarningOrange,
  )
}

@Composable
private fun DeleteContactButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_delete),
    stringResource(MR.strings.button_delete_contact),
    click = onClick,
    textColor = Color.Red,
    iconColor = Color.Red,
  )
}

@Composable
fun ShareAddressButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_share_filled),
    stringResource(MR.strings.share_address),
    onClick,
    iconColor = MaterialTheme.colors.primary,
    textColor = MaterialTheme.colors.primary,
  )
}

@Composable
fun ModalData.WallpaperEditorModal(chat: Chat) {
  val initialTheme = remember(CurrentColors.value.base) {
    (chat.chatInfo as? ChatInfo.Direct)?.contact?.uiThemes?.preferredTheme()?.copy(mode = CurrentColors.value.base.mode)
      ?: (chat.chatInfo as? ChatInfo.Group)?.groupInfo?.uiThemes?.preferredTheme()?.copy(mode = CurrentColors.value.base.mode)
      ?: ThemeModeOverride()
  }
  WallpaperEditor(
    initialTheme,
    save = { applyToMode, newTheme ->
    withBGApi {
      var changedThemes: ThemeModeOverrides? = ((chat.chatInfo as? ChatInfo.Direct)?.contact?.uiThemes ?: (chat.chatInfo as? ChatInfo.Group)?.groupInfo?.uiThemes) ?: ThemeModeOverrides()
      val changed = newTheme?.copy(mode = initialTheme.mode, wallpaper = newTheme.wallpaper?.withFilledWallpaperPath())
      changedThemes = if (applyToMode == null) {
        changedThemes?.copy(light = changed?.copy(mode = DefaultThemeMode.LIGHT), dark = changed?.copy(mode = DefaultThemeMode.DARK))
      } else if (changed?.mode == DefaultThemeMode.LIGHT) {
        changedThemes?.copy(light = changed)
      } else {
        changedThemes?.copy(dark = changed)
      }
      changedThemes = if (changedThemes?.light != null || changedThemes?.dark != null) changedThemes else null
      if (controller.apiSetChatUIThemes(chat.remoteHostId, chat.id, changedThemes)) {
        // Remove previous image only after saving
        removeBackgroundImage(initialTheme.wallpaper?.imageFile)
        if (chat.chatInfo is ChatInfo.Direct) {
          chatModel.updateChatInfo(chat.remoteHostId, chat.chatInfo.copy(contact = chat.chatInfo.contact.copy(uiThemes = changedThemes)))
        } else if (chat.chatInfo is ChatInfo.Group) {
          chatModel.updateChatInfo(chat.remoteHostId, chat.chatInfo.copy(groupInfo = chat.chatInfo.groupInfo.copy(uiThemes = changedThemes)))
        }
      }
    }
  },
    setDefaultsToAll = {
      val lightBase = DefaultTheme.LIGHT
      val darkBase = if (CurrentColors.value.base != DefaultTheme.LIGHT) CurrentColors.value.base else if (appPrefs.systemDarkTheme.get() == DefaultTheme.DARK.themeName) DefaultTheme.DARK else DefaultTheme.SIMPLEX
      val modeLight = ThemeModeOverride.withFilledAppDefaults(DefaultThemeMode.LIGHT, lightBase)
      val modeDark = ThemeModeOverride.withFilledAppDefaults(DefaultThemeMode.DARK, darkBase)
      val changedThemes = ThemeModeOverrides(light = modeLight, dark = modeDark)
      if (controller.apiSetChatUIThemes(chat.remoteHostId, chat.id, changedThemes)) {
        // Remove previous image only after saving
        removeBackgroundImage(initialTheme.wallpaper?.imageFile)
        if (chat.chatInfo is ChatInfo.Direct) {
          chatModel.updateChatInfo(chat.remoteHostId, chat.chatInfo.copy(contact = chat.chatInfo.contact.copy(uiThemes = changedThemes)))
        } else if (chat.chatInfo is ChatInfo.Group) {
          chatModel.updateChatInfo(chat.remoteHostId, chat.chatInfo.copy(groupInfo = chat.chatInfo.groupInfo.copy(uiThemes = changedThemes)))
        }
      }
      (chat.chatInfo as? ChatInfo.Direct)?.contact?.uiThemes?.preferredTheme()?.copy(mode = CurrentColors.value.base.mode)
        ?: (chat.chatInfo as? ChatInfo.Group)?.groupInfo?.uiThemes?.preferredTheme()?.copy(mode = CurrentColors.value.base.mode)
        ?: ThemeModeOverride()
    }
  )
}

@Composable
fun ModalData.WallpaperEditor(theme: ThemeModeOverride, save: (applyToMode: DefaultThemeMode?, ThemeModeOverride?) -> Unit, setDefaultsToAll: suspend () -> ThemeModeOverride) {
  ColumnWithScrollBar(
    Modifier
      .fillMaxSize()
  ) {
    val applyToMode = remember { stateGetOrPutNullable("applyToMode") { null as DefaultThemeMode? } }
    var showMore by remember { stateGetOrPut("showMore") { false } }
    val themeModeOverride = remember { stateGetOrPut("themeModeOverride") { theme } }
    val backgroundImageType: State<BackgroundImageType?> = remember { derivedStateOf { BackgroundImageType.from(themeModeOverride.value.wallpaper) } }
    val systemDark = isInDarkTheme()
    val currentTheme by remember(themeModeOverride.value, CurrentColors.collectAsState().value) {
      mutableStateOf(
        ThemeManager.currentColors(systemDark, themeModeOverride.value, chatModel.currentUser.value?.uiThemes, appPreferences.themeOverrides.get())
      )
    }

    AppBarTitle(stringResource(MR.strings.settings_section_title_wallpaper))
    val backgroundImage = remember { derivedStateOf { backgroundImageType.value?.image } }
    val backgroundColor = remember { derivedStateOf { themeModeOverride.value.wallpaper?.background?.colorFromReadableHex() } }
    val tintColor = remember { derivedStateOf { themeModeOverride.value.wallpaper?.tint?.colorFromReadableHex() } }

    fun editColor(name: ThemeColor, initialColor: Color) {
      ModalManager.end.showModalCloseable { close ->
        AppearanceScope.ColorEditor(
          name,
          initialColor,
          CurrentColors.collectAsState().value.base,
          backgroundImageType.value,
          backgroundImage.value,
          backgroundColor.value,
          tintColor.value,
          onColorChange = { color ->
            ThemeManager.applyThemeColor(name, color, themeModeOverride)
            save(applyToMode.value, themeModeOverride.value)
          },
          close
        )
      }
    }

    WallpaperSetupView(
      backgroundImageType.value,
      CurrentColors.collectAsState().value.base,
      backgroundColor.value,
      tintColor.value,
      showPresetSelection = true,
      editColor = { name, initialColor ->
        editColor(name, initialColor)
      },
      onColorChange = { name, color ->
        ThemeManager.applyThemeColor(name, color, themeModeOverride)
        save(applyToMode.value, themeModeOverride.value)
      },
      onTypeChange = { type ->
        ThemeManager.applyBackgroundImage(type, themeModeOverride)
        save(applyToMode.value, themeModeOverride.value)
      }
    )

    SectionSpacer()

    ResetToGlobalThemeButton {
      themeModeOverride.value = ThemeModeOverride()
      save(applyToMode.value, null)
    }

    SetDefaultThemeButton {
      if (applyToMode.value == null) {
        withBGApi {
          themeModeOverride.value = setDefaultsToAll()
        }
      } else {
        val lightBase = DefaultTheme.LIGHT
        val darkBase = if (CurrentColors.value.base != DefaultTheme.LIGHT) CurrentColors.value.base else if (appPrefs.systemDarkTheme.get() == DefaultTheme.DARK.themeName) DefaultTheme.DARK else DefaultTheme.SIMPLEX
        val mode = themeModeOverride.value.mode
        themeModeOverride.value = ThemeModeOverride.withFilledAppDefaults(mode, if (mode == DefaultThemeMode.LIGHT) lightBase else darkBase)
        save(null, themeModeOverride.value)
      }
    }

    SectionSpacer()

    if (!showMore) {
      AdvancedSettingsButton { showMore = true }
    } else {
      val values by remember { mutableStateOf(
        listOf(
          null to generalGetString(MR.strings.chat_theme_apply_to_all_modes),
          DefaultThemeMode.LIGHT to generalGetString(MR.strings.chat_theme_apply_to_light_mode),
          DefaultThemeMode.DARK to generalGetString(MR.strings.chat_theme_apply_to_dark_mode),
        )
      )}
      ExposedDropDownSettingRow(
        generalGetString(MR.strings.chat_theme_apply_to_mode),
        values,
        applyToMode,
        icon = null,
        enabled = remember { mutableStateOf(true) },
        onSelected = {
          applyToMode.value = it
          if (it != null && it != CurrentColors.value.base.mode) {
            val lightBase = DefaultTheme.LIGHT
            val darkBase = if (CurrentColors.value.base != DefaultTheme.LIGHT) CurrentColors.value.base else if (appPrefs.systemDarkTheme.get() == DefaultTheme.DARK.themeName) DefaultTheme.DARK else DefaultTheme.SIMPLEX
            ThemeManager.applyTheme(if (it == DefaultThemeMode.LIGHT) lightBase.themeName else darkBase.themeName, systemDark)
          }
        }
      )
      KeyChangeEffect(theme.mode) {
        themeModeOverride.value = theme
      }

      SectionSpacer()

      AppearanceScope.CustomizeThemeColorsSection(currentTheme) { name, color -> editColor(name, color) }
    }

    SectionBottomSpacer()
  }
}

@Composable
private fun ResetToGlobalThemeButton(onClick: () -> Unit) {
  SectionItemView(onClick) {
    Text(stringResource(MR.strings.chat_theme_reset_to_global_theme), color = MaterialTheme.colors.primary)
  }
}

@Composable
private fun SetDefaultThemeButton(onClick: () -> Unit) {
  SectionItemView(onClick) {
    Text(stringResource(MR.strings.chat_theme_set_default_theme), color = MaterialTheme.colors.primary)
  }
}

@Composable
private fun AdvancedSettingsButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_arrow_downward),
    stringResource(MR.strings.background_image_advanced_settings),
    click = onClick
  )
}

private fun setContactAlias(chat: Chat, localAlias: String, chatModel: ChatModel) = withBGApi {
  val chatRh = chat.remoteHostId
  chatModel.controller.apiSetContactAlias(chatRh, chat.chatInfo.apiId, localAlias)?.let {
    chatModel.updateContact(chatRh, it)
  }
}

fun showSwitchAddressAlert(switchAddress: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.switch_receiving_address_question),
    text = generalGetString(MR.strings.switch_receiving_address_desc),
    confirmText = generalGetString(MR.strings.change_verb),
    onConfirm = switchAddress
  )
}

fun showAbortSwitchAddressAlert(abortSwitchAddress: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.abort_switch_receiving_address_question),
    text = generalGetString(MR.strings.abort_switch_receiving_address_desc),
    confirmText = generalGetString(MR.strings.abort_switch_receiving_address_confirm),
    onConfirm = abortSwitchAddress,
    destructive = true,
  )
}

fun showSyncConnectionForceAlert(syncConnectionForce: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.sync_connection_force_question),
    text = generalGetString(MR.strings.sync_connection_force_desc),
    confirmText = generalGetString(MR.strings.sync_connection_force_confirm),
    onConfirm = syncConnectionForce,
    destructive = true,
  )
}

@Preview
@Composable
fun PreviewChatInfoLayout() {
  SimpleXTheme {
    ChatInfoLayout(
      chat = Chat(
        remoteHostId = null,
        chatInfo = ChatInfo.Direct.sampleData,
        chatItems = arrayListOf()
      ),
      Contact.sampleData,
      User.sampleData,
      sendReceipts = remember { mutableStateOf(SendReceipts.Yes) },
      setSendReceipts = {},
      localAlias = "",
      connectionCode = "123",
      developerTools = false,
      connStats = remember { mutableStateOf(null) },
      contactNetworkStatus = NetworkStatus.Connected(),
      onLocalAliasChanged = {},
      customUserProfile = null,
      openPreferences = {},
      deleteContact = {},
      clearChat = {},
      switchContactAddress = {},
      abortSwitchContactAddress = {},
      syncContactConnection = {},
      syncContactConnectionForce = {},
      verifyClicked = {},
    )
  }
}
