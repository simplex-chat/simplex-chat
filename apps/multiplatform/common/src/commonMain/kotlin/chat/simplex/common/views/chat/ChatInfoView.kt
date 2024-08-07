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
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.text.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.platform.*
import chat.simplex.common.views.call.CallMediaType
import chat.simplex.common.views.chatlist.*
import androidx.compose.ui.text.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.text.intl.Locale
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.model.ChatModel.withChats
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.chatlist.updateChatSettings
import chat.simplex.common.views.newchat.*
import chat.simplex.res.MR
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.*
import kotlinx.coroutines.launch
import kotlinx.datetime.Clock
import kotlinx.serialization.encodeToString
import java.io.File

@Composable
fun ChatInfoView(
  chatModel: ChatModel,
  contact: Contact,
  connectionStats: ConnectionStats?,
  customUserProfile: Profile?,
  localAlias: String,
  connectionCode: String?,
  close: () -> Unit,
  onSearchClicked: () -> Unit
) {
  BackHandler(onBack = close)
  val contact = rememberUpdatedState(contact).value
  val chat = remember(contact.id) { chatModel.chats.value.firstOrNull { it.id == contact.id } }
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
        updateChatSettings(chat.remoteHostId, chat.chatInfo, chatSettings, chatModel)
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
              withChats {
                updateContactConnectionStats(chatRh, contact, cStats)
              }
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
              withChats {
                updateContactConnectionStats(chatRh, contact, cStats)
              }
            }
          }
        })
      },
      syncContactConnection = {
        withBGApi {
          val cStats = chatModel.controller.apiSyncContactRatchet(chatRh, contact.contactId, force = false)
          connStats.value = cStats
          if (cStats != null) {
            withChats {
              updateContactConnectionStats(chatRh, contact, cStats)
            }
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
              withChats {
                updateContactConnectionStats(chatRh, contact, cStats)
              }
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
                  withChats {
                    updateContact(
                      chatRh,
                      ct.copy(
                        activeConn = ct.activeConn?.copy(
                          connectionCode = if (verified) SecurityCode(existingCode, Clock.System.now()) else null
                        )
                      )
                    )
                  }
                  r
                }
              },
              close,
            )
          }
        }
      },
      close = close,
      onSearchClicked = onSearchClicked
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
  if (chatInfo is ChatInfo.Direct) {
    val contact = chatInfo.contact
    when {
      contact.sndReady && contact.active && !chatInfo.chatDeleted ->
        deleteContactOrConversationDialog(chat, contact, chatModel, close)

      contact.sndReady && contact.active && chatInfo.chatDeleted ->
        deleteContactWithoutConversation(chat, chatModel, close)

      else -> // !(contact.sndReady && contact.active)
        deleteNotReadyContact(chat, chatModel, close)
    }
  }
}

private fun deleteContactOrConversationDialog(chat: Chat, contact: Contact, chatModel: ChatModel, close: (() -> Unit)?) {
  AlertManager.shared.showAlertDialogButtonsColumn(
    title = generalGetString(MR.strings.delete_contact_question),
    buttons = {
      Column {
        // Only delete conversation
        SectionItemView({
          AlertManager.shared.hideAlert()
          deleteContact(chat, chatModel, close, chatDeleteMode = ChatDeleteMode.Messages())
          if (chatModel.controller.appPrefs.showDeleteConversationNotice.get()) {
            showDeleteConversationNotice(contact)
          }
        }) {
          Text(generalGetString(MR.strings.only_delete_conversation), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.error)
        }
        // Delete contact
        SectionItemView({
          AlertManager.shared.hideAlert()
          deleteActiveContactDialog(chat, contact, chatModel, close)
        }) {
          Text(generalGetString(MR.strings.button_delete_contact), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.error)
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

private fun showDeleteConversationNotice(contact: Contact) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.conversation_deleted),
    text = String.format(generalGetString(MR.strings.you_can_still_send_messages_to_contact), contact.displayName),
    confirmText = generalGetString(MR.strings.ok),
    dismissText = generalGetString(MR.strings.dont_show_again),
    onDismiss = {
      chatModel.controller.appPrefs.showDeleteConversationNotice.set(false)
    },
  )
}

sealed class ContactDeleteMode {
  class Full: ContactDeleteMode()
  class Entity: ContactDeleteMode()

  fun toChatDeleteMode(notify: Boolean): ChatDeleteMode =
    when (this) {
      is Full -> ChatDeleteMode.Full(notify)
      is Entity -> ChatDeleteMode.Entity(notify)
    }
}

private fun deleteActiveContactDialog(chat: Chat, contact: Contact, chatModel: ChatModel, close: (() -> Unit)? = null) {
  val contactDeleteMode = mutableStateOf<ContactDeleteMode>(ContactDeleteMode.Full())

  AlertManager.shared.showAlertDialogButtonsColumn(
    title = generalGetString(MR.strings.delete_contact_question),
    text = generalGetString(MR.strings.delete_contact_cannot_undo_warning),
    buttons = {
      Column {
        // Keep conversation toggle
        SectionItemView {
          Row(
            Modifier.fillMaxWidth(),
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.Center
          ) {
            Text(stringResource(MR.strings.keep_conversation))
            Spacer(Modifier.width(DEFAULT_PADDING))
            DefaultSwitch(
              checked = contactDeleteMode.value is ContactDeleteMode.Entity,
              onCheckedChange = {
                contactDeleteMode.value =
                  if (it) ContactDeleteMode.Entity() else ContactDeleteMode.Full()
              },
            )
          }
        }
        // Delete without notification
        SectionItemView({
          AlertManager.shared.hideAlert()
          deleteContact(chat, chatModel, close, chatDeleteMode = contactDeleteMode.value.toChatDeleteMode(notify = false))
          if (contactDeleteMode.value is ContactDeleteMode.Entity && chatModel.controller.appPrefs.showDeleteContactNotice.get()) {
            showDeleteContactNotice(contact)
          }
        }) {
          Text(generalGetString(MR.strings.delete_without_notification), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.error)
        }
        // Delete contact and notify
        SectionItemView({
          AlertManager.shared.hideAlert()
          deleteContact(chat, chatModel, close, chatDeleteMode = contactDeleteMode.value.toChatDeleteMode(notify = true))
          if (contactDeleteMode.value is ContactDeleteMode.Entity && chatModel.controller.appPrefs.showDeleteContactNotice.get()) {
            showDeleteContactNotice(contact)
          }
        }) {
          Text(generalGetString(MR.strings.delete_and_notify_contact), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.error)
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

private fun deleteContactWithoutConversation(chat: Chat, chatModel: ChatModel, close: (() -> Unit)?) {
  AlertManager.shared.showAlertDialogButtonsColumn(
    title = generalGetString(MR.strings.confirm_delete_contact_question),
    text = generalGetString(MR.strings.delete_contact_cannot_undo_warning),
    buttons = {
      Column {
        // Delete and notify contact
        SectionItemView({
          AlertManager.shared.hideAlert()
          deleteContact(
            chat,
            chatModel,
            close,
            chatDeleteMode = ContactDeleteMode.Full().toChatDeleteMode(notify = true)
          )
        }) {
          Text(
            generalGetString(MR.strings.delete_and_notify_contact),
            Modifier.fillMaxWidth(),
            textAlign = TextAlign.Center,
            color = MaterialTheme.colors.error
          )
        }
        // Delete without notification
        SectionItemView({
          AlertManager.shared.hideAlert()
          deleteContact(
            chat,
            chatModel,
            close,
            chatDeleteMode = ContactDeleteMode.Full().toChatDeleteMode(notify = false)
          )
        }) {
          Text(
            generalGetString(MR.strings.delete_without_notification),
            Modifier.fillMaxWidth(),
            textAlign = TextAlign.Center,
            color = MaterialTheme.colors.error
          )
        }
      }
      // Cancel
      SectionItemView({
        AlertManager.shared.hideAlert()
      }) {
        Text(
          stringResource(MR.strings.cancel_verb),
          Modifier.fillMaxWidth(),
          textAlign = TextAlign.Center,
          color = MaterialTheme.colors.primary
        )
      }
    }
  )
}

private fun deleteNotReadyContact(chat: Chat, chatModel: ChatModel, close: (() -> Unit)?) {
  AlertManager.shared.showAlertDialogButtonsColumn(
    title = generalGetString(MR.strings.confirm_delete_contact_question),
    text = generalGetString(MR.strings.delete_contact_cannot_undo_warning),
    buttons = {
      // Confirm
      SectionItemView({
        AlertManager.shared.hideAlert()
        deleteContact(
          chat,
          chatModel,
          close,
          chatDeleteMode = ContactDeleteMode.Full().toChatDeleteMode(notify = false)
        )
      }) {
        Text(
          generalGetString(MR.strings.confirm_verb),
          Modifier.fillMaxWidth(),
          textAlign = TextAlign.Center,
          color = MaterialTheme.colors.error
        )
      }
      // Cancel
      SectionItemView({
        AlertManager.shared.hideAlert()
      }) {
        Text(
          stringResource(MR.strings.cancel_verb),
          Modifier.fillMaxWidth(),
          textAlign = TextAlign.Center,
          color = MaterialTheme.colors.primary
        )
      }
    }
  )
}

private fun showDeleteContactNotice(contact: Contact) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.contact_deleted),
    text = String.format(generalGetString(MR.strings.you_can_still_view_conversation_with_contact), contact.displayName),
    confirmText = generalGetString(MR.strings.ok),
    dismissText = generalGetString(MR.strings.dont_show_again),
    onDismiss = {
      chatModel.controller.appPrefs.showDeleteContactNotice.set(false)
    },
  )
}

fun deleteContact(chat: Chat, chatModel: ChatModel, close: (() -> Unit)?, chatDeleteMode: ChatDeleteMode = ChatDeleteMode.Full(notify = true)) {
  val chatInfo = chat.chatInfo
  withBGApi {
    val chatRh = chat.remoteHostId
    val ct = chatModel.controller.apiDeleteContact(chatRh, chatInfo.apiId, chatDeleteMode)
    if (ct != null) {
      withChats {
        when (chatDeleteMode) {
          is ChatDeleteMode.Full ->
            removeChat(chatRh, chatInfo.id)
          is ChatDeleteMode.Entity ->
            updateContact(chatRh, ct)
          is ChatDeleteMode.Messages ->
            clearChat(chatRh, ChatInfo.Direct(ct))
        }
      }
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
  close: () -> Unit,
  onSearchClicked: () -> Unit
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

    BoxWithConstraints(
      modifier = Modifier.fillMaxWidth()
    ) {
      val computedPadding = (maxWidth - INFO_VIEW_BUTTON_SIZE * 4) / 5
      val padding = min(computedPadding, INFO_VIEW_BUTTONS_MAX_PADDING)

      Row(
        Modifier
          .fillMaxWidth()
          .padding(horizontal = DEFAULT_PADDING),
        horizontalArrangement = Arrangement.Center,
        verticalAlignment = Alignment.CenterVertically
      ) {
        Spacer(Modifier.weight(1f))
        SearchButton(chat, contact, close, onSearchClicked)
        Spacer(Modifier.width(padding))
        AudioCallButton(chat, contact)
        Spacer(Modifier.width(padding))
        VideoButton(chat, contact)
        Spacer(Modifier.width(padding))
        MuteButton(chat, contact)
        Spacer(Modifier.weight(1f))
      }
    }

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

    SectionView {
      if (contact.ready && contact.active) {
        if (connectionCode != null) {
          VerifyCodeButton(contact.verified, verifyClicked)
        }
        ContactPreferencesButton(openPreferences)
        SendReceiptsOption(currentUser, sendReceipts, setSendReceipts)
        if (cStats != null && cStats.ratchetSyncAllowed) {
          SynchronizeConnectionButton(syncContactConnection)
        }
        // } else if (developerTools) {
        //   SynchronizeConnectionButtonForce(syncContactConnectionForce)
        // }
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
    SectionDividerSpaced()

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
        SectionItemView({
          withBGApi {
            val info = controller.apiContactQueueInfo(chat.remoteHostId, chat.chatInfo.apiId)
            if (info != null) {
              AlertManager.shared.showAlertMsg(
                title = generalGetString(MR.strings.message_queue_info),
                text = queueInfoText(info)
              )
            }
          }
        }) {
          Text(stringResource(MR.strings.info_row_debug_delivery))
        }
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
fun SearchButton(chat: Chat, contact: Contact, close: () -> Unit, onSearchClicked: () -> Unit) {
  val disabled = !contact.ready || chat.chatItems.isEmpty()
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
fun MuteButton(chat: Chat, contact: Contact) {
  val ntfsEnabled = remember { mutableStateOf(chat.chatInfo.ntfsEnabled) }
  val disabled = !contact.ready || !contact.active

  InfoViewActionButton(
    icon =  if (ntfsEnabled.value) painterResource(MR.images.ic_notifications_off) else painterResource(MR.images.ic_notifications),
    title = if (ntfsEnabled.value) stringResource(MR.strings.mute_chat) else stringResource(MR.strings.unmute_chat),
    disabled = disabled,
    disabledLook = disabled,
    onClick = {
      toggleNotifications(chat.remoteHostId, chat.chatInfo, !ntfsEnabled.value, chatModel, ntfsEnabled)
    }
  )
}

@Composable
fun AudioCallButton(chat: Chat, contact: Contact) {
  CallButton(
    chat,
    contact,
    icon = painterResource(MR.images.ic_call),
    title = generalGetString(MR.strings.info_view_call_button),
    mediaType = CallMediaType.Audio
  )
}

@Composable
fun VideoButton(chat: Chat, contact: Contact) {
  CallButton(
    chat,
    contact,
    icon = painterResource(MR.images.ic_videocam),
    title = generalGetString(MR.strings.info_view_video_button),
    mediaType = CallMediaType.Video
  )
}

@Composable
fun CallButton(chat: Chat, contact: Contact, icon: Painter, title: String, mediaType: CallMediaType) {
  val canCall = contact.ready && contact.active && contact.mergedPreferences.calls.enabled.forUser && chatModel.activeCall.value == null
  val needToAllowCallsToContact = remember(chat.chatInfo) {
    chat.chatInfo is ChatInfo.Direct && with(chat.chatInfo.contact.mergedPreferences.calls) {
      ((userPreference as? ContactUserPref.User)?.preference?.allow == FeatureAllowed.NO || (userPreference as? ContactUserPref.Contact)?.preference?.allow == FeatureAllowed.NO) &&
          contactPreference.allow == FeatureAllowed.YES
    }
  }
  val allowedCallsByPrefs = remember(chat.chatInfo) { chat.chatInfo.featureEnabled(ChatFeature.Calls) }

  InfoViewActionButton(
    icon = icon,
    title = title,
    disabled = chatModel.activeCall.value != null,
    disabledLook = !canCall,
    onClick =
      when {
        canCall -> { { startChatCall(chat.remoteHostId, chat.chatInfo, mediaType) } }
        contact.nextSendGrpInv -> { { showCantCallContactSendMessageAlert() } }
        !contact.active -> { { showCantCallContactDeletedAlert() } }
        !contact.ready -> { { showCantCallContactConnectingAlert() } }
        needToAllowCallsToContact -> { { showNeedToAllowCallsAlert(onConfirm = { allowCallsToContact(chat) }) } }
        !allowedCallsByPrefs -> { { showCallsProhibitedAlert() }}
        else -> { { AlertManager.shared.showAlertMsg(title = generalGetString(MR.strings.cant_call_contact_alert_title)) } }
      }
  )
}

private fun showCantCallContactSendMessageAlert() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.cant_call_contact_alert_title),
    text = generalGetString(MR.strings.cant_call_member_send_message_alert_text)
  )
}

private fun showCantCallContactConnectingAlert() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.cant_call_contact_alert_title),
    text = generalGetString(MR.strings.cant_call_contact_connecting_wait_alert_text)
  )
}

private fun showCantCallContactDeletedAlert() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.cant_call_contact_alert_title),
    text = generalGetString(MR.strings.cant_call_contact_deleted_alert_text)
  )
}

private fun showNeedToAllowCallsAlert(onConfirm: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.allow_calls_question),
    text = generalGetString(MR.strings.you_need_to_allow_calls),
    confirmText = generalGetString(MR.strings.allow_verb),
    dismissText = generalGetString(MR.strings.cancel_verb),
    onConfirm = onConfirm,
  )
}

private fun allowCallsToContact(chat: Chat) {
  val contact = (chat.chatInfo as ChatInfo.Direct?)?.contact ?: return
  withBGApi {
    chatModel.controller.allowFeatureToContact(chat.remoteHostId, contact, ChatFeature.Calls)
  }
}

private fun showCallsProhibitedAlert() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.calls_prohibited_alert_title),
    text = generalGetString(MR.strings.calls_prohibited_ask_to_enable_calls_alert_text)
  )
}

val INFO_VIEW_BUTTONS_MAX_PADDING = 36.dp
val INFO_VIEW_BUTTON_SIZE = 56.dp

@Composable
fun InfoViewActionButton(icon: Painter, title: String, disabled: Boolean, disabledLook: Boolean, onClick: () -> Unit) {
  Column(
    horizontalAlignment = Alignment.CenterHorizontally,
    verticalArrangement = Arrangement.Center
  ) {
    IconButton(
      onClick = onClick,
      enabled = !disabled
    ) {
      Box(
        modifier = Modifier
          .size(INFO_VIEW_BUTTON_SIZE)
          .background(
            if (disabledLook) MaterialTheme.colors.secondaryVariant else MaterialTheme.colors.primary,
            shape = CircleShape
          ),
        contentAlignment = Alignment.Center
      ) {
        Icon(
          icon,
          contentDescription = null,
          Modifier.size(24.dp * fontSizeSqrtMultiplier),
          tint = if (disabledLook) MaterialTheme.colors.secondary else MaterialTheme.colors.onPrimary
        )
      }
    }
    Text(
      title.capitalize(Locale.current),
      style = MaterialTheme.typography.subtitle2.copy(fontWeight = FontWeight.Normal, fontSize = 13.sp),
      color = MaterialTheme.colors.secondary,
      modifier = Modifier.padding(top = DEFAULT_SPACE_AFTER_ICON)
    )
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
    stringResource(MR.strings.settings_section_title_chat_theme),
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
fun ModalData.ChatWallpaperEditorModal(chat: Chat) {
  val themes = remember(CurrentColors.collectAsState().value.base) {
    (chat.chatInfo as? ChatInfo.Direct)?.contact?.uiThemes
      ?: (chat.chatInfo as? ChatInfo.Group)?.groupInfo?.uiThemes
      ?: ThemeModeOverrides()
  }
  val globalThemeUsed = remember { stateGetOrPut("globalThemeUsed") { false }  }
  val initialTheme = remember(CurrentColors.collectAsState().value.base) {
    val preferred = themes.preferredMode(!CurrentColors.value.colors.isLight)
    globalThemeUsed.value = preferred == null
    preferred ?: ThemeManager.defaultActiveTheme(chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
  }
  ChatWallpaperEditor(
    initialTheme,
    applyToMode = if (themes.light == themes.dark) null else initialTheme.mode,
    globalThemeUsed = globalThemeUsed,
    save = { applyToMode, newTheme ->
      save(applyToMode, newTheme, chatModel.getChat(chat.id) ?: chat)
    })
}

suspend fun save(applyToMode: DefaultThemeMode?, newTheme: ThemeModeOverride?, chat: Chat) {
  val unchangedThemes: ThemeModeOverrides = ((chat.chatInfo as? ChatInfo.Direct)?.contact?.uiThemes ?: (chat.chatInfo as? ChatInfo.Group)?.groupInfo?.uiThemes) ?: ThemeModeOverrides()
  val wallpaperFiles = setOf(unchangedThemes.light?.wallpaper?.imageFile, unchangedThemes.dark?.wallpaper?.imageFile)
  var changedThemes: ThemeModeOverrides? = unchangedThemes
  val changed = newTheme?.copy(wallpaper = newTheme.wallpaper?.withFilledWallpaperPath())
  changedThemes = when (applyToMode) {
    null -> changedThemes?.copy(light = changed?.copy(mode = DefaultThemeMode.LIGHT), dark = changed?.copy(mode = DefaultThemeMode.DARK))
    DefaultThemeMode.LIGHT -> changedThemes?.copy(light = changed?.copy(mode = applyToMode))
    DefaultThemeMode.DARK -> changedThemes?.copy(dark = changed?.copy(mode = applyToMode))
  }
  changedThemes = if (changedThemes?.light != null || changedThemes?.dark != null) {
    val light = changedThemes.light
    val dark = changedThemes.dark
    val currentMode = CurrentColors.value.base.mode
    // same image file for both modes, copy image to make them as different files
    if (light?.wallpaper?.imageFile != null && dark?.wallpaper?.imageFile != null && light.wallpaper.imageFile == dark.wallpaper.imageFile) {
      val imageFile = if (currentMode == DefaultThemeMode.LIGHT) {
        dark.wallpaper.imageFile
      } else {
        light.wallpaper.imageFile
      }
      val filePath = saveWallpaperFile(File(getWallpaperFilePath(imageFile)).toURI())
      changedThemes = if (currentMode == DefaultThemeMode.LIGHT) {
        changedThemes.copy(dark = dark.copy(wallpaper = dark.wallpaper.copy(imageFile = filePath)))
      } else {
        changedThemes.copy(light = light.copy(wallpaper = light.wallpaper.copy(imageFile = filePath)))
      }
    }
    changedThemes
  } else {
    null
  }
  val wallpaperFilesToDelete = wallpaperFiles - changedThemes?.light?.wallpaper?.imageFile - changedThemes?.dark?.wallpaper?.imageFile
  wallpaperFilesToDelete.forEach(::removeWallpaperFile)

  if (controller.apiSetChatUIThemes(chat.remoteHostId, chat.id, changedThemes)) {
    withChats {
      if (chat.chatInfo is ChatInfo.Direct) {
        updateChatInfo(chat.remoteHostId, chat.chatInfo.copy(contact = chat.chatInfo.contact.copy(uiThemes = changedThemes)))
      } else if (chat.chatInfo is ChatInfo.Group) {
        updateChatInfo(chat.remoteHostId, chat.chatInfo.copy(groupInfo = chat.chatInfo.groupInfo.copy(uiThemes = changedThemes)))
      }
    }
  }
}

private fun setContactAlias(chat: Chat, localAlias: String, chatModel: ChatModel) = withBGApi {
  val chatRh = chat.remoteHostId
  chatModel.controller.apiSetContactAlias(chatRh, chat.chatInfo.apiId, localAlias)?.let {
    withChats {
      updateContact(chatRh, it)
    }
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

fun queueInfoText(info: Pair<RcvMsgInfo?, QueueInfo>): String {
  val (rcvMsgInfo, qInfo) = info
  val msgInfo: String = if (rcvMsgInfo != null) json.encodeToString(rcvMsgInfo) else generalGetString(MR.strings.message_queue_info_none)
  return generalGetString(MR.strings.message_queue_info_server_info).format(json.encodeToString(qInfo), msgInfo)
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
      close = {},
      onSearchClicked = {}
    )
  }
}
