package chat.simplex.common.views.chat

import InfoRow
import InfoRowEllipsis
import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionSpacer
import SectionTextFooter
import SectionView
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.KeyboardActions
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import com.icerockdev.library.MR
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.QRCode
import chat.simplex.common.views.usersettings.*
import chat.simplex.common.platform.*
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.*
import kotlinx.datetime.Clock

@Composable
fun ChatInfoView(
  chatModel: ChatModel,
  contact: Contact,
  connStats: ConnectionStats?,
  customUserProfile: Profile?,
  localAlias: String,
  connectionCode: String?,
  close: () -> Unit,
) {
  BackHandler(onBack = close)
  val chat = chatModel.chats.firstOrNull { it.id == chatModel.chatId.value }
  val developerTools = chatModel.controller.appPrefs.developerTools.get()
  if (chat != null) {
    val contactNetworkStatus = remember(chatModel.networkStatuses.toMap()) {
      mutableStateOf(chatModel.contactNetworkStatus(contact))
    }
    ChatInfoLayout(
      chat,
      contact,
      connStats,
      contactNetworkStatus.value,
      customUserProfile,
      localAlias,
      connectionCode,
      developerTools,
      onLocalAliasChanged = {
        setContactAlias(chat.chatInfo.apiId, it, chatModel)
      },
      openPreferences = {
        ModalManager.shared.showCustomModal { close ->
          val user = chatModel.currentUser.value
          if (user != null) {
            ContactPreferencesView(chatModel, user, contact.contactId, close)
          }
        }
      },
      deleteContact = { deleteContactDialog(chat.chatInfo, chatModel, close) },
      clearChat = { clearChatDialog(chat.chatInfo, chatModel, close) },
      switchContactAddress = {
        showSwitchContactAddressAlert(chatModel, contact.contactId)
      },
      verifyClicked = {
        ModalManager.shared.showModalCloseable { close ->
          remember { derivedStateOf { (chatModel.getContactChat(contact.contactId)?.chatInfo as? ChatInfo.Direct)?.contact } }.value?.let { ct ->
            VerifyCodeView(
              ct.displayName,
              connectionCode,
              ct.verified,
              verify = { code ->
                chatModel.controller.apiVerifyContact(ct.contactId, code)?.let { r ->
                  val (verified, existingCode) = r
                  chatModel.updateContact(
                    ct.copy(
                      activeConn = ct.activeConn.copy(
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

fun deleteContactDialog(chatInfo: ChatInfo, chatModel: ChatModel, close: (() -> Unit)? = null) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.delete_contact_question),
    text = generalGetString(MR.strings.delete_contact_all_messages_deleted_cannot_undo_warning),
    confirmText = generalGetString(MR.strings.delete_verb),
    onConfirm = {
      withApi {
        val r = chatModel.controller.apiDeleteChat(chatInfo.chatType, chatInfo.apiId)
        if (r) {
          chatModel.removeChat(chatInfo.id)
          chatModel.chatId.value = null
          ntfManager.cancelNotificationsForChat(chatInfo.id)
          close?.invoke()
        }
      }
    },
    destructive = true,
  )
}

fun clearChatDialog(chatInfo: ChatInfo, chatModel: ChatModel, close: (() -> Unit)? = null) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.clear_chat_question),
    text = generalGetString(MR.strings.clear_chat_warning),
    confirmText = generalGetString(MR.strings.clear_verb),
    onConfirm = {
      withApi {
        val updatedChatInfo = chatModel.controller.apiClearChat(chatInfo.chatType, chatInfo.apiId)
        if (updatedChatInfo != null) {
          chatModel.clearChat(updatedChatInfo)
          ntfManager.cancelNotificationsForChat(chatInfo.id)
          close?.invoke()
        }
      }
    },
    destructive = true,
  )
}

@Composable
fun ChatInfoLayout(
  chat: Chat,
  contact: Contact,
  connStats: ConnectionStats?,
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
  verifyClicked: () -> Unit,
) {
  Column(
    Modifier
      .fillMaxWidth()
      .verticalScroll(rememberScrollState())
  ) {
    Row(
      Modifier.fillMaxWidth(),
      horizontalArrangement = Arrangement.Center
    ) {
      ChatInfoHeader(chat.chatInfo, contact)
    }

    LocalAliasEditor(localAlias, updateValue = onLocalAliasChanged)
    SectionSpacer()
    if (customUserProfile != null) {
      SectionView(generalGetString(MR.strings.incognito).uppercase()) {
        InfoRow(generalGetString(MR.strings.incognito_random_profile), customUserProfile.chatViewName)
      }
      SectionDividerSpaced()
    }

    SectionView {
      if (connectionCode != null) {
        VerifyCodeButton(contact.verified, verifyClicked)
      }
      ContactPreferencesButton(openPreferences)
    }

    SectionDividerSpaced()
    if (contact.contactLink != null) {
      SectionView(stringResource(MR.strings.address_section_title).uppercase()) {
        QRCode(contact.contactLink, Modifier.padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF).aspectRatio(1f))
        val clipboard = LocalClipboardManager.current
        ShareAddressButton { clipboard.shareText(contact.contactLink) }
        SectionTextFooter(stringResource(MR.strings.you_can_share_this_address_with_your_contacts).format(contact.displayName))
      }
      SectionDividerSpaced()
    }

    SectionView(title = stringResource(MR.strings.conn_stats_section_title_servers)) {
      SwitchAddressButton(switchContactAddress)
      if (connStats != null) {
        SectionItemView({
          AlertManager.shared.showAlertMsg(
            generalGetString(MR.strings.network_status),
            contactNetworkStatus.statusExplanation
          )}) {
          NetworkStatusRow(contactNetworkStatus)
        }
        val rcvServers = connStats.rcvServers
        if (rcvServers != null && rcvServers.isNotEmpty()) {
          SimplexServers(stringResource(MR.strings.receiving_via), rcvServers)
        }
        val sndServers = connStats.sndServers
        if (sndServers != null && sndServers.isNotEmpty()) {
          SimplexServers(stringResource(MR.strings.sending_via), sndServers)
        }
      }
    }
    SectionDividerSpaced()
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
    Row(Modifier.padding(bottom = 8.dp), verticalAlignment = Alignment.CenterVertically) {
      if (contact.verified) {
        Icon(painterResource(MR.images.ic_verified_user), null, Modifier.padding(end = 6.dp, top = 4.dp).size(24.dp), tint = MaterialTheme.colors.secondary)
      }
      Text(
        contact.profile.displayName, style = MaterialTheme.typography.h1.copy(fontWeight = FontWeight.Normal),
        color = MaterialTheme.colors.onBackground,
        maxLines = 1,
        overflow = TextOverflow.Ellipsis,
      )
    }
    if (cInfo.fullName != "" && cInfo.fullName != cInfo.displayName && cInfo.fullName != contact.profile.displayName) {
      Text(
        cInfo.fullName, style = MaterialTheme.typography.h2,
        color = MaterialTheme.colors.onBackground,
        maxLines = 2,
        overflow = TextOverflow.Ellipsis
      )
    }
  }
}

@Composable
fun LocalAliasEditor(
  initialValue: String,
  center: Boolean = true,
  leadingIcon: Boolean = false,
  focus: Boolean = false,
  updateValue: (String) -> Unit
) {
  var value by rememberSaveable { mutableStateOf(initialValue) }
  val modifier = if (center)
    Modifier.padding(horizontal = if (!leadingIcon) DEFAULT_PADDING else 0.dp).widthIn(min = 100.dp)
  else
    Modifier.padding(horizontal = if (!leadingIcon) DEFAULT_PADDING else 0.dp).fillMaxWidth()
  Row(Modifier.fillMaxWidth(), horizontalArrangement = if (center) Arrangement.Center else Arrangement.Start) {
    DefaultBasicTextField(
      modifier,
      value,
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
      textStyle = TextStyle.Default.copy(textAlign = if (value.isEmpty() || !center) TextAlign.Start else TextAlign.Center),
      keyboardActions = KeyboardActions(onDone = { updateValue(value) })
    ) {
      value = it
    }
  }
  LaunchedEffect(Unit) {
    snapshotFlow { value }
      .onEach { delay(500) } // wait a little after every new character, don't emit until user stops typing
      .conflate() // get the latest value
      .filter { it == value } // don't process old ones
      .collect {
        updateValue(value)
      }
  }
  DisposableEffect(Unit) {
    onDispose { updateValue(value) } // just in case snapshotFlow will be canceled when user presses Back too fast
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
        Icon(painterResource(MR.images.ic_circle_filled), stringResource(MR.strings.icon_descr_server_status_connected), tint = MaterialTheme.colors.primaryVariant)
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
fun SwitchAddressButton(onClick: () -> Unit) {
  SectionItemView(onClick) {
    Text(stringResource(MR.strings.switch_receiving_address), color = MaterialTheme.colors.primary)
  }
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

private fun setContactAlias(contactApiId: Long, localAlias: String, chatModel: ChatModel) = withApi {
  chatModel.controller.apiSetContactAlias(contactApiId, localAlias)?.let {
    chatModel.updateContact(it)
  }
}

private fun showSwitchContactAddressAlert(m: ChatModel, contactId: Long) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.switch_receiving_address_question),
    text = generalGetString(MR.strings.switch_receiving_address_desc),
    confirmText = generalGetString(MR.strings.switch_verb),
    onConfirm = {
      switchContactAddress(m, contactId)
    },
    destructive = true,
  )
}

private fun switchContactAddress(m: ChatModel, contactId: Long) = withApi {
  m.controller.apiSwitchContact(contactId)
}

@Preview
@Composable
fun PreviewChatInfoLayout() {
  SimpleXTheme {
    ChatInfoLayout(
      chat = Chat(
        chatInfo = ChatInfo.Direct.sampleData,
        chatItems = arrayListOf()
      ),
      Contact.sampleData,
      localAlias = "",
      connectionCode = "123",
      developerTools = false,
      connStats = null,
      contactNetworkStatus = NetworkStatus.Connected(),
      onLocalAliasChanged = {},
      customUserProfile = null,
      openPreferences = {},
      deleteContact = {},
      clearChat = {},
      switchContactAddress = {},
      verifyClicked = {},
    )
  }
}
