package chat.simplex.common.views.remote

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionItemViewLongClickable
import SectionSpacer
import SectionView
import TextIconSpaced
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.snapshots.SnapshotStateList
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.switchToLocalSession
import chat.simplex.common.model.ChatModel.connectedToRemote
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.QRCodeScanner
import chat.simplex.common.views.usersettings.PreferenceToggle
import chat.simplex.common.views.usersettings.SettingsActionItem
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
fun ConnectDesktopView(close: () -> Unit) {
  val deviceName = remember { controller.appPrefs.deviceNameForRemoteAccess.state }
  val closeWithAlert = {
    if (!connectedToRemote()) {
      close()
    } else {
      showDisconnectDesktopAlert(close)
    }
  }
  ModalView(close = closeWithAlert) {
    ConnectDesktopLayout(
      deviceName = deviceName.value!!,
      close
    )
  }
  val ntfModeService = remember { chatModel.controller.appPrefs.notificationsMode.get() == NotificationsMode.SERVICE }
  DisposableEffect(Unit) {
    withBGApi {
      if (!ntfModeService) platform.androidServiceStart()
    }
    onDispose {
      if (!ntfModeService) platform.androidServiceSafeStop()
    }
  }
}

@Composable
private fun ConnectDesktopLayout(deviceName: String, close: () -> Unit) {
  val showConnectScreen = remember { mutableStateOf(true) }
  val sessionAddress = remember { mutableStateOf("") }
  val remoteCtrls = remember { mutableStateListOf<RemoteCtrlInfo>() }
  val session = remember { chatModel.remoteCtrlSession }.value
  ColumnWithScrollBar(
    Modifier.fillMaxWidth(),
  ) {
    val discovery = if (session == null) null else session.sessionState is UIRemoteCtrlSessionState.Searching
    if (discovery == true || (discovery == null && !showConnectScreen.value)) {
      SearchingDesktop(deviceName, remoteCtrls)
    } else if (session != null) {
      when (session.sessionState) {
        is UIRemoteCtrlSessionState.Starting -> ConnectingDesktop(session, null)
        is UIRemoteCtrlSessionState.Searching -> SearchingDesktop(deviceName, remoteCtrls)
        is UIRemoteCtrlSessionState.Found -> FoundDesktop(session, session.sessionState.remoteCtrl, session.sessionState.compatible, remember { controller.appPrefs.connectRemoteViaMulticastAuto.state }, deviceName, remoteCtrls, sessionAddress)
        is UIRemoteCtrlSessionState.Connecting -> ConnectingDesktop(session, session.sessionState.remoteCtrl_)
        is UIRemoteCtrlSessionState.PendingConfirmation -> {
          if (controller.appPrefs.confirmRemoteSessions.get() || session.sessionState.remoteCtrl_ == null) {
            VerifySession(session, session.sessionState.remoteCtrl_, session.sessionCode!!, remoteCtrls)
          } else {
            ConnectingDesktop(session, session.sessionState.remoteCtrl_)
            LaunchedEffect(Unit) {
              verifyDesktopSessionCode(remoteCtrls, session.sessionCode!!)
            }
          }
        }

        is UIRemoteCtrlSessionState.Connected -> ActiveSession(session, session.sessionState.remoteCtrl, close)
      }
    } else {
      ConnectDesktop(deviceName, remoteCtrls, sessionAddress)
    }
    SectionBottomSpacer()
  }
  LaunchedEffect(Unit) {
    setDeviceName(deviceName)
    updateRemoteCtrls(remoteCtrls)
    val useMulticast = useMulticast(remoteCtrls)
    showConnectScreen.value = !useMulticast
    if (chatModel.remoteCtrlSession.value != null) {
      disconnectDesktop()
    } else if (useMulticast) {
      findKnownDesktop(showConnectScreen)
    }
  }
  DisposableEffect(Unit) {
    onDispose {
      if (chatModel.remoteCtrlSession.value != null) {
        showConnectScreen.value = false
        disconnectDesktop()
      }
    }
  }
}

@Composable
private fun ConnectDesktop(deviceName: String, remoteCtrls: SnapshotStateList<RemoteCtrlInfo>, sessionAddress: MutableState<String>) {
  AppBarTitle(stringResource(MR.strings.connect_to_desktop))
  SectionView(stringResource(MR.strings.this_device_name).uppercase()) {
    DevicesView(deviceName, remoteCtrls) {
      if (it != "") {
        setDeviceName(it)
        controller.appPrefs.deviceNameForRemoteAccess.set(it)
      }
    }
  }
  SectionDividerSpaced()
  ScanDesktopAddressView(sessionAddress)
  if (controller.appPrefs.developerTools.get()) {
    SectionSpacer()
    DesktopAddressView(sessionAddress)
  }
}

@Composable
private fun ConnectingDesktop(session: RemoteCtrlSession, rc: RemoteCtrlInfo?) {
  AppBarTitle(stringResource(MR.strings.connecting_to_desktop))
  SectionView(stringResource(MR.strings.connecting_to_desktop).uppercase(), padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
    CtrlDeviceNameText(session, rc)
    Spacer(Modifier.height(DEFAULT_PADDING_HALF))
    CtrlDeviceVersionText(session)
  }

  if (session.sessionCode != null) {
    SectionSpacer()
    SectionView(stringResource(MR.strings.session_code).uppercase()) {
      SessionCodeText(session.sessionCode!!)
    }
  }

  SectionSpacer()

  SectionView {
    DisconnectButton(onClick = ::disconnectDesktop)
  }
}

@Composable
private fun SearchingDesktop(deviceName: String, remoteCtrls: SnapshotStateList<RemoteCtrlInfo>) {
  AppBarTitle(stringResource(MR.strings.connecting_to_desktop))
  SectionView(stringResource(MR.strings.this_device_name).uppercase()) {
    DevicesView(deviceName, remoteCtrls) {
      if (it != "") {
        setDeviceName(it)
        controller.appPrefs.deviceNameForRemoteAccess.set(it)
      }
    }
  }
  SectionDividerSpaced()
  SectionView(stringResource(MR.strings.found_desktop).uppercase(), padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
    Text(stringResource(MR.strings.waiting_for_desktop), fontStyle = FontStyle.Italic)
  }
  SectionSpacer()
  DisconnectButton(stringResource(MR.strings.scan_QR_code).replace('\n', ' '), MR.images.ic_qr_code, ::disconnectDesktop)
}

@Composable
private fun FoundDesktop(
  session: RemoteCtrlSession,
  rc: RemoteCtrlInfo,
  compatible: Boolean,
  connectRemoteViaMulticastAuto: State<Boolean>,
  deviceName: String,
  remoteCtrls: SnapshotStateList<RemoteCtrlInfo>,
  sessionAddress: MutableState<String>,
) {
  AppBarTitle(stringResource(MR.strings.found_desktop))
  SectionView(stringResource(MR.strings.this_device_name).uppercase()) {
    DevicesView(deviceName, remoteCtrls) {
      if (it != "") {
        setDeviceName(it)
        controller.appPrefs.deviceNameForRemoteAccess.set(it)
      }
    }
  }
  SectionDividerSpaced()
  SectionView(stringResource(MR.strings.found_desktop).uppercase(), padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
    CtrlDeviceNameText(session, rc)
    CtrlDeviceVersionText(session)
    if (!compatible) {
      Text(stringResource(MR.strings.not_compatible), color = MaterialTheme.colors.error)
    }
  }

  SectionSpacer()

  if (compatible) {
    SectionItemView({ confirmKnownDesktop(sessionAddress, rc) }) {
      Icon(painterResource(MR.images.ic_check), generalGetString(MR.strings.connect_button), tint = MaterialTheme.colors.secondary)
      TextIconSpaced(false)
      Text(generalGetString(MR.strings.connect_button))
    }
  }

  if (!compatible || !connectRemoteViaMulticastAuto.value) {
    DisconnectButton(stringResource(MR.strings.cancel_verb), onClick = ::disconnectDesktop)
  }

  if (compatible && connectRemoteViaMulticastAuto.value) {
    LaunchedEffect(Unit) {
      confirmKnownDesktop(sessionAddress, rc)
    }
  }
}

@Composable
private fun VerifySession(session: RemoteCtrlSession, rc: RemoteCtrlInfo?, sessCode: String, remoteCtrls: SnapshotStateList<RemoteCtrlInfo>) {
  AppBarTitle(stringResource(MR.strings.verify_connection))
  SectionView(stringResource(MR.strings.connected_to_desktop).uppercase(), padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
    CtrlDeviceNameText(session, rc)
    Spacer(Modifier.height(DEFAULT_PADDING_HALF))
    CtrlDeviceVersionText(session)
  }

  SectionSpacer()

  SectionView(stringResource(MR.strings.verify_code_with_desktop).uppercase()) {
    SessionCodeText(sessCode)
  }

  SectionSpacer()

  SectionItemView({ verifyDesktopSessionCode(remoteCtrls, sessCode) }) {
    Icon(painterResource(MR.images.ic_check), generalGetString(MR.strings.confirm_verb), tint = MaterialTheme.colors.secondary)
    TextIconSpaced(false)
    Text(generalGetString(MR.strings.confirm_verb))
  }

  SectionView {
    DisconnectButton(onClick = ::disconnectDesktop)
  }
}

@Composable
private fun CtrlDeviceNameText(session: RemoteCtrlSession, rc: RemoteCtrlInfo?) {
  val newDesktop = annotatedStringResource(MR.strings.new_desktop)
  val text = remember(rc) {
    var t = AnnotatedString(rc?.deviceViewName ?: session.ctrlAppInfo?.deviceName ?: "")
    if (rc == null) {
      t = t + AnnotatedString(" ") + newDesktop
    }
    t
  }
  Text(text)
}

@Composable
private fun CtrlDeviceVersionText(session: RemoteCtrlSession) {
  val thisDeviceVersion = annotatedStringResource(MR.strings.this_device_version, session.appVersion)
  val text = remember(session) {
    val v = AnnotatedString(session.ctrlAppInfo?.appVersionRange?.maxVersion ?: "")
    var t = AnnotatedString("v$v")
    if (v.text != session.appVersion) {
      t = t + AnnotatedString(" ") + thisDeviceVersion
    }
    t
  }
  Text(text)
}

@Composable
private fun ActiveSession(session: RemoteCtrlSession, rc: RemoteCtrlInfo, close: () -> Unit) {
  AppBarTitle(stringResource(MR.strings.connected_to_desktop))
  SectionView(stringResource(MR.strings.connected_desktop).uppercase(), padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
    Text(rc.deviceViewName)
    Spacer(Modifier.height(DEFAULT_PADDING_HALF))
    CtrlDeviceVersionText(session)
  }

  if (session.sessionCode != null) {
    SectionSpacer()
    SectionView(stringResource(MR.strings.session_code).uppercase()) {
      SessionCodeText(session.sessionCode!!)
    }
  }

  SectionSpacer()

  SectionView {
    DisconnectButton { disconnectDesktop(close) }
  }
}

@Composable
private fun SessionCodeText(code: String) {
  SelectionContainer {
    Text(
      code.substring(0, 23),
      Modifier.padding(start = DEFAULT_PADDING, top = 5.dp, end = DEFAULT_PADDING, bottom = 10.dp),
      style = TextStyle(fontFamily = FontFamily.Monospace, fontSize = 16.sp)
    )
  }
}

@Composable
private fun DevicesView(deviceName: String, remoteCtrls: SnapshotStateList<RemoteCtrlInfo>, updateDeviceName: (String) -> Unit) {
  DeviceNameField(deviceName) { updateDeviceName(it) }
  if (remoteCtrls.isNotEmpty()) {
    SectionItemView({ ModalManager.start.showModal { LinkedDesktopsView(remoteCtrls) }
    }) {
      Text(generalGetString(MR.strings.linked_desktops))
    }
  }
}

@Composable
private fun ScanDesktopAddressView(sessionAddress: MutableState<String>) {
  SectionView(stringResource(MR.strings.scan_qr_code_from_desktop).uppercase()) {
    QRCodeScanner { text ->
      sessionAddress.value = text
      processDesktopQRCode(sessionAddress, text)
    }
  }
}

@Composable
private fun DesktopAddressView(sessionAddress: MutableState<String>) {
  val clipboard = LocalClipboardManager.current
  SectionView(stringResource(MR.strings.desktop_address).uppercase()) {
    if (sessionAddress.value.isEmpty()) {
      SettingsActionItem(
        painterResource(MR.images.ic_content_paste),
        stringResource(MR.strings.paste_desktop_address),
        disabled = !clipboard.hasText(),
        click = {
          sessionAddress.value = clipboard.getText()?.text ?: ""
        },
      )
    } else {
      Row(Modifier.padding(horizontal = DEFAULT_PADDING).fillMaxWidth()) {
        val state = remember {
          mutableStateOf(TextFieldValue(sessionAddress.value))
        }
        DefaultBasicTextField(
          Modifier.fillMaxWidth(),
          state,
          color = MaterialTheme.colors.secondary,
        ) {
          state.value = it
        }
        KeyChangeEffect(state.value.text) {
          if (state.value.text.isNotEmpty()) {
            sessionAddress.value = state.value.text
          }
        }
      }
    }
    SettingsActionItem(
      painterResource(MR.images.ic_wifi),
      stringResource(MR.strings.connect_to_desktop),
      disabled = sessionAddress.value.isEmpty(),
      click = {
        connectDesktopAddress(sessionAddress, sessionAddress.value)
      },
    )
  }
}

@Composable
private fun LinkedDesktopsView(remoteCtrls: SnapshotStateList<RemoteCtrlInfo>) {
  ColumnWithScrollBar(
    Modifier.fillMaxWidth(),
  ) {
    AppBarTitle(stringResource(MR.strings.linked_desktops))
    SectionView(stringResource(MR.strings.desktop_devices).uppercase()) {
      remoteCtrls.forEach { rc ->
        val showMenu = rememberSaveable { mutableStateOf(false) }
        SectionItemViewLongClickable(click = {}, longClick = { showMenu.value = true }) {
          RemoteCtrl(rc)
          DefaultDropdownMenu(showMenu) {
            ItemAction(stringResource(MR.strings.delete_verb), painterResource(MR.images.ic_delete), color = Color.Red) {
              unlinkDesktop(remoteCtrls, rc)
              showMenu.value = false
            }
          }
        }

      }
    }
    SectionDividerSpaced()

    SectionView(stringResource(MR.strings.linked_desktop_options).uppercase()) {
      PreferenceToggle(stringResource(MR.strings.verify_connections), remember { controller.appPrefs.confirmRemoteSessions.state }.value) {
        controller.appPrefs.confirmRemoteSessions.set(it)
      }
      PreferenceToggle(stringResource(MR.strings.discover_on_network), remember { controller.appPrefs.connectRemoteViaMulticast.state }.value) {
        controller.appPrefs.connectRemoteViaMulticast.set(it)
      }
      if (remember { controller.appPrefs.connectRemoteViaMulticast.state }.value) {
        PreferenceToggle(stringResource(MR.strings.multicast_connect_automatically), remember { controller.appPrefs.connectRemoteViaMulticastAuto.state }.value) {
          controller.appPrefs.connectRemoteViaMulticastAuto.set(it)
        }
      }
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun RemoteCtrl(rc: RemoteCtrlInfo) {
  Text(rc.deviceViewName)
}

private fun setDeviceName(name: String) {
  withBGApi {
    controller.setLocalDeviceName(name)
  }
}

private suspend fun updateRemoteCtrls(remoteCtrls: SnapshotStateList<RemoteCtrlInfo>) {
  val res = controller.listRemoteCtrls()
  if (res != null) {
    remoteCtrls.clear()
    remoteCtrls.addAll(res)
  }
}

private fun processDesktopQRCode(sessionAddress: MutableState<String>, resp: String) {
  connectDesktopAddress(sessionAddress, resp)
}

private fun findKnownDesktop(showConnectScreen: MutableState<Boolean>) {
  withBGApi {
    if (controller.findKnownRemoteCtrl()) {
      chatModel.remoteCtrlSession.value = RemoteCtrlSession(
        ctrlAppInfo = null,
        appVersion = "",
        sessionState = UIRemoteCtrlSessionState.Searching
      )
      showConnectScreen.value = true
    }
  }
}

private fun confirmKnownDesktop(sessionAddress: MutableState<String>, rc: RemoteCtrlInfo) {
  connectDesktop(sessionAddress) {
    controller.confirmRemoteCtrl(rc.remoteCtrlId)
  }
}

private fun connectDesktopAddress(sessionAddress: MutableState<String>, addr: String) {
  connectDesktop(sessionAddress) {
    controller.connectRemoteCtrl(addr)
  }
}

private fun connectDesktop(sessionAddress: MutableState<String>, connect: suspend () -> Pair<SomeRemoteCtrl?, CR.ChatCmdError?>) {
  withBGApi {
    val res = connect()
    if (res.first != null) {
      val (rc_, ctrlAppInfo, v) = res.first!!
      sessionAddress.value = ""
      chatModel.remoteCtrlSession.value = RemoteCtrlSession(
        ctrlAppInfo = ctrlAppInfo,
        appVersion = v,
        sessionState = UIRemoteCtrlSessionState.Connecting(remoteCtrl_ = rc_)
      )
    } else {
      val e = res.second ?: return@withBGApi
      when {
        e.chatError is ChatError.ChatErrorRemoteCtrl && e.chatError.remoteCtrlError is RemoteCtrlError.BadInvitation -> showBadInvitationErrorAlert()
        e.chatError is ChatError.ChatErrorChat && e.chatError.errorType is ChatErrorType.CommandError -> showBadInvitationErrorAlert()
        e.chatError is ChatError.ChatErrorRemoteCtrl && e.chatError.remoteCtrlError is RemoteCtrlError.BadVersion -> showBadVersionAlert(v = e.chatError.remoteCtrlError.appVersion)
        e.chatError is ChatError.ChatErrorAgent && e.chatError.agentError is AgentErrorType.RCP && e.chatError.agentError.rcpErr is RCErrorType.VERSION -> showBadVersionAlert(v = null)
        e.chatError is ChatError.ChatErrorAgent && e.chatError.agentError is AgentErrorType.RCP && e.chatError.agentError.rcpErr is RCErrorType.CTRL_AUTH -> showDesktopDisconnectedErrorAlert()
        else -> {
          val errMsg = "${e.responseType}: ${e.details}"
          Log.e(TAG, "bad response: $errMsg")
          AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error), errMsg)
        }
      }
    }
  }
}

private fun verifyDesktopSessionCode(remoteCtrls: SnapshotStateList<RemoteCtrlInfo>, sessCode: String) {
  withBGApi {
    val rc = controller.verifyRemoteCtrlSession(sessCode)
    if (rc != null) {
      chatModel.remoteCtrlSession.value = chatModel.remoteCtrlSession.value?.copy(sessionState = UIRemoteCtrlSessionState.Connected(remoteCtrl = rc, sessionCode = sessCode))
    }
    updateRemoteCtrls(remoteCtrls)
  }
}

@Composable
private fun DisconnectButton(label: String = generalGetString(MR.strings.disconnect_remote_host), icon: ImageResource = MR.images.ic_close, onClick: () -> Unit) {
  SectionItemView(onClick) {
    Icon(painterResource(icon), label, tint = MaterialTheme.colors.secondary)
    TextIconSpaced(false)
    Text(label)
  }
}

private fun useMulticast(remoteCtrls: List<RemoteCtrlInfo>): Boolean =
  controller.appPrefs.connectRemoteViaMulticast.get() && remoteCtrls.isNotEmpty()

private fun disconnectDesktop(close: (() -> Unit)? = null) {
  withBGApi {
    controller.stopRemoteCtrl()
    if (chatModel.remoteCtrlSession.value?.sessionState is UIRemoteCtrlSessionState.Connected) {
      switchToLocalSession()
    } else {
      chatModel.remoteCtrlSession.value = null
    }
    close?.invoke()
  }
}

private fun unlinkDesktop(remoteCtrls: SnapshotStateList<RemoteCtrlInfo>, rc: RemoteCtrlInfo) {
  withBGApi {
    controller.deleteRemoteCtrl(rc.remoteCtrlId)
    remoteCtrls.removeAll { it.remoteCtrlId == rc.remoteCtrlId }
  }
}

private fun showUnlinkDesktopAlert(remoteCtrls: SnapshotStateList<RemoteCtrlInfo>, rc: RemoteCtrlInfo) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.unlink_desktop_question),
    confirmText = generalGetString(MR.strings.unlink_desktop),
    destructive = true,
    onConfirm = {
      unlinkDesktop(remoteCtrls, rc)
    }
  )
}

private fun showDisconnectDesktopAlert(close: (() -> Unit)?) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.disconnect_desktop_question),
    text = generalGetString(MR.strings.only_one_device_can_work_at_the_same_time),
    confirmText = generalGetString(MR.strings.disconnect_remote_host),
    destructive = true,
    onConfirm = { disconnectDesktop(close) }
  )
}

private fun showBadInvitationErrorAlert() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.bad_desktop_address),
  )
}

private fun showBadVersionAlert(v: String?) {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.desktop_incompatible_version),
    text = generalGetString(MR.strings.desktop_app_version_is_incompatible).format(v ?: "")
  )
}

private fun showDesktopDisconnectedErrorAlert() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.desktop_connection_terminated),
  )
}
