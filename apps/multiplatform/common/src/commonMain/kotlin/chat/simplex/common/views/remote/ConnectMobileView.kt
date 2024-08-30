package chat.simplex.common.views.remote

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionItemViewLongClickable
import SectionTextFooter
import SectionView
import TextIconSpaced
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.KeyboardActions
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.*
import androidx.compose.ui.text.input.*
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.stopRemoteHostAndReloadHosts
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.chatlist.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.QRCode
import chat.simplex.common.views.usersettings.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.flow.distinctUntilChanged

@Composable
fun ConnectMobileView() {
  val connecting = rememberSaveable() { mutableStateOf(false) }
  val remoteHosts = remember { chatModel.remoteHosts }
  val deviceName = chatModel.controller.appPrefs.deviceNameForRemoteAccess
  LaunchedEffect(Unit) {
    withBGApi {
      controller.reloadRemoteHosts()
    }
  }
  ConnectMobileLayout(
    deviceName = remember { deviceName.state },
    remoteHosts = remoteHosts,
    connecting,
    connectedHost = remember { chatModel.currentRemoteHost },
    updateDeviceName = {
      withBGApi {
        if (it != "") {
          chatModel.controller.setLocalDeviceName(it)
          deviceName.set(it)
        }
      }
    },
    addMobileDevice = { showAddingMobileDevice(connecting) },
    connectMobileDevice = { connectMobileDevice(it, connecting) },
    connectDesktop = { withBGApi { chatController.switchUIRemoteHost(null) } },
    deleteHost = { host ->
      withBGApi {
        val success = controller.deleteRemoteHost(host.remoteHostId)
        if (success) {
          chatModel.remoteHosts.removeAll { it.remoteHostId == host.remoteHostId }
        }
      }
    }
  )
}

@Composable
fun ConnectMobileLayout(
  deviceName: State<String?>,
  remoteHosts: List<RemoteHostInfo>,
  connecting: MutableState<Boolean>,
  connectedHost: State<RemoteHostInfo?>,
  updateDeviceName: (String) -> Unit,
  addMobileDevice: () -> Unit,
  connectMobileDevice: (RemoteHostInfo) -> Unit,
  connectDesktop: () -> Unit,
  deleteHost: (RemoteHostInfo) -> Unit,
) {
  ColumnWithScrollBar(Modifier.fillMaxWidth()) {
    AppBarTitle(stringResource(if (remember { chatModel.remoteHosts }.isEmpty()) MR.strings.link_a_mobile else MR.strings.linked_mobiles))
    SectionView(generalGetString(MR.strings.this_device_name).uppercase()) {
      DeviceNameField(deviceName.value ?: "") { updateDeviceName(it) }
      SectionTextFooter(generalGetString(MR.strings.this_device_name_shared_with_mobile))
      PreferenceToggle(stringResource(MR.strings.multicast_discoverable_via_local_network), checked = remember { controller.appPrefs.offerRemoteMulticast.state }.value) {
        controller.appPrefs.offerRemoteMulticast.set(it)
      }
      SectionDividerSpaced()
    }
    SectionView(stringResource(MR.strings.devices).uppercase()) {
      if (chatModel.localUserCreated.value == true) {
        SettingsActionItemWithContent(text = stringResource(MR.strings.this_device), icon = painterResource(MR.images.ic_desktop), click = connectDesktop) {
          if (connectedHost.value == null) {
            Icon(painterResource(MR.images.ic_done_filled), null, Modifier.size(20.dp), tint = MaterialTheme.colors.onBackground)
          }
        }
      }

      for (host in remoteHosts) {
        val showMenu = rememberSaveable { mutableStateOf(false) }
        SectionItemViewLongClickable({ connectMobileDevice(host) }, { showMenu.value = true }, disabled = connecting.value) {
          Icon(painterResource(MR.images.ic_smartphone_300), host.hostDeviceName, tint = MaterialTheme.colors.secondary)
          TextIconSpaced(false)
          Text(host.hostDeviceName)
          Spacer(Modifier.weight(1f))
          if (host.activeHost) {
            Icon(painterResource(MR.images.ic_done_filled), null, Modifier.size(20.dp), tint = MaterialTheme.colors.onBackground)
          } else if (host.sessionState is RemoteHostSessionState.Connected) {
            HostDisconnectButton { stopRemoteHostAndReloadHosts(host, false) }
          }
        }
        Box(Modifier.padding(horizontal = DEFAULT_PADDING)) {
          DefaultDropdownMenu(showMenu) {
            if (host.activeHost) {
              ItemAction(stringResource(MR.strings.disconnect_remote_host), painterResource(MR.images.ic_wifi_off), color = WarningOrange) {
                stopRemoteHostAndReloadHosts(host, true)
                showMenu.value = false
              }
            } else {
              ItemAction(stringResource(MR.strings.delete_verb), painterResource(MR.images.ic_delete), color = Color.Red) {
                deleteHost(host)
                showMenu.value = false
              }
            }
          }
        }
      }
      SectionItemView(addMobileDevice) {
        Icon(painterResource(MR.images.ic_add), stringResource(MR.strings.link_a_mobile), tint = MaterialTheme.colors.primary)
        Spacer(Modifier.padding(horizontal = 10.dp))
        Text(stringResource(MR.strings.link_a_mobile), color = MaterialTheme.colors.primary)
      }
    }
    SectionBottomSpacer()
  }
}

@Composable
fun DeviceNameField(
  initialValue: String,
  onChange: (String) -> Unit
) {
  // TODO get user-defined device name
  val state = remember { mutableStateOf(TextFieldValue(initialValue)) }
  DefaultConfigurableTextField(
    state = state,
    placeholder = generalGetString(MR.strings.enter_this_device_name),
    modifier = Modifier.fillMaxWidth().padding(start = DEFAULT_PADDING),
    isValid = { true },
  )
  KeyChangeEffect(state.value) {
    onChange(state.value.text)
  }
}

@Composable
private fun ConnectMobileViewLayout(
  title: String?,
  invitation: String?,
  deviceName: String?,
  sessionCode: String?,
  port: String?,
  staleQrCode: Boolean = false,
  refreshQrCode: () -> Unit = {},
  UnderQrLayout: @Composable () -> Unit = {},
) {
  ColumnWithScrollBar(Modifier.fillMaxWidth()) {
    if (title != null) {
      AppBarTitle(title)
    }
    SectionView {
      if (invitation != null && sessionCode == null && port != null) {
        Box {
          QRCode(invitation)
          if (staleQrCode) {
            Box(Modifier.matchParentSize().background(MaterialTheme.colors.background.copy(alpha = 0.9f)), contentAlignment = Alignment.Center) {
              SimpleButtonDecorated(stringResource(MR.strings.refresh_qr_code), painterResource(MR.images.ic_refresh), click = refreshQrCode)
            }
          }
        }
        SectionTextFooter(annotatedStringResource(MR.strings.open_on_mobile_and_scan_qr_code), textAlign = TextAlign.Center)
        SectionTextFooter(annotatedStringResource(MR.strings.waiting_for_mobile_to_connect), textAlign = TextAlign.Center)

        UnderQrLayout()

        if (remember { controller.appPrefs.developerTools.state }.value) {
          val clipboard = LocalClipboardManager.current
          Spacer(Modifier.height(DEFAULT_PADDING_HALF))
          SectionItemView({ clipboard.shareText(invitation) }) {
            Text(generalGetString(MR.strings.share_link), color = MaterialTheme.colors.primary)
          }
        }

        Spacer(Modifier.height(DEFAULT_PADDING))
      }
      if (deviceName != null || sessionCode != null) {
        SectionView(stringResource(MR.strings.connected_mobile).uppercase()) {
          SelectionContainer {
            Text(
              deviceName ?: stringResource(MR.strings.new_mobile_device),
              Modifier.padding(start = DEFAULT_PADDING, top = 5.dp, end = DEFAULT_PADDING, bottom = 10.dp),
              style = TextStyle(fontFamily = FontFamily.Monospace, fontSize = 16.sp, fontStyle = if (deviceName != null) FontStyle.Normal else FontStyle.Italic)
            )
          }
        }
        Spacer(Modifier.height(DEFAULT_PADDING_HALF))
      }

      if (sessionCode != null) {
        SectionView(stringResource(MR.strings.verify_code_on_mobile).uppercase()) {
          SelectionContainer {
            Text(
              sessionCode.substring(0, 23),
              Modifier.padding(start = DEFAULT_PADDING, top = 5.dp, end = DEFAULT_PADDING, bottom = 10.dp),
              style = TextStyle(fontFamily = FontFamily.Monospace, fontSize = 16.sp)
            )
          }
        }
      }
    }
    if (invitation != null) {
      SectionBottomSpacer()
    }
  }
}

fun connectMobileDevice(rh: RemoteHostInfo, connecting: MutableState<Boolean>) {
  if (!rh.activeHost() && rh.sessionState is RemoteHostSessionState.Connected) {
    withBGApi {
      controller.switchUIRemoteHost(rh.remoteHostId)
    }
  } else if (rh.activeHost()) {
    showConnectedMobileDevice(rh) {
      stopRemoteHostAndReloadHosts(rh, true)
    }
  } else {
    showConnectMobileDevice(rh, connecting)
  }
}

private fun showAddingMobileDevice(connecting: MutableState<Boolean>) {
  ModalManager.start.showModalCloseable { close ->
    AddingMobileDevice(true, remember { mutableStateOf(false) }, connecting, close)
  }
}

@Composable
fun AddingMobileDevice(showTitle: Boolean, staleQrCode: MutableState<Boolean>, connecting: MutableState<Boolean>, close: () -> Unit) {
  var cachedR by remember { mutableStateOf<CR.RemoteHostStarted?>(null) }
  val customAddress = rememberSaveable { mutableStateOf<RemoteCtrlAddress?>(null) }
  val customPort = rememberSaveable { mutableStateOf<Int?>(null) }
  var userChangedAddress by rememberSaveable { mutableStateOf(false) }
  var userChangedPort by rememberSaveable { mutableStateOf(false) }
  val startRemoteHost = suspend {
    if (customAddress.value != cachedR.address && cachedR != null) {
      userChangedAddress = true
    }
    if (customPort.value != cachedR.port && cachedR != null) {
      userChangedPort = true
    }
    val r = chatModel.controller.startRemoteHost(
      rhId = null,
      multicast = controller.appPrefs.offerRemoteMulticast.get(),
      address = if (customAddress.value != null && userChangedAddress) customAddress.value else cachedR.rh?.bindAddress_,
      port = if (customPort.value != null && userChangedPort) customPort.value else cachedR.rh?.bindPort_
    )
    if (r != null) {
      cachedR = r
      connecting.value = true
      customAddress.value = cachedR.addresses.firstOrNull()
      customPort.value = cachedR.port
      chatModel.remoteHostPairing.value = null to RemoteHostSessionState.Starting
    }
  }
  val pairing = remember { chatModel.remoteHostPairing }
  val sessionCode = when (val state = pairing.value?.second) {
    is RemoteHostSessionState.PendingConfirmation -> state.sessionCode
    else -> null
  }
  /** It's needed to prevent screen flashes when [chatModel.newRemoteHostPairing] sets to null in background */
  var cachedSessionCode by remember { mutableStateOf<String?>(null) }
  if (cachedSessionCode == null && sessionCode != null) {
    cachedSessionCode = sessionCode
  }
  val remoteDeviceName = pairing.value?.first?.hostDeviceName
  ConnectMobileViewLayout(
    title = if (!showTitle) null else if (cachedSessionCode == null) stringResource(MR.strings.link_a_mobile) else stringResource(MR.strings.verify_connection),
    invitation = cachedR?.invitation,
    deviceName = remoteDeviceName,
    sessionCode = cachedSessionCode,
    port = cachedR?.ctrlPort,
    staleQrCode = staleQrCode.value || (cachedR.address != customAddress.value && customAddress.value != null) || cachedR.port != customPort.value,
    refreshQrCode = {
      withBGApi {
        if (chatController.stopRemoteHost(null)) {
          startRemoteHost()
          staleQrCode.value = false
        }
      }
    },
    UnderQrLayout = { UnderQrLayout(cachedR, customAddress, customPort) }
  )
  val oldRemoteHostId by remember { mutableStateOf(chatModel.currentRemoteHost.value?.remoteHostId) }
  LaunchedEffect(remember { chatModel.currentRemoteHost }.value) {
    if (chatModel.currentRemoteHost.value?.remoteHostId != null && chatModel.currentRemoteHost.value?.remoteHostId != oldRemoteHostId) {
      close()
    }
  }
  KeyChangeEffect(pairing.value) {
    if (pairing.value == null) {
      close()
    }
  }
  DisposableEffect(Unit) {
    withBGApi {
      startRemoteHost()
    }
    onDispose {
      if (chatModel.currentRemoteHost.value?.remoteHostId == oldRemoteHostId) {
        withBGApi {
          chatController.stopRemoteHost(null)
        }
      }
      chatModel.remoteHostPairing.value = null
    }
  }
}

private fun showConnectMobileDevice(rh: RemoteHostInfo, connecting: MutableState<Boolean>) {
  ModalManager.start.showModalCloseable { close ->
    var cachedR by remember { mutableStateOf<CR.RemoteHostStarted?>(null) }
    val customAddress = rememberSaveable { mutableStateOf<RemoteCtrlAddress?>(null) }
    val customPort = rememberSaveable { mutableStateOf<Int?>(null) }
    var userChangedAddress by rememberSaveable { mutableStateOf(false) }
    var userChangedPort by rememberSaveable { mutableStateOf(false) }
    val startRemoteHost = suspend {
      if (customAddress.value != cachedR.address && cachedR != null) {
        userChangedAddress = true
      }
      if (customPort.value != cachedR.port && cachedR != null) {
        userChangedPort = true
      }
      val r = chatModel.controller.startRemoteHost(
        rhId = rh.remoteHostId,
        multicast = controller.appPrefs.offerRemoteMulticast.get(),
        address = if (customAddress.value != null && userChangedAddress) customAddress.value else cachedR.rh?.bindAddress_ ?: rh.bindAddress_,
        port = if (customPort.value != null && userChangedPort) customPort.value else cachedR.rh?.bindPort_ ?: rh.bindPort_
      )
      if (r != null) {
        cachedR = r
        connecting.value = true
        customAddress.value = cachedR.addresses.firstOrNull()
        customPort.value = cachedR.port
        chatModel.remoteHostPairing.value = null to RemoteHostSessionState.Starting
      }
    }
    val pairing = remember { chatModel.remoteHostPairing }
    val sessionCode = when (val state = pairing.value?.second) {
      is RemoteHostSessionState.PendingConfirmation -> state.sessionCode
      else -> null
    }
    /** It's needed to prevent screen flashes when [chatModel.newRemoteHostPairing] sets to null in background */
    var cachedSessionCode by remember { mutableStateOf<String?>(null) }
    if (cachedSessionCode == null && sessionCode != null) {
      cachedSessionCode = sessionCode
    }
    ConnectMobileViewLayout(
      title = if (cachedSessionCode == null) stringResource(MR.strings.scan_from_mobile) else stringResource(MR.strings.verify_connection),
      invitation = cachedR?.invitation,
      deviceName = pairing.value?.first?.hostDeviceName ?: rh.hostDeviceName,
      sessionCode = cachedSessionCode,
      port = cachedR?.ctrlPort,
      staleQrCode = (cachedR.address != customAddress.value && customAddress.value != null) || cachedR.port != customPort.value,
      refreshQrCode = {
        withBGApi {
          if (chatController.stopRemoteHost(rh.remoteHostId)) {
            startRemoteHost()
          }
        }
      },
      UnderQrLayout = { UnderQrLayout(cachedR, customAddress, customPort) }
    )
    LaunchedEffect(remember { chatModel.currentRemoteHost }.value) {
      if (cachedR.remoteHostId != null && chatModel.currentRemoteHost.value?.remoteHostId == cachedR.remoteHostId) {
        close()
      }
    }
    KeyChangeEffect(pairing.value) {
      if (pairing.value == null) {
        close()
      }
    }
    DisposableEffect(Unit) {
      withBGApi {
        startRemoteHost()
      }
      onDispose {
        if (cachedR.remoteHostId != null && chatModel.currentRemoteHost.value?.remoteHostId != cachedR.remoteHostId) {
          withBGApi {
            chatController.stopRemoteHost(cachedR.remoteHostId)
          }
        }
        chatModel.remoteHostPairing.value = null
      }
    }
  }
}

private fun showConnectedMobileDevice(rh: RemoteHostInfo, disconnectHost: () -> Unit) {
  ModalManager.start.showModalCloseable { close ->
    val sessionCode = when (val state = rh.sessionState) {
      is RemoteHostSessionState.Connected -> state.sessionCode
      else -> null
    }
    Column {
      ConnectMobileViewLayout(
        title = stringResource(MR.strings.connected_to_mobile),
        invitation = null,
        deviceName = rh.hostDeviceName,
        sessionCode = sessionCode,
        port = null,
      )
      Spacer(Modifier.height(DEFAULT_PADDING_HALF))
      SectionItemView(disconnectHost) {
        Text(generalGetString(MR.strings.disconnect_remote_host), Modifier.fillMaxWidth(), color = WarningOrange)
      }
    }
    KeyChangeEffect(remember { chatModel.currentRemoteHost }.value) {
      close()
    }
  }
}

@Composable
private fun UnderQrLayout(cachedR: CR.RemoteHostStarted?, customAddress: MutableState<RemoteCtrlAddress?>, customPort: MutableState<Int?>) {
  Row(Modifier.fillMaxWidth(), verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.Center) {
    if (cachedR.addresses.size > 1) {
      ExposedDropDownSetting(
        cachedR.addresses.map { it to it.address + " (${it.`interface`})" },
        customAddress,
        textColor = MaterialTheme.colors.onBackground,
        fontSize = 14.sp,
        minWidth = 250.dp,
        maxWidth = with(LocalDensity.current) { 250.sp.toDp() },
        enabled = remember { mutableStateOf(cachedR.addresses.size > 1) },
        onSelected = {
          customAddress.value = it
        }
      )
    } else {
      Spacer(Modifier.width(10.dp))
      Text(customAddress.value?.address + " (${customAddress.value?.`interface`})", fontSize = 14.sp, color = MaterialTheme.colors.onBackground)
    }
    val portUnsaved = rememberSaveable(stateSaver = TextFieldValue.Saver) {
      mutableStateOf(TextFieldValue((customPort.value ?: cachedR.port!!).toString()))
    }
    Spacer(Modifier.width(DEFAULT_PADDING))
    Box {
      DefaultConfigurableTextField(
        portUnsaved,
        stringResource(MR.strings.random_port),
        modifier = Modifier.widthIn(max = 132.dp),
        isValid = { (validPort(it) && it.toInt() > 1023) || it.isBlank() },
        keyboardActions = KeyboardActions(onDone = { defaultKeyboardAction(ImeAction.Done) }),
        keyboardType = KeyboardType.Number,
        fontSize = 14.sp,
      )
      if (validPort(portUnsaved.value.text) && portUnsaved.value.text.toInt() > 1023) {
        Icon(painterResource(MR.images.ic_edit), stringResource(MR.strings.edit_verb), Modifier.padding(end = 56.dp).size(16.dp).align(Alignment.CenterEnd), tint = MaterialTheme.colors.secondary)
        IconButton(::showOpenPortAlert, Modifier.align(Alignment.TopEnd).padding(top = 2.dp)) {
          Icon(painterResource(MR.images.ic_info), null, tint = MaterialTheme.colors.primary)
        }
      }
    }
    LaunchedEffect(Unit) {
      snapshotFlow { portUnsaved.value.text }
        .distinctUntilChanged()
        .collect {
          if (validPort(it) && it.toInt() > 1023) {
            customPort.value = it.toInt()
          } else {
            customPort.value = null
          }
        }
    }
    KeyChangeEffect(customPort.value) {
      if (customPort.value != null) {
        portUnsaved.value = portUnsaved.value.copy(text = customPort.value.toString())
      }
    }
  }
}

private fun showOpenPortAlert() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.open_port_in_firewall_title),
    text = generalGetString(MR.strings.open_port_in_firewall_desc),
  )
}

private val CR.RemoteHostStarted?.rh: RemoteHostInfo? get() = this?.remoteHost_
private val CR.RemoteHostStarted?.remoteHostId: Long? get() = this?.remoteHost_?.remoteHostId
private val CR.RemoteHostStarted?.address: RemoteCtrlAddress? get() = this?.localAddrs?.firstOrNull()
private val CR.RemoteHostStarted?.addresses: List<RemoteCtrlAddress> get() =
  (if (controller.appPrefs.developerTools.get() || this?.localAddrs?.indexOfFirst { it.address == "127.0.0.1" } == 0) this?.localAddrs else this?.localAddrs?.filterNot { it.address == "127.0.0.1" }) ?: emptyList()
private val CR.RemoteHostStarted?.port: Int? get() = this?.ctrlPort?.toIntOrNull()
