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
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.*
import androidx.compose.ui.text.input.*
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
import chat.simplex.common.views.usersettings.PreferenceToggle
import chat.simplex.common.views.usersettings.SettingsActionItemWithContent
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
fun ConnectMobileView() {
  val connecting = rememberSaveable() { mutableStateOf(false) }
  val remoteHosts = remember { chatModel.remoteHosts }
  val deviceName = chatModel.controller.appPrefs.deviceNameForRemoteAccess
  LaunchedEffect(Unit) {
    controller.reloadRemoteHosts()
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
  Column(
    Modifier.fillMaxWidth().verticalScroll(rememberScrollState()),
    verticalArrangement = Arrangement.spacedBy(8.dp)
  ) {
    AppBarTitle(stringResource(if (remember { chatModel.remoteHosts }.isEmpty()) MR.strings.link_a_mobile else MR.strings.linked_mobiles))
    SectionView(generalGetString(MR.strings.this_device_name).uppercase()) {
      DeviceNameField(deviceName.value ?: "") { updateDeviceName(it) }
      SectionTextFooter(generalGetString(MR.strings.this_device_name_shared_with_mobile))
      PreferenceToggle(stringResource(MR.strings.multicast_discoverable_via_local_network), remember { controller.appPrefs.offerRemoteMulticast.state }.value) {
        controller.appPrefs.offerRemoteMulticast.set(it)
      }
      SectionDividerSpaced(maxBottomPadding = false)
    }
    SectionView(stringResource(MR.strings.devices).uppercase()) {
      SettingsActionItemWithContent(text = stringResource(MR.strings.this_device), icon = painterResource(MR.images.ic_desktop), click = connectDesktop) {
        if (connectedHost.value == null) {
          Icon(painterResource(MR.images.ic_done_filled), null, Modifier.size(20.dp), tint = MaterialTheme.colors.onBackground)
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
    modifier = Modifier.padding(start = DEFAULT_PADDING),
    isValid = { true },
  )
  KeyChangeEffect(state.value) {
    onChange(state.value.text)
  }
}

@Composable
private fun ConnectMobileViewLayout(
  title: String,
  invitation: String?,
  deviceName: String?,
  sessionCode: String?,
  port: String?
) {
  Column(
    Modifier.fillMaxWidth().verticalScroll(rememberScrollState()),
    verticalArrangement = Arrangement.spacedBy(8.dp)
  ) {
    AppBarTitle(title)
    SectionView {
      if (invitation != null && sessionCode == null && port != null) {
        QRCode(
          invitation, Modifier
            .padding(start = DEFAULT_PADDING, top = DEFAULT_PADDING_HALF, end = DEFAULT_PADDING, bottom = DEFAULT_PADDING_HALF)
            .aspectRatio(1f)
        )
        SectionTextFooter(annotatedStringResource(MR.strings.open_on_mobile_and_scan_qr_code))
        SectionTextFooter(annotatedStringResource(MR.strings.waiting_for_mobile_to_connect_on_port, port))

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
    val invitation = rememberSaveable { mutableStateOf<String?>(null) }
    val port = rememberSaveable { mutableStateOf<String?>(null) }
    val pairing = remember { chatModel.newRemoteHostPairing }
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
      title = if (cachedSessionCode == null) stringResource(MR.strings.link_a_mobile) else stringResource(MR.strings.verify_connection),
      invitation = invitation.value,
      deviceName = remoteDeviceName,
      sessionCode = cachedSessionCode,
      port = port.value
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
        val r = chatModel.controller.startRemoteHost(null, controller.appPrefs.offerRemoteMulticast.get())
        if (r != null) {
          connecting.value = true
          invitation.value = r.second
          port.value = r.third
        }
      }
      onDispose {
        if (chatModel.currentRemoteHost.value?.remoteHostId == oldRemoteHostId) {
          withBGApi {
            chatController.stopRemoteHost(null)
          }
        }
        chatModel.newRemoteHostPairing.value = null
      }
    }
  }
}

private fun showConnectMobileDevice(rh: RemoteHostInfo, connecting: MutableState<Boolean>) {
  ModalManager.start.showModalCloseable { close ->
    val pairing = remember { chatModel.newRemoteHostPairing }
    val invitation = rememberSaveable { mutableStateOf<String?>(null) }
    val port = rememberSaveable { mutableStateOf<String?>(null) }
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
      invitation = invitation.value,
      deviceName = pairing.value?.first?.hostDeviceName ?: rh.hostDeviceName,
      sessionCode = cachedSessionCode,
      port = port.value
    )
    var remoteHostId by rememberSaveable { mutableStateOf<Long?>(null) }
    LaunchedEffect(Unit) {
      val r = chatModel.controller.startRemoteHost(rh.remoteHostId, controller.appPrefs.offerRemoteMulticast.get())
      if (r != null) {
        val (rh_, inv) = r
        connecting.value = true
        remoteHostId = rh_?.remoteHostId
        invitation.value = inv
        port.value = r.third
      }
    }
    LaunchedEffect(remember { chatModel.currentRemoteHost }.value) {
      if (remoteHostId != null && chatModel.currentRemoteHost.value?.remoteHostId == remoteHostId) {
        close()
      }
    }
    KeyChangeEffect(pairing.value) {
      if (pairing.value == null) {
        close()
      }
    }
    DisposableEffect(Unit) {
      onDispose {
        if (remoteHostId != null && chatModel.currentRemoteHost.value?.remoteHostId != remoteHostId) {
          withBGApi {
            chatController.stopRemoteHost(remoteHostId)
          }
        }
        chatModel.newRemoteHostPairing.value = null
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
