package chat.simplex.common.views.remote

import SectionDividerSpaced
import SectionItemView
import SectionItemViewLongClickable
import SectionTextFooter
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.ZeroCornerSize
import androidx.compose.foundation.text.*
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.*
import androidx.compose.material.TextFieldDefaults.indicatorLine
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.SolidColor
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.*
import androidx.compose.ui.text.input.*
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.chatlist.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.QRCode
import chat.simplex.common.views.usersettings.SettingsActionItem
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
fun ConnectMobileView(
  m: ChatModel
) {
  val connecting = rememberSaveable() { mutableStateOf(false) }
  val remoteHosts = remember { chatModel.remoteHosts }
  val deviceName = m.controller.appPrefs.deviceNameForRemoteAccess
  LaunchedEffect(Unit) {
    val hosts = m.controller.listRemoteHosts() ?: return@LaunchedEffect
    remoteHosts.clear()
    remoteHosts.addAll(hosts)
  }
  ConnectMobileLayout(
    deviceName = deviceName,
    remoteHosts = remoteHosts,
    connecting,
    connectedHost = remember { m.currentRemoteHost },
    updateDeviceName = {
      withBGApi {
        if (it != "") {
          m.controller.setLocalDeviceName(it)
          deviceName.set(it)
        }
      }
    },
    addMobileDevice = {
      ModalManager.start.showModalCloseable { close ->
        val invitation = rememberSaveable { mutableStateOf<String?>(null) }
        val pairing = remember { chatModel.newRemoteHostPairing }
        val sessionCode = when (val state = pairing.value?.second) {
          is RemoteHostSessionState.PendingConfirmation -> state.sessionCode
          else -> null
        }
        val remoteDeviceName = pairing.value?.first?.hostDeviceName
        ConnectMobileViewLayout(stringResource(MR.strings.link_a_mobile), invitation.value, remoteDeviceName, sessionCode)
        val oldRemoteHostId by remember { mutableStateOf(chatModel.currentRemoteHost.value?.remoteHostId) }
        LaunchedEffect(remember { chatModel.currentRemoteHost }.value) {
          if (chatModel.currentRemoteHost.value?.remoteHostId != null && chatModel.currentRemoteHost.value?.remoteHostId != oldRemoteHostId) {
            close()
          }
        }
        DisposableEffect(Unit) {
          withBGApi {
            val r = chatModel.controller.startRemoteHost(null)
            if (r != null) {
              connecting.value = true
              invitation.value = r.second
            }
          }
          onDispose {
            if (m.currentRemoteHost.value?.remoteHostId == oldRemoteHostId) {
              withBGApi {
                chatController.stopRemoteHost(null)
              }
            }
            chatModel.newRemoteHostPairing.value = null
          }
        }
      }
    },
    connectMobileDevice = { connectMobileDevice(it, connecting) },
    connectDesktop = ::localDeviceSelected,
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
  deviceName: SharedPreference<String?>,
  remoteHosts: List<RemoteHostInfo>,
  connecting: MutableState<Boolean>,
  connectedHost: MutableState<RemoteHostInfo?>,
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
      DeviceNameField(deviceName.state.value ?: "") { updateDeviceName(it) }
      SectionTextFooter(generalGetString(MR.strings.this_device_name_shared_with_mobile))
      SectionDividerSpaced(maxBottomPadding = false)

      SectionItemView({ connectDesktop() }, disabled = connecting.value) {
        Text(stringResource(MR.strings.this_device))
        Spacer(Modifier.weight(1f))
        if (remember { connectedHost }.value == null) {
          Icon(painterResource(MR.images.ic_done_filled), null, Modifier.size(20.dp), tint = MaterialTheme.colors.onBackground)
        }
      }

      for (host in remoteHosts) {
        val showMenu = rememberSaveable { mutableStateOf(false) }
        SectionItemViewLongClickable({ connectMobileDevice(host) }, { showMenu.value = true }, disabled = connecting.value) {
          Text(host.hostDeviceName)
          Spacer(Modifier.weight(1f))
          if (host.activeHost) {
            Icon(painterResource(MR.images.ic_done_filled), null, Modifier.size(20.dp), tint = MaterialTheme.colors.onBackground)
          } else if (host.sessionState is RemoteHostSessionState.Connected) {
            HostDisconnectButton { stopRemoteHost(host) }
          }
        }
        Box(Modifier.padding(horizontal = DEFAULT_PADDING)) {
          DefaultDropdownMenu(showMenu) {
            ItemAction(stringResource(MR.strings.delete_verb), painterResource(MR.images.ic_delete), color = Color.Red, onClick = {
              deleteHost(host)
              showMenu.value = false
            })
          }
        }
      }
      SettingsActionItem(painterResource(MR.images.ic_smartphone), stringResource(MR.strings.link_a_mobile), addMobileDevice, disabled = connecting.value, extraPadding = false)
    }
  }
}

@Composable
private fun DeviceNameField(
  initialValue: String,
  onChange: (String) -> Unit
) {
  // TODO get user-defined device name
  val state = remember { mutableStateOf(TextFieldValue(initialValue)) }
  val colors = TextFieldDefaults.textFieldColors(
    backgroundColor = Color.Unspecified,
    textColor = MaterialTheme.colors.onBackground,
    focusedIndicatorColor = Color.Unspecified,
    unfocusedIndicatorColor = Color.Unspecified,
  )
  val enabled = true
  val shape = MaterialTheme.shapes.small.copy(bottomEnd = ZeroCornerSize, bottomStart = ZeroCornerSize)
  val interactionSource = remember { MutableInteractionSource() }
  BasicTextField(
    value = state.value,
    modifier = Modifier
      .padding(horizontal = DEFAULT_PADDING)
      .fillMaxWidth()
      .background(colors.backgroundColor(enabled).value, shape)
      .indicatorLine(enabled, false, interactionSource, colors)
      .defaultMinSize(
        minWidth = TextFieldDefaults.MinWidth,
        minHeight = TextFieldDefaults.MinHeight
      ),
    onValueChange = {
      state.value = it
      onChange(it.text)
    },
    cursorBrush = SolidColor(colors.cursorColor(false).value),
    singleLine = true,
    textStyle = TextStyle.Default.copy(
      color = MaterialTheme.colors.onBackground,
      fontWeight = FontWeight.Normal,
      fontSize = 16.sp
    ),
    interactionSource = interactionSource,
    decorationBox = @Composable { innerTextField ->
      TextFieldDefaults.TextFieldDecorationBox(
        value = state.value.text,
        innerTextField = innerTextField,
        placeholder = { Text(generalGetString(MR.strings.enter_this_device_name), color = MaterialTheme.colors.secondary) },
        singleLine = true,
        enabled = enabled,
        isError = false,
        interactionSource = interactionSource,
        contentPadding = TextFieldDefaults.textFieldWithLabelPadding(start = 0.dp, end = 0.dp),
        visualTransformation = VisualTransformation.None,
        colors = colors
      )
    }
  )
}

@Composable
private fun ConnectMobileViewLayout(
  title: String,
  invitation: String?,
  deviceName: String?,
  sessionCode: String?
) {
  Column(
    Modifier.fillMaxWidth().verticalScroll(rememberScrollState()),
    verticalArrangement = Arrangement.spacedBy(8.dp)
  ) {
    AppBarTitle(title)
    SectionView {
      if (invitation != null && deviceName == null && sessionCode == null) {
        QRCode(
          invitation, Modifier
            .padding(start = DEFAULT_PADDING, top = DEFAULT_PADDING_HALF, end = DEFAULT_PADDING, bottom = DEFAULT_PADDING)
            .aspectRatio(1f)
        )
      }
      if (deviceName != null || sessionCode != null) {
        SectionView(stringResource(MR.strings.connected_mobile).uppercase()) {
          SelectionContainer {
            Text(
              deviceName ?: stringResource(MR.strings.new_device),
              Modifier.padding(start = DEFAULT_PADDING, top = 5.dp, end = DEFAULT_PADDING, bottom = 10.dp),
              style = TextStyle(fontFamily = FontFamily.Monospace, fontSize = 16.sp, fontStyle = if (deviceName != null) FontStyle.Normal else FontStyle.Italic)
            )
          }
        }
        Spacer(Modifier.height(DEFAULT_PADDING_HALF))
      }

      if (sessionCode != null) {
        SectionView(stringResource(MR.strings.verify_code_with_mobile).uppercase()) {
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
    showConnectedMobileDevice(rh)
  } else {
    showConnectMobileDevice(rh, connecting)
  }
}

private fun showConnectMobileDevice(rh: RemoteHostInfo, connecting: MutableState<Boolean>) {
  ModalManager.start.showModalCloseable { close ->
    val pairing = remember { chatModel.newRemoteHostPairing }
    val invitation = rememberSaveable { mutableStateOf<String?>(null) }
    val sessionCode = when (val state = pairing.value?.second) {
      is RemoteHostSessionState.PendingConfirmation -> state.sessionCode
      else -> null
    }
    ConnectMobileViewLayout(
      title = stringResource(MR.strings.scan_from_mobile),
      invitation = invitation.value,
      deviceName = pairing.value?.first?.hostDeviceName,
      sessionCode = sessionCode,
    )
    var remoteHostId by rememberSaveable { mutableStateOf<Long?>(null) }
    LaunchedEffect(Unit) {
      val r = chatModel.controller.startRemoteHost(rh.remoteHostId)
      if (r != null) {
        val (rh_, inv) = r
        connecting.value = true
        remoteHostId = rh_?.remoteHostId
        invitation.value = inv
      }
    }
    LaunchedEffect(remember { chatModel.currentRemoteHost }.value) {
      if (remoteHostId != null && chatModel.currentRemoteHost.value?.remoteHostId == remoteHostId) {
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

private fun showConnectedMobileDevice(rh: RemoteHostInfo) {
  ModalManager.start.showModalCloseable { close ->
    val sessionCode = when (val state = rh.sessionState) {
      is RemoteHostSessionState.Connected -> state.sessionCode
      else -> null
    }
    ConnectMobileViewLayout(
      title = stringResource(MR.strings.verify_connection),
      invitation = null,
      deviceName = rh.hostDeviceName,
      sessionCode = sessionCode
    )
    KeyChangeEffect(remember { chatModel.currentRemoteHost }.value) {
      close()
    }
  }
}
