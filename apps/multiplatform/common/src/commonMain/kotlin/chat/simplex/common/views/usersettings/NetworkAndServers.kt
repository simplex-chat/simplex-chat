package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionItemWithValue
import SectionView
import SectionViewSelectable
import TextIconSpaced
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.KeyboardActions
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.*
import androidx.compose.ui.text.input.*
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR

@Composable
fun NetworkAndServersView() {
  val currentRemoteHost by remember { chatModel.currentRemoteHost }
  // It's not a state, just a one-time value. Shouldn't be used in any state-related situations
  val netCfg = remember { chatModel.controller.getNetCfg() }
  val networkUseSocksProxy: MutableState<Boolean> = remember { mutableStateOf(netCfg.useSocksProxy) }

  val proxyPort = remember { derivedStateOf { chatModel.controller.appPrefs.networkProxyHostPort.state.value?.split(":")?.lastOrNull()?.toIntOrNull() ?: 9050 } }
  NetworkAndServersLayout(
    currentRemoteHost = currentRemoteHost,
    networkUseSocksProxy = networkUseSocksProxy,
    toggleSocksProxy = { enable ->
      val def = NetCfg.defaults
      val proxyDef = NetCfg.proxyDefaults
      if (enable) {
        AlertManager.shared.showAlertDialog(
          title = generalGetString(MR.strings.network_enable_socks),
          text = generalGetString(MR.strings.network_enable_socks_info).format(proxyPort.value),
          confirmText = generalGetString(MR.strings.confirm_verb),
          onConfirm = {
            withBGApi {
              var conf = controller.getNetCfg().withHostPort(controller.appPrefs.networkProxyHostPort.get())
              if (conf.tcpConnectTimeout == def.tcpConnectTimeout) {
                conf = conf.copy(tcpConnectTimeout = proxyDef.tcpConnectTimeout)
              }
              if (conf.tcpTimeout == def.tcpTimeout) {
                conf = conf.copy(tcpTimeout = proxyDef.tcpTimeout)
              }
              if (conf.tcpTimeoutPerKb == def.tcpTimeoutPerKb) {
                conf = conf.copy(tcpTimeoutPerKb = proxyDef.tcpTimeoutPerKb)
              }
              if (conf.rcvConcurrency == def.rcvConcurrency) {
                conf = conf.copy(rcvConcurrency = proxyDef.rcvConcurrency)
              }
              chatModel.controller.apiSetNetworkConfig(conf)
              chatModel.controller.setNetCfg(conf)
              networkUseSocksProxy.value = true
            }
          }
        )
      } else {
        AlertManager.shared.showAlertDialog(
          title = generalGetString(MR.strings.network_disable_socks),
          text = generalGetString(MR.strings.network_disable_socks_info),
          confirmText = generalGetString(MR.strings.confirm_verb),
          onConfirm = {
            withBGApi {
              var conf = controller.getNetCfg().copy(socksProxy = null)
              if (conf.tcpConnectTimeout == proxyDef.tcpConnectTimeout) {
                conf = conf.copy(tcpConnectTimeout = def.tcpConnectTimeout)
              }
              if (conf.tcpTimeout == proxyDef.tcpTimeout) {
                conf = conf.copy(tcpTimeout = def.tcpTimeout)
              }
              if (conf.tcpTimeoutPerKb == proxyDef.tcpTimeoutPerKb) {
                conf = conf.copy(tcpTimeoutPerKb = def.tcpTimeoutPerKb)
              }
              if (conf.rcvConcurrency == proxyDef.rcvConcurrency) {
                conf = conf.copy(rcvConcurrency = def.rcvConcurrency)
              }
              chatModel.controller.apiSetNetworkConfig(conf)
              chatModel.controller.setNetCfg(conf)
              networkUseSocksProxy.value = false
            }
          }
        )
      }
    }
  )
}

@Composable fun NetworkAndServersLayout(
  currentRemoteHost: RemoteHostInfo?,
  networkUseSocksProxy: MutableState<Boolean>,
  toggleSocksProxy: (Boolean) -> Unit,
) {
  val m = chatModel
  ColumnWithScrollBar(
    Modifier.fillMaxWidth(),
    verticalArrangement = Arrangement.spacedBy(8.dp)
  ) {
    val showModal = { it: @Composable ModalData.() -> Unit ->  ModalManager.start.showModal(content = it) }
    val showCustomModal = { it: @Composable (close: () -> Unit) -> Unit -> ModalManager.start.showCustomModal { close -> it(close) }}

    AppBarTitle(stringResource(MR.strings.network_and_servers))
    if (!chatModel.desktopNoUserNoRemote) {
      SectionView(generalGetString(MR.strings.settings_section_title_messages)) {
        SettingsActionItem(painterResource(MR.images.ic_dns), stringResource(MR.strings.message_servers), { ModalManager.start.showCustomModal { close -> ProtocolServersView(m, m.remoteHostId, ServerProtocol.SMP, close) } })

        SettingsActionItem(painterResource(MR.images.ic_dns), stringResource(MR.strings.media_and_file_servers), { ModalManager.start.showCustomModal { close -> ProtocolServersView(m, m.remoteHostId, ServerProtocol.XFTP, close) } })

        if (currentRemoteHost == null) {
          UseSocksProxySwitch(networkUseSocksProxy, toggleSocksProxy)
          SocksProxyHostPort(showCustomModal, appPrefs.networkProxyHostPort, false)
          SettingsActionItem(painterResource(MR.images.ic_cable), stringResource(MR.strings.network_settings), { ModalManager.start.showCustomModal { AdvancedNetworkSettingsView(showModal, it) } })
        }
      }
    }
    Divider(Modifier.padding(start = DEFAULT_PADDING_HALF, top = 24.dp, end = DEFAULT_PADDING_HALF, bottom = 30.dp))

    SectionView(generalGetString(MR.strings.settings_section_title_calls)) {
      SettingsActionItem(painterResource(MR.images.ic_electrical_services), stringResource(MR.strings.webrtc_ice_servers), { ModalManager.start.showModal { RTCServersView(m) } })
    }

    if (appPlatform.isAndroid) {
      SectionDividerSpaced()
      SectionView(generalGetString(MR.strings.settings_section_title_network_connection).uppercase()) {
        val info = remember { chatModel.networkInfo }.value
        SettingsActionItemWithContent(icon = null, info.networkType.text) {
          Icon(painterResource(MR.images.ic_circle_filled), stringResource(MR.strings.icon_descr_server_status_connected), tint = if (info.online) Color.Green else MaterialTheme.colors.error)
        }
      }
    }
    SectionBottomSpacer()
  }
}

@Composable fun OnionRelatedLayout(
  developerTools: Boolean,
  networkUseSocksProxy: MutableState<Boolean>,
  onionHosts: MutableState<OnionHosts>,
  sessionMode: MutableState<TransportSessionMode>,
  networkProxyHostPort: SharedPreference<String?>,
  toggleSocksProxy: (Boolean) -> Unit,
  useOnion: (OnionHosts) -> Unit,
  updateSessionMode: (TransportSessionMode) -> Unit,
) {
  val showModal = { it: @Composable ModalData.() -> Unit ->  ModalManager.fullscreen.showModal(content = it) }
  val showCustomModal = { it: @Composable (close: () -> Unit) -> Unit -> ModalManager.fullscreen.showCustomModal { close -> it(close) }}
  UseSocksProxySwitch(networkUseSocksProxy, toggleSocksProxy)
  SocksProxyHostPort(showCustomModal, networkProxyHostPort, true)
  UseOnionHosts(onionHosts, networkUseSocksProxy, showModal, useOnion)
  if (developerTools) {
    SessionModePicker(sessionMode, showModal, updateSessionMode)
  }
}

@Composable
fun UseSocksProxySwitch(
  networkUseSocksProxy: MutableState<Boolean>,
  toggleSocksProxy: (Boolean) -> Unit,
) {
  Row(
    Modifier.fillMaxWidth().padding(end = DEFAULT_PADDING),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.SpaceBetween
  ) {
    Row(
      Modifier.weight(1f).padding(horizontal = DEFAULT_PADDING),
      verticalAlignment = Alignment.CenterVertically
    ) {
      Icon(
        painterResource(MR.images.ic_settings_ethernet),
        stringResource(MR.strings.network_socks_toggle_use_socks_proxy),
        tint = MaterialTheme.colors.secondary
      )
      TextIconSpaced(false)
      Text(generalGetString(MR.strings.network_socks_toggle_use_socks_proxy))
    }
    DefaultSwitch(
      checked = networkUseSocksProxy.value,
      onCheckedChange = toggleSocksProxy,
    )
  }
}

@Composable
fun SocksProxyHostPort(
  showCustomModal: (@Composable (close: () -> Unit) -> Unit) -> Unit,
  networkProxyHostPort: SharedPreference<String?> = appPrefs.networkProxyHostPort,
  migration: Boolean = false
) {
  SectionItemView({ showCustomModal { SocksProxySettings(networkProxyHostPort, migration, it) } }) {
    Text(stringResource(MR.strings.network_socks_proxy_settings))
  }
}

@Composable
fun SocksProxySettings(
  networkProxyHostPort: SharedPreference<String?> = appPrefs.networkProxyHostPort,
  migration: Boolean,
  close: () -> Unit
) {
  val defaultHostPort = remember { "localhost:9050" }
  val hostPortSaved by remember { networkProxyHostPort.state }
  val hostUnsaved = rememberSaveable(stateSaver = TextFieldValue.Saver) {
    mutableStateOf(TextFieldValue(hostPortSaved?.split(":")?.firstOrNull() ?: "localhost"))
  }
  val portUnsaved = rememberSaveable(stateSaver = TextFieldValue.Saver) {
    mutableStateOf(TextFieldValue(hostPortSaved?.split(":")?.lastOrNull() ?: "9050"))
  }
  val save = {
    networkProxyHostPort.set(hostUnsaved.value.text + ":" + portUnsaved.value.text)
    if (appPrefs.networkUseSocksProxy.get() && !migration) {
      withBGApi {
        controller.apiSetNetworkConfig(controller.getNetCfg())
      }
    }
  }
  val saveDisabled = hostPortSaved == (hostUnsaved.value.text + ":" + portUnsaved.value.text) ||
      remember { derivedStateOf { !validHost(hostUnsaved.value.text) } }.value ||
      remember { derivedStateOf { !validPort(portUnsaved.value.text) } }.value
  val resetDisabled = hostUnsaved.value.text + ":" + portUnsaved.value.text == defaultHostPort
  ModalView(
    close = {
      if (saveDisabled) {
        close()
      } else {
        showUnsavedSocksHostPortAlert(
          confirmText = generalGetString(if (appPrefs.networkUseSocksProxy.get() && !migration) MR.strings.network_options_save_and_reconnect else MR.strings.network_options_save),
          save = {
            save()
            close()
          },
          close = close
        )
      }
    },
  ) {
    ColumnWithScrollBar(
      Modifier
        .fillMaxWidth()
    ) {
      AppBarTitle(generalGetString(MR.strings.network_socks_proxy_settings))
      SectionView {
        SectionItemView {
          DefaultConfigurableTextField(
            hostUnsaved,
            stringResource(MR.strings.host_verb),
            modifier = Modifier.fillMaxWidth(),
            isValid = ::validHost,
            keyboardActions = KeyboardActions(onNext = { defaultKeyboardAction(ImeAction.Next) }),
            keyboardType = KeyboardType.Text,
          )
        }
        SectionItemView {
          DefaultConfigurableTextField(
            portUnsaved,
            stringResource(MR.strings.port_verb),
            modifier = Modifier.fillMaxWidth(),
            isValid = ::validPort,
            keyboardActions = KeyboardActions(onDone = { defaultKeyboardAction(ImeAction.Done); save() }),
            keyboardType = KeyboardType.Number,
          )
        }
      }

      Divider(Modifier.padding(start = DEFAULT_PADDING_HALF, top = 27.dp, end = DEFAULT_PADDING_HALF, bottom = 30.dp))

      SectionView {
        SectionItemView({
          val newHost = defaultHostPort.split(":").first()
          val newPort = defaultHostPort.split(":").last()
          hostUnsaved.value = hostUnsaved.value.copy(newHost, TextRange(newHost.length))
          portUnsaved.value = portUnsaved.value.copy(newPort, TextRange(newPort.length))
        }, disabled = resetDisabled) {
          Text(stringResource(MR.strings.network_options_reset_to_defaults), color = if (resetDisabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary)
        }
        SectionItemView(
          click = { if (appPrefs.networkUseSocksProxy.get() && !migration) showUpdateNetworkSettingsDialog { save() } else save() },
          disabled = saveDisabled
        ) {
          Text(stringResource(MR.strings.network_options_save), color = if (saveDisabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary)
        }
      }
      SectionBottomSpacer()
    }
  }
}

private fun showUnsavedSocksHostPortAlert(confirmText: String, save: () -> Unit, close: () -> Unit) {
  AlertManager.shared.showAlertDialogStacked(
    title = generalGetString(MR.strings.update_network_settings_question),
    confirmText = confirmText,
    dismissText = generalGetString(MR.strings.exit_without_saving),
    onConfirm = save,
    onDismiss = close,
  )
}

@Composable
fun UseOnionHosts(
  onionHosts: MutableState<OnionHosts>,
  enabled: State<Boolean>,
  showModal: (@Composable ModalData.() -> Unit) -> Unit,
  useOnion: (OnionHosts) -> Unit,
) {
  val values = remember {
    OnionHosts.values().map {
      when (it) {
        OnionHosts.NEVER -> ValueTitleDesc(OnionHosts.NEVER, generalGetString(MR.strings.network_use_onion_hosts_no), AnnotatedString(generalGetString(MR.strings.network_use_onion_hosts_no_desc)))
        OnionHosts.PREFER -> ValueTitleDesc(OnionHosts.PREFER, generalGetString(MR.strings.network_use_onion_hosts_prefer), AnnotatedString(generalGetString(MR.strings.network_use_onion_hosts_prefer_desc)))
        OnionHosts.REQUIRED -> ValueTitleDesc(OnionHosts.REQUIRED, generalGetString(MR.strings.network_use_onion_hosts_required), AnnotatedString(generalGetString(MR.strings.network_use_onion_hosts_required_desc)))
      }
    }
  }
  val onSelected = {
    showModal {
      ColumnWithScrollBar(
        Modifier.fillMaxWidth(),
      ) {
        AppBarTitle(stringResource(MR.strings.network_use_onion_hosts))
        SectionViewSelectable(null, onionHosts, values, useOnion)
      }
    }
  }

  if (enabled.value) {
    SectionItemWithValue(
      generalGetString(MR.strings.network_use_onion_hosts),
      onionHosts,
      values,
      icon = painterResource(MR.images.ic_security),
      enabled = enabled,
      onSelected = onSelected
    )
  } else {
    // In reality, when socks proxy is disabled, this option acts like NEVER regardless of what was chosen before
    SectionItemWithValue(
      generalGetString(MR.strings.network_use_onion_hosts),
      remember { mutableStateOf(OnionHosts.NEVER) },
      listOf(ValueTitleDesc(OnionHosts.NEVER, generalGetString(MR.strings.network_use_onion_hosts_no), AnnotatedString(generalGetString(MR.strings.network_use_onion_hosts_no_desc)))),
      icon = painterResource(MR.images.ic_security),
      enabled = enabled,
      onSelected = {}
    )
  }
}

@Composable
fun SessionModePicker(
  sessionMode: MutableState<TransportSessionMode>,
  showModal: (@Composable ModalData.() -> Unit) -> Unit,
  updateSessionMode: (TransportSessionMode) -> Unit,
) {
  val density = LocalDensity.current
  val values = remember {
    TransportSessionMode.values().map {
      when (it) {
        TransportSessionMode.User -> ValueTitleDesc(TransportSessionMode.User, generalGetString(MR.strings.network_session_mode_user), escapedHtmlToAnnotatedString(generalGetString(MR.strings.network_session_mode_user_description), density))
        TransportSessionMode.Entity -> ValueTitleDesc(TransportSessionMode.Entity, generalGetString(MR.strings.network_session_mode_entity), escapedHtmlToAnnotatedString(generalGetString(MR.strings.network_session_mode_entity_description), density))
      }
    }
  }

  SectionItemWithValue(
    generalGetString(MR.strings.network_session_mode_transport_isolation),
    sessionMode,
    values,
    icon = painterResource(MR.images.ic_safety_divider),
    onSelected = {
      showModal {
        ColumnWithScrollBar(
          Modifier.fillMaxWidth(),
        ) {
          AppBarTitle(stringResource(MR.strings.network_session_mode_transport_isolation))
          SectionViewSelectable(null, sessionMode, values, updateSessionMode)
        }
      }
    }
  )
}

// https://stackoverflow.com/a/106223
private fun validHost(s: String): Boolean {
  val validIp = Regex("^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])[.]){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$")
  val validHostname = Regex("^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9-]*[a-zA-Z0-9])[.])*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9-]*[A-Za-z0-9])$");
  return s.matches(validIp) || s.matches(validHostname)
}

// https://ihateregex.io/expr/port/
fun validPort(s: String): Boolean {
  val validPort = Regex("^(6553[0-5])|(655[0-2][0-9])|(65[0-4][0-9]{2})|(6[0-4][0-9]{3})|([1-5][0-9]{4})|([0-5]{0,5})|([0-9]{1,4})$")
  return s.isNotBlank() && s.matches(validPort)
}

fun showUpdateNetworkSettingsDialog(
  title: String,
  startsWith: String = "",
  message: String = generalGetString(MR.strings.updating_settings_will_reconnect_client_to_all_servers),
  onDismiss: () -> Unit,
  onConfirm: () -> Unit
) {
  AlertManager.shared.showAlertDialog(
    title = title,
    text = startsWith + "\n\n" + message,
    confirmText = generalGetString(MR.strings.update_network_settings_confirmation),
    onDismiss = onDismiss,
    onConfirm = onConfirm,
    onDismissRequest = onDismiss
  )
}

@Preview
@Composable
fun PreviewNetworkAndServersLayout() {
  SimpleXTheme {
    NetworkAndServersLayout(
      currentRemoteHost = null,
      networkUseSocksProxy = remember { mutableStateOf(true) },
      toggleSocksProxy = {},
    )
  }
}
