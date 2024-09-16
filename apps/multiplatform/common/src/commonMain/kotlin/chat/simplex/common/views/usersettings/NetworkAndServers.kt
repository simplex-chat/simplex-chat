package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionItemWithValue
import SectionTextFooter
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
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import kotlinx.serialization.Serializable

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
    onionHosts = remember { mutableStateOf(netCfg.onionHosts) },
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
  onionHosts: MutableState<OnionHosts>,
  toggleSocksProxy: (Boolean) -> Unit,
) {
  val m = chatModel
  ColumnWithScrollBar(Modifier.fillMaxWidth()) {
    val showModal = { it: @Composable ModalData.() -> Unit ->  ModalManager.start.showModal(content = it) }
    val showCustomModal = { it: @Composable (close: () -> Unit) -> Unit -> ModalManager.start.showCustomModal { close -> it(close) }}

    AppBarTitle(stringResource(MR.strings.network_and_servers))
    if (!chatModel.desktopNoUserNoRemote) {
      SectionView(generalGetString(MR.strings.settings_section_title_messages)) {
        SettingsActionItem(painterResource(MR.images.ic_dns), stringResource(MR.strings.message_servers), { ModalManager.start.showCustomModal { close -> ProtocolServersView(m, m.remoteHostId, ServerProtocol.SMP, close) } })

        SettingsActionItem(painterResource(MR.images.ic_dns), stringResource(MR.strings.media_and_file_servers), { ModalManager.start.showCustomModal { close -> ProtocolServersView(m, m.remoteHostId, ServerProtocol.XFTP, close) } })

        if (currentRemoteHost == null) {
          UseSocksProxySwitch(networkUseSocksProxy, toggleSocksProxy)
          SettingsActionItem(painterResource(MR.images.ic_settings_ethernet), stringResource(MR.strings.network_socks_proxy_settings), { showCustomModal { SocksProxySettings(networkUseSocksProxy.value, appPrefs.networkProxyHostPort, onionHosts, false, it) }})
          SettingsActionItem(painterResource(MR.images.ic_cable), stringResource(MR.strings.network_settings), { ModalManager.start.showCustomModal { AdvancedNetworkSettingsView(showModal, it) } })
          if (networkUseSocksProxy.value) {
            SectionTextFooter(annotatedStringResource(MR.strings.socks_proxy_setting_limitations))
            SectionDividerSpaced(maxTopPadding = true)
          } else {
            SectionDividerSpaced()
          }
        }
      }
    }

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
  updateSessionMode: (TransportSessionMode) -> Unit,
) {
  val showModal = { it: @Composable ModalData.() -> Unit ->  ModalManager.fullscreen.showModal(content = it) }
  val showCustomModal = { it: @Composable (close: () -> Unit) -> Unit -> ModalManager.fullscreen.showCustomModal { close -> it(close) }}
  UseSocksProxySwitch(networkUseSocksProxy, toggleSocksProxy)
  SettingsActionItem(painterResource(MR.images.ic_settings_ethernet), stringResource(MR.strings.network_socks_proxy_settings), { showCustomModal { SocksProxySettings(networkUseSocksProxy.value, networkProxyHostPort, onionHosts, true, it) } })
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
fun SocksProxySettings(
  networkUseSocksProxy: Boolean,
  networkProxyHostPort: SharedPreference<String?> = appPrefs.networkProxyHostPort,
  onionHosts: MutableState<OnionHosts>,
  migration: Boolean,
  close: () -> Unit
) {
  val defaultHostPort = remember { "localhost:9050" }
  val proxyStringSaved by remember { networkProxyHostPort.state }
  val proxyComponentsSaved by remember(proxyStringSaved) { mutableStateOf(ProxyComponents.from(proxyStringSaved)) }
  val onionHostsSaved = remember { mutableStateOf(onionHosts.value) }

  val usernameUnsaved = rememberSaveable(stateSaver = TextFieldValue.Saver) {
    mutableStateOf(TextFieldValue(proxyComponentsSaved.usernamePassword.first))
  }
  val passwordUnsaved = rememberSaveable(stateSaver = TextFieldValue.Saver) {
    mutableStateOf(TextFieldValue(proxyComponentsSaved.usernamePassword.second))
  }
  val hostUnsaved = rememberSaveable(stateSaver = TextFieldValue.Saver) {
    mutableStateOf(TextFieldValue(proxyComponentsSaved.host))
  }
  val portUnsaved = rememberSaveable(stateSaver = TextFieldValue.Saver) {
    mutableStateOf(TextFieldValue(proxyComponentsSaved.port.toString()))
  }
  val proxyAuthRandomUnsaved = rememberSaveable { mutableStateOf(proxyComponentsSaved.authMode == ProxyAuthenticationMode.ISOLATE_BY_AUTH) }
  LaunchedEffect(proxyAuthRandomUnsaved.value) {
    if (!proxyAuthRandomUnsaved.value && onionHosts.value != OnionHosts.NEVER) {
      onionHosts.value = OnionHosts.NEVER
    }
  }
  val proxyAuthModeUnsaved = remember(proxyAuthRandomUnsaved.value, usernameUnsaved.value.text, passwordUnsaved.value.text) {
    derivedStateOf {
      if (proxyAuthRandomUnsaved.value) {
        ProxyAuthenticationMode.ISOLATE_BY_AUTH
      } else if (usernameUnsaved.value.text.isBlank() && passwordUnsaved.value.text.isBlank()) {
        ProxyAuthenticationMode.NO_AUTH
      } else {
        ProxyAuthenticationMode.USERNAME_PASSWORD
      }
    }
  }

  val save: (Boolean) -> Unit = { closeOnSuccess ->
    val oldValue = networkProxyHostPort.get()
    networkProxyHostPort.set(
      ProxyComponents(
        usernamePassword = usernameUnsaved.value.text to passwordUnsaved.value.text,
        host = hostUnsaved.value.text,
        port = portUnsaved.value.text.trim().toIntOrNull() ?: 9050,
        authMode = proxyAuthModeUnsaved.value
      ).toProxyString()
    )
    val oldCfg = controller.getNetCfg()
    val cfg = oldCfg.withOnionHosts(onionHosts.value)
    val oldOnionHosts = onionHostsSaved.value
    onionHostsSaved.value = onionHosts.value

    if (!migration) {
      controller.setNetCfg(cfg)
    }
    if (networkUseSocksProxy && !migration) {
      withBGApi {
        if (controller.apiSetNetworkConfig(cfg, showAlertOnError = false)) {
          val comp = ProxyComponents.from(networkProxyHostPort.get())
          usernameUnsaved.value = usernameUnsaved.value.copy(comp.usernamePassword.first)
          passwordUnsaved.value = passwordUnsaved.value.copy(comp.usernamePassword.second)
          hostUnsaved.value = hostUnsaved.value.copy(comp.host)
          portUnsaved.value = portUnsaved.value.copy(comp.port.toString())
          proxyAuthRandomUnsaved.value = comp.authMode == ProxyAuthenticationMode.ISOLATE_BY_AUTH
          onionHosts.value = cfg.onionHosts
          onionHostsSaved.value = onionHosts.value
          if (closeOnSuccess) {
            close()
          }
        } else {
          controller.setNetCfg(oldCfg)
          networkProxyHostPort.set(oldValue)
          onionHostsSaved.value = oldOnionHosts
          showWrongProxyConfigAlert()
        }
      }
    }
  }
  val saveDisabled =
    (
        proxyComponentsSaved.usernamePassword == usernameUnsaved.value.text.trim() to passwordUnsaved.value.text.trim() &&
        proxyComponentsSaved.host == hostUnsaved.value.text.trim() &&
        proxyComponentsSaved.port.toString() == portUnsaved.value.text.trim() &&
        proxyComponentsSaved.authMode == proxyAuthModeUnsaved.value &&
        onionHosts.value == onionHostsSaved.value
    ) ||
      remember { derivedStateOf { !validPort(portUnsaved.value.text) } }.value
  val resetDisabled = hostUnsaved.value.text.trim() + ":" + portUnsaved.value.text.trim() == defaultHostPort && proxyAuthRandomUnsaved.value && onionHosts.value == NetCfg.defaults.onionHosts
  ModalView(
    close = {
      if (saveDisabled) {
        close()
      } else {
        showUnsavedSocksHostPortAlert(
          confirmText = generalGetString(if (networkUseSocksProxy && !migration) MR.strings.network_options_save_and_reconnect else MR.strings.network_options_save),
          save = { save(true) },
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
      SectionView(stringResource(MR.strings.network_socks_proxy).uppercase()) {
        Column(Modifier.padding(horizontal = DEFAULT_PADDING)) {
          DefaultConfigurableTextField(
            hostUnsaved,
            stringResource(MR.strings.host_verb),
            modifier = Modifier.fillMaxWidth(),
            isValid = { true },
            keyboardActions = KeyboardActions(onNext = { defaultKeyboardAction(ImeAction.Next) }),
            keyboardType = KeyboardType.Text,
          )
          DefaultConfigurableTextField(
            portUnsaved,
            stringResource(MR.strings.port_verb),
            modifier = Modifier.fillMaxWidth(),
            isValid = ::validPort,
            keyboardActions = KeyboardActions(onDone = { defaultKeyboardAction(ImeAction.Done); save(false) }),
            keyboardType = KeyboardType.Number,
          )
        }

        UseOnionHosts(onionHosts, rememberUpdatedState(networkUseSocksProxy && proxyAuthRandomUnsaved.value)) {
          onionHosts.value = it
        }
        SectionTextFooter(annotatedStringResource(MR.strings.disable_onion_hosts_when_not_supported))
      }

      SectionDividerSpaced(maxTopPadding = true)

      SectionView(stringResource(MR.strings.network_proxy_auth).uppercase()) {
        PreferenceToggle(
          stringResource(MR.strings.network_proxy_random_credentials),
          checked = proxyAuthRandomUnsaved.value,
          onChange = { proxyAuthRandomUnsaved.value = it }
        )
        if (!proxyAuthRandomUnsaved.value) {
          Column(Modifier.padding(horizontal = DEFAULT_PADDING)) {
            DefaultConfigurableTextField(
              usernameUnsaved,
              stringResource(MR.strings.network_proxy_username),
              modifier = Modifier.fillMaxWidth(),
              isValid = { !it.contains(':') && !it.contains('@') },
              keyboardActions = KeyboardActions(onNext = { defaultKeyboardAction(ImeAction.Next) }),
              keyboardType = KeyboardType.Text,
            )
            DefaultConfigurableTextField(
              passwordUnsaved,
              stringResource(MR.strings.network_proxy_password),
              modifier = Modifier.fillMaxWidth(),
              isValid = { !it.contains(':') && !it.contains('@') },
              keyboardActions = KeyboardActions(onNext = { defaultKeyboardAction(ImeAction.Next) }),
              keyboardType = KeyboardType.Password,
            )
          }
        }
        SectionTextFooter(proxyAuthModeUnsaved.value.text)
      }

      SectionDividerSpaced(maxBottomPadding = false, maxTopPadding = true)

      SectionView {
        SectionItemView({
          val newHost = defaultHostPort.split(":").first()
          val newPort = defaultHostPort.split(":").last()
          hostUnsaved.value = hostUnsaved.value.copy(newHost, TextRange(newHost.length))
          portUnsaved.value = portUnsaved.value.copy(newPort, TextRange(newPort.length))
          usernameUnsaved.value = TextFieldValue()
          passwordUnsaved.value = TextFieldValue()
          proxyAuthRandomUnsaved.value = true
          onionHosts.value = NetCfg.defaults.onionHosts
        }, disabled = resetDisabled) {
          Text(stringResource(MR.strings.network_options_reset_to_defaults), color = if (resetDisabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary)
        }
        SectionItemView(
          click = { if (networkUseSocksProxy && !migration) showUpdateNetworkSettingsDialog { save(false) } else save(false) },
          disabled = saveDisabled
        ) {
          Text(stringResource(if (networkUseSocksProxy && !migration) MR.strings.network_options_save_and_reconnect else MR.strings.network_options_save), color = if (saveDisabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary)
        }
      }
      SectionBottomSpacer()
    }
  }
}

@Serializable
private data class ProxyComponents(
  val usernamePassword: Pair<String, String>,
  val host: String,
  val port: Int,
  val authMode: ProxyAuthenticationMode
) {
  fun toProxyString(): String? {
    var res = ""
    if (authMode == ProxyAuthenticationMode.USERNAME_PASSWORD && (usernamePassword.first.isNotBlank() || usernamePassword.second.isNotBlank())) {
      res += usernamePassword.first.trim() + ":" + usernamePassword.second.trim() + "@"
    } else if (authMode == ProxyAuthenticationMode.USERNAME_PASSWORD || authMode == ProxyAuthenticationMode.NO_AUTH) {
      res += "@"
    }
    if (host != "localhost") {
      res += if (host.contains(':')) "[${host.trim(' ', '[', ']')}]" else host.trim()
    }
    if (port != 9050) {
      res += ":$port"
    }
    return res.ifBlank { null }
  }
  companion object {
    fun from(proxy: String?): ProxyComponents {
      if (proxy == null) {
        return ProxyComponents(usernamePassword = "" to "", host = "localhost", port = 9050, authMode = ProxyAuthenticationMode.ISOLATE_BY_AUTH)
      }
      val username = if (proxy.contains("@")) proxy.substringBefore("@").substringBefore(":") else ""
      val password = if (proxy.contains("@")) proxy.substringBefore("@").substringAfter(":") else ""
      val hostPort = proxy.substringAfter("@")
      val host: String?
      val port: Int?
      if (hostPort.contains("[") && hostPort.contains("]")) {
        // ipv6 with or without port
        host = hostPort.substringBefore("]") + "]"
        port = hostPort.substringAfter("]").trim(':').toIntOrNull()
      } else {
        // ipv4 with or without port
        host = hostPort.substringBefore(":")
        port = hostPort.substringAfter(":").toIntOrNull()
      }
      return ProxyComponents(
        usernamePassword = username to password,
        host = host.ifBlank { "localhost" },
        port = port ?: 9050,
        authMode = ProxyAuthenticationMode.from(proxy)
      )
    }
  }
}

enum class ProxyAuthenticationMode {
  ISOLATE_BY_AUTH,
  NO_AUTH,
  USERNAME_PASSWORD;

  val text: String
    get() = when (this) {
      ISOLATE_BY_AUTH -> generalGetString(if (appPrefs.networkSessionMode.get() == TransportSessionMode.User) MR.strings.network_proxy_auth_mode_isolate_by_auth_user else MR.strings.network_proxy_auth_mode_isolate_by_auth_entity)
      NO_AUTH -> generalGetString(MR.strings.network_proxy_auth_mode_no_auth)
      USERNAME_PASSWORD -> generalGetString(MR.strings.network_proxy_auth_mode_username_password)
    }

  companion object {
    fun from(proxy: String?): ProxyAuthenticationMode = when {
      proxy.isNullOrEmpty() -> ISOLATE_BY_AUTH
      !proxy.contains("@") -> ISOLATE_BY_AUTH
      proxy.startsWith("@") -> NO_AUTH
      else -> USERNAME_PASSWORD
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

  Column {
    if (enabled.value) {
      ExposedDropDownSettingRow(
        generalGetString(MR.strings.network_use_onion_hosts),
        values.map { it.value to it.title },
        onionHosts,
        icon = painterResource(MR.images.ic_security),
        enabled = enabled,
        onSelected = useOnion
      )
    } else {
      // In reality, when socks proxy is disabled, this option acts like NEVER regardless of what was chosen before
      ExposedDropDownSettingRow(
        generalGetString(MR.strings.network_use_onion_hosts),
        listOf(OnionHosts.NEVER to generalGetString(MR.strings.network_use_onion_hosts_no)),
        remember { mutableStateOf(OnionHosts.NEVER) },
        icon = painterResource(MR.images.ic_security),
        enabled = enabled,
        onSelected = {}
      )
    }
    SectionTextFooter(values.first { it.value == onionHosts.value }.description)
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

// https://ihateregex.io/expr/port/
fun validPort(s: String): Boolean {
  val validPort = Regex("^(6553[0-5])|(655[0-2][0-9])|(65[0-4][0-9]{2})|(6[0-4][0-9]{3})|([1-5][0-9]{4})|([0-5]{0,5})|([0-9]{1,4})$")
  return s.isNotBlank() && s.matches(validPort)
}

fun showWrongProxyConfigAlert() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.network_proxy_incorrect_config_title),
    text = generalGetString(MR.strings.network_proxy_incorrect_config_desc),
  )
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
      onionHosts = remember { mutableStateOf(OnionHosts.PREFER) },
      toggleSocksProxy = {},
    )
  }
}
