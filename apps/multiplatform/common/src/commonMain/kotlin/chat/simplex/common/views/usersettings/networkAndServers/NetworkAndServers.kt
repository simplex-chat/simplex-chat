package chat.simplex.common.views.usersettings.networkAndServers

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
import androidx.compose.foundation.Image
import androidx.compose.ui.graphics.*
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatController.getUserServers
import chat.simplex.common.model.ChatController.setUserServers
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.res.MR
import kotlinx.coroutines.launch

@Composable
fun ModalData.NetworkAndServersView(close: () -> Unit) {
  val currentRemoteHost by remember { chatModel.currentRemoteHost }
  // It's not a state, just a one-time value. Shouldn't be used in any state-related situations
  val netCfg = remember { chatModel.controller.getNetCfg() }
  val networkUseSocksProxy: MutableState<Boolean> = remember { mutableStateOf(netCfg.useSocksProxy) }
  val currUserServers = remember { stateGetOrPut("currUserServers") { emptyList<UserOperatorServers>() } }
  val userServers = remember { stateGetOrPut("userServers") { emptyList<UserOperatorServers>() } }
  val serverErrors = remember { stateGetOrPut("serverErrors") { emptyList<UserServersError>() } }

  val scope = rememberCoroutineScope()

  val proxyPort = remember { derivedStateOf { appPrefs.networkProxy.state.value.port } }
  ModalView(
    close = {
      if (currUserServers.value == userServers.value) {
        close()
      } else {
        showUnsavedChangesAlert(
          { scope.launch { saveServers(currentRemoteHost?.remoteHostId, currUserServers, userServers) }},
          close
        )
      }
    }
  ) {
    NetworkAndServersLayout(
      currentRemoteHost = currentRemoteHost,
      networkUseSocksProxy = networkUseSocksProxy,
      onionHosts = remember { mutableStateOf(netCfg.onionHosts) },
      currUserServers = currUserServers,
      userServers = userServers,
      serverErrors = serverErrors,
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
                var conf = controller.getNetCfg().withProxy(controller.appPrefs.networkProxy.get())
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
}

@Composable fun NetworkAndServersLayout(
  currentRemoteHost: RemoteHostInfo?,
  networkUseSocksProxy: MutableState<Boolean>,
  onionHosts: MutableState<OnionHosts>,
  currUserServers: MutableState<List<UserOperatorServers>>,
  serverErrors: MutableState<List<UserServersError>>,
  userServers: MutableState<List<UserOperatorServers>>,
  toggleSocksProxy: (Boolean) -> Unit,
) {
  val m = chatModel
  val conditionsAction = remember { m.conditions.value.conditionsAction }
  val anyOperatorEnabled = remember { derivedStateOf { userServers.value.any { it.operator?.enabled == true } } }
  val scope = rememberCoroutineScope()

  LaunchedEffect(Unit) {
    if (currUserServers.value.isNotEmpty() || userServers.value.isNotEmpty()) {
      return@LaunchedEffect
    }
    try {
      val servers = getUserServers(rh = currentRemoteHost?.remoteHostId)
      if (servers != null) {
        currUserServers.value = servers
        userServers.value = servers
      }
    } catch (e: Exception) {
      Log.e(TAG, e.stackTraceToString())
    }
  }

  ColumnWithScrollBar {
    val showModal = { it: @Composable ModalData.() -> Unit ->  ModalManager.start.showModal(content = it) }
    val showCustomModal = { it: @Composable (close: () -> Unit) -> Unit -> ModalManager.start.showCustomModal { close -> it(close) }}

    AppBarTitle(stringResource(MR.strings.network_and_servers))
    // TODO: Review this and socks.
    if (!chatModel.desktopNoUserNoRemote) {
      SectionView(generalGetString(MR.strings.network_preset_servers_title).uppercase()) {
        userServers.value.forEachIndexed { index, srv ->
          srv.operator?.let { ServerOperatorRow(index, it, currUserServers, userServers, serverErrors, currentRemoteHost?.remoteHostId) }
        }
      }
      if (conditionsAction != null && anyOperatorEnabled.value) {
        ConditionsButton(conditionsAction)
      }
      val footerText = if (conditionsAction is UsageConditionsAction.Review && conditionsAction.deadline != null && anyOperatorEnabled.value) {
        String.format(generalGetString(MR.strings.operator_conditions_accepted_for_enabled_operators_on), localTimestamp(conditionsAction.deadline))
      } else null

      if (footerText != null) {
        SectionTextFooter(footerText)
      }
      SectionDividerSpaced()
    }

    SectionView(generalGetString(MR.strings.settings_section_title_messages)) {
      val nullOperatorIndex = userServers.value.indexOfFirst { it.operator == null }

      if (nullOperatorIndex != -1) {
        SectionItemView({
          ModalManager.start.showModal {
            YourServersView(
              currUserServers = currUserServers,
              userServers = userServers,
              serverErrors = serverErrors,
              operatorIndex = nullOperatorIndex,
              rhId = currentRemoteHost?.remoteHostId
            )
          }
        }) {
          Icon(
            painterResource(MR.images.ic_dns),
            stringResource(MR.strings.your_servers),
            tint = MaterialTheme.colors.secondary
          )
          TextIconSpaced()
          Text(stringResource(MR.strings.your_servers), color = MaterialTheme.colors.onBackground)

          if (currUserServers.value.getOrNull(nullOperatorIndex) != userServers.value.getOrNull(nullOperatorIndex)) {
            Spacer(Modifier.weight(1f))
            UnsavedChangesIndicator()
          }
        }
      }

      if (currentRemoteHost == null) {
        UseSocksProxySwitch(networkUseSocksProxy, toggleSocksProxy)
        SettingsActionItem(painterResource(MR.images.ic_settings_ethernet), stringResource(MR.strings.network_socks_proxy_settings), { showCustomModal { SocksProxySettings(networkUseSocksProxy.value, appPrefs.networkProxy, onionHosts, sessionMode = appPrefs.networkSessionMode.get(), false, it) }})
        SettingsActionItem(painterResource(MR.images.ic_cable), stringResource(MR.strings.network_settings), { ModalManager.start.showCustomModal { AdvancedNetworkSettingsView(showModal, it) } })
        if (networkUseSocksProxy.value) {
          SectionTextFooter(annotatedStringResource(MR.strings.socks_proxy_setting_limitations))
          SectionDividerSpaced(maxTopPadding = true)
        } else {
          SectionDividerSpaced(maxBottomPadding = false)
        }
      }
    }

    val saveDisabled = userServers.value == currUserServers.value

    SectionItemView(
      { scope.launch { saveServers(rhId = currentRemoteHost?.remoteHostId, currUserServers, userServers) } },
      disabled = saveDisabled,
    ) {
      Text(stringResource(MR.strings.smp_servers_save), color = if (!saveDisabled) MaterialTheme.colors.onBackground else MaterialTheme.colors.secondary)
    }

    SectionDividerSpaced()

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
  networkProxy: SharedPreference<NetworkProxy>,
  toggleSocksProxy: (Boolean) -> Unit,
  updateSessionMode: (TransportSessionMode) -> Unit,
) {
  val showModal = { it: @Composable ModalData.() -> Unit ->  ModalManager.fullscreen.showModal(content = it) }
  val showCustomModal = { it: @Composable (close: () -> Unit) -> Unit -> ModalManager.fullscreen.showCustomModal { close -> it(close) }}
  UseSocksProxySwitch(networkUseSocksProxy, toggleSocksProxy)
  SettingsActionItem(painterResource(MR.images.ic_settings_ethernet), stringResource(MR.strings.network_socks_proxy_settings), { showCustomModal { SocksProxySettings(networkUseSocksProxy.value, networkProxy, onionHosts, sessionMode.value, true, it) } })
  SessionModePicker(sessionMode, showModal, updateSessionMode)
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
  networkProxy: SharedPreference<NetworkProxy>,
  onionHosts: MutableState<OnionHosts>,
  sessionMode: TransportSessionMode,
  migration: Boolean,
  close: () -> Unit
) {
  val networkProxySaved by remember { networkProxy.state }
  val onionHostsSaved = remember { mutableStateOf(onionHosts.value) }

  val usernameUnsaved = rememberSaveable(stateSaver = TextFieldValue.Saver) {
    mutableStateOf(TextFieldValue(networkProxySaved.username))
  }
  val passwordUnsaved = rememberSaveable(stateSaver = TextFieldValue.Saver) {
    mutableStateOf(TextFieldValue(networkProxySaved.password))
  }
  val hostUnsaved = rememberSaveable(stateSaver = TextFieldValue.Saver) {
    mutableStateOf(TextFieldValue(networkProxySaved.host))
  }
  val portUnsaved = rememberSaveable(stateSaver = TextFieldValue.Saver) {
    mutableStateOf(TextFieldValue(networkProxySaved.port.toString()))
  }
  val proxyAuthRandomUnsaved = rememberSaveable { mutableStateOf(networkProxySaved.auth == NetworkProxyAuth.ISOLATE) }
  LaunchedEffect(proxyAuthRandomUnsaved.value) {
    if (!proxyAuthRandomUnsaved.value && onionHosts.value != OnionHosts.NEVER) {
      onionHosts.value = OnionHosts.NEVER
    }
  }
  val proxyAuthModeUnsaved = remember(proxyAuthRandomUnsaved.value, usernameUnsaved.value.text, passwordUnsaved.value.text) {
    derivedStateOf {
      if (proxyAuthRandomUnsaved.value) {
        NetworkProxyAuth.ISOLATE
      } else {
        NetworkProxyAuth.USERNAME
      }
    }
  }

  val save: (Boolean) -> Unit = { closeOnSuccess ->
    val oldValue = networkProxy.get()
    usernameUnsaved.value = usernameUnsaved.value.copy(if (proxyAuthModeUnsaved.value == NetworkProxyAuth.USERNAME) usernameUnsaved.value.text.trim() else "")
    passwordUnsaved.value = passwordUnsaved.value.copy(if (proxyAuthModeUnsaved.value == NetworkProxyAuth.USERNAME) passwordUnsaved.value.text.trim() else "")
    hostUnsaved.value = hostUnsaved.value.copy(hostUnsaved.value.text.trim())
    portUnsaved.value = portUnsaved.value.copy(portUnsaved.value.text.trim())

    networkProxy.set(
      NetworkProxy(
        username = usernameUnsaved.value.text,
        password = passwordUnsaved.value.text,
        host = hostUnsaved.value.text,
        port = portUnsaved.value.text.toIntOrNull() ?: 9050,
        auth = proxyAuthModeUnsaved.value
      )
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
          onionHosts.value = cfg.onionHosts
          onionHostsSaved.value = onionHosts.value
          if (closeOnSuccess) {
            close()
          }
        } else {
          controller.setNetCfg(oldCfg)
          networkProxy.set(oldValue)
          onionHostsSaved.value = oldOnionHosts
          showWrongProxyConfigAlert()
        }
      }
    }
  }
  val saveDisabled =
    (
        networkProxySaved.username == usernameUnsaved.value.text.trim() &&
        networkProxySaved.password == passwordUnsaved.value.text.trim() &&
        networkProxySaved.host == hostUnsaved.value.text.trim() &&
        networkProxySaved.port.toString() == portUnsaved.value.text.trim() &&
        networkProxySaved.auth == proxyAuthModeUnsaved.value &&
        onionHosts.value == onionHostsSaved.value
    ) ||
        !validCredential(usernameUnsaved.value.text) ||
        !validCredential(passwordUnsaved.value.text) ||
        !validHost(hostUnsaved.value.text) ||
        !validPort(portUnsaved.value.text)
  val resetDisabled = hostUnsaved.value.text.trim() == "localhost" && portUnsaved.value.text.trim() == "9050" && proxyAuthRandomUnsaved.value && onionHosts.value == NetCfg.defaults.onionHosts
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
    ColumnWithScrollBar {
      AppBarTitle(generalGetString(MR.strings.network_socks_proxy_settings))
      SectionView(stringResource(MR.strings.network_socks_proxy).uppercase()) {
        Column(Modifier.padding(horizontal = DEFAULT_PADDING)) {
          DefaultConfigurableTextField(
            hostUnsaved,
            stringResource(MR.strings.host_verb),
            modifier = Modifier.fillMaxWidth(),
            isValid = ::validHost,
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
              isValid = ::validCredential,
              keyboardActions = KeyboardActions(onNext = { defaultKeyboardAction(ImeAction.Next) }),
              keyboardType = KeyboardType.Text,
            )
            DefaultConfigurableTextField(
              passwordUnsaved,
              stringResource(MR.strings.network_proxy_password),
              modifier = Modifier.fillMaxWidth(),
              isValid = ::validCredential,
              keyboardActions = KeyboardActions(onNext = { defaultKeyboardAction(ImeAction.Next) }),
              keyboardType = KeyboardType.Password,
            )
          }
        }
        SectionTextFooter(proxyAuthFooter(usernameUnsaved.value.text, passwordUnsaved.value.text, proxyAuthModeUnsaved.value, sessionMode))
      }

      SectionDividerSpaced(maxBottomPadding = false, maxTopPadding = true)

      SectionView {
        SectionItemView({
          hostUnsaved.value = hostUnsaved.value.copy("localhost", TextRange(9))
          portUnsaved.value = portUnsaved.value.copy("9050", TextRange(4))
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

private fun proxyAuthFooter(username: String, password: String, auth: NetworkProxyAuth, sessionMode: TransportSessionMode): String = when {
  auth == NetworkProxyAuth.ISOLATE -> generalGetString(if (sessionMode == TransportSessionMode.User) MR.strings.network_proxy_auth_mode_isolate_by_auth_user else MR.strings.network_proxy_auth_mode_isolate_by_auth_entity)
  username.isBlank() && password.isBlank() -> generalGetString(MR.strings.network_proxy_auth_mode_no_auth)
  else -> generalGetString(MR.strings.network_proxy_auth_mode_username_password)
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
    val safeModes = TransportSessionMode.safeValues
    val modes: Array<TransportSessionMode> =
      if (appPrefs.developerTools.get()) TransportSessionMode.values()
      else if (safeModes.contains(sessionMode.value)) safeModes
      else safeModes + sessionMode.value
    modes.map {
      val userModeDescr: AnnotatedString = escapedHtmlToAnnotatedString(generalGetString(MR.strings.network_session_mode_user_description), density)
      when (it) {
        TransportSessionMode.User -> ValueTitleDesc(TransportSessionMode.User, generalGetString(MR.strings.network_session_mode_user), userModeDescr)
        TransportSessionMode.Session -> ValueTitleDesc(TransportSessionMode.Session, generalGetString(MR.strings.network_session_mode_session), userModeDescr + AnnotatedString("\n") + escapedHtmlToAnnotatedString(generalGetString(MR.strings.network_session_mode_session_description), density))
        TransportSessionMode.Server -> ValueTitleDesc(TransportSessionMode.Server, generalGetString(MR.strings.network_session_mode_server), userModeDescr + AnnotatedString("\n") + escapedHtmlToAnnotatedString(generalGetString(MR.strings.network_session_mode_server_description), density))
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
        ColumnWithScrollBar {
          AppBarTitle(stringResource(MR.strings.network_session_mode_transport_isolation))
          SectionViewSelectable(null, sessionMode, values, updateSessionMode)
        }
      }
    }
  )
}

private fun validHost(s: String): Boolean =
  !s.contains('@')

// https://ihateregex.io/expr/port/
fun validPort(s: String): Boolean {
  val validPort = Regex("^(6553[0-5])|(655[0-2][0-9])|(65[0-4][0-9]{2})|(6[0-4][0-9]{3})|([1-5][0-9]{4})|([0-5]{0,5})|([0-9]{1,4})$")
  return s.isNotBlank() && s.matches(validPort)
}

private fun validCredential(s: String): Boolean =
  !s.contains(':') && !s.contains('@')

fun showWrongProxyConfigAlert() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.network_proxy_incorrect_config_title),
    text = generalGetString(MR.strings.network_proxy_incorrect_config_desc),
  )
}

@Composable()
private fun ServerOperatorRow(
  index: Int,
  operator: ServerOperator,
  currUserServers: MutableState<List<UserOperatorServers>>,
  userServers: MutableState<List<UserOperatorServers>>,
  serverErrors: MutableState<List<UserServersError>>,
  rhId: Long?
) {
  SectionItemView(
    {
      ModalManager.start.showModalCloseable { close ->
        OperatorView(
          currUserServers,
          userServers,
          serverErrors,
          index,
          rhId
        )
      }
    }
  ) {
    Image(
      painterResource(MR.images.decentralized),
      operator.tradeName,
      modifier = Modifier.size(24.dp),
      colorFilter = if (operator.enabled) null else ColorFilter.colorMatrix(ColorMatrix().apply {
        setToSaturation(0f)
      })
    )
    TextIconSpaced()
    Text(operator.tradeName, color = if (operator.enabled) MaterialTheme.colors.onBackground else MaterialTheme.colors.secondary)

    if (currUserServers.value.getOrNull(index) != userServers.value.getOrNull(index)) {
      Spacer(Modifier.weight(1f))
      UnsavedChangesIndicator()
    }
  }
}

@Composable
private fun UnsavedChangesIndicator() {
  Icon(
    painterResource(MR.images.ic_edit_filled),
    stringResource(MR.strings.icon_descr_edited),
    tint = MaterialTheme.colors.secondary,
    modifier = Modifier.size(16.dp)
  )
}

@Composable
private fun ModalData.UsageConditionsView(conditionsAction: UsageConditionsAction, close: () -> Unit) {
  ModalView(close = close) {
    Text("Hello")
  }
}

@Composable
private fun ConditionsButton(conditionsAction: UsageConditionsAction) {
  SectionItemView(
    click = { ModalManager.start.showModalCloseable { close -> UsageConditionsView(conditionsAction, close) } },
  ) {
    Text(
      stringResource(if (conditionsAction is UsageConditionsAction.Review) MR.strings.operator_review_conditions else MR.strings.operator_conditions_accepted),
      color = MaterialTheme.colors.primary
    )
  }
}

@Composable
fun ServerErrorsView(errStr: String) {
  Row {
    Icon(
      painterResource(MR.images.ic_error),
      contentDescription = stringResource(MR.strings.server_error),
      tint = Color.Red,
      modifier = Modifier
        .size(19.sp.toDp())
        .offset(x = 2.sp.toDp())
    )
    TextIconSpaced()
    Text(errStr, color = MaterialTheme.colors.secondary)
  }
}

private fun showUnsavedChangesAlert(save: () -> Unit, revert: () -> Unit) {
  AlertManager.shared.showAlertDialogStacked(
    title = generalGetString(MR.strings.smp_save_servers_question),
    confirmText = generalGetString(MR.strings.save_verb),
    dismissText = generalGetString(MR.strings.exit_without_saving),
    onConfirm = save,
    onDismiss = revert,
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

fun updateOperatorsConditionsAcceptance(usvs: MutableState<List<UserOperatorServers>>, updatedOperators: List<ServerOperator>) {
  for (i in usvs.value.indices) {
    val updatedOperator = updatedOperators.firstOrNull { it.operatorId == usvs.value[i].operator?.operatorId } ?: continue
    usvs.value[i].operator?.conditionsAcceptance = updatedOperator.conditionsAcceptance
  }
}

suspend fun validateServers(
  rhId: Long?,
  userServers: MutableState<List<UserOperatorServers>>,
  serverErrors: MutableState<List<UserServersError>>
) {
  val userServersToValidate = userServers.value

  try {
    val errors = chatController.validateServers(rhId, userServersToValidate) ?: return
    serverErrors.value = errors
  } catch (ex: Exception) {
    Log.e(TAG, ex.stackTraceToString())
  }
}

fun serversCanBeSaved(
  currUserServers: List<UserOperatorServers>,
  userServers: List<UserOperatorServers>,
  serverErrors: List<UserServersError>
): Boolean {
  return userServers != currUserServers && serverErrors.isEmpty()
}

fun globalServersError(serverErrors: List<UserServersError>): String? {
  for (err in serverErrors) {
    if (err.globalError != null) {
      return err.globalError
    }
  }
  return null
}

fun globalSMPServersError(serverErrors: List<UserServersError>): String? {
  for (err in serverErrors) {
    if (err.globalSMPError != null) {
      return err.globalSMPError
    }
  }
  return null
}

fun globalXFTPServersError(serverErrors: List<UserServersError>): String? {
  for (err in serverErrors) {
    if (err.globalXFTPError != null) {
      return err.globalXFTPError
    }
  }
  return null
}

fun findDuplicateHosts(serverErrors: List<UserServersError>): Set<String> {
  val duplicateHostsList = serverErrors.mapNotNull { err ->
    if (err is UserServersError.DuplicateServer) {
      err.duplicateHost
    } else {
      null
    }
  }
  return duplicateHostsList.toSet()
}

private suspend fun saveServers(
  rhId: Long?,
  currUserServers: MutableState<List<UserOperatorServers>>,
  userServers: MutableState<List<UserOperatorServers>>
) {
  val userServersToSave = userServers.value
  try {
    val set = setUserServers(rhId, userServersToSave)

    if (set) {
      val updatedServers = getUserServers(rhId)

      if (updatedServers != null) {
        currUserServers.value = updatedServers
        userServers.value = updatedServers
      } else {
        currUserServers.value = userServersToSave
      }
    } else {
      currUserServers.value = userServersToSave
    }
  } catch (ex: Exception) {
    Log.e(TAG, ex.stackTraceToString())
  }
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
      currUserServers =  remember { mutableStateOf(emptyList()) },
      userServers = remember { mutableStateOf(emptyList()) },
      serverErrors = remember { mutableStateOf(emptyList()) }
    )
  }
}