package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionCustomFooter
import SectionItemView
import SectionItemWithValue
import SectionView
import SectionViewSelectable
import TextIconSpaced
import androidx.compose.foundation.*
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
import androidx.compose.ui.text.font.*
import androidx.compose.ui.text.input.*
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.item.ClickableText
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.helpers.annotatedStringResource
import chat.simplex.res.MR

@Composable
fun NetworkAndServersView() {
  val currentRemoteHost by remember { chatModel.currentRemoteHost }
  // It's not a state, just a one-time value. Shouldn't be used in any state-related situations
  val netCfg = remember { chatModel.controller.getNetCfg() }
  val networkUseSocksProxy: MutableState<Boolean> = remember { mutableStateOf(netCfg.useSocksProxy) }
  val developerTools = chatModel.controller.appPrefs.developerTools.get()
  val onionHosts = remember { mutableStateOf(netCfg.onionHosts) }
  val sessionMode = remember { mutableStateOf(netCfg.sessionMode) }

  val proxyPort = remember { derivedStateOf { chatModel.controller.appPrefs.networkProxyHostPort.state.value?.split(":")?.lastOrNull()?.toIntOrNull() ?: 9050 } }
  NetworkAndServersLayout(
    currentRemoteHost = currentRemoteHost,
    developerTools = developerTools,
    networkUseSocksProxy = networkUseSocksProxy,
    onionHosts = onionHosts,
    sessionMode = sessionMode,
    proxyPort = proxyPort,
    toggleSocksProxy = { enable ->
      if (enable) {
        AlertManager.shared.showAlertDialog(
          title = generalGetString(MR.strings.network_enable_socks),
          text = generalGetString(MR.strings.network_enable_socks_info).format(proxyPort.value),
          confirmText = generalGetString(MR.strings.confirm_verb),
          onConfirm = {
            withBGApi {
              val conf = NetCfg.proxyDefaults.withHostPort(chatModel.controller.appPrefs.networkProxyHostPort.get())
              chatModel.controller.apiSetNetworkConfig(conf)
              chatModel.controller.setNetCfg(conf)
              networkUseSocksProxy.value = true
              onionHosts.value = conf.onionHosts
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
              val conf = NetCfg.defaults
              chatModel.controller.apiSetNetworkConfig(conf)
              chatModel.controller.setNetCfg(conf)
              networkUseSocksProxy.value = false
              onionHosts.value = conf.onionHosts
            }
          }
        )
      }
    },
    useOnion = {
      if (onionHosts.value == it) return@NetworkAndServersLayout
      val prevValue = onionHosts.value
      onionHosts.value = it
      val startsWith = when (it) {
        OnionHosts.NEVER -> generalGetString(MR.strings.network_use_onion_hosts_no_desc_in_alert)
        OnionHosts.PREFER -> generalGetString(MR.strings.network_use_onion_hosts_prefer_desc_in_alert)
        OnionHosts.REQUIRED -> generalGetString(MR.strings.network_use_onion_hosts_required_desc_in_alert)
      }
      showUpdateNetworkSettingsDialog(
        title = generalGetString(MR.strings.update_onion_hosts_settings_question),
        startsWith,
        onDismiss = {
          onionHosts.value = prevValue
        }
      ) {
        withBGApi {
          val newCfg = chatModel.controller.getNetCfg().withOnionHosts(it)
          val res = chatModel.controller.apiSetNetworkConfig(newCfg)
          if (res) {
            chatModel.controller.setNetCfg(newCfg)
            onionHosts.value = it
          } else {
            onionHosts.value = prevValue
          }
        }
      }
    },
    updateSessionMode = {
      if (sessionMode.value == it) return@NetworkAndServersLayout
      val prevValue = sessionMode.value
      sessionMode.value = it
      val startsWith = when (it) {
        TransportSessionMode.User -> generalGetString(MR.strings.network_session_mode_user_description)
        TransportSessionMode.Entity -> generalGetString(MR.strings.network_session_mode_entity_description)
      }
      showUpdateNetworkSettingsDialog(
        title = generalGetString(MR.strings.update_network_session_mode_question),
        startsWith,
        onDismiss = { sessionMode.value = prevValue }
      ) {
        withBGApi {
          val newCfg = chatModel.controller.getNetCfg().copy(sessionMode = it)
          val res = chatModel.controller.apiSetNetworkConfig(newCfg)
          if (res) {
            chatModel.controller.setNetCfg(newCfg)
            sessionMode.value = it
          } else {
            sessionMode.value = prevValue
          }
        }
      }
    }
  )
}

@Composable fun NetworkAndServersLayout(
  currentRemoteHost: RemoteHostInfo?,
  developerTools: Boolean,
  networkUseSocksProxy: MutableState<Boolean>,
  onionHosts: MutableState<OnionHosts>,
  sessionMode: MutableState<TransportSessionMode>,
  proxyPort: State<Int>,
  toggleSocksProxy: (Boolean) -> Unit,
  useOnion: (OnionHosts) -> Unit,
  updateSessionMode: (TransportSessionMode) -> Unit,
) {
  val m = chatModel
  Column(
    Modifier.fillMaxWidth().verticalScroll(rememberScrollState()),
    verticalArrangement = Arrangement.spacedBy(8.dp)
  ) {
    AppBarTitle(stringResource(MR.strings.network_and_servers))
    if (!chatModel.desktopNoUserNoRemote) {
      SectionView(generalGetString(MR.strings.settings_section_title_messages)) {
        SettingsActionItem(painterResource(MR.images.ic_dns), stringResource(MR.strings.smp_servers), { ModalManager.start.showCustomModal { close -> ProtocolServersView(m, m.remoteHostId, ServerProtocol.SMP, close) } })

        SettingsActionItem(painterResource(MR.images.ic_dns), stringResource(MR.strings.xftp_servers), { ModalManager.start.showCustomModal { close -> ProtocolServersView(m, m.remoteHostId, ServerProtocol.XFTP, close) } })

        if (currentRemoteHost == null) {
          val showModal = { it: @Composable ModalData.() -> Unit ->  ModalManager.start.showModal(content = it) }
          UseSocksProxySwitch(networkUseSocksProxy, proxyPort, toggleSocksProxy, showModal, chatModel.controller.appPrefs.networkProxyHostPort, false)
          UseOnionHosts(onionHosts, networkUseSocksProxy, showModal, useOnion)
          if (developerTools) {
            SessionModePicker(sessionMode, showModal, updateSessionMode)
          }
          SettingsActionItem(painterResource(MR.images.ic_cable), stringResource(MR.strings.network_settings), { ModalManager.start.showModal { AdvancedNetworkSettingsView(m) } })
        }
      }
    }
    if (currentRemoteHost == null && networkUseSocksProxy.value) {
      SectionCustomFooter {
        Column {
          Text(annotatedStringResource(MR.strings.disable_onion_hosts_when_not_supported))
          Spacer(Modifier.height(DEFAULT_PADDING_HALF))
          Text(annotatedStringResource(MR.strings.socks_proxy_setting_limitations))
        }
      }
      Divider(Modifier.padding(start = DEFAULT_PADDING_HALF, top = 32.dp, end = DEFAULT_PADDING_HALF, bottom = 30.dp))
    } else if (!chatModel.desktopNoUserNoRemote) {
      Divider(Modifier.padding(start = DEFAULT_PADDING_HALF, top = 24.dp, end = DEFAULT_PADDING_HALF, bottom = 30.dp))
    }

    SectionView(generalGetString(MR.strings.settings_section_title_calls)) {
      SettingsActionItem(painterResource(MR.images.ic_electrical_services), stringResource(MR.strings.webrtc_ice_servers), { ModalManager.start.showModal { RTCServersView(m) } })
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
  proxyPort: State<Int>,
  toggleSocksProxy: (Boolean) -> Unit,
  useOnion: (OnionHosts) -> Unit,
  updateSessionMode: (TransportSessionMode) -> Unit,
) {
  val showModal = { it: @Composable ModalData.() -> Unit ->  ModalManager.fullscreen.showModal(content = it) }
  UseSocksProxySwitch(networkUseSocksProxy, proxyPort, toggleSocksProxy, showModal, networkProxyHostPort, true)
  UseOnionHosts(onionHosts, networkUseSocksProxy, showModal, useOnion)
  if (developerTools) {
    SessionModePicker(sessionMode, showModal, updateSessionMode)
  }
}

@Composable
fun UseSocksProxySwitch(
  networkUseSocksProxy: MutableState<Boolean>,
  proxyPort: State<Int>,
  toggleSocksProxy: (Boolean) -> Unit,
  showModal: (@Composable ModalData.() -> Unit) -> Unit,
  networkProxyHostPort: SharedPreference<String?> = chatModel.controller.appPrefs.networkProxyHostPort,
  migration: Boolean = false,
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
      val text = buildAnnotatedString {
        append(generalGetString(MR.strings.network_socks_toggle_use_socks_proxy) + " (")
        val style = SpanStyle(color = MaterialTheme.colors.primary)
        val disabledStyle = SpanStyle(color = MaterialTheme.colors.onBackground)
        withAnnotation(tag = "PORT", annotation = generalGetString(MR.strings.network_proxy_port).format(proxyPort.value)) {
          withStyle(if (networkUseSocksProxy.value || !migration) style else disabledStyle) {
            append(generalGetString(MR.strings.network_proxy_port).format(proxyPort.value))
          }
        }
        append(")")
      }
      ClickableText(
        text,
        style = TextStyle(color = MaterialTheme.colors.onBackground, fontSize = 16.sp, fontFamily = Inter, fontWeight = FontWeight.Normal),
        onClick = { offset ->
          text.getStringAnnotations(tag = "PORT", start = offset, end = offset)
            .firstOrNull()?.let { _ ->
              if (networkUseSocksProxy.value || !migration) {
                showModal { SockProxySettings(chatModel, networkProxyHostPort, migration) }
              }
            }
        },
        shouldConsumeEvent = { offset ->
          text.getStringAnnotations(tag = "PORT", start = offset, end = offset).any()
        }
      )
    }
    DefaultSwitch(
      checked = networkUseSocksProxy.value,
      onCheckedChange = toggleSocksProxy,
    )
  }
}

@Composable
fun SockProxySettings(
  m: ChatModel,
  networkProxyHostPort: SharedPreference<String?> = m.controller.appPrefs.networkProxyHostPort,
  migration: Boolean,
) {
  Column(
    Modifier
      .fillMaxWidth()
      .verticalScroll(rememberScrollState())
  ) {
    val defaultHostPort = remember { "localhost:9050" }
    AppBarTitle(generalGetString(MR.strings.network_socks_proxy_settings))
    val hostPortSaved by remember { networkProxyHostPort.state }
    val hostUnsaved = rememberSaveable(stateSaver = TextFieldValue.Saver) {
      mutableStateOf(TextFieldValue(hostPortSaved?.split(":")?.firstOrNull() ?: "localhost"))
    }
    val portUnsaved = rememberSaveable(stateSaver = TextFieldValue.Saver) {
      mutableStateOf(TextFieldValue(hostPortSaved?.split(":")?.lastOrNull() ?: "9050"))
    }
    val save = {
      withBGApi {
        networkProxyHostPort.set(hostUnsaved.value.text + ":" + portUnsaved.value.text)
        if (m.controller.appPrefs.networkUseSocksProxy.get() && !migration) {
          m.controller.apiSetNetworkConfig(m.controller.getNetCfg())
        }
      }
    }
    SectionView {
      SectionItemView {
        ResetToDefaultsButton({
          val reset = {
            networkProxyHostPort.set(defaultHostPort)
            val newHost = defaultHostPort.split(":").first()
            val newPort = defaultHostPort.split(":").last()
            hostUnsaved.value = hostUnsaved.value.copy(newHost, TextRange(newHost.length))
            portUnsaved.value = portUnsaved.value.copy(newPort, TextRange(newPort.length))
            save()
          }
          if (m.controller.appPrefs.networkUseSocksProxy.get() && !migration) {
            showUpdateNetworkSettingsDialog {
              reset()
            }
          } else {
            reset()
          }
        }, disabled = hostPortSaved == defaultHostPort)
      }
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
    SectionCustomFooter {
      NetworkSectionFooter(
        revert = {
          val prevHost = hostPortSaved?.split(":")?.firstOrNull() ?: "localhost"
          val prevPort = hostPortSaved?.split(":")?.lastOrNull() ?: "9050"
          hostUnsaved.value = hostUnsaved.value.copy(prevHost, TextRange(prevHost.length))
          portUnsaved.value = portUnsaved.value.copy(prevPort, TextRange(prevPort.length))
        },
        save = { if (m.controller.appPrefs.networkUseSocksProxy.get() && !migration) showUpdateNetworkSettingsDialog { save() } else save() },
        revertDisabled = hostPortSaved == (hostUnsaved.value.text + ":" + portUnsaved.value.text),
        saveDisabled = hostPortSaved == (hostUnsaved.value.text + ":" + portUnsaved.value.text) ||
            remember { derivedStateOf { !validHost(hostUnsaved.value.text) } }.value ||
            remember { derivedStateOf { !validPort(portUnsaved.value.text) } }.value
      )
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun UseOnionHosts(
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
      Column(
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
private fun SessionModePicker(
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
        Column(
          Modifier.fillMaxWidth(),
        ) {
          AppBarTitle(stringResource(MR.strings.network_session_mode_transport_isolation))
          SectionViewSelectable(null, sessionMode, values, updateSessionMode)
        }
      }
    }
  )
}

@Composable
private fun NetworkSectionFooter(revert: () -> Unit, save: () -> Unit, revertDisabled: Boolean, saveDisabled: Boolean) {
  Row(
    Modifier.fillMaxWidth(),
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    FooterButton(painterResource(MR.images.ic_replay), stringResource(MR.strings.network_options_revert), revert, revertDisabled)
    FooterButton(painterResource(MR.images.ic_check), stringResource(MR.strings.network_options_save), save, saveDisabled)
  }
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

private fun showUpdateNetworkSettingsDialog(
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
      developerTools = true,
      networkUseSocksProxy = remember { mutableStateOf(true) },
      proxyPort = remember { mutableStateOf(9050) },
      toggleSocksProxy = {},
      onionHosts = remember { mutableStateOf(OnionHosts.PREFER) },
      sessionMode = remember { mutableStateOf(TransportSessionMode.User) },
      useOnion = {},
      updateSessionMode = {},
    )
  }
}
