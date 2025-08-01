package chat.simplex.common.views.usersettings.networkAndServers

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionItemWithValue
import SectionTextFooter
import SectionView
import SectionViewSelectableCards
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalDensity
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.usersettings.PreferenceToggle
import chat.simplex.common.views.usersettings.SettingsPreferenceItem
import chat.simplex.res.MR
import java.text.DecimalFormat

@Composable
fun ModalData.AdvancedNetworkSettingsView(showModal: (ModalData.() -> Unit) -> Unit, close: () -> Unit) {
  val currentRemoteHost by remember { chatModel.currentRemoteHost }
  val developerTools = remember { appPrefs.developerTools.get() }

  // Will be actual once the screen is re-opened
  val savedCfg = remember { mutableStateOf(controller.getNetCfg()) }
  // Will have an edited state when the screen is re-opened
  val currentCfg = remember { stateGetOrPut("currentCfg") { controller.getNetCfg() } }
  val currentCfgVal = currentCfg.value // used only on initialization

  val sessionMode = remember { mutableStateOf(currentCfgVal.sessionMode) }
  val smpProxyMode = remember { mutableStateOf(currentCfgVal.smpProxyMode) }
  val smpProxyFallback = remember { mutableStateOf(currentCfgVal.smpProxyFallback) }
  val smpWebPortServers = remember { mutableStateOf(currentCfgVal.smpWebPortServers) }

  val networkTCPConnectTimeoutInteractive = remember { mutableStateOf(currentCfgVal.tcpConnectTimeout.interactiveTimeout) }
  val networkTCPConnectTimeoutBackground = remember { mutableStateOf(currentCfgVal.tcpConnectTimeout.backgroundTimeout) }
  val networkTCPTimeoutInteractive = remember { mutableStateOf(currentCfgVal.tcpTimeout.interactiveTimeout) }
  val networkTCPTimeoutBackground = remember { mutableStateOf(currentCfgVal.tcpTimeout.backgroundTimeout) }
  val networkTCPTimeoutPerKb = remember { mutableStateOf(currentCfgVal.tcpTimeoutPerKb) }
  val networkRcvConcurrency = remember { mutableStateOf(currentCfgVal.rcvConcurrency) }
  val networkSMPPingInterval = remember { mutableStateOf(currentCfgVal.smpPingInterval) }
  val networkSMPPingCount = remember { mutableStateOf(currentCfgVal.smpPingCount) }
  val networkEnableKeepAlive = remember { mutableStateOf(currentCfgVal.enableKeepAlive) }
  val networkTCPKeepIdle: MutableState<Int>
  val networkTCPKeepIntvl: MutableState<Int>
  val networkTCPKeepCnt: MutableState<Int>
  if (currentCfgVal.tcpKeepAlive != null) {
    networkTCPKeepIdle = remember { mutableStateOf(currentCfgVal.tcpKeepAlive.keepIdle) }
    networkTCPKeepIntvl = remember { mutableStateOf(currentCfgVal.tcpKeepAlive.keepIntvl) }
    networkTCPKeepCnt = remember { mutableStateOf(currentCfgVal.tcpKeepAlive.keepCnt) }
  } else {
    networkTCPKeepIdle = remember { mutableStateOf(KeepAliveOpts.defaults.keepIdle) }
    networkTCPKeepIntvl = remember { mutableStateOf(KeepAliveOpts.defaults.keepIntvl) }
    networkTCPKeepCnt = remember { mutableStateOf(KeepAliveOpts.defaults.keepCnt) }
  }

  fun buildCfg(): NetCfg {
    val enableKeepAlive = networkEnableKeepAlive.value
    val tcpKeepAlive = if (enableKeepAlive) {
      val keepIdle = networkTCPKeepIdle.value
      val keepIntvl = networkTCPKeepIntvl.value
      val keepCnt = networkTCPKeepCnt.value
      KeepAliveOpts(keepIdle = keepIdle, keepIntvl = keepIntvl, keepCnt = keepCnt)
    } else {
      null
    }
    return NetCfg(
      socksProxy = currentCfg.value.socksProxy,
//      hostMode = currentCfg.value.hostMode,
//      requiredHostMode = currentCfg.value.requiredHostMode,
      sessionMode = sessionMode.value,
      smpProxyMode = smpProxyMode.value,
      smpProxyFallback = smpProxyFallback.value,
      smpWebPortServers = smpWebPortServers.value,
      tcpConnectTimeout = NetworkTimeout(
        backgroundTimeout = networkTCPConnectTimeoutBackground.value,
        interactiveTimeout = networkTCPConnectTimeoutInteractive.value
      ) ,
      tcpTimeout = NetworkTimeout(
        backgroundTimeout = networkTCPTimeoutBackground.value,
        interactiveTimeout = networkTCPTimeoutInteractive.value
      ),
      tcpTimeoutPerKb = networkTCPTimeoutPerKb.value,
      rcvConcurrency = networkRcvConcurrency.value,
      tcpKeepAlive = tcpKeepAlive,
      smpPingInterval = networkSMPPingInterval.value,
      smpPingCount = networkSMPPingCount.value
    ).withOnionHosts(currentCfg.value.onionHosts)
  }

  fun updateView(cfg: NetCfg) {
    sessionMode.value = cfg.sessionMode
    smpProxyMode.value = cfg.smpProxyMode
    smpProxyFallback.value = cfg.smpProxyFallback
    smpWebPortServers.value = cfg.smpWebPortServers
    networkTCPConnectTimeoutInteractive.value = cfg.tcpConnectTimeout.interactiveTimeout
    networkTCPConnectTimeoutBackground.value = cfg.tcpConnectTimeout.backgroundTimeout
    networkTCPTimeoutInteractive.value = cfg.tcpTimeout.interactiveTimeout
    networkTCPTimeoutBackground.value = cfg.tcpTimeout.backgroundTimeout
    networkTCPTimeoutPerKb.value = cfg.tcpTimeoutPerKb
    networkRcvConcurrency.value = cfg.rcvConcurrency
    networkSMPPingInterval.value = cfg.smpPingInterval
    networkSMPPingCount.value = cfg.smpPingCount
    networkEnableKeepAlive.value = cfg.enableKeepAlive
    if (cfg.tcpKeepAlive != null) {
      networkTCPKeepIdle.value = cfg.tcpKeepAlive.keepIdle
      networkTCPKeepIntvl.value = cfg.tcpKeepAlive.keepIntvl
      networkTCPKeepCnt.value = cfg.tcpKeepAlive.keepCnt
    } else {
      networkTCPKeepIdle.value = KeepAliveOpts.defaults.keepIdle
      networkTCPKeepIntvl.value = KeepAliveOpts.defaults.keepIntvl
      networkTCPKeepCnt.value = KeepAliveOpts.defaults.keepCnt
    }
  }

  fun saveCfg(cfg: NetCfg, close: (() -> Unit)? = null) {
    withBGApi {
      if (chatModel.controller.apiSetNetworkConfig(cfg)) {
        currentCfg.value = cfg
        savedCfg.value = cfg
        chatModel.controller.setNetCfg(cfg)
        close?.invoke()
      }
    }
  }

  fun reset() {
    val newCfg = if (currentCfg.value.useSocksProxy) NetCfg.proxyDefaults else NetCfg.defaults
    updateView(newCfg)
    currentCfg.value = newCfg
  }

  val saveDisabled = buildCfg() == savedCfg.value

  ModalView(
    close = {
      if (saveDisabled) {
        close()
      } else {
        showUnsavedChangesAlert({
          saveCfg(buildCfg(), close)
        }, close)
      }
    },
  ) {
    AdvancedNetworkSettingsLayout(
      currentRemoteHost = currentRemoteHost,
      developerTools = developerTools,
      sessionMode = sessionMode,
      smpProxyMode = smpProxyMode,
      smpProxyFallback = smpProxyFallback,
      smpWebPortServers,
      networkTCPConnectTimeoutInteractive,
      networkTCPConnectTimeoutBackground,
      networkTCPTimeoutInteractive,
      networkTCPTimeoutBackground,
      networkTCPTimeoutPerKb,
      networkRcvConcurrency,
      networkSMPPingInterval,
      networkSMPPingCount,
      networkEnableKeepAlive,
      networkTCPKeepIdle,
      networkTCPKeepIntvl,
      networkTCPKeepCnt,
      updateSessionMode = { sessionMode.value = it; currentCfg.value = currentCfg.value.copy(sessionMode = it) },
      updateSMPProxyMode = { smpProxyMode.value = it; currentCfg.value = currentCfg.value.copy(smpProxyMode = it) },
      updateSMPProxyFallback = { smpProxyFallback.value = it; currentCfg.value = currentCfg.value.copy(smpProxyFallback = it) },
      showModal = showModal,
      resetDisabled = if (currentCfg.value.useSocksProxy) buildCfg() == NetCfg.proxyDefaults else buildCfg() == NetCfg.defaults,
      reset = ::reset,
      saveDisabled = saveDisabled,
      save = {
        showUpdateNetworkSettingsDialog {
          saveCfg(buildCfg())
        }
      }
    )
  }
}

@Composable fun AdvancedNetworkSettingsLayout(
  currentRemoteHost: RemoteHostInfo?,
  developerTools: Boolean,
  sessionMode: MutableState<TransportSessionMode>,
  smpProxyMode: MutableState<SMPProxyMode>,
  smpProxyFallback: MutableState<SMPProxyFallback>,
  smpWebPortServers: MutableState<SMPWebPortServers>,
  networkTCPConnectTimeoutInteractive: MutableState<Long>,
  networkTCPConnectTimeoutBackground: MutableState<Long>,
  networkTCPTimeoutInteractive: MutableState<Long>,
  networkTCPTimeoutBackground: MutableState<Long>,
  networkTCPTimeoutPerKb: MutableState<Long>,
  networkRcvConcurrency: MutableState<Int>,
  networkSMPPingInterval: MutableState<Long>,
  networkSMPPingCount: MutableState<Int>,
  networkEnableKeepAlive: MutableState<Boolean>,
  networkTCPKeepIdle: MutableState<Int>,
  networkTCPKeepIntvl: MutableState<Int>,
  networkTCPKeepCnt: MutableState<Int>,
  updateSessionMode: (TransportSessionMode) -> Unit,
  updateSMPProxyMode: (SMPProxyMode) -> Unit,
  updateSMPProxyFallback: (SMPProxyFallback) -> Unit,
  showModal: (ModalData.() -> Unit) -> Unit,
  resetDisabled: Boolean,
  reset: () -> Unit,
  saveDisabled: Boolean,
  save: () -> Unit
) {
  val secondsLabel = stringResource(MR.strings.network_option_seconds_label)

  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.network_settings_title))

    if (currentRemoteHost == null) {
      SectionView(generalGetString(MR.strings.settings_section_title_private_message_routing)) {
        SMPProxyModePicker(smpProxyMode, showModal, updateSMPProxyMode)
        SMPProxyFallbackPicker(smpProxyFallback, showModal, updateSMPProxyFallback, enabled = remember { derivedStateOf { smpProxyMode.value != SMPProxyMode.Never } })
        SettingsPreferenceItem(painterResource(MR.images.ic_arrow_forward), stringResource(MR.strings.private_routing_show_message_status), chatModel.controller.appPrefs.showSentViaProxy)
      }
      SectionTextFooter(stringResource(MR.strings.private_routing_explanation))
      SectionDividerSpaced(maxTopPadding = true)

      SectionView(stringResource(MR.strings.network_session_mode_transport_isolation).uppercase()) {
        SessionModePicker(sessionMode, showModal, updateSessionMode)
      }
      SectionDividerSpaced()
      SectionView(stringResource(MR.strings.network_smp_web_port_section_title).uppercase()) {
        ExposedDropDownSettingRow(
          stringResource(MR.strings.network_smp_web_port_toggle),
          SMPWebPortServers.entries.map { it to stringResource(it.text) },
          smpWebPortServers
        ) { smpWebPortServers.value = it }
      }
      SectionTextFooter(
        if (smpWebPortServers.value == SMPWebPortServers.Preset) stringResource(MR.strings.network_smp_web_port_preset_footer)
        else String.format(stringResource(MR.strings.network_smp_web_port_footer), if (smpWebPortServers.value == SMPWebPortServers.All) "443" else "5223")
      )
      SectionDividerSpaced(maxTopPadding = true)

      SectionView(stringResource(MR.strings.network_option_tcp_connection).uppercase()) {
        SectionItemView {
          TimeoutSettingRow(
            stringResource(MR.strings.network_option_tcp_connection_timeout), networkTCPConnectTimeoutInteractive,
            listOf(10_000000, 15_000000, 20_000000, 30_000000), secondsLabel
          )
        }
        SectionItemView {
          TimeoutSettingRow(
            stringResource(MR.strings.network_option_tcp_connection_timeout_background), networkTCPConnectTimeoutBackground,
            listOf(30_000000, 45_000000, 60_000000, 90_000000), secondsLabel
          )
        }
        SectionItemView {
          TimeoutSettingRow(
            stringResource(MR.strings.network_option_protocol_timeout), networkTCPTimeoutInteractive,
            listOf(5_000000, 7_000000, 10_000000, 15_000000, 20_000_000), secondsLabel
          )
        }
        SectionItemView {
          TimeoutSettingRow(
            stringResource(MR.strings.network_option_protocol_timeout_background), networkTCPTimeoutBackground,
            listOf(15_000000, 20_000000, 30_000000, 45_000000, 60_000_000), secondsLabel
          )
        }
        SectionItemView {
          // can't be higher than 130ms to avoid overflow on 32bit systems
          TimeoutSettingRow(
            stringResource(MR.strings.network_option_protocol_timeout_per_kb), networkTCPTimeoutPerKb,
            listOf(2_500, 5_000, 10_000, 15_000, 20_000, 30_000), secondsLabel
          )
        }
        // SectionItemView {
        //   IntSettingRow(
        //     stringResource(MR.strings.network_option_rcv_concurrency), networkRcvConcurrency,
        //     listOf(1, 2, 4, 8, 12, 16, 24), ""
        //   )
        // }
        SectionItemView {
          TimeoutSettingRow(
            stringResource(MR.strings.network_option_ping_interval), networkSMPPingInterval,
            listOf(120_000000, 300_000000, 600_000000, 1200_000000, 2400_000000, 3600_000000), secondsLabel
          )
        }
        SectionItemView {
          IntSettingRow(
            stringResource(MR.strings.network_option_ping_count), networkSMPPingCount,
            listOf(1, 2, 3, 5, 8), ""
          )
        }
        SectionItemView {
          EnableKeepAliveSwitch(networkEnableKeepAlive)
        }
        if (networkEnableKeepAlive.value) {
          SectionItemView {
            IntSettingRow("TCP_KEEPIDLE", networkTCPKeepIdle, listOf(15, 30, 60, 120, 180), secondsLabel)
          }
          SectionItemView {
            IntSettingRow("TCP_KEEPINTVL", networkTCPKeepIntvl, listOf(5, 10, 15, 30, 60), secondsLabel)
          }
          SectionItemView {
            IntSettingRow("TCP_KEEPCNT", networkTCPKeepCnt, listOf(1, 2, 4, 6, 8), "")
          }
        } else {
          SectionItemView {
            Text("TCP_KEEPIDLE", color = MaterialTheme.colors.secondary)
          }
          SectionItemView {
            Text("TCP_KEEPINTVL", color = MaterialTheme.colors.secondary)
          }
          SectionItemView {
            Text("TCP_KEEPCNT", color = MaterialTheme.colors.secondary)
          }
        }
      }
    }

    SectionDividerSpaced(maxBottomPadding = false)

    SectionView {
      SectionItemView(reset, disabled = resetDisabled) {
        Text(stringResource(MR.strings.network_options_reset_to_defaults), color = if (resetDisabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary)
      }
      SectionItemView(save, disabled = saveDisabled) {
        Text(stringResource(MR.strings.network_options_save_and_reconnect), color = if (saveDisabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary)
      }
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun SMPProxyModePicker(
  smpProxyMode: MutableState<SMPProxyMode>,
  showModal: (@Composable ModalData.() -> Unit) -> Unit,
  updateSMPProxyMode: (SMPProxyMode) -> Unit,
) {
  val density = LocalDensity.current
  val values = remember {
    SMPProxyMode.entries.map {
      when (it) {
        SMPProxyMode.Always -> ValueTitleDesc(SMPProxyMode.Always, generalGetString(MR.strings.network_smp_proxy_mode_always), escapedHtmlToAnnotatedString(generalGetString(MR.strings.network_smp_proxy_mode_always_description), density))
        SMPProxyMode.Unknown -> ValueTitleDesc(SMPProxyMode.Unknown, generalGetString(MR.strings.network_smp_proxy_mode_unknown), escapedHtmlToAnnotatedString(generalGetString(MR.strings.network_smp_proxy_mode_unknown_description), density))
        SMPProxyMode.Unprotected -> ValueTitleDesc(SMPProxyMode.Unprotected, generalGetString(MR.strings.network_smp_proxy_mode_unprotected), escapedHtmlToAnnotatedString(generalGetString(MR.strings.network_smp_proxy_mode_unprotected_description), density))
        SMPProxyMode.Never -> ValueTitleDesc(SMPProxyMode.Never, generalGetString(MR.strings.network_smp_proxy_mode_never), escapedHtmlToAnnotatedString(generalGetString(MR.strings.network_smp_proxy_mode_never_description), density))
      }
    }
  }

  SectionItemWithValue(
    generalGetString(MR.strings.network_smp_proxy_mode_private_routing),
    smpProxyMode,
    values,
    icon = painterResource(MR.images.ic_settings_ethernet),
    onSelected = {
      showModal {
        ColumnWithScrollBar {
          AppBarTitle(stringResource(MR.strings.network_smp_proxy_mode_private_routing))
          SectionViewSelectableCards(null, smpProxyMode, values, updateSMPProxyMode)
        }
      }
    }
  )
}

@Composable
private fun SMPProxyFallbackPicker(
  smpProxyFallback: MutableState<SMPProxyFallback>,
  showModal: (@Composable ModalData.() -> Unit) -> Unit,
  updateSMPProxyFallback: (SMPProxyFallback) -> Unit,
  enabled: State<Boolean>,
) {
  val density = LocalDensity.current
  val values = remember {
    SMPProxyFallback.entries.map {
      when (it) {
        SMPProxyFallback.Allow -> ValueTitleDesc(SMPProxyFallback.Allow, generalGetString(MR.strings.network_smp_proxy_fallback_allow), escapedHtmlToAnnotatedString(generalGetString(MR.strings.network_smp_proxy_fallback_allow_description), density))
        SMPProxyFallback.AllowProtected -> ValueTitleDesc(SMPProxyFallback.AllowProtected, generalGetString(MR.strings.network_smp_proxy_fallback_allow_protected), escapedHtmlToAnnotatedString(generalGetString(MR.strings.network_smp_proxy_fallback_allow_protected_description), density))
        SMPProxyFallback.Prohibit -> ValueTitleDesc(SMPProxyFallback.Prohibit, generalGetString(MR.strings.network_smp_proxy_fallback_prohibit), escapedHtmlToAnnotatedString(generalGetString(MR.strings.network_smp_proxy_fallback_prohibit_description), density))
      }
    }
  }

  SectionItemWithValue(
    generalGetString(MR.strings.network_smp_proxy_fallback_allow_downgrade),
    smpProxyFallback,
    values,
    icon = painterResource(MR.images.ic_arrows_left_right),
    enabled = enabled,
    onSelected = {
      showModal {
        ColumnWithScrollBar {
          AppBarTitle(stringResource(MR.strings.network_smp_proxy_fallback_allow_downgrade))
          SectionViewSelectableCards(null, smpProxyFallback, values, updateSMPProxyFallback)
        }
      }
    }
  )
}

@Composable
fun EnableKeepAliveSwitch(
  networkEnableKeepAlive: MutableState<Boolean>
) {
  Row(
    Modifier.fillMaxWidth(),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.SpaceBetween
  ) {
    Text(stringResource(MR.strings.network_option_enable_tcp_keep_alive))
    DefaultSwitch(
      checked = networkEnableKeepAlive.value,
      onCheckedChange = { networkEnableKeepAlive.value = it },
    )
  }
}

@Composable
fun IntSettingRow(title: String, selection: MutableState<Int>, values: List<Int>, label: String) {
  Row(
    Modifier.fillMaxWidth(),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.SpaceBetween
  ) {
    val expanded = rememberSaveable { mutableStateOf(false) }

    Text(title)

    ExposedDropdownMenuBox(
      expanded = expanded.value,
      onExpandedChange = {
        expanded.value = !expanded.value
      }
    ) {
      Row(
        Modifier.width(140.dp),
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.End
      ) {
        Text(
          "${selection.value} $label",
          maxLines = 1,
          overflow = TextOverflow.Ellipsis,
          color = MaterialTheme.colors.secondary
        )
        Spacer(Modifier.size(4.dp))
        Icon(
          if (!expanded.value) painterResource(MR.images.ic_arrow_drop_down) else painterResource(MR.images.ic_arrow_drop_up),
          contentDescription = null,
          modifier = Modifier.padding(start = 8.dp),
          tint = MaterialTheme.colors.secondary
        )
      }
      DefaultExposedDropdownMenu(
        expanded = expanded,
      ) {
        values.forEach { selectionOption ->
          DropdownMenuItem(
            onClick = {
              selection.value = selectionOption
              expanded.value = false
            },
            contentPadding = PaddingValues(horizontal = DEFAULT_PADDING * 1.5f)
          ) {
            Text(
              "$selectionOption $label",
              maxLines = 1,
              overflow = TextOverflow.Ellipsis,
            )
          }
        }
      }
    }
  }
}

@Composable
fun TimeoutSettingRow(title: String, selection: MutableState<Long>, values: List<Long>, label: String) {
  Row(
    Modifier.fillMaxWidth(),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.SpaceBetween
  ) {
    val expanded = remember { mutableStateOf(false) }

    Text(title)

    ExposedDropdownMenuBox(
      expanded = expanded.value,
      onExpandedChange = {
        expanded.value = !expanded.value
      }
    ) {
      val df = DecimalFormat("#.###")
      Row(
        Modifier.width(140.dp),
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.End
      ) {
        Text(
          "${df.format(selection.value / 1_000_000.0)} $label",
          maxLines = 1,
          overflow = TextOverflow.Ellipsis,
          color = MaterialTheme.colors.secondary
        )
        Spacer(Modifier.size(4.dp))
        Icon(
          if (!expanded.value) painterResource(MR.images.ic_arrow_drop_down) else painterResource(MR.images.ic_arrow_drop_up),
          contentDescription = null,
          modifier = Modifier.padding(start = 8.dp),
          tint = MaterialTheme.colors.secondary
        )
      }
      DefaultExposedDropdownMenu(
        expanded = expanded
      ) {
        val v = selection.value
        val vs = if (values.contains(v)) values else values + v
        vs.forEach { selectionOption ->
          DropdownMenuItem(
            onClick = {
              selection.value = selectionOption
              expanded.value = false
            },
            contentPadding = PaddingValues(horizontal = DEFAULT_PADDING * 1.5f)
          ) {
            Text(
              "${df.format(selectionOption / 1_000_000.0)} $label",
              maxLines = 1,
              overflow = TextOverflow.Ellipsis,
            )
          }
        }
      }
    }
  }
}

fun showUpdateNetworkSettingsDialog(action: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.update_network_settings_question),
    text = generalGetString(MR.strings.updating_settings_will_reconnect_client_to_all_servers),
    confirmText = generalGetString(MR.strings.update_network_settings_confirmation),
    onConfirm = action
  )
}

private fun showUnsavedChangesAlert(save: () -> Unit, revert: () -> Unit) {
  AlertManager.shared.showAlertDialogStacked(
    title = generalGetString(MR.strings.update_network_settings_question),
    confirmText = generalGetString(MR.strings.network_options_save_and_reconnect),
    dismissText = generalGetString(MR.strings.exit_without_saving),
    onConfirm = save,
    onDismiss = revert,
  )
}

@Preview
@Composable
fun PreviewAdvancedNetworkSettingsLayout() {
  SimpleXTheme {
    AdvancedNetworkSettingsLayout(
      currentRemoteHost = null,
      developerTools = false,
      sessionMode = remember { mutableStateOf(TransportSessionMode.User) },
      smpProxyMode = remember { mutableStateOf(SMPProxyMode.Never) },
      smpProxyFallback = remember { mutableStateOf(SMPProxyFallback.Allow) },
      smpWebPortServers = remember { mutableStateOf(SMPWebPortServers.Preset) },
      networkTCPConnectTimeoutInteractive = remember { mutableStateOf(15_000000) },
      networkTCPConnectTimeoutBackground = remember { mutableStateOf(45_000000) },
      networkTCPTimeoutInteractive = remember { mutableStateOf(10_000000) },
      networkTCPTimeoutBackground = remember { mutableStateOf(30_000000) },
      networkTCPTimeoutPerKb = remember { mutableStateOf(10_000) },
      networkRcvConcurrency = remember { mutableStateOf(8) },
      networkSMPPingInterval = remember { mutableStateOf(10_000000) },
      networkSMPPingCount = remember { mutableStateOf(3) },
      networkEnableKeepAlive = remember { mutableStateOf(true) },
      networkTCPKeepIdle = remember { mutableStateOf(10) },
      networkTCPKeepIntvl = remember { mutableStateOf(10) },
      networkTCPKeepCnt = remember { mutableStateOf(10) },
      updateSessionMode = {},
      updateSMPProxyMode = {},
      updateSMPProxyFallback = {},
      showModal = {},
      resetDisabled = false,
      reset = {},
      saveDisabled = false,
      save = {}
    )
  }
}
