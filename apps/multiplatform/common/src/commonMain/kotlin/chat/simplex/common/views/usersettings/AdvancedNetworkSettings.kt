package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionCustomFooter
import SectionItemView
import SectionView
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.ChatModel
import chat.simplex.res.MR
import java.text.DecimalFormat

@Composable
fun AdvancedNetworkSettingsView(chatModel: ChatModel) {
  val currentCfg = remember { mutableStateOf(chatModel.controller.getNetCfg()) }
  val currentCfgVal = currentCfg.value // used only on initialization
  val networkTCPConnectTimeout = remember { mutableStateOf(currentCfgVal.tcpConnectTimeout) }
  val networkTCPTimeout = remember { mutableStateOf(currentCfgVal.tcpTimeout) }
  val networkTCPTimeoutPerKb = remember { mutableStateOf(currentCfgVal.tcpTimeoutPerKb) }
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
      hostMode = currentCfg.value.hostMode,
      requiredHostMode = currentCfg.value.requiredHostMode,
      sessionMode = currentCfg.value.sessionMode,
      tcpConnectTimeout = networkTCPConnectTimeout.value,
      tcpTimeout = networkTCPTimeout.value,
      tcpTimeoutPerKb = networkTCPTimeoutPerKb.value,
      tcpKeepAlive = tcpKeepAlive,
      smpPingInterval = networkSMPPingInterval.value,
      smpPingCount = networkSMPPingCount.value
    )
  }

  fun updateView(cfg: NetCfg) {
    networkTCPConnectTimeout.value = cfg.tcpConnectTimeout
    networkTCPTimeout.value = cfg.tcpTimeout
    networkTCPTimeoutPerKb.value = cfg.tcpTimeoutPerKb
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

  fun saveCfg(cfg: NetCfg) {
    withApi {
      chatModel.controller.apiSetNetworkConfig(cfg)
      currentCfg.value = cfg
      chatModel.controller.setNetCfg(cfg)
    }
  }

  fun reset() {
    val newCfg = if (currentCfg.value.useSocksProxy) NetCfg.proxyDefaults else NetCfg.defaults
    updateView(newCfg)
    saveCfg(newCfg)
  }

  AdvancedNetworkSettingsLayout(
    networkTCPConnectTimeout,
    networkTCPTimeout,
    networkTCPTimeoutPerKb,
    networkSMPPingInterval,
    networkSMPPingCount,
    networkEnableKeepAlive,
    networkTCPKeepIdle,
    networkTCPKeepIntvl,
    networkTCPKeepCnt,
    resetDisabled = if (currentCfg.value.useSocksProxy) currentCfg.value == NetCfg.proxyDefaults else currentCfg.value == NetCfg.defaults,
    reset = { showUpdateNetworkSettingsDialog(::reset) },
    footerDisabled = buildCfg() == currentCfg.value,
    revert = { updateView(currentCfg.value) },
    save = { showUpdateNetworkSettingsDialog { saveCfg(buildCfg()) } }
  )
}

@Composable fun AdvancedNetworkSettingsLayout(
  networkTCPConnectTimeout: MutableState<Long>,
  networkTCPTimeout: MutableState<Long>,
  networkTCPTimeoutPerKb: MutableState<Long>,
  networkSMPPingInterval: MutableState<Long>,
  networkSMPPingCount: MutableState<Int>,
  networkEnableKeepAlive: MutableState<Boolean>,
  networkTCPKeepIdle: MutableState<Int>,
  networkTCPKeepIntvl: MutableState<Int>,
  networkTCPKeepCnt: MutableState<Int>,
  resetDisabled: Boolean,
  reset: () -> Unit,
  footerDisabled: Boolean,
  revert: () -> Unit,
  save: () -> Unit
) {
  val secondsLabel = stringResource(MR.strings.network_option_seconds_label)

  Column(
    Modifier
      .fillMaxWidth()
      .verticalScroll(rememberScrollState()),
  ) {
    AppBarTitle(stringResource(MR.strings.network_settings_title))
    SectionView {
      SectionItemView {
        ResetToDefaultsButton(reset, disabled = resetDisabled)
      }
      SectionItemView {
        TimeoutSettingRow(
          stringResource(MR.strings.network_option_tcp_connection_timeout), networkTCPConnectTimeout,
          listOf(7_500000, 10_000000, 15_000000, 20_000000, 30_000_000, 45_000_000), secondsLabel
        )
      }
      SectionItemView {
        TimeoutSettingRow(
          stringResource(MR.strings.network_option_protocol_timeout), networkTCPTimeout,
          listOf(5_000000, 7_000000, 10_000000, 15_000000, 20_000_000, 30_000_000), secondsLabel
        )
      }
      SectionItemView {
        // can't be higher than 130ms to avoid overflow on 32bit systems
        TimeoutSettingRow(
          stringResource(MR.strings.network_option_protocol_timeout_per_kb), networkTCPTimeoutPerKb,
          listOf(15_000, 30_000, 45_000, 60_000, 90_000, 120_000), secondsLabel
        )
      }
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
    SectionCustomFooter {
      SettingsSectionFooter(revert, save, footerDisabled)
    }
    SectionBottomSpacer()
  }
}

@Composable
fun ResetToDefaultsButton(reset: () -> Unit, disabled: Boolean) {
  val modifier = if (disabled) Modifier else Modifier.clickable { reset() }
  Row(
    modifier.fillMaxSize(),
    verticalAlignment = Alignment.CenterVertically
  ) {
    val color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
    Text(stringResource(MR.strings.network_options_reset_to_defaults), color = color)
  }
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
          if (!expanded.value) painterResource(MR.images.ic_expand_more) else painterResource(MR.images.ic_expand_less),
          generalGetString(MR.strings.invite_to_group_button),
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
          if (!expanded.value) painterResource(MR.images.ic_expand_more) else painterResource(MR.images.ic_expand_less),
          generalGetString(MR.strings.invite_to_group_button),
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

@Composable
fun SettingsSectionFooter(revert: () -> Unit, save: () -> Unit, disabled: Boolean) {
  Row(
    Modifier.fillMaxWidth(),
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    FooterButton(painterResource(MR.images.ic_replay), stringResource(MR.strings.network_options_revert), revert, disabled)
    FooterButton(painterResource(MR.images.ic_check), stringResource(MR.strings.network_options_save), save, disabled)
  }
}

@Composable
fun FooterButton(icon: Painter, title: String, action: () -> Unit, disabled: Boolean) {
  Surface(
    shape = RoundedCornerShape(20.dp),
    color = Color.Black.copy(alpha = 0f)
  ) {
    val modifier = if (disabled) Modifier else Modifier.clickable { action() }
    Row(
      modifier.padding(8.dp),
      horizontalArrangement = Arrangement.spacedBy(8.dp)
    ) {
      Icon(
        icon,
        title,
        tint = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
      )
      Text(
        title,
        color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
      )
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

@Preview
@Composable
fun PreviewAdvancedNetworkSettingsLayout() {
  SimpleXTheme {
    AdvancedNetworkSettingsLayout(
      networkTCPConnectTimeout = remember { mutableStateOf(10_000000) },
      networkTCPTimeout = remember { mutableStateOf(10_000000) },
      networkTCPTimeoutPerKb = remember { mutableStateOf(10_000) },
      networkSMPPingInterval = remember { mutableStateOf(10_000000) },
      networkSMPPingCount = remember { mutableStateOf(3) },
      networkEnableKeepAlive = remember { mutableStateOf(true) },
      networkTCPKeepIdle = remember { mutableStateOf(10) },
      networkTCPKeepIntvl = remember { mutableStateOf(10) },
      networkTCPKeepCnt = remember { mutableStateOf(10) },
      resetDisabled = false,
      reset = {},
      footerDisabled = false,
      revert = {},
      save = {}
    )
  }
}
