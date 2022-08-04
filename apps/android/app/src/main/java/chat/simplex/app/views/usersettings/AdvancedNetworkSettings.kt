package chat.simplex.app.views.usersettings

import SectionCustomFooter
import SectionDivider
import SectionItemView
import SectionSpacer
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import java.text.DecimalFormat

@Composable
fun AdvancedNetworkSettingsView(chatModel: ChatModel, netCfg: NetCfg) {
  val networkTCPConnectTimeout = remember { mutableStateOf(netCfg.tcpConnectTimeout) }
  val networkTCPTimeout = remember { mutableStateOf(netCfg.tcpTimeout) }
  val networkSMPPingInterval = remember { mutableStateOf(netCfg.smpPingInterval) }
  val networkEnableKeepAlive = remember { mutableStateOf(netCfg.enableKeepAlive) }
  val networkTCPKeepIdle: MutableState<Int>
  val networkTCPKeepIntvl: MutableState<Int>
  val networkTCPKeepCnt: MutableState<Int>
  if (netCfg.tcpKeepAlive != null) {
    networkTCPKeepIdle = remember { mutableStateOf(netCfg.tcpKeepAlive.keepIdle) }
    networkTCPKeepIntvl = remember { mutableStateOf(netCfg.tcpKeepAlive.keepIntvl) }
    networkTCPKeepCnt = remember { mutableStateOf(netCfg.tcpKeepAlive.keepCnt) }
  } else {
    networkTCPKeepIdle = remember { mutableStateOf(KeepAliveOpts.defaults().keepIdle) }
    networkTCPKeepIntvl = remember { mutableStateOf(KeepAliveOpts.defaults().keepIntvl) }
    networkTCPKeepCnt = remember { mutableStateOf(KeepAliveOpts.defaults().keepCnt) }
  }

  AdvancedNetworkSettingsLayout(
    networkTCPConnectTimeout,
    networkTCPTimeout,
    networkSMPPingInterval,
    networkEnableKeepAlive,
    networkTCPKeepIdle,
    networkTCPKeepIntvl,
    networkTCPKeepCnt,
    reset = {},
    revert = {},
    save = {}
  )
}

@Composable fun AdvancedNetworkSettingsLayout(
  networkTCPConnectTimeout: MutableState<Long>,
  networkTCPTimeout: MutableState<Long>,
  networkSMPPingInterval: MutableState<Long>,
  networkEnableKeepAlive: MutableState<Boolean>,
  networkTCPKeepIdle: MutableState<Int>,
  networkTCPKeepIntvl: MutableState<Int>,
  networkTCPKeepCnt: MutableState<Int>,
  reset: () -> Unit,
  revert: () -> Unit,
  save: () -> Unit
) {
  val secondsLabel = stringResource(R.string.network_option_seconds_label)

  Column(
    Modifier
      .fillMaxWidth()
      .verticalScroll(rememberScrollState()),
    horizontalAlignment = Alignment.Start,
  ) {
    Text(
      stringResource(R.string.network_settings_title),
      Modifier.padding(start = 16.dp, bottom = 24.dp),
      style = MaterialTheme.typography.h1
    )
    SectionSpacer()

    SectionView {
      SectionItemView {
        ResetToDefaultsButton(reset, disabled = false)
      }
      SectionDivider()
      SectionItemView {
        TimeoutSettingRow(
          stringResource(R.string.network_option_tcp_connection_timeout), networkTCPConnectTimeout,
          listOf(2_500000, 5_000000, 7_500000, 10_000000, 15_000000, 20_000000), secondsLabel
        )
      }
      SectionDivider()
      SectionItemView {
        TimeoutSettingRow(
          stringResource(R.string.network_option_protocol_timeout), networkTCPTimeout,
          listOf(1_500000, 3_000000, 5_000000, 7_000000, 10_000000, 15_000000), secondsLabel
        )
      }
      SectionDivider()
      SectionItemView {
        TimeoutSettingRow(
          stringResource(R.string.network_option_ping_interval), networkSMPPingInterval,
          listOf(120_000000, 300_000000, 600_000000, 1200_000000, 2400_000000), secondsLabel
        )
      }
      SectionDivider()
      SectionItemView {
        EnableKeepAliveSwitch(networkEnableKeepAlive)
      }
      SectionDivider()
      if (networkEnableKeepAlive.value) {
        SectionItemView {
          IntSettingRow("TCP_KEEPIDLE", networkTCPKeepIdle, listOf(15, 30, 60, 120, 180), secondsLabel)
        }
        SectionDivider()
        SectionItemView {
          IntSettingRow("TCP_KEEPINTVL", networkTCPKeepIntvl, listOf(5, 10, 15, 30, 60), secondsLabel)
        }
        SectionDivider()
        SectionItemView {
          IntSettingRow("TCP_KEEPCNT", networkTCPKeepCnt, listOf(1, 2, 4, 6, 8), "")
        }
      } else {
        SectionItemView {
          Text("TCP_KEEPIDLE", color = HighOrLowlight)
        }
        SectionDivider()
        SectionItemView {
          Text("TCP_KEEPINTVL", color = HighOrLowlight)
        }
        SectionDivider()
        SectionItemView {
          Text("TCP_KEEPCNT", color = HighOrLowlight)
        }
      }
    }
    SectionCustomFooter {
      SettingsSectionFooter(revert, save)
    }
    SectionSpacer()
  }
}

@Composable
fun ResetToDefaultsButton(reset: () -> Unit, disabled: Boolean) {
  val modifier = if (disabled) Modifier else Modifier.clickable { reset() }
  Row(
    modifier.fillMaxSize(),
    verticalAlignment = Alignment.CenterVertically
  ) {
    val color = if (disabled) HighOrLowlight else MaterialTheme.colors.primary
    Text(stringResource(R.string.network_options_reset_to_defaults), color = color)
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
    Text(stringResource(R.string.network_option_enable_tcp_keep_alive))
    Switch(
      checked = networkEnableKeepAlive.value,
      onCheckedChange = { networkEnableKeepAlive.value = it },
      colors = SwitchDefaults.colors(
        checkedThumbColor = MaterialTheme.colors.primary,
        uncheckedThumbColor = HighOrLowlight
      ),
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
    var expanded by remember { mutableStateOf(false) }

    Text(title)

    ExposedDropdownMenuBox(
      expanded = expanded,
      onExpandedChange = {
        expanded = !expanded
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
          color = HighOrLowlight
        )
        Spacer(Modifier.size(4.dp))
        Icon(
          if (!expanded) Icons.Outlined.ExpandMore else Icons.Outlined.ExpandLess,
          generalGetString(R.string.invite_to_group_button),
          modifier = Modifier.padding(start = 8.dp),
          tint = HighOrLowlight
        )
      }
      ExposedDropdownMenu(
        expanded = expanded,
        onDismissRequest = {
          expanded = false
        }
      ) {
        values.forEach { selectionOption ->
          DropdownMenuItem(
            onClick = {
              selection.value = selectionOption
              expanded = false
            }
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
    var expanded by remember { mutableStateOf(false) }

    Text(title)

    ExposedDropdownMenuBox(
      expanded = expanded,
      onExpandedChange = {
        expanded = !expanded
      }
    ) {
      val df = DecimalFormat("#.#")

      Row(
        Modifier.width(140.dp),
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.End
      ) {
        Text(
          "${df.format(selection.value / 1_000_000.toDouble())} $label",
          maxLines = 1,
          overflow = TextOverflow.Ellipsis,
          color = HighOrLowlight
        )
        Spacer(Modifier.size(4.dp))
        Icon(
          if (!expanded) Icons.Outlined.ExpandMore else Icons.Outlined.ExpandLess,
          generalGetString(R.string.invite_to_group_button),
          modifier = Modifier.padding(start = 8.dp),
          tint = HighOrLowlight
        )
      }
      ExposedDropdownMenu(
        expanded = expanded,
        onDismissRequest = {
          expanded = false
        }
      ) {
        values.forEach { selectionOption ->
          DropdownMenuItem(
            onClick = {
              selection.value = selectionOption
              expanded = false
            }
          ) {
            Text(
              "${df.format(selectionOption / 1_000_000.toDouble())} $label",
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
fun SettingsSectionFooter(revert: () -> Unit, save: () -> Unit) {
  Row(
    Modifier.fillMaxWidth(),
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    Surface(
      shape = RoundedCornerShape(20.dp),
      color = Color.Black.copy(alpha = 0f)
    ) {
      Row(
        Modifier
          .clickable { revert() }
          .padding(8.dp),
        horizontalArrangement = Arrangement.spacedBy(8.dp)
      ) {
        Icon(
          Icons.Outlined.Replay,
          stringResource(R.string.network_options_revert),
          tint = MaterialTheme.colors.primary
        )
        Text(
          stringResource(R.string.network_options_revert),
          color = MaterialTheme.colors.primary
        )
      }
    }

    Surface(
      shape = RoundedCornerShape(20.dp),
      color = Color.Black.copy(alpha = 0f)
    ) {
      Row(
        Modifier
          .clickable { save() }
          .padding(8.dp),
        horizontalArrangement = Arrangement.spacedBy(8.dp)
      ) {
        Icon(
          Icons.Outlined.Replay,
          stringResource(R.string.network_options_save),
          tint = MaterialTheme.colors.primary
        )
        Text(
          stringResource(R.string.network_options_save),
          color = MaterialTheme.colors.primary
        )
      }
    }
  }
}

@Preview(showBackground = true)
@Composable
fun PreviewAdvancedNetworkSettingsLayout() {
  SimpleXTheme {
    AdvancedNetworkSettingsLayout(
      networkTCPConnectTimeout = remember { mutableStateOf(10_000000) },
      networkTCPTimeout = remember { mutableStateOf(10_000000) },
      networkSMPPingInterval = remember { mutableStateOf(10_000000) },
      networkEnableKeepAlive = remember { mutableStateOf(true) },
      networkTCPKeepIdle = remember { mutableStateOf(10) },
      networkTCPKeepIntvl = remember { mutableStateOf(10) },
      networkTCPKeepCnt = remember { mutableStateOf(10) },
      reset = {},
      revert = {},
      save = {}
    )
  }
}
