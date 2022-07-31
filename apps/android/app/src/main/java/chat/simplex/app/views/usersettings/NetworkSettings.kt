package chat.simplex.app.views.usersettings

import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.NetCfg
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.*

@Composable
fun NetworkSettingsView(chatModel: ChatModel, netCfg: NetCfg) {
  val useSocksProxy = remember { mutableStateOf(netCfg.socksProxy != null) }

  NetworkSettingsLayout(
    useSocksProxy,
    toggleSocksProxy = { enable ->
      if (enable) {
        AlertManager.shared.showAlertMsg(
          title = generalGetString(R.string.network_enable_socks),
          text = generalGetString(R.string.network_enable_socks_info),
          confirmText = generalGetString(R.string.confirm_verb),
          onConfirm = {
            withApi {
              chatModel.controller.setNetworkConfig(NetCfg(socksProxy = ":9050", tcpTimeout = 10_000_000))
              useSocksProxy.value = true
            }
          }
        )
      } else {
        AlertManager.shared.showAlertMsg(
          title = generalGetString(R.string.network_disable_socks),
          text = generalGetString(R.string.network_disable_socks_info),
          confirmText = generalGetString(R.string.confirm_verb),
          onConfirm = {
            withApi {
              chatModel.controller.setNetworkConfig(NetCfg(tcpTimeout = 5_000_000))
              useSocksProxy.value = false
            }
          }
        )
      }
    }
  )
}

@Composable fun NetworkSettingsLayout(
  useSocksProxy: MutableState<Boolean>,
  toggleSocksProxy: (Boolean) -> Unit
) {
  Column(
    Modifier.fillMaxWidth(),
    horizontalAlignment = Alignment.Start,
    verticalArrangement = Arrangement.spacedBy(8.dp)
  ) {
    Text(
      stringResource(R.string.network_settings_title),
      Modifier.padding(start = 16.dp, bottom = 24.dp),
      style = MaterialTheme.typography.h1
    )
    SectionView(stringResource(R.string.settings_section_title_socks)) {
      Row(
        Modifier.padding(start = 10.dp),
        verticalAlignment = Alignment.CenterVertically
      ) {
        Text(stringResource(R.string.network_socks_toggle))
        Spacer(Modifier.fillMaxWidth().weight(1f))
        Switch(
          checked = useSocksProxy.value,
          onCheckedChange = toggleSocksProxy,
          colors = SwitchDefaults.colors(
            checkedThumbColor = MaterialTheme.colors.primary,
            uncheckedThumbColor = HighOrLowlight
          ),
        )
      }
    }
  }
}

@Preview(showBackground = true)
@Composable
fun PreviewNetworkSettings() {
  SimpleXTheme {
    NetworkSettingsLayout(
      useSocksProxy = remember { mutableStateOf(true) },
      toggleSocksProxy = {}
    )
  }
}
