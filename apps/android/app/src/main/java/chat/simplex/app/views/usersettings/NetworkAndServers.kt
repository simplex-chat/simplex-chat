package chat.simplex.app.views.usersettings

import SectionDivider
import SectionItemView
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.NetCfg
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*

@Composable
fun NetworkAndServersView(
  chatModel: ChatModel,
  showModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit)
) {
  val netCfg: MutableState<NetCfg> = remember { mutableStateOf(chatModel.controller.getNetCfg()) }
  val networkUseSocksProxy: MutableState<Boolean> = remember { mutableStateOf(netCfg.value.useSocksProxy) }
  val developerTools = chatModel.controller.appPrefs.developerTools.get()

  NetworkAndServersLayout(
    developerTools = developerTools,
    networkUseSocksProxy = networkUseSocksProxy,
    showModal = showModal,
    showSettingsModal = showSettingsModal,
        toggleSocksProxy = { enable ->
      if (enable) {
        AlertManager.shared.showAlertMsg(
          title = generalGetString(R.string.network_enable_socks),
          text = generalGetString(R.string.network_enable_socks_info),
          confirmText = generalGetString(R.string.confirm_verb),
          onConfirm = {
            withApi {
              chatModel.controller.apiSetNetworkConfig(NetCfg.proxyDefaults)
              chatModel.controller.setNetCfg(NetCfg.proxyDefaults)
              networkUseSocksProxy.value = true
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
              chatModel.controller.apiSetNetworkConfig(NetCfg.defaults)
              chatModel.controller.setNetCfg(NetCfg.defaults)
              networkUseSocksProxy.value = false
            }
          }
        )
      }
    }
  )
}

@Composable fun NetworkAndServersLayout(
  developerTools: Boolean,
  networkUseSocksProxy: MutableState<Boolean>,
  showModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  toggleSocksProxy: (Boolean) -> Unit
) {
  Column(
    Modifier.fillMaxWidth(),
    horizontalAlignment = Alignment.Start,
    verticalArrangement = Arrangement.spacedBy(8.dp)
  ) {
    Text(
      stringResource(R.string.network_and_servers),
      Modifier.padding(start = 16.dp, bottom = 24.dp),
      style = MaterialTheme.typography.h1
    )
    SectionView {
      SettingsActionItem(Icons.Outlined.Dns, stringResource(R.string.smp_servers), showModal { SMPServersView(it) })
      SectionDivider()
      SectionItemView {
        UseSocksProxySwitch(networkUseSocksProxy, toggleSocksProxy)
      }
      if (developerTools) {
        SectionDivider()
        SettingsActionItem(Icons.Outlined.Cable, stringResource(R.string.network_settings), showSettingsModal { AdvancedNetworkSettingsView(it) })
      }
    }
  }
}

@Composable
fun UseSocksProxySwitch(
  networkUseSocksProxy: MutableState<Boolean>,
  toggleSocksProxy: (Boolean) -> Unit
) {
  Row(
    Modifier.fillMaxWidth(),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.SpaceBetween
  ) {
    Row(
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.spacedBy(8.dp)
    ) {
      Icon(
        Icons.Outlined.SettingsEthernet,
        stringResource(R.string.network_socks_toggle),
        tint = HighOrLowlight
      )
      Text(stringResource(R.string.network_socks_toggle))
    }
    Switch(
      checked = networkUseSocksProxy.value,
      onCheckedChange = toggleSocksProxy,
      colors = SwitchDefaults.colors(
        checkedThumbColor = MaterialTheme.colors.primary,
        uncheckedThumbColor = HighOrLowlight
      ),
    )
  }
}

@Preview(showBackground = true)
@Composable
fun PreviewNetworkAndServersLayout() {
  SimpleXTheme {
    NetworkAndServersLayout(
      developerTools = true,
      networkUseSocksProxy = remember { mutableStateOf(true) },
      showModal = { {} },
      showSettingsModal = { {} },
      toggleSocksProxy = {}
    )
  }
}
