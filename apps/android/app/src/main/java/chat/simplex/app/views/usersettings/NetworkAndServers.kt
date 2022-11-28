package chat.simplex.app.views.usersettings

import SectionDivider
import SectionItemView
import SectionItemWithValue
import SectionView
import SectionViewSelectable
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
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*

@Composable
fun NetworkAndServersView(
  chatModel: ChatModel,
  showModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
) {
  // It's not a state, just a one-time value. Shouldn't be used in any state-related situations
  val netCfg = remember { chatModel.controller.getNetCfg() }
  val networkUseSocksProxy: MutableState<Boolean> = remember { mutableStateOf(netCfg.useSocksProxy) }
  val developerTools = chatModel.controller.appPrefs.developerTools.get()
  val onionHosts = remember { mutableStateOf(netCfg.onionHosts) }

  LaunchedEffect(Unit) {
    chatModel.userSMPServersUnsaved.value = null
  }

  NetworkAndServersLayout(
    developerTools = developerTools,
    networkUseSocksProxy = networkUseSocksProxy,
    onionHosts = onionHosts,
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
              onionHosts.value = NetCfg.proxyDefaults.onionHosts
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
              onionHosts.value = NetCfg.defaults.onionHosts
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
        OnionHosts.NEVER -> generalGetString(R.string.network_use_onion_hosts_no_desc_in_alert)
        OnionHosts.PREFER -> generalGetString(R.string.network_use_onion_hosts_prefer_desc_in_alert)
        OnionHosts.REQUIRED -> generalGetString(R.string.network_use_onion_hosts_required_desc_in_alert)
      }
      updateOnionHostsDialog(startsWith, onDismiss = {
        onionHosts.value = prevValue
      }) {
        withApi {
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
    }
  )
}

@Composable fun NetworkAndServersLayout(
  developerTools: Boolean,
  networkUseSocksProxy: MutableState<Boolean>,
  onionHosts: MutableState<OnionHosts>,
  showModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  toggleSocksProxy: (Boolean) -> Unit,
  useOnion: (OnionHosts) -> Unit,
) {
  Column(
    Modifier.fillMaxWidth(),
    horizontalAlignment = Alignment.Start,
    verticalArrangement = Arrangement.spacedBy(8.dp)
  ) {
    AppBarTitle(stringResource(R.string.network_and_servers))
    SectionView(generalGetString(R.string.settings_section_title_messages)) {
      SettingsActionItem(Icons.Outlined.Dns, stringResource(R.string.smp_servers), showSettingsModal { SMPServersView(it) })
      SectionDivider()
      SectionItemView {
        UseSocksProxySwitch(networkUseSocksProxy, toggleSocksProxy)
      }
      SectionDivider()
      UseOnionHosts(onionHosts, networkUseSocksProxy, showSettingsModal, useOnion)
      if (developerTools) {
        SectionDivider()
        SettingsActionItem(Icons.Outlined.Cable, stringResource(R.string.network_settings), showSettingsModal { AdvancedNetworkSettingsView(it) })
      }
    }
    Spacer(Modifier.height(8.dp))
    SectionView(generalGetString(R.string.settings_section_title_calls)) {
      SettingsActionItem(Icons.Outlined.ElectricalServices, stringResource(R.string.webrtc_ice_servers), showModal { RTCServersView(it) })
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
      Modifier.weight(1f),
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

@Composable
private fun UseOnionHosts(
  onionHosts: MutableState<OnionHosts>,
  enabled: State<Boolean>,
  showModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  useOnion: (OnionHosts) -> Unit,
) {
  val values = remember {
    OnionHosts.values().map {
      when (it) {
        OnionHosts.NEVER -> ValueTitleDesc(OnionHosts.NEVER, generalGetString(R.string.network_use_onion_hosts_no), generalGetString(R.string.network_use_onion_hosts_no_desc))
        OnionHosts.PREFER -> ValueTitleDesc(OnionHosts.PREFER, generalGetString(R.string.network_use_onion_hosts_prefer), generalGetString(R.string.network_use_onion_hosts_prefer_desc))
        OnionHosts.REQUIRED -> ValueTitleDesc(OnionHosts.REQUIRED, generalGetString(R.string.network_use_onion_hosts_required), generalGetString(R.string.network_use_onion_hosts_required_desc))
      }
    }
  }
  val onSelected = showModal {

    Column(
      Modifier.fillMaxWidth(),
      horizontalAlignment = Alignment.Start,
    ) {
      AppBarTitle(stringResource(R.string.network_use_onion_hosts))
      SectionViewSelectable(null, onionHosts, values, useOnion)
    }
  }

  SectionItemWithValue(
    generalGetString(R.string.network_use_onion_hosts),
    onionHosts,
    values,
    icon = Icons.Outlined.Security,
    enabled = enabled,
    onSelected = onSelected
  )
}

private fun updateOnionHostsDialog(
  startsWith: String = "",
  message: String = generalGetString(R.string.updating_settings_will_reconnect_client_to_all_servers),
  onDismiss: () -> Unit,
  onConfirm: () -> Unit
) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.update_onion_hosts_settings_question),
    text = startsWith + "\n\n" + message,
    confirmText = generalGetString(R.string.update_network_settings_confirmation),
    onDismiss = onDismiss,
    onConfirm = onConfirm,
    onDismissRequest = onDismiss
  )
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
      toggleSocksProxy = {},
      onionHosts = remember { mutableStateOf(OnionHosts.PREFER) },
      useOnion = {},
    )
  }
}
