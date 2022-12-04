package chat.simplex.app.views.usersettings

import SectionDivider
import SectionItemView
import SectionSpacer
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.model.ServerAddress.Companion.parseServerAddress
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import kotlinx.coroutines.launch

@Composable
fun SMPServersView(m: ChatModel) {
  var servers by remember {
    mutableStateOf(m.userSMPServersUnsaved.value ?: m.userSMPServers.value ?: emptyList())
  }
  val testing = rememberSaveable { mutableStateOf(false) }
  val serversUnchanged = remember { derivedStateOf { servers == m.userSMPServers.value || testing.value } }
  val allServersDisabled = remember { derivedStateOf { servers.all { !it.enabled } } }
  val saveDisabled = remember {
    derivedStateOf {
      servers.isEmpty() ||
      servers == m.userSMPServers.value ||
      testing.value ||
      !servers.all { srv ->
        val address = parseServerAddress(srv.server)
        address != null && uniqueAddress(srv, address, servers)
      } ||
      allServersDisabled.value
    }
  }

  fun showServer(server: ServerCfg) {
    ModalManager.shared.showModalCloseable(true) { close ->
      var old by remember { mutableStateOf(server) }
      val index = servers.indexOf(old)
      SMPServerView(
        m,
        old,
        onUpdate = { updated ->
          val newServers = ArrayList(servers)
          newServers.removeAt(index)
          newServers.add(index, updated)
          old = updated
          servers = newServers
          m.userSMPServersUnsaved.value = servers
        },
        onDelete = {
          val newServers = ArrayList(servers)
          newServers.removeAt(index)
          servers = newServers
          m.userSMPServersUnsaved.value = servers
          close()
        })
    }
  }
  val scope = rememberCoroutineScope()

  SMPServersLayout(
    testing = testing.value,
    servers = servers,
    serversUnchanged = serversUnchanged.value,
    saveDisabled = saveDisabled.value,
    allServersDisabled = allServersDisabled.value,
    addServer = {
      AlertManager.shared.showAlertDialogButtonsColumn(
        title = generalGetString(R.string.smp_servers_add),
        buttons = {
          Column {
            SectionItemView({
              AlertManager.shared.hideAlert()
              servers = servers + ServerCfg.empty
              // No saving until something will be changed on the next screen to prevent blank servers on the list
              showServer(servers.last())
            }) {
              Text(stringResource(R.string.smp_servers_enter_manually))
            }
            SectionItemView({
              AlertManager.shared.hideAlert()
              ModalManager.shared.showModalCloseable { close ->
                ScanSMPServer {
                  close()
                  servers = servers + it
                  m.userSMPServersUnsaved.value = servers
                }
              }
            }
            ) {
              Text(stringResource(R.string.smp_servers_scan_qr))
            }
            val hasAllPresets = hasAllPresets(servers, m)
            if (!hasAllPresets) {
              SectionItemView({
                AlertManager.shared.hideAlert()
                servers = (servers + addAllPresets(servers, m)).sortedByDescending { it.preset }
              }) {
                Text(stringResource(R.string.smp_servers_preset_add), color = MaterialTheme.colors.onBackground)
              }
            }
          }
        }
      )
    },
    testServers = {
      scope.launch {
        testServers(testing, servers, m) {
          servers = it
          m.userSMPServersUnsaved.value = servers
        }
      }
    },
    resetServers = {
      servers = m.userSMPServers.value ?: emptyList()
      m.userSMPServersUnsaved.value = null
    },
    saveSMPServers = {
      saveSMPServers(servers, m)
    },
    showServer = ::showServer,
  )

  if (testing.value) {
    Box(
      Modifier.fillMaxSize(),
      contentAlignment = Alignment.Center
    ) {
      CircularProgressIndicator(
        Modifier
          .padding(horizontal = 2.dp)
          .size(30.dp),
        color = HighOrLowlight,
        strokeWidth = 2.5.dp
      )
    }
  }
}

@Composable
private fun SMPServersLayout(
  testing: Boolean,
  servers: List<ServerCfg>,
  serversUnchanged: Boolean,
  saveDisabled: Boolean,
  allServersDisabled: Boolean,
  addServer: () -> Unit,
  testServers: () -> Unit,
  resetServers: () -> Unit,
  saveSMPServers: () -> Unit,
  showServer: (ServerCfg) -> Unit,
) {
  Column(
    Modifier
      .fillMaxWidth()
      .verticalScroll(rememberScrollState())
      .padding(bottom = DEFAULT_PADDING),
  ) {
    AppBarTitle(stringResource(R.string.your_SMP_servers))

    SectionView(stringResource(R.string.smp_servers).uppercase()) {
      for (srv in servers) {
        SectionItemView({ showServer(srv) }, disabled = testing) {
          SmpServerView(srv, servers, testing)
        }
        SectionDivider()
      }
      SettingsActionItem(
        Icons.Outlined.Add,
        stringResource(R.string.smp_servers_add),
        addServer,
        disabled = testing,
        textColor = if (testing) HighOrLowlight else MaterialTheme.colors.primary,
        iconColor = if (testing) HighOrLowlight else MaterialTheme.colors.primary
      )
    }
    SectionSpacer()
    SectionView {
      SectionItemView(resetServers, disabled = serversUnchanged) {
        Text(stringResource(R.string.reset_verb), color = if (!serversUnchanged) MaterialTheme.colors.onBackground else HighOrLowlight)
      }
      SectionDivider()
      val testServersDisabled = testing || allServersDisabled
      SectionItemView(testServers, disabled = testServersDisabled) {
        Text(stringResource(R.string.smp_servers_test_servers), color = if (!testServersDisabled) MaterialTheme.colors.onBackground else HighOrLowlight)
      }
      SectionDivider()
      SectionItemView(saveSMPServers, disabled = saveDisabled) {
        Text(stringResource(R.string.smp_servers_save), color = if (!saveDisabled) MaterialTheme.colors.onBackground else HighOrLowlight)
      }
    }
    SectionSpacer()
    SectionView {
      HowToButton()
    }
  }
}

@Composable
private fun SmpServerView(srv: ServerCfg, servers: List<ServerCfg>, disabled: Boolean) {
  val address = parseServerAddress(srv.server)
  when {
    address == null || !address.valid || !uniqueAddress(srv, address, servers) -> InvalidServer()
    !srv.enabled -> Icon(Icons.Outlined.DoNotDisturb, null, tint = HighOrLowlight)
    else -> ShowTestStatus(srv)
  }
  Spacer(Modifier.padding(horizontal = 4.dp))
  val text = address?.hostnames?.firstOrNull() ?: srv.server
  if (srv.enabled) {
    Text(text, color = if (disabled) HighOrLowlight else MaterialTheme.colors.onBackground, maxLines = 1)
  } else {
    Text(text, maxLines = 1, color = HighOrLowlight)
  }
}

@Composable
private fun HowToButton() {
  val uriHandler = LocalUriHandler.current
  SettingsActionItem(
    Icons.Outlined.OpenInNew,
    stringResource(R.string.how_to_use_your_servers),
    { uriHandler.openUri("https://github.com/simplex-chat/simplex-chat/blob/stable/docs/SERVER.md") },
    textColor = MaterialTheme.colors.primary,
    iconColor = MaterialTheme.colors.primary
  )
}

@Composable
fun InvalidServer() {
  Icon(Icons.Outlined.ErrorOutline, null, tint = MaterialTheme.colors.error)
}

private fun uniqueAddress(s: ServerCfg, address: ServerAddress, servers: List<ServerCfg>): Boolean = servers.all { srv ->
  address.hostnames.all { host ->
    srv.id == s.id || !srv.server.contains(host)
  }
}

private fun hasAllPresets(servers: List<ServerCfg>, m: ChatModel): Boolean =
  m.presetSMPServers.value?.all { hasPreset(it, servers) } ?: true

private fun addAllPresets(servers: List<ServerCfg>, m: ChatModel): List<ServerCfg> {
  val toAdd = ArrayList<ServerCfg>()
  for (srv in m.presetSMPServers.value ?: emptyList()) {
    if (!hasPreset(srv, servers)) {
      toAdd.add(ServerCfg(srv, preset = true, tested = null, enabled = true))
    }
  }
  return toAdd
}

private fun hasPreset(srv: String, servers: List<ServerCfg>): Boolean =
  servers.any { it.server == srv }

private suspend fun testServers(testing: MutableState<Boolean>, servers: List<ServerCfg>, m: ChatModel, onUpdated: (List<ServerCfg>) -> Unit) {
  val resetStatus = resetTestStatus(servers)
  onUpdated(resetStatus)
  testing.value = true
  val fs = runServersTest(resetStatus, m) { onUpdated(it) }
  testing.value = false
  if (fs.isNotEmpty()) {
    val msg = fs.map { it.key + ": " + it.value.localizedDescription }.joinToString("\n")
    AlertManager.shared.showAlertMsg(
      title = generalGetString(R.string.smp_servers_test_failed),
      text = generalGetString(R.string.smp_servers_test_some_failed) + "\n" + msg
    )
  }
}

private fun resetTestStatus(servers: List<ServerCfg>): List<ServerCfg> {
  val copy = ArrayList(servers)
  for ((index, server) in servers.withIndex()) {
    if (server.enabled) {
      copy.removeAt(index)
      copy.add(index, server.copy(tested = null))
    }
  }
  return copy
}

private suspend fun runServersTest(servers: List<ServerCfg>, m: ChatModel, onUpdated: (List<ServerCfg>) -> Unit): Map<String, SMPTestFailure> {
  val fs: MutableMap<String, SMPTestFailure> = mutableMapOf()
  val updatedServers = ArrayList<ServerCfg>(servers)
  for ((index, server) in servers.withIndex()) {
    if (server.enabled) {
      val (updatedServer, f) = testServerConnection(server, m)
      updatedServers.removeAt(index)
      updatedServers.add(index, updatedServer)
      // toList() is important. Otherwise, Compose will not redraw the screen after first update
      onUpdated(updatedServers.toList())
      if (f != null) {
        fs[serverHostname(updatedServer.server)] = f
      }
    }
  }
  return fs
}

private fun saveSMPServers(servers: List<ServerCfg>, m: ChatModel) {
  withApi {
    if (m.controller.setUserSMPServers(servers)) {
      m.userSMPServers.value = servers
      m.userSMPServersUnsaved.value = null
    }
  }
}

