package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalUriHandler
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import com.icerockdev.library.MR
import chat.simplex.common.model.*
import chat.simplex.common.model.ServerAddress.Companion.parseServerAddress
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.User
import kotlinx.coroutines.launch

@Composable
fun ProtocolServersView(m: ChatModel, serverProtocol: ServerProtocol, close: () -> Unit) {
  var presetServers by remember { mutableStateOf(emptyList<String>()) }
  var servers by remember {
    mutableStateOf(m.userSMPServersUnsaved.value ?: emptyList())
  }
  val currServers = remember { mutableStateOf(servers) }
  val testing = rememberSaveable { mutableStateOf(false) }
  val serversUnchanged = remember { derivedStateOf { servers == currServers.value || testing.value } }
  val allServersDisabled = remember { derivedStateOf { servers.all { !it.enabled } } }
  val saveDisabled = remember {
    derivedStateOf {
      servers.isEmpty() ||
      servers == currServers.value ||
      testing.value ||
      !servers.all { srv ->
        val address = parseServerAddress(srv.server)
        address != null && uniqueAddress(srv, address, servers)
      } ||
      allServersDisabled.value
    }
  }

  LaunchedEffect(Unit) {
    val res = m.controller.getUserProtoServers(serverProtocol)
    if (res != null) {
      currServers.value = res.protoServers
      presetServers = res.presetServers
      if (servers.isEmpty()) {
        servers = currServers.value
      }
    }
  }

  fun showServer(server: ServerCfg) {
    ModalManager.shared.showModalCloseable(true) { close ->
      var old by remember { mutableStateOf(server) }
      val index = servers.indexOf(old)
      ProtocolServerView(
        m,
        old,
        serverProtocol,
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
  ModalView(
    close = {
      if (saveDisabled.value) close()
      else showUnsavedChangesAlert({ saveServers(serverProtocol, currServers, servers, m, close) }, close)
    },
  ) {
    ProtocolServersLayout(
      serverProtocol,
      testing = testing.value,
      servers = servers,
      serversUnchanged = serversUnchanged.value,
      saveDisabled = saveDisabled.value,
      allServersDisabled = allServersDisabled.value,
      m.currentUser.value,
      addServer = {
        AlertManager.shared.showAlertDialogButtonsColumn(
          title = generalGetString(MR.strings.smp_servers_add),
          buttons = {
            Column {
              SectionItemView({
                AlertManager.shared.hideAlert()
                servers = servers + ServerCfg.empty
                // No saving until something will be changed on the next screen to prevent blank servers on the list
                showServer(servers.last())
              }) {
                Text(stringResource(MR.strings.smp_servers_enter_manually), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
              }
              SectionItemView({
                AlertManager.shared.hideAlert()
                ModalManager.shared.showModalCloseable { close ->
                  ScanProtocolServer {
                    close()
                    servers = servers + it
                    m.userSMPServersUnsaved.value = servers
                  }
                }
              }
              ) {
                Text(stringResource(MR.strings.smp_servers_scan_qr), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
              }
              val hasAllPresets = hasAllPresets(presetServers, servers, m)
              if (!hasAllPresets) {
                SectionItemView({
                  AlertManager.shared.hideAlert()
                  servers = (servers + addAllPresets(presetServers, servers, m)).sortedByDescending { it.preset }
                }) {
                  Text(stringResource(MR.strings.smp_servers_preset_add), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.onBackground)
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
        servers = currServers.value ?: emptyList()
        m.userSMPServersUnsaved.value = null
      },
      saveSMPServers = {
        saveServers(serverProtocol, currServers, servers, m)
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
          color = MaterialTheme.colors.secondary,
          strokeWidth = 2.5.dp
        )
      }
    }
  }
}

@Composable
private fun ProtocolServersLayout(
  serverProtocol: ServerProtocol,
  testing: Boolean,
  servers: List<ServerCfg>,
  serversUnchanged: Boolean,
  saveDisabled: Boolean,
  allServersDisabled: Boolean,
  currentUser: User?,
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
  ) {
    AppBarTitle(stringResource(if (serverProtocol == ServerProtocol.SMP) MR.strings.your_SMP_servers else MR.strings.your_XFTP_servers))

    SectionView(stringResource(if (serverProtocol == ServerProtocol.SMP) MR.strings.smp_servers else MR.strings.xftp_servers).uppercase()) {
      for (srv in servers) {
        SectionItemView({ showServer(srv) }, disabled = testing) {
          ProtocolServerView(serverProtocol, srv, servers, testing)
        }
      }
      SettingsActionItem(
        painterResource(MR.images.ic_add),
        stringResource(MR.strings.smp_servers_add),
        addServer,
        disabled = testing,
        textColor = if (testing) MaterialTheme.colors.secondary else MaterialTheme.colors.primary,
        iconColor = if (testing) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
      )
    }
    SectionTextFooter(
      remember(currentUser?.displayName) {
        buildAnnotatedString {
          append(generalGetString(MR.strings.smp_servers_per_user) + " ")
          withStyle(SpanStyle(fontWeight = FontWeight.Bold)) {
            append(currentUser?.displayName ?: "")
          }
          append(".")
        }
      }
    )
    SectionDividerSpaced(maxTopPadding = true, maxBottomPadding = false)
    SectionView {
      SectionItemView(resetServers, disabled = serversUnchanged) {
        Text(stringResource(MR.strings.reset_verb), color = if (!serversUnchanged) MaterialTheme.colors.onBackground else MaterialTheme.colors.secondary)
      }
      val testServersDisabled = testing || allServersDisabled
      SectionItemView(testServers, disabled = testServersDisabled) {
        Text(stringResource(MR.strings.smp_servers_test_servers), color = if (!testServersDisabled) MaterialTheme.colors.onBackground else MaterialTheme.colors.secondary)
      }
      SectionItemView(saveSMPServers, disabled = saveDisabled) {
        Text(stringResource(MR.strings.smp_servers_save), color = if (!saveDisabled) MaterialTheme.colors.onBackground else MaterialTheme.colors.secondary)
      }
    }
    SectionDividerSpaced(maxBottomPadding = false)
    SectionView {
      HowToButton()
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun ProtocolServerView(serverProtocol: ServerProtocol, srv: ServerCfg, servers: List<ServerCfg>, disabled: Boolean) {
  val address = parseServerAddress(srv.server)
  when {
    address == null || !address.valid || address.serverProtocol != serverProtocol || !uniqueAddress(srv, address, servers) -> InvalidServer()
    !srv.enabled -> Icon(painterResource(MR.images.ic_do_not_disturb_on), null, tint = MaterialTheme.colors.secondary)
    else -> ShowTestStatus(srv)
  }
  Spacer(Modifier.padding(horizontal = 4.dp))
  val text = address?.hostnames?.firstOrNull() ?: srv.server
  if (srv.enabled) {
    Text(text, color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.onBackground, maxLines = 1)
  } else {
    Text(text, maxLines = 1, color = MaterialTheme.colors.secondary)
  }
}

@Composable
private fun HowToButton() {
  val uriHandler = LocalUriHandler.current
  SettingsActionItem(
    painterResource(MR.images.ic_open_in_new),
    stringResource(MR.strings.how_to_use_your_servers),
    { uriHandler.openUriCatching("https://simplex.chat/docs/server.html") },
    textColor = MaterialTheme.colors.primary,
    iconColor = MaterialTheme.colors.primary
  )
}

@Composable
fun InvalidServer() {
  Icon(painterResource(MR.images.ic_error), null, tint = MaterialTheme.colors.error)
}

private fun uniqueAddress(s: ServerCfg, address: ServerAddress, servers: List<ServerCfg>): Boolean = servers.all { srv ->
  address.hostnames.all { host ->
    srv.id == s.id || !srv.server.contains(host)
  }
}

private fun hasAllPresets(presetServers: List<String>, servers: List<ServerCfg>, m: ChatModel): Boolean =
  presetServers.all { hasPreset(it, servers) } ?: true

private fun addAllPresets(presetServers: List<String>, servers: List<ServerCfg>, m: ChatModel): List<ServerCfg> {
  val toAdd = ArrayList<ServerCfg>()
  for (srv in presetServers) {
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
      title = generalGetString(MR.strings.smp_servers_test_failed),
      text = generalGetString(MR.strings.smp_servers_test_some_failed) + "\n" + msg
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

private suspend fun runServersTest(servers: List<ServerCfg>, m: ChatModel, onUpdated: (List<ServerCfg>) -> Unit): Map<String, ProtocolTestFailure> {
  val fs: MutableMap<String, ProtocolTestFailure> = mutableMapOf()
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

private fun saveServers(protocol: ServerProtocol, currServers: MutableState<List<ServerCfg>>, servers: List<ServerCfg>, m: ChatModel, afterSave: () -> Unit = {}) {
  withApi {
    if (m.controller.setUserProtoServers(protocol, servers)) {
      currServers.value = servers
      m.userSMPServersUnsaved.value = null
    }
    afterSave()
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
