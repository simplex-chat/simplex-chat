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
import chat.simplex.common.model.ServerAddress.Companion.parseServerAddress
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.platform.appPlatform
import chat.simplex.res.MR

@Composable
fun ModalData.ProtocolServersView(m: ChatModel, rhId: Long?, serverProtocol: ServerProtocol, close: () -> Unit) {
  var presetServers by remember(rhId) { mutableStateOf(emptyList<ServerCfg>()) }
  var servers by remember { stateGetOrPut("servers") { emptyList<ServerCfg>() } }
  var serversAlreadyLoaded by remember { stateGetOrPut("serversAlreadyLoaded") { false } }
  val currServers = remember(rhId) { mutableStateOf(servers) }
  val testing = rememberSaveable(rhId) { mutableStateOf(false) }
  val serversUnchanged = remember(servers) { derivedStateOf { servers == currServers.value || testing.value } }
  val allServersDisabled = remember { derivedStateOf { servers.none { it.enabled } } }
  val saveDisabled = remember(servers) {
    derivedStateOf {
      servers.isEmpty() ||
      servers == currServers.value ||
      testing.value ||
      servers.none { srv ->
        val address = parseServerAddress(srv.server)
        address != null && uniqueAddress(srv, address, servers)
      } ||
      allServersDisabled.value
    }
  }

  KeyChangeEffect(rhId) {
    servers = emptyList()
    serversAlreadyLoaded = false
  }

  LaunchedEffect(rhId) {
    withApi {
      val res = m.controller.getUserProtoServers(rhId, serverProtocol)
      if (res != null) {
        currServers.value = res.protoServers
        presetServers = res.presetServers
        if (servers.isEmpty() && !serversAlreadyLoaded) {
          servers = currServers.value
          serversAlreadyLoaded = true
        }
      }
    }
  }
  val testServersJob = CancellableOnGoneJob()
  fun showServer(server: ServerCfg) {
    ModalManager.start.showModalCloseable(true) { close ->
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
        },
        onDelete = {
          val newServers = ArrayList(servers)
          newServers.removeAt(index)
          servers = newServers
          close()
        })
    }
  }
  ModalView(
    close = {
      if (saveDisabled.value) close()
      else showUnsavedChangesAlert({ saveServers(rhId, serverProtocol, currServers, servers, m, close) }, close)
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
              if (appPlatform.isAndroid) {
                SectionItemView({
                  AlertManager.shared.hideAlert()
                  ModalManager.start.showModalCloseable { close ->
                    ScanProtocolServer(rhId) {
                      close()
                      servers = servers + it
                    }
                  }
                }
                ) {
                  Text(stringResource(MR.strings.smp_servers_scan_qr), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
                }
              }
              val hasAllPresets = hasAllPresets(presetServers, servers, m)
              if (!hasAllPresets) {
                SectionItemView({
                  AlertManager.shared.hideAlert()
                  servers = (servers + addAllPresets(rhId, presetServers, servers, m)).sortedByDescending { it.preset }
                }) {
                  Text(stringResource(MR.strings.smp_servers_preset_add), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
                }
              }
            }
          }
        )
      },
      testServers = {
        testServersJob.value = withLongRunningApi {
          testServers(testing, servers, m) {
            servers = it
          }
        }
      },
      resetServers = {
        servers = currServers.value
      },
      saveSMPServers = {
        saveServers(rhId, serverProtocol, currServers, servers, m)
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
  ColumnWithScrollBar(
    Modifier
      .fillMaxWidth()
  ) {
    AppBarTitle(stringResource(if (serverProtocol == ServerProtocol.SMP) MR.strings.your_SMP_servers else MR.strings.your_XFTP_servers))

    val configuredServers = servers.filter { it.preset || it.enabled }
    val otherServers = servers.filter { !(it.preset || it.enabled) }

    if (configuredServers.isNotEmpty()) {
      SectionView(stringResource(if (serverProtocol == ServerProtocol.SMP) MR.strings.smp_servers_configured else MR.strings.xftp_servers_configured).uppercase()) {
        for (srv in configuredServers) {
          SectionItemView({ showServer(srv) }, disabled = testing) {
            ProtocolServerView(serverProtocol, srv, servers, testing)
          }
        }
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
    }

    if (otherServers.isNotEmpty()) {
      SectionView(stringResource(if (serverProtocol == ServerProtocol.SMP) MR.strings.smp_servers_other else MR.strings.xftp_servers_other).uppercase()) {
        for (srv in otherServers.filter { !(it.preset || it.enabled) }) {
          SectionItemView({ showServer(srv) }, disabled = testing) {
            ProtocolServerView(serverProtocol, srv, servers, testing)
          }
        }
      }
    }

    SectionView {
      SettingsActionItem(
        painterResource(MR.images.ic_add),
        stringResource(MR.strings.smp_servers_add),
        addServer,
        disabled = testing,
        textColor = if (testing) MaterialTheme.colors.secondary else MaterialTheme.colors.primary,
        iconColor = if (testing) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
      )
      SectionDividerSpaced(maxTopPadding = false, maxBottomPadding = false)
    }

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

private fun hasAllPresets(presetServers: List<ServerCfg>, servers: List<ServerCfg>, m: ChatModel): Boolean =
  presetServers.all { hasPreset(it, servers) } ?: true

private fun addAllPresets(rhId: Long?, presetServers: List<ServerCfg>, servers: List<ServerCfg>, m: ChatModel): List<ServerCfg> {
  val toAdd = ArrayList<ServerCfg>()
  for (srv in presetServers) {
    if (!hasPreset(srv, servers)) {
      toAdd.add(srv)
    }
  }
  return toAdd
}

private fun hasPreset(srv: ServerCfg, servers: List<ServerCfg>): Boolean =
  servers.any { it.server == srv.server }

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
      interruptIfCancelled()
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

private fun saveServers(rhId: Long?, protocol: ServerProtocol, currServers: MutableState<List<ServerCfg>>, servers: List<ServerCfg>, m: ChatModel, afterSave: () -> Unit = {}) {
  withBGApi {
    if (m.controller.setUserProtoServers(rhId, protocol, servers)) {
      currServers.value = servers
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
