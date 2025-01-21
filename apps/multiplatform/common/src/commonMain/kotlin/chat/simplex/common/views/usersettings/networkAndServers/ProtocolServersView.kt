package chat.simplex.common.views.usersettings.networkAndServers

import SectionBottomSpacer
import SectionCustomFooter
import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
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
import chat.simplex.common.platform.*
import chat.simplex.common.views.usersettings.SettingsActionItem
import chat.simplex.res.MR
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.launch

@Composable
fun ModalData.YourServersView(
  userServers: MutableState<List<UserOperatorServers>>,
  serverErrors: MutableState<List<UserServersError>>,
  operatorIndex: Int,
  rhId: Long?
) {
  val testing = remember { mutableStateOf(false) }
  val currentUser = remember { chatModel.currentUser }.value
  val scope = rememberCoroutineScope()

  LaunchedEffect(userServers) {
    snapshotFlow { userServers.value }
      .collect { updatedServers ->
        validateServers_(rhId = rhId, userServersToValidate = updatedServers, serverErrors = serverErrors)
      }
  }

  Box {
    ColumnWithScrollBar {
      AppBarTitle(stringResource(MR.strings.your_servers))
      YourServersViewLayout(
        scope,
        userServers,
        serverErrors,
        operatorIndex,
        navigateToProtocolView = { serverIndex, server, protocol ->
          navigateToProtocolView(userServers, serverErrors, operatorIndex, rhId, serverIndex, server, protocol)
        },
        currentUser,
        rhId,
        testing
      )
    }

    if (testing.value) {
      DefaultProgressView(null)
    }
  }
}

@Composable
fun YourServersViewLayout(
  scope: CoroutineScope,
  userServers: MutableState<List<UserOperatorServers>>,
  serverErrors: MutableState<List<UserServersError>>,
  operatorIndex: Int,
  navigateToProtocolView: (Int, UserServer, ServerProtocol) -> Unit,
  currentUser: User?,
  rhId: Long?,
  testing: MutableState<Boolean>
) {
  val duplicateHosts = findDuplicateHosts(serverErrors.value)

  Column {
    if (userServers.value[operatorIndex].smpServers.any { !it.deleted }) {
      SectionView(generalGetString(MR.strings.message_servers).uppercase()) {
        userServers.value[operatorIndex].smpServers.forEachIndexed { i, server  ->
          if (server.deleted) return@forEachIndexed
          SectionItemView({ navigateToProtocolView(i, server, ServerProtocol.SMP) }) {
            ProtocolServerViewLink(
              srv = server,
              serverProtocol = ServerProtocol.SMP,
              duplicateHosts = duplicateHosts
            )
          }
        }
      }
      val smpErr = globalSMPServersError(serverErrors.value)
      if (smpErr != null) {
        SectionCustomFooter {
          ServersErrorFooter(smpErr)
        }
      } else {
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
      }
    }

    if (userServers.value[operatorIndex].xftpServers.any { !it.deleted }) {
      SectionDividerSpaced()
      SectionView(generalGetString(MR.strings.media_and_file_servers).uppercase()) {
        userServers.value[operatorIndex].xftpServers.forEachIndexed { i, server ->
          if (server.deleted) return@forEachIndexed
          SectionItemView({ navigateToProtocolView(i, server, ServerProtocol.XFTP) }) {
            ProtocolServerViewLink(
              srv = server,
              serverProtocol = ServerProtocol.XFTP,
              duplicateHosts = duplicateHosts
            )
          }
        }
      }
      val xftpErr = globalXFTPServersError(serverErrors.value)
      if (xftpErr != null) {
        SectionCustomFooter {
          ServersErrorFooter(xftpErr)
        }
      } else {
        SectionTextFooter(
          remember(currentUser?.displayName) {
            buildAnnotatedString {
              append(generalGetString(MR.strings.xftp_servers_per_user) + " ")
              withStyle(SpanStyle(fontWeight = FontWeight.Bold)) {
                append(currentUser?.displayName ?: "")
              }
              append(".")
            }
          }
        )
      }
    }

    if (
      userServers.value[operatorIndex].smpServers.any { !it.deleted } ||
      userServers.value[operatorIndex].xftpServers.any { !it.deleted }
      ) {
      SectionDividerSpaced(maxTopPadding = false, maxBottomPadding = false)
    }

    SectionView {
      SettingsActionItem(
        painterResource(MR.images.ic_add),
        stringResource(MR.strings.smp_servers_add),
        click = { showAddServerDialog(scope, userServers, serverErrors, rhId) },
        disabled = testing.value,
        textColor = if (testing.value) MaterialTheme.colors.secondary else MaterialTheme.colors.primary,
        iconColor = if (testing.value) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
      )
    }
    val serversErr = globalServersError(serverErrors.value)
    if (serversErr != null) {
      SectionCustomFooter {
        ServersErrorFooter(serversErr)
      }
    }
    SectionDividerSpaced(maxTopPadding = false, maxBottomPadding = false)

    SectionView {
      TestServersButton(
        testing = testing,
        smpServers = userServers.value[operatorIndex].smpServers,
        xftpServers = userServers.value[operatorIndex].xftpServers,
      ) { p, l ->
        when (p) {
          ServerProtocol.XFTP -> userServers.value = userServers.value.toMutableList().apply {
            this[operatorIndex] = this[operatorIndex].copy(
              xftpServers = l
            )
          }

          ServerProtocol.SMP -> userServers.value = userServers.value.toMutableList().apply {
            this[operatorIndex] = this[operatorIndex].copy(
              smpServers = l
            )
          }
        }
      }

      HowToButton()
    }
    SectionBottomSpacer()
  }
}

@Composable
fun TestServersButton(
  smpServers: List<UserServer>,
  xftpServers: List<UserServer>,
  testing: MutableState<Boolean>,
  onUpdate: (ServerProtocol, List<UserServer>) -> Unit
) {
  val scope = rememberCoroutineScope()
  val disabled = derivedStateOf { (smpServers.none { it.enabled } && xftpServers.none { it.enabled }) || testing.value }

  SectionItemView(
    {
      scope.launch {
        testServers(testing, smpServers, xftpServers, chatModel, onUpdate)
      }
    },
    disabled = disabled.value
  ) {
    Text(stringResource(MR.strings.smp_servers_test_servers), color = if (!disabled.value) MaterialTheme.colors.onBackground else MaterialTheme.colors.secondary)
  }
}

fun showAddServerDialog(
  scope: CoroutineScope,
  userServers: MutableState<List<UserOperatorServers>>,
  serverErrors: MutableState<List<UserServersError>>,
  rhId: Long?
) {
  AlertManager.shared.showAlertDialogButtonsColumn(
    title = generalGetString(MR.strings.smp_servers_add),
    buttons = {
      Column {
        SectionItemView({
          AlertManager.shared.hideAlert()
          ModalManager.start.showCustomModal { close ->
            NewServerView(userServers, serverErrors, rhId, close)
          }
        }) {
          Text(stringResource(MR.strings.smp_servers_enter_manually), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
        if (appPlatform.isAndroid) {
          SectionItemView({
            AlertManager.shared.hideAlert()
            ModalManager.start.showModalCloseable { close ->
              ScanProtocolServer(rhId) { server ->
                addServer(
                  scope,
                  server,
                  userServers,
                  serverErrors,
                  rhId,
                  close = close
                )
              }
            }
          }
          ) {
            Text(stringResource(MR.strings.smp_servers_scan_qr), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
          }
        }
      }
    }
  )
}

@Composable
fun ProtocolServerViewLink(serverProtocol: ServerProtocol, srv: UserServer, duplicateHosts: Set<String>) {
  val address = parseServerAddress(srv.server)
  when {
    address == null || !address.valid || address.serverProtocol != serverProtocol || address.hostnames.any { it in duplicateHosts } -> InvalidServer()
    !srv.enabled -> Icon(painterResource(MR.images.ic_do_not_disturb_on), null, tint = MaterialTheme.colors.secondary)
    else -> ShowTestStatus(srv)
  }
  Spacer(Modifier.padding(horizontal = 4.dp))
  val text = address?.hostnames?.firstOrNull() ?: srv.server
  if (srv.enabled) {
    Text(text, color = MaterialTheme.colors.onBackground, maxLines = 1)
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

private suspend fun testServers(
  testing: MutableState<Boolean>,
  smpServers: List<UserServer>,
  xftpServers: List<UserServer>,
  m: ChatModel,
  onUpdate: (ServerProtocol, List<UserServer>) -> Unit
) {
  val smpResetStatus = resetTestStatus(smpServers)
  onUpdate(ServerProtocol.SMP, smpResetStatus)
  val xftpResetStatus = resetTestStatus(xftpServers)
  onUpdate(ServerProtocol.XFTP, xftpResetStatus)
  testing.value = true
  val smpFailures = runServersTest(smpResetStatus, m) { onUpdate(ServerProtocol.SMP, it) }
  val xftpFailures = runServersTest(xftpResetStatus, m) { onUpdate(ServerProtocol.XFTP, it) }
  testing.value = false
  val fs = smpFailures + xftpFailures
  if (fs.isNotEmpty()) {
    val msg = fs.map { it.key + ": " + it.value.localizedDescription }.joinToString("\n")
    AlertManager.shared.showAlertMsg(
      title = generalGetString(MR.strings.smp_servers_test_failed),
      text = generalGetString(MR.strings.smp_servers_test_some_failed) + "\n" + msg
    )
  }
}

private fun resetTestStatus(servers: List<UserServer>): List<UserServer> {
  val copy = ArrayList(servers)
  for ((index, server) in servers.withIndex()) {
    if (server.enabled) {
      copy.removeAt(index)
      copy.add(index, server.copy(tested = null))
    }
  }
  return copy
}

private suspend fun runServersTest(servers: List<UserServer>, m: ChatModel, onUpdated: (List<UserServer>) -> Unit): Map<String, ProtocolTestFailure> {
  val fs: MutableMap<String, ProtocolTestFailure> = mutableMapOf()
  val updatedServers = ArrayList<UserServer>(servers)
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

fun deleteXFTPServer(
  userServers: MutableState<List<UserOperatorServers>>,
  operatorServersIndex: Int,
  serverIndex: Int
) {
  val serverIsSaved = userServers.value[operatorServersIndex].xftpServers[serverIndex].serverId != null

  if (serverIsSaved) {
    userServers.value = userServers.value.toMutableList().apply {
      this[operatorServersIndex] = this[operatorServersIndex].copy(
        xftpServers = this[operatorServersIndex].xftpServers.toMutableList().apply {
          this[serverIndex] = this[serverIndex].copy(deleted = true)
        }
      )
    }
  } else {
    userServers.value = userServers.value.toMutableList().apply {
      this[operatorServersIndex] = this[operatorServersIndex].copy(
        xftpServers = this[operatorServersIndex].xftpServers.toMutableList().apply {
          this.removeAt(serverIndex)
        }
      )
    }
  }
}

fun deleteSMPServer(
  userServers: MutableState<List<UserOperatorServers>>,
  operatorServersIndex: Int,
  serverIndex: Int
) {
  val serverIsSaved = userServers.value[operatorServersIndex].smpServers[serverIndex].serverId != null

  if (serverIsSaved) {
    userServers.value = userServers.value.toMutableList().apply {
      this[operatorServersIndex] = this[operatorServersIndex].copy(
        smpServers = this[operatorServersIndex].smpServers.toMutableList().apply {
          this[serverIndex] = this[serverIndex].copy(deleted = true)
        }
      )
    }
  } else {
    userServers.value = userServers.value.toMutableList().apply {
      this[operatorServersIndex] = this[operatorServersIndex].copy(
        smpServers = this[operatorServersIndex].smpServers.toMutableList().apply {
          this.removeAt(serverIndex)
        }
      )
    }
  }
}
