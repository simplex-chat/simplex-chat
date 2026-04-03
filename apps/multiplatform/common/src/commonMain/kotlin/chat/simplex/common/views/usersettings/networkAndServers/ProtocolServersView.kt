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
  serverWarnings: MutableState<List<UserServersWarning>>,
  operatorIndex: Int,
  rhId: Long?
) {
  val testing = remember { mutableStateOf(false) }
  val currentUser = remember { chatModel.currentUser }.value
  val scope = rememberCoroutineScope()

  LaunchedEffect(userServers) {
    snapshotFlow { userServers.value }
      .collect { updatedServers ->
        validateServers_(rhId = rhId, userServersToValidate = updatedServers, serverErrors = serverErrors, serverWarnings = serverWarnings)
      }
  }

  Box {
    ColumnWithScrollBar {
      AppBarTitle(stringResource(MR.strings.your_servers))
      YourServersViewLayout(
        scope,
        userServers,
        serverErrors,
        serverWarnings,
        operatorIndex,
        navigateToProtocolView = { serverIndex, server, protocol ->
          navigateToProtocolView(userServers, serverErrors, serverWarnings, operatorIndex, rhId, serverIndex, server, protocol)
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
  serverWarnings: MutableState<List<UserServersWarning>>,
  operatorIndex: Int,
  navigateToProtocolView: (Int, UserServer, ServerProtocol) -> Unit,
  currentUser: User?,
  rhId: Long?,
  testing: MutableState<Boolean>
) {
  val duplicateHosts = findDuplicateHosts(serverErrors.value)

  Column {
    if (userServers.value[operatorIndex].chatRelays.any { !it.deleted }) {
      val duplicateRelayAddresses = findDuplicateRelayAddresses(serverErrors.value)
      SectionView(generalGetString(MR.strings.chat_relays).uppercase()) {
        userServers.value[operatorIndex].chatRelays.forEachIndexed { i, relay ->
          if (relay.deleted) return@forEachIndexed
          ChatRelayViewLink(relay, duplicateRelayAddresses) {
            navigateToChatRelayView(userServers, serverErrors, serverWarnings, operatorIndex, i, relay, rhId)
          }
        }
      }
      SectionTextFooter(generalGetString(MR.strings.chat_relays_forward_messages_in_channels))
    }

    if (userServers.value[operatorIndex].smpServers.any { !it.deleted }) {
      SectionDividerSpaced()
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
      userServers.value[operatorIndex].xftpServers.any { !it.deleted } ||
      userServers.value[operatorIndex].chatRelays.any { !it.deleted }
      ) {
      SectionDividerSpaced(maxTopPadding = false, maxBottomPadding = false)
    }

    SectionView {
      SettingsActionItem(
        painterResource(MR.images.ic_add),
        stringResource(MR.strings.smp_servers_add),
        click = { showAddServerDialog(scope, userServers, serverErrors, serverWarnings, rhId) },
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
    val serversWarn = globalServersWarning(serverWarnings.value)
    if (serversWarn != null) {
      SectionCustomFooter {
        ServersWarningFooter(serversWarn)
      }
    }
    SectionDividerSpaced(maxTopPadding = false, maxBottomPadding = false)

    SectionView {
      TestServersButton(
        testing = testing,
        smpServers = userServers.value[operatorIndex].smpServers,
        xftpServers = userServers.value[operatorIndex].xftpServers,
        chatRelays = userServers.value[operatorIndex].chatRelays,
        onUpdate = { p, l ->
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
        },
        onUpdateRelays = { relays ->
          userServers.value = userServers.value.toMutableList().apply {
            this[operatorIndex] = this[operatorIndex].copy(
              chatRelays = relays
            )
          }
        }
      )

      HowToButton()
    }
    SectionBottomSpacer()
  }
}

@Composable
fun TestServersButton(
  smpServers: List<UserServer>,
  xftpServers: List<UserServer>,
  chatRelays: List<UserChatRelay> = emptyList(),
  testing: MutableState<Boolean>,
  onUpdate: (ServerProtocol, List<UserServer>) -> Unit,
  onUpdateRelays: ((List<UserChatRelay>) -> Unit)? = null
) {
  val scope = rememberCoroutineScope()
  val disabled = derivedStateOf {
    (smpServers.none { it.enabled } && xftpServers.none { it.enabled } && chatRelays.filter { !it.deleted }.none { it.enabled }) || testing.value
  }

  SectionItemView(
    {
      scope.launch {
        testServers(testing, smpServers, xftpServers, chatRelays, chatModel, onUpdate, onUpdateRelays)
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
  serverWarnings: MutableState<List<UserServersWarning>>,
  rhId: Long?
) {
  AlertManager.shared.showAlertDialogButtonsColumn(
    title = generalGetString(MR.strings.smp_servers_add),
    buttons = {
      Column {
        SectionItemView({
          AlertManager.shared.hideAlert()
          ModalManager.start.showCustomModal { close ->
            NewServerView(userServers, serverErrors, serverWarnings, rhId, close)
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
                  serverWarnings,
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
        SectionItemView({
          AlertManager.shared.hideAlert()
          ModalManager.start.showCustomModal { close ->
            NewChatRelayView(userServers, serverErrors, serverWarnings, rhId, close)
          }
        }) {
          Text(stringResource(MR.strings.chat_relay), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
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
  chatRelays: List<UserChatRelay>,
  m: ChatModel,
  onUpdate: (ServerProtocol, List<UserServer>) -> Unit,
  onUpdateRelays: ((List<UserChatRelay>) -> Unit)?
) {
  val relaysResetStatus = resetRelayTestStatus(chatRelays)
  onUpdateRelays?.invoke(relaysResetStatus)
  val smpResetStatus = resetTestStatus(smpServers)
  onUpdate(ServerProtocol.SMP, smpResetStatus)
  val xftpResetStatus = resetTestStatus(xftpServers)
  onUpdate(ServerProtocol.XFTP, xftpResetStatus)
  testing.value = true
  val relayFailures = runRelaysTest(relaysResetStatus) { onUpdateRelays?.invoke(it) }
  val smpFailures = runServersTest(smpResetStatus, m) { onUpdate(ServerProtocol.SMP, it) }
  val xftpFailures = runServersTest(xftpResetStatus, m) { onUpdate(ServerProtocol.XFTP, it) }
  testing.value = false
  val failures = mutableListOf<String>()
  failures += relayFailures.map { (name, f) -> "$name: ${f.localizedDescription}" }
  failures += smpFailures.map { (srv, f) -> "$srv: ${f.localizedDescription}" }
  failures += xftpFailures.map { (srv, f) -> "$srv: ${f.localizedDescription}" }
  if (failures.isNotEmpty()) {
    val msg = failures.joinToString("\n")
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

private fun resetRelayTestStatus(relays: List<UserChatRelay>): List<UserChatRelay> {
  val copy = ArrayList(relays)
  for ((index, relay) in relays.withIndex()) {
    if (relay.enabled && !relay.deleted) {
      copy.removeAt(index)
      copy.add(index, relay.copy(tested = null))
    }
  }
  return copy
}

private suspend fun runRelaysTest(relays: List<UserChatRelay>, onUpdated: (List<UserChatRelay>) -> Unit): Map<String, RelayTestFailure> {
  val fs: MutableMap<String, RelayTestFailure> = mutableMapOf()
  val updatedRelays = ArrayList<UserChatRelay>(relays)
  for ((index, relay) in relays.withIndex()) {
    if (relay.enabled && !relay.deleted) {
      interruptIfCancelled()
      val relayState = mutableStateOf(relay)
      val f = testRelayConnection(relayState)
      updatedRelays.removeAt(index)
      updatedRelays.add(index, relayState.value)
      onUpdated(updatedRelays.toList())
      if (f != null) {
        val name = relayState.value.displayName.ifEmpty { relayState.value.domains.firstOrNull() ?: relayState.value.address }
        fs[name] = f
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

fun deleteChatRelay(
  userServers: MutableState<List<UserOperatorServers>>,
  operatorServersIndex: Int,
  relayIndex: Int
) {
  val relay = userServers.value[operatorServersIndex].chatRelays[relayIndex]
  if (relay.chatRelayId == null) {
    userServers.value = userServers.value.toMutableList().apply {
      this[operatorServersIndex] = this[operatorServersIndex].copy(
        chatRelays = this[operatorServersIndex].chatRelays.toMutableList().apply {
          this.removeAt(relayIndex)
        }
      )
    }
  } else {
    userServers.value = userServers.value.toMutableList().apply {
      this[operatorServersIndex] = this[operatorServersIndex].copy(
        chatRelays = this[operatorServersIndex].chatRelays.toMutableList().apply {
          this[relayIndex] = this[relayIndex].copy(deleted = true)
        }
      )
    }
  }
}
