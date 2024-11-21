package chat.simplex.common.views.usersettings.networkAndServers

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionItemViewSpaceBetween
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ServerAddress.Companion.parseServerAddress
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.QRCode
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.*
import chat.simplex.common.views.usersettings.PreferenceToggle
import chat.simplex.res.MR
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.distinctUntilChanged

@Composable
fun ProtocolServerView(
  m: ChatModel,
  server: UserServer,
  serverProtocol: ServerProtocol,
  userServers: MutableState<List<UserOperatorServers>>,
  serverErrors: MutableState<List<UserServersError>>,
  onDelete: () -> Unit,
  onUpdate: (UserServer) -> Unit,
  close: () -> Unit,
  rhId: Long?
) {
  var testing by remember { mutableStateOf(false) }
  val scope = rememberCoroutineScope()
  val draftServer = remember { mutableStateOf(server) }

  ModalView(
    close = {
      scope.launch {
        val draftResult = serverProtocolAndOperator(draftServer.value, userServers.value)
        val savedResult = serverProtocolAndOperator(server, userServers.value)

        if (draftResult != null && savedResult != null) {
          val (serverToEditProtocol, serverToEditOperator) = draftResult
          val (svProtocol, serverOperator) = savedResult

          if (serverToEditProtocol != svProtocol) {
            close()
            AlertManager.shared.showAlertMsg(
              title = generalGetString(MR.strings.error_updating_server_title),
              text = generalGetString(MR.strings.error_server_protocol_changed)
            )
          } else if (serverToEditOperator != serverOperator) {
            close()
            AlertManager.shared.showAlertMsg(
              title = generalGetString(MR.strings.error_updating_server_title),
              text = generalGetString(MR.strings.error_server_operator_changed)
            )
          } else {
            onUpdate(draftServer.value)
            validateServers(rhId, userServers, serverErrors)
            close()
          }
        } else {
          close()
          AlertManager.shared.showAlertMsg(
            title = generalGetString(MR.strings.smp_servers_invalid_address),
            text = generalGetString(MR.strings.smp_servers_check_address)
          )
        }
      }
    }
  ) {
    ProtocolServerLayout(
      testing,
      draftServer.value,
      serverProtocol,
      testServer = {
        testing = true
        withLongRunningApi {
          val res = testServerConnection(draftServer.value, m)
          if (isActive) {
            draftServer.value = res.first
            testing = false
          }
        }
      },
      onUpdate = {
        draftServer.value = it
      },
      onDelete
    )
    if (testing) {
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
private fun ProtocolServerLayout(
  testing: Boolean,
  server: UserServer,
  serverProtocol: ServerProtocol,
  testServer: () -> Unit,
  onUpdate: (UserServer) -> Unit,
  onDelete: () -> Unit,
) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(if (serverProtocol == ServerProtocol.XFTP) MR.strings.xftp_server else MR.strings.smp_server))

    if (server.preset) {
      PresetServer(testing, server, testServer, onUpdate)
    } else {
      CustomServer(testing, server, serverProtocol, testServer, onUpdate, onDelete)
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun PresetServer(
  testing: Boolean,
  server: UserServer,
  testServer: () -> Unit,
  onUpdate: (UserServer) -> Unit,
) {
  SectionView(stringResource(MR.strings.smp_servers_preset_address).uppercase()) {
    SelectionContainer {
      Text(
        server.server,
        Modifier.padding(start = DEFAULT_PADDING, top = 5.dp, end = DEFAULT_PADDING, bottom = 10.dp),
        style = TextStyle(
          fontFamily = FontFamily.Monospace, fontSize = 16.sp,
          color = MaterialTheme.colors.secondary
        )
      )
    }
  }
  SectionDividerSpaced()
  UseServerSection(true, testing, server, testServer, onUpdate)
}

@Composable
private fun CustomServer(
  testing: Boolean,
  server: UserServer,
  serverProtocol: ServerProtocol,
  testServer: () -> Unit,
  onUpdate: (UserServer) -> Unit,
  onDelete: () -> Unit,
) {
  val serverAddress = remember { mutableStateOf(server.server) }
  val valid = remember {
    derivedStateOf {
      with(parseServerAddress(serverAddress.value)) {
        this?.valid == true && this.serverProtocol == serverProtocol
      }
    }
  }
  SectionView(
    stringResource(MR.strings.smp_servers_your_server_address).uppercase(),
    icon = painterResource(MR.images.ic_error),
    iconTint = if (!valid.value) MaterialTheme.colors.error else Color.Transparent,
  ) {
    val testedPreviously = remember { mutableMapOf<String, Boolean?>() }
    TextEditor(
      serverAddress,
      Modifier.height(144.dp)
    )
    LaunchedEffect(Unit) {
      snapshotFlow { serverAddress.value }
        .distinctUntilChanged()
        .collect {
          testedPreviously[server.server] = server.tested
          onUpdate(server.copy(server = it, tested = testedPreviously[serverAddress.value]))
        }
    }
  }
  SectionDividerSpaced(maxTopPadding = true)
  UseServerSection(valid.value, testing, server, testServer, onUpdate, onDelete)

  if (valid.value) {
    SectionDividerSpaced()
    SectionView(stringResource(MR.strings.smp_servers_add_to_another_device).uppercase()) {
      QRCode(serverAddress.value)
    }
  }
}

@Composable
private fun UseServerSection(
  valid: Boolean,
  testing: Boolean,
  server: UserServer,
  testServer: () -> Unit,
  onUpdate: (UserServer) -> Unit,
  onDelete: (() -> Unit)? = null,
) {
  SectionView(stringResource(MR.strings.smp_servers_use_server).uppercase()) {
    SectionItemViewSpaceBetween(testServer, disabled = !valid || testing) {
      Text(stringResource(MR.strings.smp_servers_test_server), color = if (valid && !testing) MaterialTheme.colors.onBackground else MaterialTheme.colors.secondary)
      ShowTestStatus(server)
    }

    val enabled = rememberUpdatedState(server.enabled)
    PreferenceToggle(
      stringResource(MR.strings.smp_servers_use_server_for_new_conn),
      // disabled = server.tested != true && !server.preset,
      checked = enabled.value
    ) {
      onUpdate(server.copy(enabled = it))
    }

    if (onDelete != null) {
      SectionItemView(onDelete, disabled = testing) {
        Text(stringResource(MR.strings.smp_servers_delete_server), color = if (testing) MaterialTheme.colors.secondary else MaterialTheme.colors.error)
      }
    }
  }
}

@Composable
fun ShowTestStatus(server: UserServer, modifier: Modifier = Modifier) =
  when (server.tested) {
    true -> Icon(painterResource(MR.images.ic_check), null, modifier, tint = SimplexGreen)
    false -> Icon(painterResource(MR.images.ic_close), null, modifier, tint = MaterialTheme.colors.error)
    else -> Icon(painterResource(MR.images.ic_check), null, modifier, tint = Color.Transparent)
  }

suspend fun testServerConnection(server: UserServer, m: ChatModel): Pair<UserServer, ProtocolTestFailure?> =
  try {
    val r = m.controller.testProtoServer(server.remoteHostId, server.server)
    server.copy(tested = r == null) to r
  } catch (e: Exception) {
    Log.e(TAG, "testServerConnection ${e.stackTraceToString()}")
    server.copy(tested = false) to null
  }

fun serverHostname(srv: String): String =
  parseServerAddress(srv)?.hostnames?.firstOrNull() ?: srv
