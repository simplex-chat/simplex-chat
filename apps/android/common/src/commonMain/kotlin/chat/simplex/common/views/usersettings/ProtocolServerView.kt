package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionItemViewSpaceBetween
import SectionView
import chat.simplex.common.platform.Log
import androidx.compose.foundation.*
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
import com.icerockdev.library.MR
import chat.simplex.common.platform.TAG
import chat.simplex.common.model.*
import chat.simplex.common.model.ServerAddress.Companion.parseServerAddress
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.QRCode
import chat.simplex.common.model.ChatModel
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.coroutines.isActive
import kotlinx.coroutines.launch

@Composable
fun ProtocolServerView(m: ChatModel, server: ServerCfg, serverProtocol: ServerProtocol, onUpdate: (ServerCfg) -> Unit, onDelete: () -> Unit) {
  var testing by remember { mutableStateOf(false) }
  val scope = rememberCoroutineScope()
  ProtocolServerLayout(
    testing,
    server,
    serverProtocol,
    testServer = {
      testing = true
      scope.launch {
        val res = testServerConnection(server, m)
        if (isActive) {
          onUpdate(res.first)
          testing = false
        }
      }
    },
    onUpdate,
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

@Composable
private fun ProtocolServerLayout(
  testing: Boolean,
  server: ServerCfg,
  serverProtocol: ServerProtocol,
  testServer: () -> Unit,
  onUpdate: (ServerCfg) -> Unit,
  onDelete: () -> Unit,
) {
  Column(
    Modifier
      .fillMaxWidth()
      .verticalScroll(rememberScrollState())
  ) {
    AppBarTitle(stringResource(if (server.preset) MR.strings.smp_servers_preset_server else MR.strings.smp_servers_your_server))

    if (server.preset) {
      PresetServer(testing, server, testServer, onUpdate, onDelete)
    } else {
      CustomServer(testing, server, serverProtocol, testServer, onUpdate, onDelete)
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun PresetServer(
  testing: Boolean,
  server: ServerCfg,
  testServer: () -> Unit,
  onUpdate: (ServerCfg) -> Unit,
  onDelete: () -> Unit,
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
  SectionDividerSpaced(maxTopPadding = true)
  UseServerSection(true, testing, server, testServer, onUpdate, onDelete)
}

@Composable
private fun CustomServer(
  testing: Boolean,
  server: ServerCfg,
  serverProtocol: ServerProtocol,
  testServer: () -> Unit,
  onUpdate: (ServerCfg) -> Unit,
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
  SectionDividerSpaced()
  UseServerSection(valid.value, testing, server, testServer, onUpdate, onDelete)

  if (valid.value) {
    SectionDividerSpaced()
    SectionView(stringResource(MR.strings.smp_servers_add_to_another_device).uppercase()) {
      QRCode(serverAddress.value, Modifier.aspectRatio(1f).padding(horizontal = DEFAULT_PADDING))
    }
  }
}

@Composable
private fun UseServerSection(
  valid: Boolean,
  testing: Boolean,
  server: ServerCfg,
  testServer: () -> Unit,
  onUpdate: (ServerCfg) -> Unit,
  onDelete: () -> Unit,
) {
  SectionView(stringResource(MR.strings.smp_servers_use_server).uppercase()) {
    SectionItemViewSpaceBetween(testServer, disabled = !valid || testing) {
      Text(stringResource(MR.strings.smp_servers_test_server), color = if (valid && !testing) MaterialTheme.colors.onBackground else MaterialTheme.colors.secondary)
      ShowTestStatus(server)
    }
    val enabled = rememberUpdatedState(server.enabled)
    PreferenceToggle(stringResource(MR.strings.smp_servers_use_server_for_new_conn), enabled.value) { onUpdate(server.copy(enabled = it)) }
    SectionItemView(onDelete, disabled = testing) {
      Text(stringResource(MR.strings.smp_servers_delete_server), color = if (testing) MaterialTheme.colors.secondary else MaterialTheme.colors.error)
    }
  }
}

@Composable
fun ShowTestStatus(server: ServerCfg, modifier: Modifier = Modifier) =
  when (server.tested) {
    true -> Icon(painterResource(MR.images.ic_check), null, modifier, tint = SimplexGreen)
    false -> Icon(painterResource(MR.images.ic_close), null, modifier, tint = MaterialTheme.colors.error)
    else -> Icon(painterResource(MR.images.ic_check), null, modifier, tint = Color.Transparent)
  }

suspend fun testServerConnection(server: ServerCfg, m: ChatModel): Pair<ServerCfg, ProtocolTestFailure?> =
  try {
    val r = m.controller.testProtoServer(server.server)
    server.copy(tested = r == null) to r
  } catch (e: Exception) {
    Log.e(TAG, "testServerConnection ${e.stackTraceToString()}")
    server.copy(tested = false) to null
  }

fun serverHostname(srv: String): String =
  parseServerAddress(srv)?.hostnames?.firstOrNull() ?: srv
