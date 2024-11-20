package chat.simplex.common.views.usersettings.networkAndServers

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionItemViewSpaceBetween
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.model.ServerAddress.Companion.parseServerAddress
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.QRCode
import chat.simplex.common.platform.*
import chat.simplex.common.views.usersettings.PreferenceToggle
import chat.simplex.res.MR
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.distinctUntilChanged

@Composable
fun ModalData.NewServerView(
  userServers: MutableState<List<UserOperatorServers>>,
  serverErrors: MutableState<List<UserServersError>>,
  operatorIndex: Int,
  rhId: Long?,
  close: () -> Unit
) {
  val newServer = remember { mutableStateOf(UserServer.empty) }
  val testing = remember { mutableStateOf(false) }

  ModalView(close = {
    addServer(
      newServer.value,
      userServers,
      serverErrors,
      rhId,
      close = close
    )
    close()
  }) {
    NewServerLayout(
      userServers,
      serverErrors,
      operatorIndex,
      rhId,
      newServer,
      testing
      //    testServer = {
      //      testing = true
      //      withLongRunningApi {
      //        val res = testServerConnection(server, m)
      //        if (isActive) {
      //          onUpdate(res.first)
      //          testing = false
      //        }
      //      }
      //    }
    )
  }
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

@Composable
private fun NewServerLayout(
  userServers: MutableState<List<UserOperatorServers>>,
  serverErrors: MutableState<List<UserServersError>>,
  operatorIndex: Int,
  rhId: Long?,
  server: MutableState<UserServer>,
  testing: MutableState<Boolean>
) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.smp_servers_new_server))
    CustomServer(userServers, serverErrors, operatorIndex, rhId, server, testing)
    SectionBottomSpacer()
  }
}

@Composable
private fun CustomServer(
  userServers: MutableState<List<UserOperatorServers>>,
  serverErrors: MutableState<List<UserServersError>>,
  operatorIndex: Int,
  rhId: Long?,
  server: MutableState<UserServer>,
  testing: MutableState<Boolean>
) {
  val serverAddress = remember { mutableStateOf(server.value.server) }
  val valid = remember {
    derivedStateOf {
      with(parseServerAddress(serverAddress.value)) {
        this?.valid == true
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
          testedPreviously[server.value.server] = server.value.tested
          server.value = server.value.copy(server = it, tested = testedPreviously[serverAddress.value])
        }
    }
  }
  SectionDividerSpaced(maxTopPadding = true)
  // UseServerSection(valid.value, testing, server, testServer, onUpdate, onDelete)

  if (valid.value) {
    SectionDividerSpaced()
    SectionView(stringResource(MR.strings.smp_servers_add_to_another_device).uppercase()) {
      QRCode(serverAddress.value)
    }
  }
}

fun serverProtocolAndOperator(
  server: UserServer,
  userServers: List<UserOperatorServers>
): Pair<ServerProtocol, ServerOperator?>? {
  val serverAddress = parseServerAddress(server.server)
  return if (serverAddress != null) {
    val serverProtocol = serverAddress.serverProtocol
    val hostnames = serverAddress.hostnames
    val matchingOperator = userServers.mapNotNull { it.operator }.firstOrNull { op ->
      op.serverDomains.any { domain ->
        hostnames.any { hostname ->
          hostname.endsWith(domain)
        }
      }
    }
    Pair(serverProtocol, matchingOperator)
  } else {
    null
  }
}

fun addServer(
  server: UserServer,
  userServers: MutableState<List<UserOperatorServers>>,
  serverErrors: MutableState<List<UserServersError>>,
  rhId: Long?,
  close: () -> Unit
) {
  val result = serverProtocolAndOperator(server, userServers.value)
  if (result != null) {
    val (serverProtocol, matchingOperator) = result
    val operatorIndex = userServers.value.indexOfFirst { it.operator?.operatorId == matchingOperator?.operatorId }
    if (operatorIndex != -1) {
      // Create a mutable copy of the userServers list
      val updatedUserServers = userServers.value.toMutableList()
      val operatorServers = updatedUserServers[operatorIndex]

      // Create a mutable copy of the smpServers or xftpServers and add the server
      when (serverProtocol) {
        ServerProtocol.SMP -> {
          val updatedSMPServers = operatorServers.smpServers.toMutableList()
          updatedSMPServers.add(server)
          updatedUserServers[operatorIndex] = operatorServers.copy(smpServers = updatedSMPServers)
        }
        ServerProtocol.XFTP -> {
          val updatedXFTPServers = operatorServers.xftpServers.toMutableList()
          updatedXFTPServers.add(server)
          updatedUserServers[operatorIndex] = operatorServers.copy(xftpServers = updatedXFTPServers)
        }
      }

      userServers.value = updatedUserServers
      // TODO
      // validateServers(rhId, userServers, serverErrors)
      close()
      matchingOperator?.let { op ->
        AlertManager.shared.showAlertMsg(
          title = "Operator server",
          text = "Server added to operator ${op.tradeName}."
        )
      }
    } else { // Shouldn't happen
      close()
      AlertManager.shared.showAlertMsg(title = "Error adding server")
    }
  } else {
    close()
    if (server.server.trim().isNotEmpty()) {
      AlertManager.shared.showAlertMsg(
        title = "Invalid server address!",
        text = "Check server address and try again."
      )
    }
  }
}

//@Composable
//private fun UseServerSection(
//  valid: Boolean,
//  testing: MutableState<Boolean>,
//  server: UserServer
//) {
//  SectionView(stringResource(MR.strings.smp_servers_use_server).uppercase()) {
//    SectionItemViewSpaceBetween(testServer, disabled = !valid || testing.value) {
//      Text(stringResource(MR.strings.smp_servers_test_server), color = if (valid && !testing.value) MaterialTheme.colors.onBackground else MaterialTheme.colors.secondary)
//      ShowTestStatus(server)
//    }
//
//    val enabled = rememberUpdatedState(server.enabled)
//    PreferenceToggle(
//      stringResource(MR.strings.smp_servers_use_server_for_new_conn),
//      disabled = server.tested != true && !server.preset,
//      checked = enabled.value
//    ) {
//      onUpdate(server.copy(enabled = it))
//    }
//
//    SectionItemView(onDelete, disabled = testing) {
//      Text(stringResource(MR.strings.smp_servers_delete_server), color = if (testing) MaterialTheme.colors.secondary else MaterialTheme.colors.error)
//    }
//  }
//}
