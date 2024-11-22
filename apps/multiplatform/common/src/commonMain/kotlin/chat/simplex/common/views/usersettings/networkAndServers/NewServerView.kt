package chat.simplex.common.views.usersettings.networkAndServers

import SectionBottomSpacer
import androidx.compose.foundation.layout.*
import androidx.compose.runtime.*
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.model.*
import chat.simplex.common.model.ServerAddress.Companion.parseServerAddress
import chat.simplex.common.views.helpers.*
import chat.simplex.common.platform.*
import chat.simplex.res.MR
import kotlinx.coroutines.*

@Composable
fun ModalData.NewServerView(
  userServers: MutableState<List<UserOperatorServers>>,
  serverErrors: MutableState<List<UserServersError>>,
  rhId: Long?,
  close: () -> Unit
) {
  val testing = remember { mutableStateOf(false) }
  val scope = rememberCoroutineScope()
  val newServer = remember { mutableStateOf(UserServer.empty) }

  ModalView(close = {
    addServer(
      scope,
      newServer.value,
      userServers,
      serverErrors,
      rhId,
      close = close
    )
  }) {
    Box {
      NewServerLayout(
        newServer,
        testing.value,
        testServer = {
          testing.value = true
          withLongRunningApi {
            val res = testServerConnection(newServer.value, chatModel)
            if (isActive) {
              newServer.value = res.first
              testing.value = false
            }
          }
        },
      )

      if (testing.value) {
        DefaultProgressView(null)
      }
    }
  }
}

@Composable
private fun NewServerLayout(
  server: MutableState<UserServer>,
  testing: Boolean,
  testServer: () -> Unit,
) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.smp_servers_new_server))
    CustomServer(server, testing, testServer, onDelete = null)
    SectionBottomSpacer()
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
  scope: CoroutineScope,
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
      close()
      matchingOperator?.let { op ->
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.operator_server_alert_title),
          text = String.format(generalGetString(MR.strings.server_added_to_operator__name), op.tradeName)
        )
      }
    } else { // Shouldn't happen
      close()
      AlertManager.shared.showAlertMsg(title = generalGetString(MR.strings.error_adding_server))
    }
  } else {
    close()
    if (server.server.trim().isNotEmpty()) {
      AlertManager.shared.showAlertMsg(
        title = generalGetString(MR.strings.smp_servers_invalid_address),
        text = generalGetString(MR.strings.smp_servers_check_address)
      )
    }
  }
}
