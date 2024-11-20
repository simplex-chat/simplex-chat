package chat.simplex.common.views.usersettings.networkAndServers

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ServerAddress.Companion.parseServerAddress
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
fun ModalData.YourServersView(userServers: MutableState<List<UserOperatorServers>>, operatorIndex: Int) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.your_servers))
  }
}

@Composable
fun ProtocolServerViewLink(
  userServers: MutableState<List<UserOperatorServers>>,
  serverErrors: MutableState<List<UserServersError>>,
  server: UserServer,
  serverProtocol: ServerProtocol,
  duplicateHosts: Set<String>,
  rhId: Long?,
) {
  val address = parseServerAddress(server.server)
    when {
      address == null || !address.valid || address.serverProtocol != serverProtocol || address.hostnames.any { it in duplicateHosts } -> InvalidServer()
      !server.enabled -> Icon(painterResource(MR.images.ic_do_not_disturb_on), null, tint = MaterialTheme.colors.secondary)
      else -> ShowTestStatus(server)
    }
    Spacer(Modifier.padding(horizontal = 4.dp))
    val text = address?.hostnames?.firstOrNull() ?: server.server
    if (server.enabled) {
      Text(text, color = MaterialTheme.colors.onBackground, maxLines = 1)
    } else {
      Text(text, maxLines = 1, color = MaterialTheme.colors.secondary)
    }
}

@Composable
private fun InvalidServer() {
  Icon(
    painterResource(MR.images.ic_error),
    contentDescription = stringResource(MR.strings.server_error),
    tint = Color.Red,
    modifier = Modifier
      .size(19.sp.toDp())
      .offset(x = 2.sp.toDp())
  )
}