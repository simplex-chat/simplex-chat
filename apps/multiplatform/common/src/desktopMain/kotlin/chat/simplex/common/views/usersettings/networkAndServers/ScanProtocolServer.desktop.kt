package chat.simplex.common.views.usersettings.networkAndServers

import androidx.compose.runtime.Composable
import chat.simplex.common.model.UserServer

@Composable
actual fun ScanProtocolServer(rhId: Long?, close: () -> Unit, onNext: (UserServer) -> Unit) {
  ScanProtocolServerLayout(rhId, close, onNext)
}
