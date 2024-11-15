package chat.simplex.common.views.usersettings

import androidx.compose.runtime.Composable
import chat.simplex.common.model.UserServer

@Composable
actual fun ScanProtocolServer(rhId: Long?, onNext: (UserServer) -> Unit) {
  ScanProtocolServerLayout(rhId, onNext)
}
