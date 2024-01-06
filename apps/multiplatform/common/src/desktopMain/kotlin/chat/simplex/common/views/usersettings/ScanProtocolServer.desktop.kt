package chat.simplex.common.views.usersettings

import androidx.compose.runtime.Composable
import chat.simplex.common.model.ServerCfg

@Composable
actual fun ScanProtocolServer(rhId: Long?, onNext: (ServerCfg) -> Unit) {
  ScanProtocolServerLayout(rhId, onNext)
}
