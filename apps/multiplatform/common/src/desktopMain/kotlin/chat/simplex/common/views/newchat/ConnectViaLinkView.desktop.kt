package chat.simplex.common.views.newchat

import androidx.compose.runtime.*
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.RemoteHostInfo

@Composable
actual fun ConnectViaLinkView(m: ChatModel, rh: RemoteHostInfo?, close: () -> Unit) {
  // TODO this should close if remote host changes in model
  PasteToConnectView(m, rh, close)
}
