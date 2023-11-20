package chat.simplex.common.views.newchat

import androidx.compose.runtime.*
import chat.simplex.common.model.ChatModel

@Composable
actual fun ConnectViaLinkView(m: ChatModel, rhId: Long?, close: () -> Unit) {
  // TODO this should close if remote host changes in model
  PasteToConnectView(m, rhId, close)
}
