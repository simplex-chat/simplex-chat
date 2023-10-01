package chat.simplex.common.views.newchat

import androidx.compose.runtime.*
import chat.simplex.common.model.ChatModel

@Composable
actual fun ConnectViaLinkView(m: ChatModel, close: () -> Unit) {
  PasteToConnectView(m, close)
}
