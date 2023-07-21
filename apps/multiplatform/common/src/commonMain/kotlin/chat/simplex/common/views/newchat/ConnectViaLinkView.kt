package chat.simplex.common.views.newchat

import androidx.compose.runtime.*
import chat.simplex.common.model.ChatModel

enum class ConnectViaLinkTab {
  SCAN, PASTE
}

@Composable
expect fun ConnectViaLinkView(m: ChatModel, close: () -> Unit)
