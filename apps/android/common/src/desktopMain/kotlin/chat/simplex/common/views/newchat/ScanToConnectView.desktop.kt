package chat.simplex.common.views.newchat

import androidx.compose.runtime.Composable
import chat.simplex.common.model.ChatModel

@Composable
actual fun ScanToConnectView(chatModel: ChatModel, close: () -> Unit) {
  ConnectContactLayout(
    chatModelIncognito = chatModel.incognito.value,
    close = close
  )
}