package chat.simplex.common.views.newchat

import androidx.compose.runtime.Composable
import chat.simplex.common.model.ChatModel

@Composable
actual fun ScanToConnectView(chatModel: ChatModel, rhId: Long?, close: () -> Unit) {
  ConnectContactLayout(
    chatModel = chatModel,
    rhId = rhId,
    incognitoPref = chatModel.controller.appPrefs.incognito,
    close = close
  )
}
