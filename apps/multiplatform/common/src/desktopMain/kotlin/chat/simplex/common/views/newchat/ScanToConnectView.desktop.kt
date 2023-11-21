package chat.simplex.common.views.newchat

import androidx.compose.runtime.Composable
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.RemoteHostInfo

@Composable
actual fun ScanToConnectView(chatModel: ChatModel, rh: RemoteHostInfo?, close: () -> Unit) {
  ConnectContactLayout(
    chatModel = chatModel,
    rh = rh,
    incognitoPref = chatModel.controller.appPrefs.incognito,
    close = close
  )
}
