package chat.simplex.common.views.database

import androidx.compose.runtime.mutableStateOf
import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.helpers.withBGApi
import kotlinx.coroutines.delay
import kotlinx.datetime.Instant

actual fun restartChatOrApp() {
  if (chatModel.chatRunning.value == false) {
    chatModel.chatDbChanged.value = true
    startChat(chatModel, mutableStateOf(Instant.DISTANT_PAST), chatModel.chatDbChanged)
  } else {
    authStopChat(chatModel) {
      withBGApi {
        // adding delay in order to prevent locked database by previous initialization
        delay(1000)
        chatModel.chatDbChanged.value = true
        startChat(chatModel, mutableStateOf(Instant.DISTANT_PAST), chatModel.chatDbChanged)
      }
    }
  }
}
