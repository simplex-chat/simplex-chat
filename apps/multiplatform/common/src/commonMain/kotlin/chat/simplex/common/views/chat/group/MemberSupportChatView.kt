package chat.simplex.common.views.chat.group

import androidx.compose.runtime.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.chatlist.*
import chat.simplex.common.views.helpers.*

@Composable
private fun MemberSupportChatView(staleChatId: State<String?>, scrollToItemId: MutableState<Long?>) {
  ChatView(staleChatId, contentTag = null, scrollToItemId, onComposed = {})
}

suspend fun showMemberSupportChatView(staleChatId: State<String?>, scrollToItemId: MutableState<Long?>, chatInfo: ChatInfo) {
  openChat(chatModel.remoteHostId(), chatInfo, contentTag = null)
  ModalManager.end.showCustomModal(true, id = ModalViewId.SIDE_PANEL_CHAT) { close ->
    ModalView({}, showAppBar = false) {
      if (chatInfo is ChatInfo.Group && chatInfo.groupChatScope != null) {
        MemberSupportChatView(staleChatId, scrollToItemId)
      } else {
        LaunchedEffect(Unit) {
          close()
        }
      }
    }
  }
}
