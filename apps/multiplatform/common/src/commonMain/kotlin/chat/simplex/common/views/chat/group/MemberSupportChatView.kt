package chat.simplex.common.views.chat.group

import androidx.compose.runtime.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.chatlist.*
import chat.simplex.common.views.helpers.*

@Composable
private fun MemberSupportChatView(memberSupportChatsCtx: ChatModel.ChatsContext, staleChatId: State<String?>, scrollToItemId: MutableState<Long?>) {
  ChatView(memberSupportChatsCtx, staleChatId, scrollToItemId, onComposed = {})
}

suspend fun showMemberSupportChatView(staleChatId: State<String?>, scrollToItemId: MutableState<Long?>, chatInfo: ChatInfo) {
  val memberSupportChatsCtx = ChatModel.ChatsContext(contentTag = null) // TODO [knocking] sum type for group scope / content tag
  openChat(secondaryChatsCtx = memberSupportChatsCtx, chatModel.remoteHostId(), chatInfo)
  ModalManager.end.showCustomModal(true, id = ModalViewId.SECONDARY_CHAT) { close ->
    ModalView({}, showAppBar = false) {
      if (chatInfo is ChatInfo.Group && chatInfo.groupChatScope != null) {
        MemberSupportChatView(memberSupportChatsCtx, staleChatId, scrollToItemId)
      } else {
        LaunchedEffect(Unit) {
          close()
        }
      }
    }
  }
}
