package chat.simplex.common.views.chat.group

import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.chatlist.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.flow.*

@Composable
private fun GroupReportsView(reportsChatsCtx: ChatModel.ChatsContext, staleChatId: State<String?>, scrollToItemId: MutableState<Long?>) {
  ChatView(reportsChatsCtx, staleChatId, scrollToItemId, onComposed = {})
}

@Composable
fun GroupReportsAppBar(
  chatsCtx: ChatModel.ChatsContext,
  close: () -> Unit,
  onSearchValueChanged: (String) -> Unit
) {
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  val showSearch = rememberSaveable { mutableStateOf(false) }
  val onBackClicked = {
    if (!showSearch.value) {
      close()
    } else {
      onSearchValueChanged("")
      showSearch.value = false
    }
  }
  BackHandler(onBack = onBackClicked)
  DefaultAppBar(
    navigationButton = { NavigationButtonBack(onBackClicked) },
    fixedTitleText = stringResource(MR.strings.group_reports_member_reports),
    onTitleClick = null,
    onTop = !oneHandUI.value,
    showSearch = showSearch.value,
    onSearchValueChanged = onSearchValueChanged,
    buttons = {
      IconButton({ showSearch.value = true }) {
        Icon(painterResource(MR.images.ic_search), stringResource(MR.strings.search_verb), tint = MaterialTheme.colors.primary)
      }
    }
  )
  ItemsReload(chatsCtx)
}

@Composable
private fun ItemsReload(chatsCtx: ChatModel.ChatsContext,) {
  LaunchedEffect(Unit) {
    snapshotFlow { chatModel.chatId.value }
      .distinctUntilChanged()
      .drop(1)
      .filterNotNull()
      .map { chatModel.getChat(it) }
      .filterNotNull()
      .filter { it.chatInfo is ChatInfo.Group }
      .collect { chat ->
        reloadItems(chatsCtx, chat)
      }
  }
}

suspend fun showGroupReportsView(staleChatId: State<String?>, scrollToItemId: MutableState<Long?>, chatInfo: ChatInfo) {
  val reportsChatsCtx = ChatModel.ChatsContext(contentTag = MsgContentTag.Report)
  openChat(secondaryChatsCtx = reportsChatsCtx, chatModel.remoteHostId(), chatInfo)
  ModalManager.end.showCustomModal(true, id = ModalViewId.SECONDARY_CHAT) { close ->
    ModalView({}, showAppBar = false) {
      val chatInfo = remember { derivedStateOf { chatModel.chats.value.firstOrNull { it.id == chatModel.chatId.value }?.chatInfo } }.value
      if (chatInfo is ChatInfo.Group && chatInfo.groupInfo.canModerate) {
        GroupReportsView(reportsChatsCtx, staleChatId, scrollToItemId)
      } else {
        LaunchedEffect(Unit) {
          close()
        }
      }
    }
  }
}

private suspend fun reloadItems(chatsCtx: ChatModel.ChatsContext, chat: Chat) {
  apiLoadMessages(chatsCtx, chat.remoteHostId, chat.chatInfo.chatType, chat.chatInfo.apiId, ChatPagination.Initial(ChatPagination.INITIAL_COUNT))
}
