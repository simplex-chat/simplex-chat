package chat.simplex.app.views.chat

import android.content.res.Configuration
import android.util.Log
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.ArrowBack
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chat.item.ChatItemView
import chat.simplex.app.views.helpers.ChatInfoImage
import chat.simplex.app.views.helpers.withApi
import chat.simplex.app.views.newchat.ModalManager
import com.google.accompanist.insets.ProvideWindowInsets
import com.google.accompanist.insets.navigationBarsWithImePadding
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.datetime.Clock

@Composable
fun ChatView(chatModel: ChatModel) {
  val chat: Chat? = chatModel.chats.firstOrNull { chat -> chat.chatInfo.id == chatModel.chatId.value }
  if (chat == null) {
    chatModel.chatId.value = null
  } else {
    BackHandler { chatModel.chatId.value = null }
    // TODO a more advanced version would mark as read only if in view
    LaunchedEffect(chat.chatItems) {
      Log.d("SIMPLEX", "ChatView ${chatModel.chatId.value}: LaunchedEffect")
      delay(1000L)
      if (chat.chatItems.count() > 0) {
        chatModel.markChatItemsRead(chat.chatInfo)
        withApi {
          chatModel.controller.apiChatRead(
            chat.chatInfo.chatType,
            chat.chatInfo.apiId,
            CC.ItemRange(chat.chatStats.minUnreadItemId, chat.chatItems.last().id)
          )
        }
      }
    }
    ChatLayout(chat, chatModel.chatItems,
      back = { chatModel.chatId.value = null },
      info = { ModalManager.shared.showCustomModal { close -> ChatInfoView(chatModel, close) } },
      sendMessage = { msg ->
        withApi {
          // show "in progress"
          val cInfo = chat.chatInfo
          val newItem = chatModel.controller.apiSendMessage(
            type = cInfo.chatType,
            id = cInfo.apiId,
            mc = MsgContent.MCText(msg)
          )
          // hide "in progress"
          if (newItem != null) chatModel.addChatItem(cInfo, newItem.chatItem)
        }
      }
    )
  }
}

@Composable
fun ChatLayout(
  chat: Chat, chatItems: List<ChatItem>,
  back: () -> Unit,
  info: () -> Unit,
  sendMessage: (String) -> Unit
) {
  ProvideWindowInsets(windowInsetsAnimationsEnabled = true) {
    Scaffold(
      topBar = { ChatInfoToolbar(chat, back, info) },
      bottomBar = { SendMsgView(sendMessage) },
      modifier = Modifier.navigationBarsWithImePadding()
    ) { contentPadding ->
      Surface(
        modifier = Modifier
          .fillMaxWidth()
          .background(MaterialTheme.colors.background)
      ) {
        ChatItemsList(chatItems)
      }
    }
  }
}

@Composable
fun ChatInfoToolbar(chat: Chat, back: () -> Unit, info: () -> Unit) {
  Box(
    Modifier
      .height(60.dp)
      .padding(horizontal = 8.dp),
    contentAlignment = Alignment.CenterStart
  ) {
    IconButton(onClick = back) {
      Icon(
        Icons.Outlined.ArrowBack,
        "Back",
        tint = MaterialTheme.colors.primary,
        modifier = Modifier.padding(10.dp)
      )
    }
    Row(
      Modifier
        .padding(horizontal = 68.dp)
        .fillMaxWidth()
        .clickable(onClick = info),
      horizontalArrangement = Arrangement.Center,
      verticalAlignment = Alignment.CenterVertically
    ) {
      val cInfo = chat.chatInfo
      ChatInfoImage(chat, size = 40.dp)
      Column(Modifier.padding(start = 8.dp),
        horizontalAlignment = Alignment.CenterHorizontally
      ) {
        Text(cInfo.displayName, fontWeight = FontWeight.Bold,
          maxLines = 1, overflow = TextOverflow.Ellipsis)
        if (cInfo.fullName != "" && cInfo.fullName != cInfo.displayName) {
          Text(cInfo.fullName,
            maxLines = 1, overflow = TextOverflow.Ellipsis)
        }
      }
    }
  }
}

@Composable
fun ChatItemsList(chatItems: List<ChatItem>) {
  val listState = rememberLazyListState()
  val scope = rememberCoroutineScope()
  val uriHandler = LocalUriHandler.current
  LazyColumn(state = listState) {
    items(chatItems) { cItem ->
      ChatItemView(cItem, uriHandler)
    }
    val len = chatItems.count()
    if (len > 1) {
      scope.launch {
        listState.animateScrollToItem(len - 1)
      }
    }
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewChatLayout() {
  SimpleXTheme {
    val chatItems = listOf(
      ChatItem.getSampleData(
        1, CIDirection.DirectSnd(), Clock.System.now(), "hello"
      ),
      ChatItem.getSampleData(
        2, CIDirection.DirectRcv(), Clock.System.now(), "hello"
      ),
      ChatItem.getSampleData(
        3, CIDirection.DirectSnd(), Clock.System.now(), "hello"
      ),
      ChatItem.getSampleData(
        4, CIDirection.DirectSnd(), Clock.System.now(), "hello"
      ),
      ChatItem.getSampleData(
        5, CIDirection.DirectRcv(), Clock.System.now(), "hello"
      )
    )
    ChatLayout(
      chat = Chat(
        chatInfo = ChatInfo.Direct.sampleData,
        chatItems = chatItems,
        chatStats = Chat.ChatStats()
      ),
      chatItems = chatItems,
      back = {},
      info = {},
      sendMessage = {}
    )
  }
}
