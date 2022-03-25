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
import androidx.compose.runtime.saveable.mapSaver
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.TAG
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chat.item.ChatItemView
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.newchat.ModalManager
import com.google.accompanist.insets.ProvideWindowInsets
import com.google.accompanist.insets.navigationBarsWithImePadding
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.datetime.Clock

@Composable
fun ChatView(chatModel: ChatModel) {
  val chat: Chat? = chatModel.chats.firstOrNull { chat -> chat.chatInfo.id == chatModel.chatId.value }
  val user = chatModel.currentUser.value
  if (chat == null || user == null) {
    chatModel.chatId.value = null
  } else {
    val quotedItem = remember { mutableStateOf<ChatItem?>(null) }
    val editingItem = remember { mutableStateOf<ChatItem?>(null) }
    var msg = remember { mutableStateOf("") }
    BackHandler { chatModel.chatId.value = null }
    // TODO a more advanced version would mark as read only if in view
    LaunchedEffect(chat.chatItems) {
      Log.d(TAG, "ChatView ${chatModel.chatId.value}: LaunchedEffect")
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
    ChatLayout(user, chat, chatModel.chatItems, msg, quotedItem, editingItem,
      back = { chatModel.chatId.value = null },
      info = { ModalManager.shared.showCustomModal { close -> ChatInfoView(chatModel, close) } },
      sendMessage = { msg ->
        withApi {
          // show "in progress"
          val cInfo = chat.chatInfo
          val ei = editingItem.value
          if (ei != null) {
            val updatedItem = chatModel.controller.apiUpdateMessage(
              type = cInfo.chatType,
              id = cInfo.apiId,
              itemId = ei.meta.itemId,
              mc = MsgContent.MCText(msg)
            )
            if (updatedItem != null) chatModel.upsertChatItem(cInfo, updatedItem.chatItem)
          } else {
            val newItem = chatModel.controller.apiSendMessage(
              type = cInfo.chatType,
              id = cInfo.apiId,
              quotedItemId = quotedItem.value?.meta?.itemId,
              mc = MsgContent.MCText(msg)
            )
            if (newItem != null) chatModel.addChatItem(cInfo, newItem.chatItem)
          }
          // hide "in progress"
          editingItem.value = null
          quotedItem.value = null
        }
      },
      resetMessage = { msg.value = "" }
    )
  }
}

@Composable
fun ChatLayout(
  user: User,
  chat: Chat,
  chatItems: List<ChatItem>,
  msg: MutableState<String>,
  quotedItem: MutableState<ChatItem?>,
  editingItem: MutableState<ChatItem?>,
  back: () -> Unit,
  info: () -> Unit,
  sendMessage: (String) -> Unit,
  resetMessage: () -> Unit
) {
  Surface(
    Modifier
      .fillMaxWidth()
      .background(MaterialTheme.colors.background)
  ) {
    ProvideWindowInsets(windowInsetsAnimationsEnabled = true) {
      Scaffold(
        topBar = { ChatInfoToolbar(chat, back, info) },
        bottomBar = { ComposeView(msg, quotedItem, editingItem, sendMessage, resetMessage) },
        modifier = Modifier.navigationBarsWithImePadding()
      ) { contentPadding ->
        Box(Modifier.padding(contentPadding)) {
          ChatItemsList(user, chatItems, msg, quotedItem, editingItem)
        }
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
      Column(
        Modifier.padding(start = 8.dp),
        horizontalAlignment = Alignment.CenterHorizontally
      ) {
        Text(
          cInfo.displayName, fontWeight = FontWeight.Bold,
          maxLines = 1, overflow = TextOverflow.Ellipsis
        )
        if (cInfo.fullName != "" && cInfo.fullName != cInfo.displayName) {
          Text(
            cInfo.fullName,
            maxLines = 1, overflow = TextOverflow.Ellipsis
          )
        }
      }
    }
  }
}

data class CIListState(val scrolled: Boolean, val itemCount: Int, val keyboardState: KeyboardState)

val CIListStateSaver = run {
  val scrolledKey = "scrolled"
  val countKey = "itemCount"
  val keyboardKey = "keyboardState"
  mapSaver(
    save = { mapOf(scrolledKey to it.scrolled, countKey to it.itemCount, keyboardKey to it.keyboardState) },
    restore = { CIListState(it[scrolledKey] as Boolean, it[countKey] as Int, it[keyboardKey] as KeyboardState) }
  )
}

@Composable
fun ChatItemsList(
  user: User,
  chatItems: List<ChatItem>,
  msg: MutableState<String>,
  quotedItem: MutableState<ChatItem?>,
  editingItem: MutableState<ChatItem?>
) {
  val listState = rememberLazyListState()
  val keyboardState by getKeyboardState()
  val ciListState = rememberSaveable(stateSaver = CIListStateSaver) {
    mutableStateOf(CIListState(false, chatItems.count(), keyboardState))
  }
  val scope = rememberCoroutineScope()
  val uriHandler = LocalUriHandler.current
  val cxt = LocalContext.current
  LazyColumn(state = listState) {
    items(chatItems) { cItem ->
      ChatItemView(user, cItem, msg, quotedItem, editingItem, cxt, uriHandler)
    }
    val len = chatItems.count()
    if (len > 1 && (keyboardState != ciListState.value.keyboardState || !ciListState.value.scrolled || len != ciListState.value.itemCount)) {
      scope.launch {
        ciListState.value = CIListState(true, len, keyboardState)
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
      user = User.sampleData,
      chat = Chat(
        chatInfo = ChatInfo.Direct.sampleData,
        chatItems = chatItems,
        chatStats = Chat.ChatStats()
      ),
      chatItems = chatItems,
      msg = remember { mutableStateOf("") },
      quotedItem = remember { mutableStateOf(null) },
      editingItem = remember { mutableStateOf(null) },
      back = {},
      info = {},
      sendMessage = {},
      resetMessage = {}
    )
  }
}
