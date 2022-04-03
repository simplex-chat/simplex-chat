package chat.simplex.app.views.chat

import android.content.res.Configuration
import android.util.Log
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.ArrowBackIos
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.mapSaver
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.TAG
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.item.ChatItemView
import chat.simplex.app.views.chatlist.openChat
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
      openDirectChat = { contactId ->
        val c = chatModel.chats.firstOrNull {
          it.chatInfo is ChatInfo.Direct && it.chatInfo.contact.contactId == contactId
        }
        if (c != null) withApi { openChat(chatModel, c.chatInfo) }
      },
      sendMessage = { msg ->
        withApi {
          // show "in progress"
          val cInfo = chat.chatInfo
          val ei = editingItem.value
          if (ei != null) {
            val updatedItem = chatModel.controller.apiUpdateChatItem(
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
      resetMessage = { msg.value = "" },
      deleteMessage = { itemId, mode ->
        withApi {
          val cInfo = chat.chatInfo
          val toItem = chatModel.controller.apiDeleteChatItem(
            type = cInfo.chatType,
            id = cInfo.apiId,
            itemId = itemId,
            mode = mode
          )
          if (toItem != null) chatModel.removeChatItem(cInfo, toItem.chatItem)
        }
      }
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
  openDirectChat: (Long) -> Unit,
  sendMessage: (String) -> Unit,
  resetMessage: () -> Unit,
  deleteMessage: (Long, CIDeleteMode) -> Unit
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
          ChatItemsList(user, chat, chatItems, msg, quotedItem, editingItem, openDirectChat, deleteMessage)
        }
      }
    }
  }
}

@Composable
fun ChatInfoToolbar(chat: Chat, back: () -> Unit, info: () -> Unit) {
  Column {
    Box(
      Modifier
        .fillMaxWidth()
        .height(52.dp)
        .background(if (isSystemInDarkTheme()) ToolbarDark else ToolbarLight)
        .padding(horizontal = 8.dp),
      contentAlignment = Alignment.CenterStart,
    ) {
      IconButton(onClick = back) {
        Icon(
          Icons.Outlined.ArrowBackIos,
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
            cInfo.displayName, fontWeight = FontWeight.SemiBold,
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
    Divider()
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
  chat: Chat,
  chatItems: List<ChatItem>,
  msg: MutableState<String>,
  quotedItem: MutableState<ChatItem?>,
  editingItem: MutableState<ChatItem?>,
  openDirectChat: (Long) -> Unit,
  deleteMessage: (Long, CIDeleteMode) -> Unit
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
    itemsIndexed(chatItems) { i, cItem ->
      if (i == 0) {
        Spacer(Modifier.size(8.dp))
      }
      if (chat.chatInfo is ChatInfo.Group) {
        if (cItem.chatDir is CIDirection.GroupRcv) {
          val prevItem = if (i > 0) chatItems[i - 1] else null
          val member = cItem.chatDir.groupMember
          val showMember = showMemberImage(member, prevItem)
          Row(Modifier.padding(start = 8.dp, end = 66.dp)) {
            if (showMember) {
              val contactId = member.memberContactId
              if (contactId == null) {
                MemberImage(member)
              } else {
                Box(
                  Modifier
                    .clip(CircleShape)
                    .clickable { openDirectChat(contactId) }
                ) {
                  MemberImage(member)
                }
              }
              Spacer(Modifier.size(4.dp))
            } else {
              Spacer(Modifier.size(42.dp))
            }
            ChatItemView(user, cItem, msg, quotedItem, editingItem, cxt, uriHandler, showMember = showMember, deleteMessage = deleteMessage)
          }
        } else {
          Box(Modifier.padding(start = 86.dp, end = 12.dp)) {
            ChatItemView(user, cItem, msg, quotedItem, editingItem, cxt, uriHandler, deleteMessage = deleteMessage)
          }
        }
      } else { // direct message
        val sent = cItem.chatDir.sent
        Box(
          Modifier.padding(
            start = if (sent) 76.dp else 12.dp,
            end = if (sent) 12.dp else 76.dp,
          )
        ) {
          ChatItemView(user, cItem, msg, quotedItem, editingItem, cxt, uriHandler, deleteMessage = deleteMessage)
        }
      }
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

fun showMemberImage(member: GroupMember, prevItem: ChatItem?): Boolean {
  return prevItem == null || prevItem.chatDir is CIDirection.GroupSnd ||
      (prevItem.chatDir is CIDirection.GroupRcv && prevItem.chatDir.groupMember.groupMemberId != member.groupMemberId)
}

@Composable
fun MemberImage(member: GroupMember) {
  ProfileImage(38.dp, member.memberProfile.image)
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
      ChatItem.getDeletedContentSampleData(3),
      ChatItem.getSampleData(
        4, CIDirection.DirectSnd(), Clock.System.now(), "hello"
      ),
      ChatItem.getSampleData(
        5, CIDirection.DirectSnd(), Clock.System.now(), "hello"
      ),
      ChatItem.getSampleData(
        6, CIDirection.DirectRcv(), Clock.System.now(), "hello"
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
      openDirectChat = {},
      sendMessage = {},
      resetMessage = {},
      deleteMessage = { _, _ -> }
    )
  }
}

@Preview(showBackground = true)
@Composable
fun PreviewGroupChatLayout() {
  SimpleXTheme {
    val chatItems = listOf(
      ChatItem.getSampleData(
        1, CIDirection.GroupSnd(), Clock.System.now(), "hello"
      ),
      ChatItem.getSampleData(
        2, CIDirection.GroupRcv(GroupMember.sampleData), Clock.System.now(), "hello"
      ),
      ChatItem.getDeletedContentSampleData(3),
      ChatItem.getSampleData(
        4, CIDirection.GroupRcv(GroupMember.sampleData), Clock.System.now(), "hello"
      ),
      ChatItem.getSampleData(
        5, CIDirection.GroupSnd(), Clock.System.now(), "hello"
      ),
      ChatItem.getSampleData(
        6, CIDirection.GroupRcv(GroupMember.sampleData), Clock.System.now(), "hello"
      )
    )
    ChatLayout(
      user = User.sampleData,
      chat = Chat(
        chatInfo = ChatInfo.Group.sampleData,
        chatItems = chatItems,
        chatStats = Chat.ChatStats()
      ),
      chatItems = chatItems,
      msg = remember { mutableStateOf("") },
      quotedItem = remember { mutableStateOf(null) },
      editingItem = remember { mutableStateOf(null) },
      back = {},
      info = {},
      openDirectChat = {},
      sendMessage = {},
      resetMessage = {},
      deleteMessage = { _, _ -> }
    )
  }
}
