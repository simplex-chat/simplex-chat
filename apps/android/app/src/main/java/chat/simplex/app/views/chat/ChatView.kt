package chat.simplex.app.views.chat

import android.content.res.Configuration
import android.util.Log
import androidx.activity.compose.BackHandler
import androidx.annotation.StringRes
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.mapSaver
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.TAG
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.call.*
import chat.simplex.app.views.chat.item.ChatItemView
import chat.simplex.app.views.chatlist.openChat
import chat.simplex.app.views.helpers.*
import com.google.accompanist.insets.ProvideWindowInsets
import com.google.accompanist.insets.navigationBarsWithImePadding
import kotlinx.coroutines.*
import kotlinx.datetime.Clock

@Composable
fun ChatView(chatModel: ChatModel) {
  val chat: Chat? = chatModel.chats.firstOrNull { chat -> chat.chatInfo.id == chatModel.chatId.value }
  val user = chatModel.currentUser.value
  val composeState = remember { mutableStateOf(ComposeState()) }
  val attachmentOption = remember { mutableStateOf<AttachmentOption?>(null) }
  val attachmentBottomSheetState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden)
  val scope = rememberCoroutineScope()

  if (chat == null || user == null) {
    chatModel.chatId.value = null
  } else {
    BackHandler { chatModel.chatId.value = null }
    // TODO a more advanced version would mark as read only if in view
    LaunchedEffect(chat.chatItems) {
      Log.d(TAG, "ChatView ${chatModel.chatId.value}: LaunchedEffect")
      delay(1000L)
      if (chat.chatItems.count() > 0) {
        chatModel.markChatItemsRead(chat.chatInfo)
        chatModel.controller.cancelNotificationsForChat(chat.id)
        withApi {
          chatModel.controller.apiChatRead(
            chat.chatInfo.chatType,
            chat.chatInfo.apiId,
            CC.ItemRange(chat.chatStats.minUnreadItemId, chat.chatItems.last().id)
          )
        }
      }
    }
    ChatLayout(
      user,
      chat,
      composeState,
      composeView = {
        ComposeView(
          chatModel, chat, composeState, attachmentOption,
          showChooseAttachment = { scope.launch { attachmentBottomSheetState.show() } }
        )
      },
      attachmentOption,
      scope,
      attachmentBottomSheetState,
      chatModel.chatItems,
      back = { chatModel.chatId.value = null },
      info = { ModalManager.shared.showCustomModal { close -> ChatInfoView(chatModel, close) } },
      openDirectChat = { contactId ->
        val c = chatModel.chats.firstOrNull {
          it.chatInfo is ChatInfo.Direct && it.chatInfo.contact.contactId == contactId
        }
        if (c != null) withApi { openChat(c.chatInfo, chatModel) }
      },
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
      },
      receiveFile = { fileId ->
        withApi {
          val chatItem = chatModel.controller.apiReceiveFile(fileId)
          if (chatItem != null) {
            val cInfo = chatItem.chatInfo
            val cItem = chatItem.chatItem
            chatModel.upsertChatItem(cInfo, cItem)
          }
        }
      },
      startCall = { media ->
        val cInfo = chat.chatInfo
        if (cInfo is ChatInfo.Direct) {
          chatModel.activeCall.value = Call(contact = cInfo.contact, callState = CallState.WaitCapabilities, localMedia = media)
          chatModel.showCallView.value = true
          chatModel.callCommand.value = WCallCommand.Capabilities
        }
      },
      acceptCall = { contact ->
        val invitation = chatModel.callInvitations.remove(contact.id)
        if (invitation == null) {
          AlertManager.shared.showAlertMsg("Call already ended!")
        } else {
          chatModel.activeCallInvitation.value = null
          chatModel.activeCall.value = Call(
            contact = contact,
            callState = CallState.InvitationReceived,
            localMedia = invitation.peerMedia,
            sharedKey = invitation.sharedKey
          )
          chatModel.showCallView.value = true
          chatModel.callCommand.value = WCallCommand.Start(media = invitation.peerMedia, aesKey = invitation.sharedKey)
        }
      }
    )
  }
}

@Composable
fun ChatLayout(
  user: User,
  chat: Chat,
  composeState: MutableState<ComposeState>,
  composeView: (@Composable () -> Unit),
  attachmentOption: MutableState<AttachmentOption?>,
  scope: CoroutineScope,
  attachmentBottomSheetState: ModalBottomSheetState,
  chatItems: List<ChatItem>,
  back: () -> Unit,
  info: () -> Unit,
  openDirectChat: (Long) -> Unit,
  deleteMessage: (Long, CIDeleteMode) -> Unit,
  receiveFile: (Long) -> Unit,
  startCall: (CallMediaType) -> Unit,
  acceptCall: (Contact) -> Unit
) {
  Surface(
    Modifier
      .fillMaxWidth()
      .background(MaterialTheme.colors.background)
  ) {
    ProvideWindowInsets(windowInsetsAnimationsEnabled = true) {
      ModalBottomSheetLayout(
        scrimColor = Color.Black.copy(alpha = 0.12F),
        modifier = Modifier.navigationBarsWithImePadding(),
        sheetContent = {
          ChooseAttachmentView(
            attachmentOption,
            hide = { scope.launch { attachmentBottomSheetState.hide() } }
          )
        },
        sheetState = attachmentBottomSheetState,
        sheetShape = RoundedCornerShape(topStart = 18.dp, topEnd = 18.dp)
      ) {
        Scaffold(
          topBar = { ChatInfoToolbar(chat, back, info, startCall) },
          bottomBar = composeView,
          modifier = Modifier.navigationBarsWithImePadding()
        ) { contentPadding ->
          Box(Modifier.padding(contentPadding)) {
            ChatItemsList(user, chat, composeState, chatItems, openDirectChat, deleteMessage, receiveFile, acceptCall)
          }
        }
      }
    }
  }
}

@Composable
fun ChatInfoToolbar(chat: Chat, back: () -> Unit, info: () -> Unit, startCall: (CallMediaType) -> Unit) {
  @Composable fun toolbarButton(icon: ImageVector, @StringRes textId: Int, modifier: Modifier = Modifier.padding(0.dp), onClick: () -> Unit) {
    IconButton(onClick, modifier = modifier) {
      Icon(icon, stringResource(textId), tint = MaterialTheme.colors.primary)
    }
  }
  Column {
    Box(
      Modifier
        .fillMaxWidth()
        .height(52.dp)
        .background(if (isSystemInDarkTheme()) ToolbarDark else ToolbarLight)
        .padding(horizontal = 4.dp),
      contentAlignment = Alignment.CenterStart,
    ) {
      val cInfo = chat.chatInfo
      toolbarButton(Icons.Outlined.ArrowBackIos, R.string.back, onClick = back)
      if (cInfo is ChatInfo.Direct) {
        Box(Modifier.fillMaxWidth(), contentAlignment = Alignment.CenterEnd) {
          Box(Modifier.width(85.dp), contentAlignment = Alignment.CenterStart) {
            toolbarButton(Icons.Outlined.Phone, R.string.icon_descr_audio_call) {
              startCall(CallMediaType.Audio)
            }
          }
          toolbarButton(Icons.Outlined.Videocam, R.string.icon_descr_video_call) {
            startCall(CallMediaType.Video)
          }
        }
      }
      Row(
        Modifier
          .padding(horizontal = 80.dp)
          .fillMaxWidth()
          .clickable(onClick = info),
        horizontalArrangement = Arrangement.Center,
        verticalAlignment = Alignment.CenterVertically
      ) {
        ChatInfoImage(cInfo, size = 40.dp)
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
  composeState: MutableState<ComposeState>,
  chatItems: List<ChatItem>,
  openDirectChat: (Long) -> Unit,
  deleteMessage: (Long, CIDeleteMode) -> Unit,
  receiveFile: (Long) -> Unit,
  acceptCall: (Contact) -> Unit
) {
  val listState = rememberLazyListState(initialFirstVisibleItemIndex = chatItems.size - chatItems.count { it.isRcvNew })
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
            ChatItemView(user, chat.chatInfo, cItem, composeState, cxt, uriHandler, showMember = showMember, deleteMessage = deleteMessage, receiveFile = receiveFile, acceptCall = acceptCall)
          }
        } else {
          Box(Modifier.padding(start = 86.dp, end = 12.dp)) {
            ChatItemView(user, chat.chatInfo, cItem, composeState, cxt, uriHandler, deleteMessage = deleteMessage, receiveFile = receiveFile, acceptCall = acceptCall)
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
          ChatItemView(user, chat.chatInfo, cItem, composeState, cxt, uriHandler, deleteMessage = deleteMessage, receiveFile = receiveFile, acceptCall = acceptCall)
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
      composeState = remember { mutableStateOf(ComposeState()) },
      composeView = {},
      attachmentOption = remember { mutableStateOf<AttachmentOption?>(null) },
      scope = rememberCoroutineScope(),
      attachmentBottomSheetState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden),
      chatItems = chatItems,
      back = {},
      info = {},
      openDirectChat = {},
      deleteMessage = { _, _ -> },
      receiveFile = {},
      startCall = {},
      acceptCall = { _ ->  }
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
      composeState = remember { mutableStateOf(ComposeState()) },
      composeView = {},
      attachmentOption = remember { mutableStateOf<AttachmentOption?>(null) },
      scope = rememberCoroutineScope(),
      attachmentBottomSheetState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden),
      chatItems = chatItems,
      back = {},
      info = {},
      openDirectChat = {},
      deleteMessage = { _, _ -> },
      receiveFile = {},
      startCall = {},
      acceptCall = { _ ->  }
    )
  }
}
