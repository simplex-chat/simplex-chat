package chat.simplex.app.views.chat

import android.content.res.Configuration
import androidx.activity.compose.BackHandler
import androidx.annotation.StringRes
import androidx.compose.foundation.*
import androidx.compose.foundation.gestures.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.ArrowDownward
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.mapSaver
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.platform.*
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.*
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.call.*
import chat.simplex.app.views.chat.group.AddGroupMembersView
import chat.simplex.app.views.chat.group.GroupChatInfoView
import chat.simplex.app.views.chat.item.ChatItemView
import chat.simplex.app.views.chatlist.*
import chat.simplex.app.views.helpers.*
import com.google.accompanist.insets.ProvideWindowInsets
import com.google.accompanist.insets.navigationBarsWithImePadding
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*
import kotlinx.datetime.Clock

@Composable
fun ChatView(chatModel: ChatModel) {
  var activeChat by remember { mutableStateOf(chatModel.chats.firstOrNull { chat -> chat.chatInfo.id == chatModel.chatId.value }) }
  val user = chatModel.currentUser.value
  val useLinkPreviews = chatModel.controller.appPrefs.privacyLinkPreviews.get()
  val composeState = remember { mutableStateOf(ComposeState(useLinkPreviews = useLinkPreviews)) }
  val attachmentOption = remember { mutableStateOf<AttachmentOption?>(null) }
  val attachmentBottomSheetState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden)
  val scope = rememberCoroutineScope()

  if (activeChat == null || user == null) {
    chatModel.chatId.value = null
  } else {
    val chat = activeChat!!
    BackHandler { chatModel.chatId.value = null }

    // We need to have real unreadCount value for displaying it inside top right button
    // Having activeChat reloaded on every change in it is inefficient (UI lags)
    val unreadCount = remember {
      derivedStateOf {
        chatModel.chats.firstOrNull { chat -> chat.chatInfo.id == chatModel.chatId.value }?.chatStats?.unreadCount ?: 0
      }
    }

    ChatLayout(
      user,
      chat,
      unreadCount,
      composeState,
      composeView = {
        if (chat.chatInfo.sendMsgEnabled) {
          ComposeView(
            chatModel, chat, composeState, attachmentOption,
            showChooseAttachment = { scope.launch { attachmentBottomSheetState.show() } }
          )
        }
      },
      attachmentOption,
      scope,
      attachmentBottomSheetState,
      chatModel.chatItems,
      useLinkPreviews = useLinkPreviews,
      back = { chatModel.chatId.value = null },
      info = {
        withApi {
          val cInfo = chat.chatInfo
          if (cInfo is ChatInfo.Direct) {
            val connStats = chatModel.controller.apiContactInfo(cInfo.apiId)
            ModalManager.shared.showCustomModal { close ->
              ModalView(
                close = close, modifier = Modifier,
                background = if (isSystemInDarkTheme()) MaterialTheme.colors.background else SettingsBackgroundLight
              ) {
                ChatInfoView(chatModel, connStats, close)
              }
            }
          } else if (cInfo is ChatInfo.Group) {
            setGroupMembers(cInfo.groupInfo, chatModel)
            ModalManager.shared.showCustomModal { close ->
              ModalView(
                close = close, modifier = Modifier,
                background = if (isSystemInDarkTheme()) MaterialTheme.colors.background else SettingsBackgroundLight
              ) {
                GroupChatInfoView(chatModel, close)
              }
            }
          }
        }
      },
      openDirectChat = { contactId ->
        val c = chatModel.chats.firstOrNull {
          it.chatInfo is ChatInfo.Direct && it.chatInfo.contact.contactId == contactId
        }
        if (c != null) {
          withApi {
            openChat(c.chatInfo, chatModel)
            // Redisplay the whole hierarchy if the chat is different to make going from groups to direct chat working correctly
            activeChat = c
          }
        }
      },
      loadPrevMessages = { cInfo ->
        val c = chatModel.getChat(cInfo.id)
        val firstId = chatModel.chatItems.firstOrNull()?.id
        if (c != null && firstId != null) {
          withApi { apiLoadPrevMessages(firstId, c.chatInfo, chatModel) }
        }
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
        withApi { chatModel.controller.receiveFile(fileId) }
      },
      joinGroup = { groupId ->
        withApi { chatModel.controller.apiJoinGroup(groupId) }
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
          chatModel.callManager.acceptIncomingCall(invitation = invitation)
        }
      },
      addMembers = { groupInfo ->
        withApi {
          setGroupMembers(groupInfo, chatModel)
          ModalManager.shared.showCustomModal { close ->
            ModalView(
              close = close, modifier = Modifier,
              background = if (isSystemInDarkTheme()) MaterialTheme.colors.background else SettingsBackgroundLight
            ) {
              AddGroupMembersView(groupInfo, chatModel, close)
            }
          }
        }
      },
      markRead = { range, unreadCountAfter ->
        chatModel.markChatItemsRead(chat.chatInfo, range, unreadCountAfter)
        chatModel.controller.ntfManager.cancelNotificationsForChat(chat.id)
        withApi {
          chatModel.controller.apiChatRead(
            chat.chatInfo.chatType,
            chat.chatInfo.apiId,
            range
          )
        }
      }
    )
  }
}

@Composable
fun ChatLayout(
  user: User,
  chat: Chat,
  unreadCount: State<Int>,
  composeState: MutableState<ComposeState>,
  composeView: (@Composable () -> Unit),
  attachmentOption: MutableState<AttachmentOption?>,
  scope: CoroutineScope,
  attachmentBottomSheetState: ModalBottomSheetState,
  chatItems: List<ChatItem>,
  useLinkPreviews: Boolean,
  back: () -> Unit,
  info: () -> Unit,
  openDirectChat: (Long) -> Unit,
  loadPrevMessages: (ChatInfo) -> Unit,
  deleteMessage: (Long, CIDeleteMode) -> Unit,
  receiveFile: (Long) -> Unit,
  joinGroup: (Long) -> Unit,
  startCall: (CallMediaType) -> Unit,
  acceptCall: (Contact) -> Unit,
  addMembers: (GroupInfo) -> Unit,
  markRead: (CC.ItemRange, unreadCountAfter: Int?) -> Unit,
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
        val floatingButton: MutableState<@Composable () -> Unit> = remember { mutableStateOf({}) }
        Scaffold(
          topBar = { ChatInfoToolbar(chat, back, info, startCall, addMembers) },
          bottomBar = composeView,
          modifier = Modifier.navigationBarsWithImePadding(),
          floatingActionButton = floatingButton.value,
        ) { contentPadding ->
          BoxWithConstraints(Modifier.padding(contentPadding)) {
            ChatItemsList(
              user, chat, unreadCount, composeState, chatItems,
              useLinkPreviews, openDirectChat, loadPrevMessages, deleteMessage,
              receiveFile, joinGroup, acceptCall, markRead, floatingButton
            )
          }
        }
      }
    }
  }
}

@Composable
fun ChatInfoToolbar(
  chat: Chat,
  back: () -> Unit,
  info: () -> Unit,
  startCall: (CallMediaType) -> Unit,
  addMembers: (GroupInfo) -> Unit
) {
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
      } else if (cInfo is ChatInfo.Group) {
        if (cInfo.groupInfo.canAddMembers) {
          Box(Modifier.fillMaxWidth(), contentAlignment = Alignment.CenterEnd) {
            toolbarButton(Icons.Outlined.PersonAdd, R.string.icon_descr_add_members) {
              addMembers(cInfo.groupInfo)
            }
          }
        }
      }
      Box(
        Modifier
          .padding(horizontal = 80.dp).fillMaxWidth()
          .clickable(onClick = info),
        contentAlignment = Alignment.Center
      ) {
        ChatInfoToolbarTitle(cInfo)
      }
    }
    Divider()
  }
}

@Composable
fun ChatInfoToolbarTitle(cInfo: ChatInfo, imageSize: Dp = 40.dp, iconColor: Color = MaterialTheme.colors.secondary) {
  Row(
    horizontalArrangement = Arrangement.Center,
    verticalAlignment = Alignment.CenterVertically
  ) {
    ChatInfoImage(cInfo, size = imageSize, iconColor)
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
fun BoxWithConstraintsScope.ChatItemsList(
  user: User,
  chat: Chat,
  unreadCount: State<Int>,
  composeState: MutableState<ComposeState>,
  chatItems: List<ChatItem>,
  useLinkPreviews: Boolean,
  openDirectChat: (Long) -> Unit,
  loadPrevMessages: (ChatInfo) -> Unit,
  deleteMessage: (Long, CIDeleteMode) -> Unit,
  receiveFile: (Long) -> Unit,
  joinGroup: (Long) -> Unit,
  acceptCall: (Contact) -> Unit,
  markRead: (CC.ItemRange, unreadCountAfter: Int?) -> Unit,
  floatingButton: MutableState<@Composable () -> Unit>
) {
  val listState = rememberLazyListState()
  val scope = rememberCoroutineScope()
  val uriHandler = LocalUriHandler.current
  val cxt = LocalContext.current

  // Helps to scroll to bottom after moving from Group to Direct chat
  // and prevents scrolling to bottom on orientation change
  var shouldAutoScroll by rememberSaveable { mutableStateOf(true) }
  LaunchedEffect(chat.chatInfo.apiId, chat.chatInfo.chatType) {
    if (shouldAutoScroll && listState.firstVisibleItemIndex != 0) {
      scope.launch { listState.scrollToItem(0) }
    }
    // Don't autoscroll next time until it will be needed
    shouldAutoScroll = false
  }

  PreloadItems(listState, ChatPagination.UNTIL_PRELOAD_COUNT, chat, chatItems) { c ->
    loadPrevMessages(c.chatInfo)
  }

  Spacer(Modifier.size(8.dp))

  val reversedChatItems by remember { derivedStateOf { chatItems.reversed() } }
  LazyColumn(state = listState, reverseLayout = true) {
    itemsIndexed(reversedChatItems) { i, cItem ->
      if (chat.chatInfo is ChatInfo.Group) {
        if (cItem.chatDir is CIDirection.GroupRcv) {
          val prevItem = if (i < reversedChatItems.lastIndex) reversedChatItems[i + 1] else null
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
                    .clickable {
                      openDirectChat(contactId)
                      // Scroll to first unread message when direct chat will be loaded
                      shouldAutoScroll = true
                    }
                ) {
                  MemberImage(member)
                }
              }
              Spacer(Modifier.size(4.dp))
            } else {
              Spacer(Modifier.size(42.dp))
            }
            ChatItemView(user, chat.chatInfo, cItem, composeState, cxt, uriHandler, showMember = showMember, useLinkPreviews = useLinkPreviews, deleteMessage = deleteMessage, receiveFile = receiveFile, joinGroup = {}, acceptCall = acceptCall)
          }
        } else {
          Box(Modifier.padding(start = 86.dp, end = 12.dp)) {
            ChatItemView(user, chat.chatInfo, cItem, composeState, cxt, uriHandler, useLinkPreviews = useLinkPreviews, deleteMessage = deleteMessage, receiveFile = receiveFile, joinGroup = {}, acceptCall = acceptCall)
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
          ChatItemView(user, chat.chatInfo, cItem, composeState, cxt, uriHandler, useLinkPreviews = useLinkPreviews, deleteMessage = deleteMessage, receiveFile = receiveFile, joinGroup = joinGroup, acceptCall = acceptCall)
        }
      }

      if (cItem.isRcvNew) {
        LaunchedEffect(cItem.id) {
          scope.launch {
            delay(750)
            markRead(CC.ItemRange(cItem.id, cItem.id), null)
          }
        }
      }
    }
  }
  FloatingButtons(chatItems, unreadCount, chat.chatStats.minUnreadItemId, markRead, floatingButton, listState)
}

@Composable
fun BoxWithConstraintsScope.FloatingButtons(
  chatItems: List<ChatItem>,
  unreadCount: State<Int>,
  minUnreadItemId: Long,
  markRead: (CC.ItemRange, unreadCountAfter: Int?) -> Unit,
  floatingButton: MutableState<@Composable () -> Unit>,
  listState: LazyListState
) {
  val scope = rememberCoroutineScope()

  val bottomUnreadCount by remember { derivedStateOf {
    chatItems.subList(chatItems.size - listState.layoutInfo.visibleItemsInfo.lastIndex - 1, chatItems.size).count { it.isRcvNew } }
  }

  val firstItemIsVisible by remember { derivedStateOf { listState.firstVisibleItemIndex == 0 } }
  val firstVisibleOffset = (-with(LocalDensity.current) { maxHeight.roundToPx() } * 0.8).toInt()

  LaunchedEffect(bottomUnreadCount, firstItemIsVisible) {
    val showButtonWithCounter = bottomUnreadCount > 0 && !firstItemIsVisible
    val showButtonWithArrow = !showButtonWithCounter && !firstItemIsVisible
    floatingButton.value = bottomEndFloatingButton(
      bottomUnreadCount,
      showButtonWithCounter,
      showButtonWithArrow,
      onClickArrowDown = {
        scope.launch { listState.animateScrollToItem(0) }
      },
      onClickCounter = {
        val firstUnreadIndex = kotlin.math.max(kotlin.math.min(chatItems.size - 1, chatItems.count { it.isRcvNew }), 0)
        scope.launch { listState.animateScrollToItem(firstUnreadIndex, firstVisibleOffset) }
      }
    )
  }

  val fabSize = 56.dp
  val topUnreadCount by remember {
    derivedStateOf { unreadCount.value - bottomUnreadCount }
  }
  val showButtonWithCounter = topUnreadCount > 0
  val height = with(LocalDensity.current) { maxHeight.toPx() }
  var showDropDown by remember { mutableStateOf(false) }

  TopEndFloatingButton(
    Modifier.padding(end = 16.dp, top = 24.dp).align(Alignment.TopEnd),
    topUnreadCount,
    showButtonWithCounter,
    onClick = { scope.launch { listState.animateScrollBy(height) } },
    onLongClick = { showDropDown = true }
  )

  DropdownMenu(
    expanded = showDropDown,
    onDismissRequest = { showDropDown = false },
    Modifier.width(220.dp),
    offset = DpOffset(maxWidth - 16.dp, 24.dp + fabSize)
  ) {
    DropdownMenuItem(
      onClick = {
        markRead(
          CC.ItemRange(minUnreadItemId, chatItems[chatItems.size - listState.layoutInfo.visibleItemsInfo.lastIndex - 1].id - 1),
          bottomUnreadCount
        )
        showDropDown = false
      }
    ) {
      Text(
        generalGetString(R.string.mark_read),
        maxLines = 1,
      )
    }
  }
}

@Composable
fun PreloadItems(
  listState: LazyListState,
  remaining: Int = 10,
  chat: Chat,
  items: List<*>,
  onLoadMore: (chat: Chat) -> Unit,
) {
  val loadMore = remember {
    derivedStateOf {
      val layoutInfo = listState.layoutInfo
      val totalItemsNumber = layoutInfo.totalItemsCount
      val lastVisibleItemIndex = (layoutInfo.visibleItemsInfo.lastOrNull()?.index ?: 0) + 1
      if (lastVisibleItemIndex > (totalItemsNumber - remaining))
        totalItemsNumber
      else
        0
    }
  }

  LaunchedEffect(loadMore, chat, items) {
    snapshotFlow { loadMore.value }
      .distinctUntilChanged()
      .filter { it > 0 }
      .collect {
        onLoadMore(chat)
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

@Composable
private fun TopEndFloatingButton(
  modifier: Modifier = Modifier,
  unreadCount: Int,
  showButtonWithCounter: Boolean,
  onClick: () -> Unit,
  onLongClick: () -> Unit
) = when {
  showButtonWithCounter -> {
    val interactionSource = interactionSourceWithDetection(onClick, onLongClick)
    FloatingActionButton(
      {}, // no action here
      modifier,
      elevation = FloatingActionButtonDefaults.elevation(0.dp, 0.dp),
      interactionSource = interactionSource,
    ) {
      Text(unreadCount.toString(), color = contentColorFor(MaterialTheme.colors.background))
    }
  }
  else -> {
  }
}

private fun bottomEndFloatingButton(
  unreadCount: Int,
  showButtonWithCounter: Boolean,
  showButtonWithArrow: Boolean,
  onClickArrowDown: () -> Unit,
  onClickCounter: () -> Unit
): @Composable () -> Unit = when {
  showButtonWithCounter -> {
    {
      FloatingActionButton(
        onClick = onClickCounter,
        elevation = FloatingActionButtonDefaults.elevation(0.dp, 0.dp, 0.dp, 0.dp)
      ) {
        Text(unreadCount.toString(), color = contentColorFor(MaterialTheme.colors.background))
      }
    }
  }
  showButtonWithArrow -> {
    {
      FloatingActionButton(
        onClick = onClickArrowDown,
        elevation = FloatingActionButtonDefaults.elevation(0.dp, 0.dp, 0.dp, 0.dp)
      ) {
        Icon(
          imageVector = Icons.Default.ArrowDownward,
          contentDescription = null,
          tint = contentColorFor(MaterialTheme.colors.background)
        )
      }
    }
  }
  else -> {
    {}
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
    val unreadCount = remember { mutableStateOf(chatItems.count { it.isRcvNew }) }
    ChatLayout(
      user = User.sampleData,
      chat = Chat(
        chatInfo = ChatInfo.Direct.sampleData,
        chatItems = chatItems,
        chatStats = Chat.ChatStats()
      ),
      unreadCount = unreadCount,
      composeState = remember { mutableStateOf(ComposeState(useLinkPreviews = true)) },
      composeView = {},
      attachmentOption = remember { mutableStateOf<AttachmentOption?>(null) },
      scope = rememberCoroutineScope(),
      attachmentBottomSheetState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden),
      chatItems = chatItems,
      useLinkPreviews = true,
      back = {},
      info = {},
      openDirectChat = {},
      loadPrevMessages = { _ -> },
      deleteMessage = { _, _ -> },
      receiveFile = {},
      joinGroup = {},
      startCall = {},
      acceptCall = { _ -> },
      addMembers = { _ -> },
      markRead = { _, _ -> },
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
    val unreadCount = remember { mutableStateOf(chatItems.count { it.isRcvNew }) }
    ChatLayout(
      user = User.sampleData,
      chat = Chat(
        chatInfo = ChatInfo.Group.sampleData,
        chatItems = chatItems,
        chatStats = Chat.ChatStats()
      ),
      unreadCount = unreadCount,
      composeState = remember { mutableStateOf(ComposeState(useLinkPreviews = true)) },
      composeView = {},
      attachmentOption = remember { mutableStateOf<AttachmentOption?>(null) },
      scope = rememberCoroutineScope(),
      attachmentBottomSheetState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden),
      chatItems = chatItems,
      useLinkPreviews = true,
      back = {},
      info = {},
      openDirectChat = {},
      loadPrevMessages = { _ -> },
      deleteMessage = { _, _ -> },
      receiveFile = {},
      joinGroup = {},
      startCall = {},
      acceptCall = { _ -> },
      addMembers = { _ -> },
      markRead = { _, _ -> },
    )
  }
}
