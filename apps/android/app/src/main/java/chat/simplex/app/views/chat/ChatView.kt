package chat.simplex.app.views.chat

import android.content.res.Configuration
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.*
import androidx.compose.foundation.gestures.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.KeyboardArrowDown
import androidx.compose.material.icons.filled.MoreVert
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.mapSaver
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.*
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.capitalize
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.intl.Locale
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
import chat.simplex.app.views.chat.item.ItemAction
import chat.simplex.app.views.chatlist.*
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.helpers.AppBarHeight
import com.google.accompanist.insets.ProvideWindowInsets
import com.google.accompanist.insets.navigationBarsWithImePadding
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*
import kotlinx.datetime.Clock

@Composable
fun ChatView(chatModel: ChatModel) {
  var activeChat by remember { mutableStateOf(chatModel.chats.firstOrNull { chat -> chat.chatInfo.id == chatModel.chatId.value }) }
  val searchText = remember { mutableStateOf("") }
  val user = chatModel.currentUser.value
  val useLinkPreviews = chatModel.controller.appPrefs.privacyLinkPreviews.get()
  val composeState = remember { mutableStateOf(ComposeState(useLinkPreviews = useLinkPreviews)) }
  val attachmentOption = remember { mutableStateOf<AttachmentOption?>(null) }
  val attachmentBottomSheetState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden)
  val scope = rememberCoroutineScope()

  LaunchedEffect(Unit) {
    // snapshotFlow here is because it reacts much faster on changes in chatModel.chatId.value.
    // With LaunchedEffect(chatModel.chatId.value) there is a noticeable delay before reconstruction of the view
    snapshotFlow { chatModel.chatId.value }
      .distinctUntilChanged()
      .collect {
        activeChat = if (chatModel.chatId.value == null) {
          null
        } else {
          // Redisplay the whole hierarchy if the chat is different to make going from groups to direct chat working correctly
          // Also for situation when chatId changes after clicking in notification, etc
          chatModel.getChat(chatModel.chatId.value!!)
        }
      }
  }

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
      searchText,
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
                background = if (isInDarkTheme()) MaterialTheme.colors.background else SettingsBackgroundLight
              ) {
                ChatInfoView(chatModel, connStats, close)
              }
            }
          } else if (cInfo is ChatInfo.Group) {
            setGroupMembers(cInfo.groupInfo, chatModel)
            ModalManager.shared.showCustomModal { close ->
              ModalView(
                close = close, modifier = Modifier,
                background = if (isInDarkTheme()) MaterialTheme.colors.background else SettingsBackgroundLight
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
          withApi { openChat(c.chatInfo, chatModel) }
        }
      },
      loadPrevMessages = { cInfo ->
        val c = chatModel.getChat(cInfo.id)
        val firstId = chatModel.chatItems.firstOrNull()?.id
        if (c != null && firstId != null) {
          withApi {
            apiLoadPrevMessages(c.chatInfo, chatModel, firstId, searchText.value)
          }
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
              background = if (isInDarkTheme()) MaterialTheme.colors.background else SettingsBackgroundLight
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
      },
      onSearchValueChanged = { value ->
        if (searchText.value == value) return@ChatLayout
        val c = chatModel.getChat(chat.chatInfo.id) ?: return@ChatLayout
        withApi {
          apiFindMessages(c.chatInfo, chatModel, value)
          searchText.value = value
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
  searchValue: State<String>,
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
  onSearchValueChanged: (String) -> Unit,
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
        val setFloatingButton = { button: @Composable () -> Unit ->
          floatingButton.value = button
        }

        Scaffold(
          topBar = { ChatInfoToolbar(chat, back, info, startCall, addMembers, onSearchValueChanged) },
          bottomBar = composeView,
          modifier = Modifier.navigationBarsWithImePadding(),
          floatingActionButton = { floatingButton.value() },
        ) { contentPadding ->
          BoxWithConstraints(Modifier.fillMaxHeight().padding(contentPadding)) {
            ChatItemsList(
              user, chat, unreadCount, composeState, chatItems, searchValue,
              useLinkPreviews, openDirectChat, loadPrevMessages, deleteMessage,
              receiveFile, joinGroup, acceptCall, markRead, setFloatingButton
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
  addMembers: (GroupInfo) -> Unit,
  onSearchValueChanged: (String) -> Unit,
) {
  var showMenu by remember { mutableStateOf(false) }
  var showSearch by remember { mutableStateOf(false) }
  val onBackClicked = {
    if (!showSearch) {
      back()
    } else {
      onSearchValueChanged("")
      showSearch = false
    }
  }
  BackHandler(onBack = onBackClicked)
  val barButtons = arrayListOf<@Composable RowScope.() -> Unit>()
  val menuItems = arrayListOf<@Composable () -> Unit>()
  menuItems.add {
    ItemAction(stringResource(android.R.string.search_go).capitalize(Locale.current), Icons.Outlined.Search, onClick = {
      showMenu = false
      showSearch = true
    })
  }

  if (chat.chatInfo is ChatInfo.Direct) {
    barButtons.add {
      IconButton({
        showMenu = false
        startCall(CallMediaType.Audio)
      }) {
        Icon(Icons.Outlined.Phone, stringResource(R.string.icon_descr_more_button), tint = MaterialTheme.colors.primary)
      }
    }
    menuItems.add {
      ItemAction(stringResource(R.string.icon_descr_video_call).capitalize(Locale.current), Icons.Outlined.Videocam, onClick = {
        showMenu = false
        startCall(CallMediaType.Video)
      })
    }
  } else if (chat.chatInfo is ChatInfo.Group && chat.chatInfo.groupInfo.canAddMembers) {
    barButtons.add {
      IconButton({
        showMenu = false
        addMembers(chat.chatInfo.groupInfo)
      }) {
        Icon(Icons.Outlined.PersonAdd, stringResource(R.string.icon_descr_add_members), tint = MaterialTheme.colors.primary)
      }
    }
  }
  barButtons.add {
    IconButton({ showMenu = true }) {
      Icon(Icons.Default.MoreVert, stringResource(R.string.icon_descr_more_button), tint = MaterialTheme.colors.primary)
    }
  }

  DefaultTopAppBar(
    navigationButton = { NavigationButtonBack(onBackClicked) },
    title = { ChatInfoToolbarTitle(chat.chatInfo) },
    onTitleClick = info,
    showSearch = showSearch,
    onSearchValueChanged = onSearchValueChanged,
    buttons = barButtons
  )

  Divider(Modifier.padding(top = AppBarHeight))

  Box(Modifier.fillMaxWidth().wrapContentSize(Alignment.TopEnd).offset(y = AppBarHeight)) {
    DropdownMenu(
      expanded = showMenu,
      onDismissRequest = { showMenu = false },
      Modifier.widthIn(min = 220.dp)
    ) {
      menuItems.forEach { it() }
    }
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
  searchValue: State<String>,
  useLinkPreviews: Boolean,
  openDirectChat: (Long) -> Unit,
  loadPrevMessages: (ChatInfo) -> Unit,
  deleteMessage: (Long, CIDeleteMode) -> Unit,
  receiveFile: (Long) -> Unit,
  joinGroup: (Long) -> Unit,
  acceptCall: (Contact) -> Unit,
  markRead: (CC.ItemRange, unreadCountAfter: Int?) -> Unit,
  setFloatingButton: (@Composable () -> Unit) -> Unit,
) {
  val listState = rememberLazyListState()
  val scope = rememberCoroutineScope()
  val uriHandler = LocalUriHandler.current
  val cxt = LocalContext.current

  // Helps to scroll to bottom after moving from Group to Direct chat
  // and prevents scrolling to bottom on orientation change
  var shouldAutoScroll by rememberSaveable { mutableStateOf(true) }
  LaunchedEffect(chat.chatInfo.apiId, chat.chatInfo.chatType, shouldAutoScroll) {
    if (shouldAutoScroll && listState.firstVisibleItemIndex != 0) {
      scope.launch { listState.scrollToItem(0) }
    }
    // Don't autoscroll next time until it will be needed
    shouldAutoScroll = false
  }
  var prevSearchEmptiness by rememberSaveable { mutableStateOf(searchValue.value.isEmpty()) }
  // Scroll to bottom when search value changes from something to nothing and back
  LaunchedEffect(searchValue.value.isEmpty()) {
    // They are equal when orientation was changed, don't need to scroll.
    // LaunchedEffect unaware of this event since it uses remember, not rememberSaveable
    if (prevSearchEmptiness == searchValue.value.isEmpty()) return@LaunchedEffect
    prevSearchEmptiness = searchValue.value.isEmpty()

    if (listState.firstVisibleItemIndex != 0) {
      scope.launch { listState.scrollToItem(0) }
    }
  }

  PreloadItems(listState, ChatPagination.UNTIL_PRELOAD_COUNT, chat, chatItems) { c ->
    loadPrevMessages(c.chatInfo)
  }

  Spacer(Modifier.size(8.dp))

  val reversedChatItems by remember { derivedStateOf { chatItems.reversed() } }
  LazyColumn(Modifier.align(Alignment.BottomCenter), state = listState, reverseLayout = true) {
    itemsIndexed(reversedChatItems) { i, cItem ->
      CompositionLocalProvider(
        // Makes horizontal and vertical scrolling to coexist nicely.
        // With default touchSlop when you scroll LazyColumn, you can unintentionally open reply view
        LocalViewConfiguration provides LocalViewConfiguration.current.bigTouchSlop()
      ) {
        val dismissState = rememberDismissState(initialValue = DismissValue.Default) { false }
        val directions = setOf(DismissDirection.EndToStart)
        val swipeableModifier = SwipeToDismissModifier(
          state = dismissState,
          directions = directions,
          swipeDistance = with(LocalDensity.current) { 30.dp.toPx() },
        )
        val swipedToEnd = (dismissState.overflow.value > 0f && directions.contains(DismissDirection.StartToEnd))
        val swipedToStart = (dismissState.overflow.value < 0f && directions.contains(DismissDirection.EndToStart))
        if (dismissState.isAnimationRunning && (swipedToStart || swipedToEnd)) {
          LaunchedEffect(Unit) {
            scope.launch {
              if (composeState.value.editing) {
                composeState.value = ComposeState(contextItem = ComposeContextItem.QuotedItem(cItem), useLinkPreviews = useLinkPreviews)
              } else {
                composeState.value = composeState.value.copy(contextItem = ComposeContextItem.QuotedItem(cItem))
              }
            }
          }
        }

        if (chat.chatInfo is ChatInfo.Group) {
          if (cItem.chatDir is CIDirection.GroupRcv) {
            val prevItem = if (i < reversedChatItems.lastIndex) reversedChatItems[i + 1] else null
            val member = cItem.chatDir.groupMember
            val showMember = showMemberImage(member, prevItem)
            Row(Modifier.padding(start = 8.dp, end = 66.dp).then(swipeableModifier)) {
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
            Box(Modifier.padding(start = 86.dp, end = 12.dp).then(swipeableModifier)) {
              ChatItemView(user, chat.chatInfo, cItem, composeState, cxt, uriHandler, useLinkPreviews = useLinkPreviews, deleteMessage = deleteMessage, receiveFile = receiveFile, joinGroup = {}, acceptCall = acceptCall)
            }
          }
        } else { // direct message
          val sent = cItem.chatDir.sent
          Box(
            Modifier.padding(
              start = if (sent) 76.dp else 12.dp,
              end = if (sent) 12.dp else 76.dp,
            ).then(swipeableModifier)
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
  }
  FloatingButtons(chatItems, unreadCount, chat.chatStats.minUnreadItemId, searchValue, markRead, setFloatingButton, listState)
}

@Composable
fun BoxWithConstraintsScope.FloatingButtons(
  chatItems: List<ChatItem>,
  unreadCount: State<Int>,
  minUnreadItemId: Long,
  searchValue: State<String>,
  markRead: (CC.ItemRange, unreadCountAfter: Int?) -> Unit,
  setFloatingButton: (@Composable () -> Unit) -> Unit,
  listState: LazyListState
) {
  val scope = rememberCoroutineScope()

  var firstVisibleIndex by remember { mutableStateOf(listState.firstVisibleItemIndex) }
  var lastIndexOfVisibleItems by remember { mutableStateOf(listState.layoutInfo.visibleItemsInfo.lastIndex) }
  var firstItemIsVisible by remember { mutableStateOf(firstVisibleIndex == 0)  }

  LaunchedEffect(listState) {
    snapshotFlow { listState.firstVisibleItemIndex }
      .distinctUntilChanged()
      .collect {
        firstVisibleIndex = it
        firstItemIsVisible = firstVisibleIndex == 0
      }
  }

  LaunchedEffect(listState) {
    // When both snapshotFlows located in one LaunchedEffect second block will never be called because coroutine is paused on first block
    // so separate them into two LaunchedEffects
    snapshotFlow { listState.layoutInfo.visibleItemsInfo.lastIndex }
      .distinctUntilChanged()
      .collect {
        lastIndexOfVisibleItems = it
      }
  }

  val bottomUnreadCount by remember {
    derivedStateOf {
      if (unreadCount.value == 0) return@derivedStateOf 0

      val from = chatItems.lastIndex - firstVisibleIndex - lastIndexOfVisibleItems
      if (chatItems.size <= from || from < 0) return@derivedStateOf 0

      chatItems.subList(from, chatItems.size).count { it.isRcvNew }
    }
  }

  val firstVisibleOffset = (-with(LocalDensity.current) { maxHeight.roundToPx() } * 0.8).toInt()

  LaunchedEffect(bottomUnreadCount, firstItemIsVisible) {
    val showButtonWithCounter = bottomUnreadCount > 0 && !firstItemIsVisible && searchValue.value.isEmpty()
    val showButtonWithArrow = !showButtonWithCounter && !firstItemIsVisible
    setFloatingButton(
      bottomEndFloatingButton(
        bottomUnreadCount,
        showButtonWithCounter,
        showButtonWithArrow,
        onClickArrowDown = {
          scope.launch { listState.animateScrollToItem(0) }
        },
        onClickCounter = {
          scope.launch { listState.animateScrollToItem(kotlin.math.max(0, bottomUnreadCount - 1), firstVisibleOffset) }
        }
      ))
  }
  // Don't show top FAB if is in search
  if (searchValue.value.isNotEmpty()) return
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
  LaunchedEffect(listState, chat, items) {
    snapshotFlow { listState.layoutInfo }
      .map {
        val totalItemsNumber = it.totalItemsCount
        val lastVisibleItemIndex = (it.visibleItemsInfo.lastOrNull()?.index ?: 0) + 1
        if (lastVisibleItemIndex > (totalItemsNumber - remaining))
          totalItemsNumber
        else
          0
      }
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
      modifier.size(48.dp),
      elevation = FloatingActionButtonDefaults.elevation(0.dp, 0.dp),
      interactionSource = interactionSource,
    ) {
      Text(
        unreadCountStr(unreadCount),
        color = MaterialTheme.colors.primary,
        fontSize = 14.sp,
      )
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
        elevation = FloatingActionButtonDefaults.elevation(0.dp, 0.dp, 0.dp, 0.dp),
        modifier = Modifier.size(48.dp)
      ) {
        Text(
          unreadCountStr(unreadCount),
          color = MaterialTheme.colors.primary,
          fontSize = 14.sp,
        )
      }
    }
  }
  showButtonWithArrow -> {
    {
      FloatingActionButton(
        onClick = onClickArrowDown,
        elevation = FloatingActionButtonDefaults.elevation(0.dp, 0.dp, 0.dp, 0.dp),
        modifier = Modifier.size(48.dp)
      ) {
        Icon(
          imageVector = Icons.Default.KeyboardArrowDown,
          contentDescription = null,
          tint = MaterialTheme.colors.primary
        )
      }
    }
  }
  else -> {
    {}
  }
}

private fun ViewConfiguration.bigTouchSlop(slop: Float = 50f) = object: ViewConfiguration {
  override val longPressTimeoutMillis
    get() =
      this@bigTouchSlop.longPressTimeoutMillis
  override val doubleTapTimeoutMillis
    get() =
      this@bigTouchSlop.doubleTapTimeoutMillis
  override val doubleTapMinTimeMillis
    get() =
      this@bigTouchSlop.doubleTapMinTimeMillis
  override val touchSlop: Float get() = slop
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
    val searchValue = remember { mutableStateOf("") }
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
      searchValue,
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
      onSearchValueChanged = {},
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
    val searchValue = remember { mutableStateOf("") }
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
      searchValue,
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
      onSearchValueChanged = {},
    )
  }
}
