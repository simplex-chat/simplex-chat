package chat.simplex.common.views.chat

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.gestures.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.mapSaver
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.capitalize
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.intl.Locale
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.unit.*
import com.icerockdev.library.MR
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.call.*
import chat.simplex.common.views.chat.group.*
import chat.simplex.common.views.chat.item.*
import chat.simplex.common.views.chatlist.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.GroupInfo
import chat.simplex.common.platform.*
import chat.simplex.common.platform.AudioPlayer
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*
import kotlinx.datetime.Clock
import java.io.File
import java.net.URI
import kotlin.math.sign

@Composable
fun ChatView(chatId: String, chatModel: ChatModel, onComposed: () -> Unit) {
  val activeChat = remember { mutableStateOf(chatModel.chats.firstOrNull { chat -> chat.chatInfo.id == chatId }) }
  val searchText = rememberSaveable { mutableStateOf("") }
  val user = chatModel.currentUser.value
  val useLinkPreviews = chatModel.controller.appPrefs.privacyLinkPreviews.get()
  val composeState = rememberSaveable(saver = ComposeState.saver()) {
    mutableStateOf(
      if (chatModel.draftChatId.value == chatId && chatModel.draft.value != null) {
        chatModel.draft.value ?: ComposeState(useLinkPreviews = useLinkPreviews)
      } else {
        ComposeState(useLinkPreviews = useLinkPreviews)
      }
    )
  }
  val attachmentOption = rememberSaveable { mutableStateOf<AttachmentOption?>(null) }
  val attachmentBottomSheetState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden)
  val scope = rememberCoroutineScope()
  LaunchedEffect(Unit) {
    // snapshotFlow here is because it reacts much faster on changes in chatModel.chatId.value.
    // With LaunchedEffect(chatModel.chatId.value) there is a noticeable delay before reconstruction of the view
    launch {
      snapshotFlow { chatModel.chatId.value }
        .distinctUntilChanged()
        .collect {
          if (activeChat.value?.id != chatModel.chatId.value && chatModel.chatId.value != null) {
            // Redisplay the whole hierarchy if the chat is different to make going from groups to direct chat working correctly
            // Also for situation when chatId changes after clicking in notification, etc
            activeChat.value = chatModel.getChat(chatModel.chatId.value!!)
          }
          markUnreadChatAsRead(activeChat, chatModel)
        }
    }
    launch {
      snapshotFlow {
        /**
         * It's possible that in some cases concurrent modification can happen on [ChatModel.chats] list.
         * In this case only error log will be printed here (no crash).
         * TODO: Re-write [ChatModel.chats] logic to a new list assignment instead of changing content of mutableList to prevent that
         * */
        try {
          chatModel.chats.firstOrNull { chat -> chat.chatInfo.id == chatModel.chatId.value }
        } catch (e: ConcurrentModificationException) {
          Log.e(TAG, e.stackTraceToString())
          null
        }
      }
        .distinctUntilChanged()
        // Only changed chatInfo is important thing. Other properties can be skipped for reducing recompositions
        .filter { it?.chatInfo != activeChat.value?.chatInfo && it != null }
        .collect { activeChat.value = it }
    }
  }
  val view = LocalMultiplatformView()
  if (activeChat.value == null || user == null) {
    chatModel.chatId.value = null
  } else {
    val chat = activeChat.value!!
    // We need to have real unreadCount value for displaying it inside top right button
    // Having activeChat reloaded on every change in it is inefficient (UI lags)
    val unreadCount = remember {
      derivedStateOf {
        chatModel.chats.firstOrNull { chat -> chat.chatInfo.id == chatId }?.chatStats?.unreadCount ?: 0
      }
    }
    val clipboard = LocalClipboardManager.current

    ChatLayout(
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
      attachmentBottomSheetState,
      chatModel.chatItems,
      searchText,
      useLinkPreviews = useLinkPreviews,
      linkMode = chatModel.simplexLinkMode.value,
      chatModelIncognito = chatModel.incognito.value,
      back = {
        hideKeyboard(view)
        AudioPlayer.stop()
        chatModel.chatId.value = null
      },
      info = {
        hideKeyboard(view)
        withApi {
          if (chat.chatInfo is ChatInfo.Direct) {
            val contactInfo = chatModel.controller.apiContactInfo(chat.chatInfo.apiId)
            val (_, code) = chatModel.controller.apiGetContactCode(chat.chatInfo.apiId)
            ModalManager.shared.showModalCloseable(true) { close ->
              remember { derivedStateOf { (chatModel.getContactChat(chat.chatInfo.apiId)?.chatInfo as? ChatInfo.Direct)?.contact } }.value?.let { ct ->
                ChatInfoView(chatModel, ct, contactInfo?.first, contactInfo?.second, chat.chatInfo.localAlias, code, close)
              }
            }
          } else if (chat.chatInfo is ChatInfo.Group) {
            setGroupMembers(chat.chatInfo.groupInfo, chatModel)
            val link = chatModel.controller.apiGetGroupLink(chat.chatInfo.groupInfo.groupId)
            var groupLink = link?.first
            var groupLinkMemberRole = link?.second
            ModalManager.shared.showModalCloseable(true) { close ->
              GroupChatInfoView(chatModel, groupLink, groupLinkMemberRole, {
                groupLink = it.first;
                groupLinkMemberRole = it.second
              }, close)
            }
          }
        }
      },
      showMemberInfo = { groupInfo: GroupInfo, member: GroupMember ->
        hideKeyboard(view)
        withApi {
          val stats = chatModel.controller.apiGroupMemberInfo(groupInfo.groupId, member.groupMemberId)
          val (_, code) = if (member.memberActive) {
            try {
              chatModel.controller.apiGetGroupMemberCode(groupInfo.apiId, member.groupMemberId)
            } catch (e: Exception) {
              Log.e(TAG, e.stackTraceToString())
              member to null
            }
          } else {
            member to null
          }
          setGroupMembers(groupInfo, chatModel)
          ModalManager.shared.showModalCloseable(true) { close ->
            remember { derivedStateOf { chatModel.groupMembers.firstOrNull { it.memberId == member.memberId } } }.value?.let { mem ->
              GroupMemberInfoView(groupInfo, mem, stats, code, chatModel, close, close)
            }
          }
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
          val toDeleteItem = chatModel.chatItems.firstOrNull { it.id == itemId }
          val toModerate = toDeleteItem?.memberToModerate(chat.chatInfo)
          val groupInfo = toModerate?.first
          val groupMember = toModerate?.second
          val deletedChatItem: ChatItem?
          val toChatItem: ChatItem?
          if (mode == CIDeleteMode.cidmBroadcast && groupInfo != null && groupMember != null) {
            val r = chatModel.controller.apiDeleteMemberChatItem(
              groupId = groupInfo.groupId,
              groupMemberId = groupMember.groupMemberId,
              itemId = itemId
            )
            deletedChatItem = r?.first
            toChatItem = r?.second
          } else {
            val r = chatModel.controller.apiDeleteChatItem(
              type = cInfo.chatType,
              id = cInfo.apiId,
              itemId = itemId,
              mode = mode
            )
            deletedChatItem = r?.deletedChatItem?.chatItem
            toChatItem = r?.toChatItem?.chatItem
          }
          if (toChatItem == null && deletedChatItem != null) {
            chatModel.removeChatItem(cInfo, deletedChatItem)
          } else if (toChatItem != null) {
            chatModel.upsertChatItem(cInfo, toChatItem)
          }
        }
      },
      receiveFile = { fileId ->
        withApi { chatModel.controller.receiveFile(user, fileId) }
      },
      cancelFile = { fileId ->
        withApi { chatModel.controller.cancelFile(user, fileId) }
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
        hideKeyboard(view)
        val invitation = chatModel.callInvitations.remove(contact.id)
        if (invitation == null) {
          AlertManager.shared.showAlertMsg("Call already ended!")
        } else {
          chatModel.callManager.acceptIncomingCall(invitation = invitation)
        }
      },
      acceptFeature = { contact, feature, param ->
        withApi {
          chatModel.controller.allowFeatureToContact(contact, feature, param)
        }
      },
      setReaction = { cInfo, cItem, add, reaction ->
        withApi {
          val updatedCI = chatModel.controller.apiChatItemReaction(
            type = cInfo.chatType,
            id = cInfo.apiId,
            itemId = cItem.id,
            add = add,
            reaction = reaction
          )
          if (updatedCI != null) {
            chatModel.updateChatItem(cInfo, updatedCI)
          }
        }
      },
      showItemDetails = { cInfo, cItem ->
        withApi {
          val ciInfo = chatModel.controller.apiGetChatItemInfo(cInfo.chatType, cInfo.apiId, cItem.id)
          if (ciInfo != null) {
            ModalManager.shared.showModal(endButtons = { ShareButton {
              clipboard.shareText(itemInfoShareText(cItem, ciInfo, chatModel.controller.appPrefs.developerTools.get()))
            } }) {
              ChatItemInfoView(cItem, ciInfo, devTools = chatModel.controller.appPrefs.developerTools.get())
            }
          }
        }
      },
      addMembers = { groupInfo ->
        hideKeyboard(view)
        withApi {
          setGroupMembers(groupInfo, chatModel)
          ModalManager.shared.showModalCloseable(true) { close ->
              AddGroupMembersView(groupInfo, false, chatModel, close)
          }
        }
      },
      markRead = { range, unreadCountAfter ->
        chatModel.markChatItemsRead(chat.chatInfo, range, unreadCountAfter)
        ntfManager.cancelNotificationsForChat(chat.id)
        withBGApi {
          chatModel.controller.apiChatRead(
            chat.chatInfo.chatType,
            chat.chatInfo.apiId,
            range
          )
        }
      },
      changeNtfsState = { enabled, currentValue -> changeNtfsStatePerChat(enabled, currentValue, chat, chatModel) },
      onSearchValueChanged = { value ->
        if (searchText.value == value) return@ChatLayout
        val c = chatModel.getChat(chat.chatInfo.id) ?: return@ChatLayout
        withApi {
          apiFindMessages(c.chatInfo, chatModel, value)
          searchText.value = value
        }
      },
      onComposed,
    )
  }
}

@Composable
fun ChatLayout(
  chat: Chat,
  unreadCount: State<Int>,
  composeState: MutableState<ComposeState>,
  composeView: (@Composable () -> Unit),
  attachmentOption: MutableState<AttachmentOption?>,
  attachmentBottomSheetState: ModalBottomSheetState,
  chatItems: List<ChatItem>,
  searchValue: State<String>,
  useLinkPreviews: Boolean,
  linkMode: SimplexLinkMode,
  chatModelIncognito: Boolean,
  back: () -> Unit,
  info: () -> Unit,
  showMemberInfo: (GroupInfo, GroupMember) -> Unit,
  loadPrevMessages: (ChatInfo) -> Unit,
  deleteMessage: (Long, CIDeleteMode) -> Unit,
  receiveFile: (Long) -> Unit,
  cancelFile: (Long) -> Unit,
  joinGroup: (Long) -> Unit,
  startCall: (CallMediaType) -> Unit,
  acceptCall: (Contact) -> Unit,
  acceptFeature: (Contact, ChatFeature, Int?) -> Unit,
  setReaction: (ChatInfo, ChatItem, Boolean, MsgReaction) -> Unit,
  showItemDetails: (ChatInfo, ChatItem) -> Unit,
  addMembers: (GroupInfo) -> Unit,
  markRead: (CC.ItemRange, unreadCountAfter: Int?) -> Unit,
  changeNtfsState: (Boolean, currentValue: MutableState<Boolean>) -> Unit,
  onSearchValueChanged: (String) -> Unit,
  onComposed: () -> Unit,
) {
  val scope = rememberCoroutineScope()
  Box(
    Modifier
      .fillMaxWidth()
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
          topBar = { ChatInfoToolbar(chat, back, info, startCall, addMembers, changeNtfsState, onSearchValueChanged) },
          bottomBar = composeView,
          modifier = Modifier.navigationBarsWithImePadding(),
          floatingActionButton = { floatingButton.value() },
        ) { contentPadding ->
          BoxWithConstraints(Modifier
            .fillMaxHeight()
            .padding(contentPadding)
          ) {
            ChatItemsList(
              chat, unreadCount, composeState, chatItems, searchValue,
              useLinkPreviews, linkMode, chatModelIncognito, showMemberInfo, loadPrevMessages, deleteMessage,
              receiveFile, cancelFile, joinGroup, acceptCall, acceptFeature, setReaction, showItemDetails, markRead, setFloatingButton, onComposed,
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
  changeNtfsState: (Boolean, currentValue: MutableState<Boolean>) -> Unit,
  onSearchValueChanged: (String) -> Unit,
) {
  val scope = rememberCoroutineScope()
  val showMenu = rememberSaveable { mutableStateOf(false) }
  var showSearch by rememberSaveable { mutableStateOf(false) }
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
    ItemAction(stringResource(MR.strings.search).capitalize(Locale.current), painterResource(MR.images.ic_search), onClick = {
      showMenu.value = false
      showSearch = true
    })
  }

  if (chat.chatInfo is ChatInfo.Direct && chat.chatInfo.contact.allowsFeature(ChatFeature.Calls)) {
    barButtons.add {
      IconButton({
        showMenu.value = false
        startCall(CallMediaType.Audio)
      }) {
        Icon(painterResource(MR.images.ic_call_500), stringResource(MR.strings.icon_descr_more_button), tint = MaterialTheme.colors.primary)
      }
    }
    menuItems.add {
      ItemAction(stringResource(MR.strings.icon_descr_video_call).capitalize(Locale.current), painterResource(MR.images.ic_videocam), onClick = {
        showMenu.value = false
        startCall(CallMediaType.Video)
      })
    }
  } else if (chat.chatInfo is ChatInfo.Group && chat.chatInfo.groupInfo.canAddMembers && !chat.chatInfo.incognito) {
    barButtons.add {
      IconButton({
        showMenu.value = false
        addMembers(chat.chatInfo.groupInfo)
      }) {
        Icon(painterResource(MR.images.ic_person_add_500), stringResource(MR.strings.icon_descr_add_members), tint = MaterialTheme.colors.primary)
      }
    }
  }
  val ntfsEnabled = remember { mutableStateOf(chat.chatInfo.ntfsEnabled) }
  menuItems.add {
    ItemAction(
      if (ntfsEnabled.value) stringResource(MR.strings.mute_chat) else stringResource(MR.strings.unmute_chat),
      if (ntfsEnabled.value) painterResource(MR.images.ic_notifications_off) else painterResource(MR.images.ic_notifications),
      onClick = {
        showMenu.value = false
        // Just to make a delay before changing state of ntfsEnabled, otherwise it will redraw menu item with new value before closing the menu
        scope.launch {
          delay(200)
          changeNtfsState(!ntfsEnabled.value, ntfsEnabled)
        }
      }
    )
  }

  barButtons.add {
    IconButton({ showMenu.value = true }) {
      Icon(MoreVertFilled, stringResource(MR.strings.icon_descr_more_button), tint = MaterialTheme.colors.primary)
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
    DefaultDropdownMenu(showMenu) {
      menuItems.forEach { it() }
    }
  }
}

@Composable
fun ChatInfoToolbarTitle(cInfo: ChatInfo, imageSize: Dp = 40.dp, iconColor: Color = MaterialTheme.colors.secondaryVariant) {
  Row(
    horizontalArrangement = Arrangement.Center,
    verticalAlignment = Alignment.CenterVertically
  ) {
    if (cInfo.incognito) {
      IncognitoImage(size = 36.dp, Indigo)
    }
    ChatInfoImage(cInfo, size = imageSize, iconColor)
    Column(
      Modifier.padding(start = 8.dp),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      Row(verticalAlignment = Alignment.CenterVertically) {
        if ((cInfo as? ChatInfo.Direct)?.contact?.verified == true) {
          ContactVerifiedShield()
        }
        Text(
          cInfo.displayName, fontWeight = FontWeight.SemiBold,
          maxLines = 1, overflow = TextOverflow.Ellipsis
        )
      }
      if (cInfo.fullName != "" && cInfo.fullName != cInfo.displayName && cInfo.localAlias.isEmpty()) {
        Text(
          cInfo.fullName,
          maxLines = 1, overflow = TextOverflow.Ellipsis
        )
      }
    }
  }
}

@Composable
private fun ContactVerifiedShield() {
  Icon(painterResource(MR.images.ic_verified_user), null, Modifier.size(18.dp).padding(end = 3.dp, top = 1.dp), tint = MaterialTheme.colors.secondary)
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
  chat: Chat,
  unreadCount: State<Int>,
  composeState: MutableState<ComposeState>,
  chatItems: List<ChatItem>,
  searchValue: State<String>,
  useLinkPreviews: Boolean,
  linkMode: SimplexLinkMode,
  chatModelIncognito: Boolean,
  showMemberInfo: (GroupInfo, GroupMember) -> Unit,
  loadPrevMessages: (ChatInfo) -> Unit,
  deleteMessage: (Long, CIDeleteMode) -> Unit,
  receiveFile: (Long) -> Unit,
  cancelFile: (Long) -> Unit,
  joinGroup: (Long) -> Unit,
  acceptCall: (Contact) -> Unit,
  acceptFeature: (Contact, ChatFeature, Int?) -> Unit,
  setReaction: (ChatInfo, ChatItem, Boolean, MsgReaction) -> Unit,
  showItemDetails: (ChatInfo, ChatItem) -> Unit,
  markRead: (CC.ItemRange, unreadCountAfter: Int?) -> Unit,
  setFloatingButton: (@Composable () -> Unit) -> Unit,
  onComposed: () -> Unit,
) {
  val listState = rememberLazyListState()
  val scope = rememberCoroutineScope()
  ScrollToBottom(chat.id, listState, chatItems)
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
  val reversedChatItems by remember { derivedStateOf { chatItems.reversed().toList() } }
  val maxHeightRounded = with(LocalDensity.current) { maxHeight.roundToPx() }
  val scrollToItem: (Long) -> Unit = { itemId: Long ->
    val index = reversedChatItems.indexOfFirst { it.id == itemId }
    if (index != -1) {
      scope.launch { listState.animateScrollToItem(kotlin.math.min(reversedChatItems.lastIndex, index + 1), -maxHeightRounded) }
    }
  }
  LaunchedEffect(Unit) {
    var stopListening = false
    snapshotFlow { listState.layoutInfo.visibleItemsInfo.lastIndex }
      .distinctUntilChanged()
      .filter { !stopListening }
      .collect {
          onComposed()
          stopListening = true
      }
  }
  DisposableEffectOnGone(
    whenGone = {
      VideoPlayer.releaseAll()
    }
  )
  LazyColumn(Modifier.align(Alignment.BottomCenter), state = listState, reverseLayout = true) {
    itemsIndexed(reversedChatItems, key = { _, item -> item.id}) { i, cItem ->
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
              if (cItem.content is CIContent.SndMsgContent || cItem.content is CIContent.RcvMsgContent) {
                if (composeState.value.editing) {
                  composeState.value = ComposeState(contextItem = ComposeContextItem.QuotedItem(cItem), useLinkPreviews = useLinkPreviews)
                } else if (cItem.id != ChatItem.TEMP_LIVE_CHAT_ITEM_ID) {
                  composeState.value = composeState.value.copy(contextItem = ComposeContextItem.QuotedItem(cItem))
                }
              }
            }
          }
        }
        val provider = {
          providerForGallery(i, chatItems, cItem.id) { indexInReversed ->
            scope.launch {
              listState.scrollToItem(
                kotlin.math.min(reversedChatItems.lastIndex, indexInReversed + 1),
                -maxHeightRounded
              )
            }
          }
        }
        val voiceWithTransparentBack = cItem.content.msgContent is MsgContent.MCVoice && cItem.content.text.isEmpty() && cItem.quotedItem == null
        if (chat.chatInfo is ChatInfo.Group) {
          if (cItem.chatDir is CIDirection.GroupRcv) {
            val prevItem = if (i < reversedChatItems.lastIndex) reversedChatItems[i + 1] else null
            val member = cItem.chatDir.groupMember
            val showMember = showMemberImage(member, prevItem)
            Row(Modifier.padding(start = 8.dp, end = if (voiceWithTransparentBack) 12.dp else 66.dp).then(swipeableModifier)) {
              if (showMember) {
                val contactId = member.memberContactId
                if (contactId == null) {
                  MemberImage(member)
                } else {
                  Box(
                    Modifier
                      .clip(CircleShape)
                      .clickable {
                        showMemberInfo(chat.chatInfo.groupInfo, member)
                      }
                  ) {
                    MemberImage(member)
                  }
                }
                Spacer(Modifier.size(4.dp))
              } else {
                Spacer(Modifier.size(42.dp))
              }
              ChatItemView(chat.chatInfo, cItem, composeState, provider, showMember = showMember, useLinkPreviews = useLinkPreviews, linkMode = linkMode, deleteMessage = deleteMessage, receiveFile = receiveFile, cancelFile = cancelFile, joinGroup = {}, acceptCall = acceptCall, acceptFeature = acceptFeature, scrollToItem = scrollToItem, setReaction = setReaction, showItemDetails = showItemDetails)
            }
          } else {
            Box(Modifier.padding(start = if (voiceWithTransparentBack) 12.dp else 104.dp, end = 12.dp).then(swipeableModifier)) {
              ChatItemView(chat.chatInfo, cItem, composeState, provider, useLinkPreviews = useLinkPreviews, linkMode = linkMode, deleteMessage = deleteMessage, receiveFile = receiveFile, cancelFile = cancelFile, joinGroup = {}, acceptCall = acceptCall, acceptFeature = acceptFeature, scrollToItem = scrollToItem, setReaction = setReaction, showItemDetails = showItemDetails)
            }
          }
        } else { // direct message
          val sent = cItem.chatDir.sent
          Box(
            Modifier.padding(
              start = if (sent && !voiceWithTransparentBack) 76.dp else 12.dp,
              end = if (sent || voiceWithTransparentBack) 12.dp else 76.dp,
            ).then(swipeableModifier)
          ) {
            ChatItemView(chat.chatInfo, cItem, composeState, provider, useLinkPreviews = useLinkPreviews, linkMode = linkMode, deleteMessage = deleteMessage, receiveFile = receiveFile, cancelFile = cancelFile, joinGroup = joinGroup, acceptCall = acceptCall, acceptFeature = acceptFeature, scrollToItem = scrollToItem, setReaction = setReaction, showItemDetails = showItemDetails)
          }
        }

        if (cItem.isRcvNew) {
          LaunchedEffect(cItem.id) {
            scope.launch {
              delay(600)
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
private fun ScrollToBottom(chatId: ChatId, listState: LazyListState, chatItems: List<ChatItem>) {
  val scope = rememberCoroutineScope()
  // Helps to scroll to bottom after moving from Group to Direct chat
  // and prevents scrolling to bottom on orientation change
  var shouldAutoScroll by rememberSaveable { mutableStateOf(true to chatId) }
  LaunchedEffect(chatId, shouldAutoScroll) {
    if ((shouldAutoScroll.first || shouldAutoScroll.second != chatId) && listState.firstVisibleItemIndex != 0) {
      scope.launch { listState.scrollToItem(0) }
    }
    // Don't autoscroll next time until it will be needed
    shouldAutoScroll = false to chatId
  }
  val scrollDistance = with(LocalDensity.current) { -39.dp.toPx() }
  /*
  * Since we use key with each item in LazyColumn, LazyColumn will not autoscroll to bottom item. We need to do it ourselves.
  * When the first visible item (from bottom) is visible (even partially) we can autoscroll to 0 item. Or just scrollBy small distance otherwise
  * */
  LaunchedEffect(Unit) {
    snapshotFlow { chatItems.lastOrNull()?.id }
      .distinctUntilChanged()
      .filter { listState.layoutInfo.visibleItemsInfo.firstOrNull()?.key != it }
      .collect {
        try {
          if (listState.firstVisibleItemIndex == 0) {
            listState.animateScrollToItem(0)
          } else {
            listState.animateScrollBy(scrollDistance)
          }
        } catch (e: CancellationException) {
          /**
           * When you tap and hold a finger on a lazy column with chatItems, and then you receive a message,
           * this coroutine will be canceled with the message "Current mutation had a higher priority" because of animatedScroll.
           * Which breaks auto-scrolling to bottom. So just ignoring the exception
           * */
        }
      }
  }
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
  var firstItemIsVisible by remember { mutableStateOf(firstVisibleIndex == 0) }

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
  val showDropDown = remember { mutableStateOf(false) }

  TopEndFloatingButton(
    Modifier.padding(end = DEFAULT_PADDING, top = 24.dp).align(Alignment.TopEnd),
    topUnreadCount,
    showButtonWithCounter,
    onClick = { scope.launch { listState.animateScrollBy(height) } },
    onLongClick = { showDropDown.value = true }
  )

  DefaultDropdownMenu(showDropDown, offset = DpOffset(maxWidth - DEFAULT_PADDING, 24.dp + fabSize)) {
    ItemAction(
      generalGetString(MR.strings.mark_read),
      painterResource(MR.images.ic_check),
      onClick = {
        markRead(
          CC.ItemRange(minUnreadItemId, chatItems[chatItems.size - listState.layoutInfo.visibleItemsInfo.lastIndex - 1].id - 1),
          bottomUnreadCount
        )
        showDropDown.value = false
      })
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
        if (lastVisibleItemIndex > (totalItemsNumber - remaining) && totalItemsNumber >= ChatPagination.INITIAL_COUNT)
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
      backgroundColor = MaterialTheme.colors.secondaryVariant,
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
        modifier = Modifier.size(48.dp),
        backgroundColor = MaterialTheme.colors.secondaryVariant,
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
        modifier = Modifier.size(48.dp),
        backgroundColor = MaterialTheme.colors.secondaryVariant,
      ) {
        Icon(
          painter = painterResource(MR.images.ic_keyboard_arrow_down),
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

private fun markUnreadChatAsRead(activeChat: MutableState<Chat?>, chatModel: ChatModel) {
  val chat = activeChat.value
  if (chat?.chatStats?.unreadChat != true) return
  withApi {
    val success = chatModel.controller.apiChatUnread(
      chat.chatInfo.chatType,
      chat.chatInfo.apiId,
      false
    )
    if (success && chat.id == activeChat.value?.id) {
      activeChat.value = chat.copy(chatStats = chat.chatStats.copy(unreadChat = false))
      chatModel.replaceChat(chat.id, activeChat.value!!)
    }
  }
}

sealed class ProviderMedia {
  data class Image(val uri: URI, val image: ImageBitmap): ProviderMedia()
  data class Video(val uri: URI, val preview: String): ProviderMedia()
}

private fun providerForGallery(
  listStateIndex: Int,
  chatItems: List<ChatItem>,
  cItemId: Long,
  scrollTo: (Int) -> Unit
): ImageGalleryProvider {
  fun canShowMedia(item: ChatItem): Boolean =
    (item.content.msgContent is MsgContent.MCImage || item.content.msgContent is MsgContent.MCVideo) && (item.file?.loaded == true && getLoadedFilePath(item.file) != null)

  fun item(skipInternalIndex: Int, initialChatId: Long): Pair<Int, ChatItem>? {
    var processedInternalIndex = -skipInternalIndex.sign
    val indexOfFirst = chatItems.indexOfFirst { it.id == initialChatId }
    for (chatItemsIndex in if (skipInternalIndex >= 0) indexOfFirst downTo 0 else indexOfFirst..chatItems.lastIndex) {
      val item = chatItems[chatItemsIndex]
      if (canShowMedia(item)) {
        processedInternalIndex += skipInternalIndex.sign
      }
      if (processedInternalIndex == skipInternalIndex) {
        return chatItemsIndex to item
      }
    }
    return null
  }

  var initialIndex = Int.MAX_VALUE / 2
  var initialChatId = cItemId
  return object: ImageGalleryProvider {
    override val initialIndex: Int = initialIndex
    override val totalMediaSize = mutableStateOf(Int.MAX_VALUE)
    override fun getMedia(index: Int): ProviderMedia? {
      val internalIndex = initialIndex - index
      val item = item(internalIndex, initialChatId)?.second ?: return null
      return when (item.content.msgContent) {
        is MsgContent.MCImage -> {
          val imageBitmap: ImageBitmap? = getLoadedImage(item.file)
          val filePath = getLoadedFilePath(item.file)
          if (imageBitmap != null && filePath != null) {
            val uri = getAppFileUri(filePath.substringAfterLast(File.separator))
            ProviderMedia.Image(uri, imageBitmap)
          } else null
        }
        is MsgContent.MCVideo -> {
          val filePath = getLoadedFilePath(item.file)
          if (filePath != null) {
            val uri = getAppFileUri(filePath.substringAfterLast(File.separator))
            ProviderMedia.Video(uri, (item.content.msgContent as MsgContent.MCVideo).image)
          } else null
        }
        else -> null
      }
    }

    override fun currentPageChanged(index: Int) {
      val internalIndex = initialIndex - index
      val item = item(internalIndex, initialChatId) ?: return
      initialIndex = index
      initialChatId = item.second.id
    }

    override fun scrollToStart() {
      initialIndex = 0
      initialChatId = chatItems.first { canShowMedia(it) }.id
    }

    override fun onDismiss(index: Int) {
      val internalIndex = initialIndex - index
      val indexInChatItems = item(internalIndex, initialChatId)?.first ?: return
      val indexInReversed = chatItems.lastIndex - indexInChatItems
      // Do not scroll to active item, just to different items
      if (indexInReversed == listStateIndex) return
      scrollTo(indexInReversed)
    }
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

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
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
      chat = Chat(
        chatInfo = ChatInfo.Direct.sampleData,
        chatItems = chatItems,
        chatStats = Chat.ChatStats()
      ),
      unreadCount = unreadCount,
      composeState = remember { mutableStateOf(ComposeState(useLinkPreviews = true)) },
      composeView = {},
      attachmentOption = remember { mutableStateOf<AttachmentOption?>(null) },
      attachmentBottomSheetState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden),
      chatItems = chatItems,
      searchValue,
      useLinkPreviews = true,
      linkMode = SimplexLinkMode.DESCRIPTION,
      chatModelIncognito = false,
      back = {},
      info = {},
      showMemberInfo = { _, _ -> },
      loadPrevMessages = { _ -> },
      deleteMessage = { _, _ -> },
      receiveFile = {},
      cancelFile = {},
      joinGroup = {},
      startCall = {},
      acceptCall = { _ -> },
      acceptFeature = { _, _, _ -> },
      setReaction = { _, _, _, _ -> },
      showItemDetails = { _, _ -> },
      addMembers = { _ -> },
      markRead = { _, _ -> },
      changeNtfsState = { _, _ -> },
      onSearchValueChanged = {},
      onComposed = {},
    )
  }
}

@Preview
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
      chat = Chat(
        chatInfo = ChatInfo.Group.sampleData,
        chatItems = chatItems,
        chatStats = Chat.ChatStats()
      ),
      unreadCount = unreadCount,
      composeState = remember { mutableStateOf(ComposeState(useLinkPreviews = true)) },
      composeView = {},
      attachmentOption = remember { mutableStateOf<AttachmentOption?>(null) },
      attachmentBottomSheetState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden),
      chatItems = chatItems,
      searchValue,
      useLinkPreviews = true,
      linkMode = SimplexLinkMode.DESCRIPTION,
      chatModelIncognito = false,
      back = {},
      info = {},
      showMemberInfo = { _, _ -> },
      loadPrevMessages = { _ -> },
      deleteMessage = { _, _ -> },
      receiveFile = {},
      cancelFile = {},
      joinGroup = {},
      startCall = {},
      acceptCall = { _ -> },
      acceptFeature = { _, _, _ -> },
      setReaction = { _, _, _, _ -> },
      showItemDetails = { _, _ -> },
      addMembers = { _ -> },
      markRead = { _, _ -> },
      changeNtfsState = { _, _ -> },
      onSearchValueChanged = {},
      onComposed = {},
    )
  }
}
