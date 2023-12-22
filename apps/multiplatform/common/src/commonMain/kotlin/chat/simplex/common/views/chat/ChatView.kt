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
import androidx.compose.ui.*
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.intl.Locale
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.text.*
import androidx.compose.ui.unit.*
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
import chat.simplex.res.MR
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*
import kotlinx.datetime.Clock
import java.io.File
import java.net.URI
import kotlin.math.sign

@Composable
fun ChatView(chatId: String, chatModel: ChatModel, onComposed: suspend (chatId: String) -> Unit) {
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
        .onEach { Log.d(TAG, "TODOCHAT: chatId: activeChatId ${activeChat.value?.id} == new chatId $it ${activeChat.value?.id == it} ") }
        .filter { it != null && activeChat.value?.id != it }
        .collect { chatId ->
          // Redisplay the whole hierarchy if the chat is different to make going from groups to direct chat working correctly
          // Also for situation when chatId changes after clicking in notification, etc
          activeChat.value = chatModel.getChat(chatId!!)
          Log.d(TAG, "TODOCHAT: chatId: activeChatId became ${activeChat.value?.id}")
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
        .onEach { Log.d(TAG, "TODOCHAT: chats: activeChatId ${activeChat.value?.id} == new chatId ${it?.id} ${activeChat.value?.id == it?.id} ") }
        // Only changed chatInfo is important thing. Other properties can be skipped for reducing recompositions
        .filter { it != null && it?.chatInfo != activeChat.value?.chatInfo }
        .collect {
          activeChat.value = it
          Log.d(TAG, "TODOCHAT: chats: activeChatId became ${activeChat.value?.id}")
        }
    }
  }
  val view = LocalMultiplatformView()
  val chat = activeChat.value
  if (chat == null || user == null) {
    chatModel.chatId.value = null
    ModalManager.end.closeModals()
  } else {
    val chatRh = chat.remoteHostId
    // We need to have real unreadCount value for displaying it inside top right button
    // Having activeChat reloaded on every change in it is inefficient (UI lags)
    val unreadCount = remember {
      derivedStateOf {
        chatModel.chats.firstOrNull { chat -> chat.chatInfo.id == chatModel.chatId.value }?.chatStats?.unreadCount ?: 0
      }
    }
    val clipboard = LocalClipboardManager.current

    ChatLayout(
      chat,
      unreadCount,
      composeState,
      composeView = {
        Column(
          Modifier.fillMaxWidth(),
          horizontalAlignment = Alignment.CenterHorizontally
        ) {
          if (
            chat.chatInfo is ChatInfo.Direct
            && !chat.chatInfo.contact.ready
            && chat.chatInfo.contact.active
            && !chat.chatInfo.contact.nextSendGrpInv
          ) {
            Text(
              generalGetString(MR.strings.contact_connection_pending),
              Modifier.padding(top = 4.dp),
              fontSize = 14.sp,
              color = MaterialTheme.colors.secondary
            )
          }
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
      back = {
        hideKeyboard(view)
        AudioPlayer.stop()
        chatModel.chatId.value = null
        chatModel.groupMembers.clear()
      },
      info = {
        if (ModalManager.end.hasModalsOpen()) {
          ModalManager.end.closeModals()
          return@ChatLayout
        }
        hideKeyboard(view)
        withApi {
          // The idea is to preload information before showing a modal because large groups can take time to load all members
          var preloadedContactInfo: Pair<ConnectionStats?, Profile?>? = null
          var preloadedCode: String? = null
          var preloadedLink: Pair<String, GroupMemberRole>? = null
          if (chat.chatInfo is ChatInfo.Direct) {
            preloadedContactInfo = chatModel.controller.apiContactInfo(chatRh, chat.chatInfo.apiId)
            preloadedCode = chatModel.controller.apiGetContactCode(chatRh, chat.chatInfo.apiId)?.second
          } else if (chat.chatInfo is ChatInfo.Group) {
            setGroupMembers(chatRh, chat.chatInfo.groupInfo, chatModel)
            preloadedLink = chatModel.controller.apiGetGroupLink(chatRh, chat.chatInfo.groupInfo.groupId)
          }
          ModalManager.end.showModalCloseable(true) { close ->
            val chat = remember { activeChat }.value
            if (chat?.chatInfo is ChatInfo.Direct) {
              var contactInfo: Pair<ConnectionStats?, Profile?>? by remember { mutableStateOf(preloadedContactInfo) }
              var code: String? by remember { mutableStateOf(preloadedCode) }
              KeyChangeEffect(chat.id, ChatModel.networkStatuses.toMap()) {
                contactInfo = chatModel.controller.apiContactInfo(chatRh, chat.chatInfo.apiId)
                preloadedContactInfo = contactInfo
                code = chatModel.controller.apiGetContactCode(chatRh, chat.chatInfo.apiId)?.second
                preloadedCode = code
              }
              ChatInfoView(chatModel, (chat.chatInfo as ChatInfo.Direct).contact, contactInfo?.first, contactInfo?.second, chat.chatInfo.localAlias, code, close)
            } else if (chat?.chatInfo is ChatInfo.Group) {
              var link: Pair<String, GroupMemberRole>? by remember(chat.id) { mutableStateOf(preloadedLink) }
              KeyChangeEffect(chat.id) {
                setGroupMembers(chatRh, (chat.chatInfo as ChatInfo.Group).groupInfo, chatModel)
                link = chatModel.controller.apiGetGroupLink(chatRh, chat.chatInfo.groupInfo.groupId)
                preloadedLink = link
              }
              GroupChatInfoView(chatModel, chatRh, chat.id, link?.first, link?.second, {
                link = it
                preloadedLink = it
              }, close)
            }
          }
        }
      },
      showMemberInfo = { groupInfo: GroupInfo, member: GroupMember ->
        hideKeyboard(view)
        withApi {
          val r = chatModel.controller.apiGroupMemberInfo(chatRh, groupInfo.groupId, member.groupMemberId)
          val stats = r?.second
          val (_, code) = if (member.memberActive) {
            val memCode = chatModel.controller.apiGetGroupMemberCode(chatRh, groupInfo.apiId, member.groupMemberId)
            member to memCode?.second
          } else {
            member to null
          }
          setGroupMembers(chatRh, groupInfo, chatModel)
          ModalManager.end.closeModals()
          ModalManager.end.showModalCloseable(true) { close ->
            remember { derivedStateOf { chatModel.getGroupMember(member.groupMemberId) } }.value?.let { mem ->
              GroupMemberInfoView(chatRh, groupInfo, mem, stats, code, chatModel, close, close)
            }
          }
        }
      },
      loadPrevMessages = {
        if (chatModel.chatId.value != activeChat.value?.id) return@ChatLayout
        val c = chatModel.getChat(chatModel.chatId.value ?: return@ChatLayout)
        val firstId = chatModel.chatItems.firstOrNull()?.id
        if (c != null && firstId != null) {
          withApi {
            Log.d(TAG, "TODOCHAT: loadPrevMessages: loading for ${c.id}, current chatId ${ChatModel.chatId.value}, size was ${ChatModel.chatItems.size}")
            apiLoadPrevMessages(c, chatModel, firstId, searchText.value)
            Log.d(TAG, "TODOCHAT: loadPrevMessages: loaded for ${c.id}, current chatId ${ChatModel.chatId.value}, size now ${ChatModel.chatItems.size}")
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
              chatRh,
              groupId = groupInfo.groupId,
              groupMemberId = groupMember.groupMemberId,
              itemId = itemId
            )
            deletedChatItem = r?.first
            toChatItem = r?.second
          } else {
            val r = chatModel.controller.apiDeleteChatItem(
              chatRh,
              type = cInfo.chatType,
              id = cInfo.apiId,
              itemId = itemId,
              mode = mode
            )
            deletedChatItem = r?.deletedChatItem?.chatItem
            toChatItem = r?.toChatItem?.chatItem
          }
          if (toChatItem == null && deletedChatItem != null) {
            chatModel.removeChatItem(chatRh, cInfo, deletedChatItem)
          } else if (toChatItem != null) {
            chatModel.upsertChatItem(chatRh, cInfo, toChatItem)
          }
        }
      },
      deleteMessages = { itemIds ->
        if (itemIds.isNotEmpty()) {
          val chatInfo = chat.chatInfo
          withBGApi {
            val deletedItems: ArrayList<ChatItem> = arrayListOf()
            for (itemId in itemIds) {
              val di = chatModel.controller.apiDeleteChatItem(
                chatRh, chatInfo.chatType, chatInfo.apiId, itemId, CIDeleteMode.cidmInternal
              )?.deletedChatItem?.chatItem
              if (di != null) {
                deletedItems.add(di)
              }
            }
            for (di in deletedItems) {
              chatModel.removeChatItem(chatRh, chatInfo, di)
            }
          }
        }
      },
      receiveFile = { fileId, encrypted ->
        withApi { chatModel.controller.receiveFile(chatRh, user, fileId, encrypted) }
      },
      cancelFile = { fileId ->
        withApi { chatModel.controller.cancelFile(chatRh, user, fileId) }
      },
      joinGroup = { groupId, onComplete ->
        withApi {
          chatModel.controller.apiJoinGroup(chatRh, groupId)
          onComplete.invoke()
        }
      },
      startCall = out@ { media ->
        withBGApi {
          val cInfo = chat.chatInfo
          if (cInfo is ChatInfo.Direct) {
            chatModel.activeCall.value = Call(remoteHostId = chatRh, contact = cInfo.contact, callState = CallState.WaitCapabilities, localMedia = media)
            chatModel.showCallView.value = true
            chatModel.callCommand.add(WCallCommand.Capabilities(media))
          }
        }
      },
      endCall = {
        val call = chatModel.activeCall.value
        if (call != null) withApi { chatModel.callManager.endCall(call) }
      },
      acceptCall = { contact ->
        hideKeyboard(view)
        val invitation = chatModel.callInvitations.remove(contact.id)
        if (invitation == null) {
          AlertManager.shared.showAlertMsg(generalGetString(MR.strings.call_already_ended))
        } else {
          chatModel.callManager.acceptIncomingCall(invitation = invitation)
        }
      },
      acceptFeature = { contact, feature, param ->
        withApi {
          chatModel.controller.allowFeatureToContact(chatRh, contact, feature, param)
        }
      },
      openDirectChat = { contactId ->
        withApi {
          openDirectChat(chatRh, contactId, chatModel)
        }
      },
      updateContactStats = { contact ->
        withApi {
          val r = chatModel.controller.apiContactInfo(chatRh, chat.chatInfo.apiId)
          if (r != null) {
            val contactStats = r.first
            if (contactStats != null)
            chatModel.updateContactConnectionStats(chatRh, contact, contactStats)
          }
        }
      },
      updateMemberStats = { groupInfo, member ->
        withApi {
          val r = chatModel.controller.apiGroupMemberInfo(chatRh, groupInfo.groupId, member.groupMemberId)
          if (r != null) {
            val memStats = r.second
            if (memStats != null) {
              chatModel.updateGroupMemberConnectionStats(chatRh, groupInfo, r.first, memStats)
            }
          }
        }
      },
      syncContactConnection = { contact ->
        withApi {
          val cStats = chatModel.controller.apiSyncContactRatchet(chatRh, contact.contactId, force = false)
          if (cStats != null) {
            chatModel.updateContactConnectionStats(chatRh, contact, cStats)
          }
        }
      },
      syncMemberConnection = { groupInfo, member ->
        withApi {
          val r = chatModel.controller.apiSyncGroupMemberRatchet(chatRh, groupInfo.apiId, member.groupMemberId, force = false)
          if (r != null) {
            chatModel.updateGroupMemberConnectionStats(chatRh, groupInfo, r.first, r.second)
          }
        }
      },
      findModelChat = { chatId ->
        chatModel.getChat(chatId)
      },
      findModelMember = { memberId ->
        chatModel.groupMembers.find { it.id == memberId }
      },
      setReaction = { cInfo, cItem, add, reaction ->
        withApi {
          val updatedCI = chatModel.controller.apiChatItemReaction(
            rh = chatRh,
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
          val ciInfo = chatModel.controller.apiGetChatItemInfo(chatRh, cInfo.chatType, cInfo.apiId, cItem.id)
          if (ciInfo != null) {
            if (chat.chatInfo is ChatInfo.Group) {
              setGroupMembers(chatRh, chat.chatInfo.groupInfo, chatModel)
            }
            ModalManager.end.closeModals()
            ModalManager.end.showModal(endButtons = { ShareButton {
              clipboard.shareText(itemInfoShareText(chatModel, cItem, ciInfo, chatModel.controller.appPrefs.developerTools.get()))
            } }) {
              ChatItemInfoView(chatModel, cItem, ciInfo, devTools = chatModel.controller.appPrefs.developerTools.get())
            }
          }
        }
      },
      addMembers = { groupInfo ->
        hideKeyboard(view)
        withApi {
          setGroupMembers(chatRh, groupInfo, chatModel)
          ModalManager.end.closeModals()
          ModalManager.end.showModalCloseable(true) { close ->
            AddGroupMembersView(chatRh, groupInfo, false, chatModel, close)
          }
        }
      },
      openGroupLink = { groupInfo ->
        hideKeyboard(view)
        withApi {
          val link = chatModel.controller.apiGetGroupLink(chatRh, groupInfo.groupId)
          ModalManager.end.closeModals()
          ModalManager.end.showModalCloseable(true) {
            GroupLinkView(chatModel, chatRh, groupInfo, link?.first, link?.second, onGroupLinkUpdated = null)
          }
        }
      },
      markRead = { range, unreadCountAfter ->
        chatModel.markChatItemsRead(chat, range, unreadCountAfter)
        ntfManager.cancelNotificationsForChat(chat.id)
        withBGApi {
          chatModel.controller.apiChatRead(
            chatRh,
            chat.chatInfo.chatType,
            chat.chatInfo.apiId,
            range
          )
        }
      },
      changeNtfsState = { enabled, currentValue -> toggleNotifications(chat, enabled, chatModel, currentValue) },
      onSearchValueChanged = { value ->
        if (searchText.value == value) return@ChatLayout
        if (chatModel.chatId.value != activeChat.value?.id) return@ChatLayout
        val c = chatModel.getChat(chatModel.chatId.value ?: return@ChatLayout) ?: return@ChatLayout
        withApi {
          apiFindMessages(c, chatModel, value)
          searchText.value = value
        }
      },
      onComposed,
      developerTools = chatModel.controller.appPrefs.developerTools.get(),
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
  back: () -> Unit,
  info: () -> Unit,
  showMemberInfo: (GroupInfo, GroupMember) -> Unit,
  loadPrevMessages: () -> Unit,
  deleteMessage: (Long, CIDeleteMode) -> Unit,
  deleteMessages: (List<Long>) -> Unit,
  receiveFile: (Long, Boolean) -> Unit,
  cancelFile: (Long) -> Unit,
  joinGroup: (Long, () -> Unit) -> Unit,
  startCall: (CallMediaType) -> Unit,
  endCall: () -> Unit,
  acceptCall: (Contact) -> Unit,
  acceptFeature: (Contact, ChatFeature, Int?) -> Unit,
  openDirectChat: (Long) -> Unit,
  updateContactStats: (Contact) -> Unit,
  updateMemberStats: (GroupInfo, GroupMember) -> Unit,
  syncContactConnection: (Contact) -> Unit,
  syncMemberConnection: (GroupInfo, GroupMember) -> Unit,
  findModelChat: (String) -> Chat?,
  findModelMember: (String) -> GroupMember?,
  setReaction: (ChatInfo, ChatItem, Boolean, MsgReaction) -> Unit,
  showItemDetails: (ChatInfo, ChatItem) -> Unit,
  addMembers: (GroupInfo) -> Unit,
  openGroupLink: (GroupInfo) -> Unit,
  markRead: (CC.ItemRange, unreadCountAfter: Int?) -> Unit,
  changeNtfsState: (Boolean, currentValue: MutableState<Boolean>) -> Unit,
  onSearchValueChanged: (String) -> Unit,
  onComposed: suspend (chatId: String) -> Unit,
  developerTools: Boolean,
) {
  val scope = rememberCoroutineScope()
  val attachmentDisabled = remember { derivedStateOf { composeState.value.attachmentDisabled } }
  Box(
    Modifier
      .fillMaxWidth()
      .desktopOnExternalDrag(
        enabled = !attachmentDisabled.value && rememberUpdatedState(chat.userCanSend).value,
        onFiles = { paths -> composeState.onFilesAttached(paths.map { it.toURI() }) },
        onImage = {
          // TODO: file is not saved anywhere?!
          val tmpFile = File.createTempFile("image", ".bmp", tmpDir)
          tmpFile.deleteOnExit()
          chatModel.filesToDelete.add(tmpFile)
          val uri = tmpFile.toURI()
          CoroutineScope(Dispatchers.IO).launch { composeState.processPickedMedia(listOf(uri), null) }
        },
        onText = {
          // Need to parse HTML in order to correctly display the content
          //composeState.value = composeState.value.copy(message = composeState.value.message + it)
        },
      )
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
          topBar = { ChatInfoToolbar(chat, back, info, startCall, endCall, addMembers, openGroupLink, changeNtfsState, onSearchValueChanged) },
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
              useLinkPreviews, linkMode, showMemberInfo, loadPrevMessages, deleteMessage, deleteMessages,
              receiveFile, cancelFile, joinGroup, acceptCall, acceptFeature, openDirectChat,
              updateContactStats, updateMemberStats, syncContactConnection, syncMemberConnection, findModelChat, findModelMember,
              setReaction, showItemDetails, markRead, setFloatingButton, onComposed, developerTools,
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
  endCall: () -> Unit,
  addMembers: (GroupInfo) -> Unit,
  openGroupLink: (GroupInfo) -> Unit,
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
  if (appPlatform.isAndroid) {
    BackHandler(onBack = onBackClicked)
  }
  val barButtons = arrayListOf<@Composable RowScope.() -> Unit>()
  val menuItems = arrayListOf<@Composable () -> Unit>()
  val activeCall by remember { chatModel.activeCall }
  menuItems.add {
    ItemAction(stringResource(MR.strings.search_verb), painterResource(MR.images.ic_search), onClick = {
      showMenu.value = false
      showSearch = true
    })
  }

  if (chat.chatInfo is ChatInfo.Direct && chat.chatInfo.contact.allowsFeature(ChatFeature.Calls)) {
    if (activeCall == null) {
      barButtons.add {
        if (appPlatform.isAndroid) {
          IconButton({
            showMenu.value = false
            startCall(CallMediaType.Audio)
          }, enabled = chat.chatInfo.contact.ready && chat.chatInfo.contact.active
          ) {
            Icon(
              painterResource(MR.images.ic_call_500),
              stringResource(MR.strings.icon_descr_audio_call).capitalize(Locale.current),
              tint = if (chat.chatInfo.contact.ready && chat.chatInfo.contact.active) MaterialTheme.colors.primary else MaterialTheme.colors.secondary
            )
          }
        } else {
          IconButton({
            showMenu.value = false
            startCall(CallMediaType.Video)
          }, enabled = chat.chatInfo.contact.ready && chat.chatInfo.contact.active
          ) {
            Icon(
              painterResource(MR.images.ic_videocam),
              stringResource(MR.strings.icon_descr_video_call).capitalize(Locale.current),
              tint = if (chat.chatInfo.contact.ready && chat.chatInfo.contact.active) MaterialTheme.colors.primary else MaterialTheme.colors.secondary
            )
          }
        }
      }
    } else if (activeCall?.contact?.id == chat.id) {
      barButtons.add {
        val call = remember { chatModel.activeCall }.value
        val connectedAt = call?.connectedAt
        if (connectedAt != null) {
          val time = remember { mutableStateOf(durationText(0)) }
          LaunchedEffect(Unit) {
            while (true) {
              time.value = durationText((Clock.System.now() - connectedAt).inWholeSeconds.toInt())
              delay(250)
            }
          }
          val sp50 = with(LocalDensity.current) { 50.sp.toDp() }
          Text(time.value, Modifier.widthIn(min = sp50))
        }
      }
      barButtons.add {
        IconButton({
          showMenu.value = false
          endCall()
        }) {
          Icon(
            painterResource(MR.images.ic_call_end_filled),
            null,
            tint = MaterialTheme.colors.error
          )
        }
      }
    }
    if (chat.chatInfo.contact.ready && chat.chatInfo.contact.active && activeCall == null) {
      menuItems.add {
        if (appPlatform.isAndroid) {
          ItemAction(stringResource(MR.strings.icon_descr_video_call).capitalize(Locale.current), painterResource(MR.images.ic_videocam), onClick = {
            showMenu.value = false
            startCall(CallMediaType.Video)
          })
        } else {
          ItemAction(stringResource(MR.strings.icon_descr_audio_call).capitalize(Locale.current), painterResource(MR.images.ic_call_500), onClick = {
            showMenu.value = false
            startCall(CallMediaType.Audio)
          })
        }
      }
    }
  } else if (chat.chatInfo is ChatInfo.Group && chat.chatInfo.groupInfo.canAddMembers) {
    if (!chat.chatInfo.incognito) {
      barButtons.add {
        IconButton({
          showMenu.value = false
          addMembers(chat.chatInfo.groupInfo)
        }) {
          Icon(painterResource(MR.images.ic_person_add_500), stringResource(MR.strings.icon_descr_add_members), tint = MaterialTheme.colors.primary)
        }
      }
    } else {
      barButtons.add {
        IconButton({
          showMenu.value = false
          openGroupLink(chat.chatInfo.groupInfo)
        }) {
          Icon(painterResource(MR.images.ic_add_link), stringResource(MR.strings.group_link), tint = MaterialTheme.colors.primary)
        }
      }
    }
  }
  if ((chat.chatInfo is ChatInfo.Direct && chat.chatInfo.contact.ready && chat.chatInfo.contact.active) || chat.chatInfo is ChatInfo.Group) {
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
  }

  barButtons.add {
    IconButton({ showMenu.value = true }) {
      Icon(MoreVertFilled, stringResource(MR.strings.icon_descr_more_button), tint = MaterialTheme.colors.primary)
    }
  }

  DefaultTopAppBar(
    navigationButton = { if (appPlatform.isAndroid || showSearch) { NavigationButtonBack(onBackClicked) }  },
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
  showMemberInfo: (GroupInfo, GroupMember) -> Unit,
  loadPrevMessages: () -> Unit,
  deleteMessage: (Long, CIDeleteMode) -> Unit,
  deleteMessages: (List<Long>) -> Unit,
  receiveFile: (Long, Boolean) -> Unit,
  cancelFile: (Long) -> Unit,
  joinGroup: (Long, () -> Unit) -> Unit,
  acceptCall: (Contact) -> Unit,
  acceptFeature: (Contact, ChatFeature, Int?) -> Unit,
  openDirectChat: (Long) -> Unit,
  updateContactStats: (Contact) -> Unit,
  updateMemberStats: (GroupInfo, GroupMember) -> Unit,
  syncContactConnection: (Contact) -> Unit,
  syncMemberConnection: (GroupInfo, GroupMember) -> Unit,
  findModelChat: (String) -> Chat?,
  findModelMember: (String) -> GroupMember?,
  setReaction: (ChatInfo, ChatItem, Boolean, MsgReaction) -> Unit,
  showItemDetails: (ChatInfo, ChatItem) -> Unit,
  markRead: (CC.ItemRange, unreadCountAfter: Int?) -> Unit,
  setFloatingButton: (@Composable () -> Unit) -> Unit,
  onComposed: suspend (chatId: String) -> Unit,
  developerTools: Boolean,
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

  PreloadItems(listState, ChatPagination.UNTIL_PRELOAD_COUNT, loadPrevMessages)

  Spacer(Modifier.size(8.dp))
  val reversedChatItems by remember { derivedStateOf { chatItems.reversed().toList() } }
  val maxHeightRounded = with(LocalDensity.current) { maxHeight.roundToPx() }
  val scrollToItem: (Long) -> Unit = { itemId: Long ->
    val index = reversedChatItems.indexOfFirst { it.id == itemId }
    if (index != -1) {
      scope.launch { listState.animateScrollToItem(kotlin.math.min(reversedChatItems.lastIndex, index + 1), -maxHeightRounded) }
    }
  }
  LaunchedEffect(chat.id) {
    var stopListening = false
    snapshotFlow { listState.layoutInfo.visibleItemsInfo.lastIndex }
      .distinctUntilChanged()
      .filter { !stopListening }
      .collect {
        onComposed(chat.id)
        stopListening = true
      }
  }
  DisposableEffectOnGone(
    whenGone = {
      VideoPlayerHolder.releaseAll()
    }
  )
  LazyColumn(Modifier.align(Alignment.BottomCenter), state = listState, reverseLayout = true) {
    itemsIndexed(reversedChatItems, key = { _, item -> item.id }) { i, cItem ->
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

        val revealed = remember { mutableStateOf(false) }

        @Composable
        fun ChatItemViewShortHand(cItem: ChatItem, range: IntRange?) {
          tryOrShowError("${cItem.id}ChatItem", error = {
            CIBrokenComposableView(if (cItem.chatDir.sent) Alignment.CenterEnd else Alignment.CenterStart)
          }) {
            ChatItemView(chat.chatInfo, cItem, composeState, provider, useLinkPreviews = useLinkPreviews, linkMode = linkMode, revealed = revealed, range = range, deleteMessage = deleteMessage, deleteMessages = deleteMessages, receiveFile = receiveFile, cancelFile = cancelFile, joinGroup = joinGroup, acceptCall = acceptCall, acceptFeature = acceptFeature, openDirectChat = openDirectChat, updateContactStats = updateContactStats, updateMemberStats = updateMemberStats, syncContactConnection = syncContactConnection, syncMemberConnection = syncMemberConnection, findModelChat = findModelChat, findModelMember = findModelMember, scrollToItem = scrollToItem, setReaction = setReaction, showItemDetails = showItemDetails, developerTools = developerTools)
          }
        }

        @Composable
        fun ChatItemView(cItem: ChatItem, range: IntRange?, prevItem: ChatItem?) {
          val voiceWithTransparentBack = cItem.content.msgContent is MsgContent.MCVoice && cItem.content.text.isEmpty() && cItem.quotedItem == null
          if (chat.chatInfo is ChatInfo.Group) {
            if (cItem.chatDir is CIDirection.GroupRcv) {
              val member = cItem.chatDir.groupMember
              val (prevMember, memCount) =
                if (range != null) {
                  chatModel.getPrevHiddenMember(member, range)
                } else {
                  null to 1
                }
              if (prevItem == null || showMemberImage(member, prevItem) || prevMember != null) {
                Column(
                  Modifier
                    .padding(top = 8.dp)
                    .padding(start = 8.dp, end = if (voiceWithTransparentBack) 12.dp else 66.dp),
                  verticalArrangement = Arrangement.spacedBy(4.dp),
                  horizontalAlignment = Alignment.Start
                ) {
                  if (cItem.content.showMemberName) {
                    Text(
                      memberNames(member, prevMember, memCount),
                      Modifier.padding(start = MEMBER_IMAGE_SIZE + 10.dp),
                      style = TextStyle(fontSize = 13.5.sp, color = CurrentColors.value.colors.secondary)
                    )
                  }
                  Row(
                    swipeableModifier,
                    horizontalArrangement = Arrangement.spacedBy(4.dp)
                  ) {
                    Box(
                      Modifier
                        .clip(CircleShape)
                        .clickable {
                          showMemberInfo(chat.chatInfo.groupInfo, member)
                        }
                    ) {
                      MemberImage(member)
                    }
                    ChatItemViewShortHand(cItem, range)
                  }
                }
              } else {
                Row(
                  Modifier
                    .padding(start = 8.dp + MEMBER_IMAGE_SIZE + 4.dp, end = if (voiceWithTransparentBack) 12.dp else 66.dp)
                    .then(swipeableModifier)
                ) {
                  ChatItemViewShortHand(cItem, range)
                }
              }
            } else {
              Box(
                Modifier
                  .padding(start = if (voiceWithTransparentBack) 12.dp else 104.dp, end = 12.dp)
                  .then(swipeableModifier)
              ) {
                ChatItemViewShortHand(cItem, range)
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
              ChatItemViewShortHand(cItem, range)
            }
          }
        }

        val (currIndex, nextItem) = chatModel.getNextChatItem(cItem)
        val ciCategory = cItem.mergeCategory
        if (ciCategory != null && ciCategory == nextItem?.mergeCategory) {
          // memberConnected events and deleted items are aggregated at the last chat item in a row, see ChatItemView
        } else {
          val (prevHidden, prevItem) = chatModel.getPrevShownChatItem(currIndex, ciCategory)
          val range = chatViewItemsRange(currIndex, prevHidden)
          if (revealed.value && range != null) {
            reversedChatItems.subList(range.first, range.last + 1).forEachIndexed { index, ci ->
              val prev = if (index + range.first == prevHidden) prevItem else reversedChatItems[index + range.first + 1]
              ChatItemView(ci, null, prev)
            }
          } else {
            ChatItemView(cItem, range, prevItem)
          }
        }

        if (cItem.isRcvNew && chat.id == ChatModel.chatId.value) {
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
          if (listState.firstVisibleItemIndex == 0 || (listState.firstVisibleItemIndex == 1 && listState.layoutInfo.totalItemsCount == chatItems.size)) {
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

  Box {
    DefaultDropdownMenu(showDropDown, offset = DpOffset(this@FloatingButtons.maxWidth - DEFAULT_PADDING, 24.dp + fabSize)) {
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
}

@Composable
fun PreloadItems(
  listState: LazyListState,
  remaining: Int = 10,
  onLoadMore: () -> Unit,
) {
  // Prevent situation when initial load and load more happens one after another after selecting a chat with long scroll position from previous selection
  val allowLoad = remember { mutableStateOf(false) }
  LaunchedEffect(Unit) {
    snapshotFlow { chatModel.chatId.value }
      .filterNotNull()
      .collect {
        allowLoad.value = listState.layoutInfo.totalItemsCount == listState.layoutInfo.visibleItemsInfo.size
        delay(500)
        allowLoad.value = true
      }
  }
  KeyChangeEffect(allowLoad.value) {
    snapshotFlow {
      val lInfo = listState.layoutInfo
      val totalItemsNumber = lInfo.totalItemsCount
      val lastVisibleItemIndex = (lInfo.visibleItemsInfo.lastOrNull()?.index ?: 0) + 1
      if (allowLoad.value && lastVisibleItemIndex > (totalItemsNumber - remaining) && totalItemsNumber >= ChatPagination.INITIAL_COUNT)
        totalItemsNumber + ChatPagination.PRELOAD_COUNT
      else
        0
    }
      .filter { it > 0 }
      .collect {
        onLoadMore()
      }
  }
}

private fun showMemberImage(member: GroupMember, prevItem: ChatItem?): Boolean =
  when (val dir = prevItem?.chatDir) {
    is CIDirection.GroupSnd -> true
    is CIDirection.GroupRcv -> dir.groupMember.groupMemberId != member.groupMemberId
    else -> false
  }

val MEMBER_IMAGE_SIZE: Dp = 38.dp

@Composable
fun MemberImage(member: GroupMember) {
  ProfileImage(MEMBER_IMAGE_SIZE, member.memberProfile.image)
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
    val chatRh = chat.remoteHostId
    val success = chatModel.controller.apiChatUnread(
      chatRh,
      chat.chatInfo.chatType,
      chat.chatInfo.apiId,
      false
    )
    if (success && chat.id == activeChat.value?.id) {
      activeChat.value = chat.copy(chatStats = chat.chatStats.copy(unreadChat = false))
      chatModel.replaceChat(chatRh, chat.id, activeChat.value!!)
    }
  }
}

@Composable
private fun memberNames(member: GroupMember, prevMember: GroupMember?, memCount: Int): String {
  val name = member.displayName
  val prevName = prevMember?.displayName
  return if (prevName != null) {
    if (memCount > 2) {
      stringResource(MR.strings.group_members_n).format(name, prevName, memCount - 2)
    } else {
      stringResource(MR.strings.group_members_2).format(name, prevName)
    }
  } else {
    name
  }
}

fun chatViewItemsRange(currIndex: Int?, prevHidden: Int?): IntRange? =
  if (currIndex != null && prevHidden != null && prevHidden > currIndex) {
    currIndex..prevHidden
  } else {
    null
  }


sealed class ProviderMedia {
  data class Image(val data: ByteArray, val image: ImageBitmap): ProviderMedia()
  data class Video(val uri: URI, val preview: String): ProviderMedia()
}

private fun providerForGallery(
  listStateIndex: Int,
  chatItems: List<ChatItem>,
  cItemId: Long,
  scrollTo: (Int) -> Unit
): ImageGalleryProvider {
  fun canShowMedia(item: ChatItem): Boolean =
    (item.content.msgContent is MsgContent.MCImage || item.content.msgContent is MsgContent.MCVideo) && (item.file?.loaded == true && (getLoadedFilePath(item.file) != null || chatModel.connectedToRemote()))

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
          val res = runBlocking { getLoadedImage(item.file) }
          val filePath = getLoadedFilePath(item.file)
          if (res != null && filePath != null) {
            val (imageBitmap: ImageBitmap, data: ByteArray) = res
            ProviderMedia.Image(data, imageBitmap)
          } else null
        }
        is MsgContent.MCVideo -> {
          val filePath = if (chatModel.connectedToRemote() && item.file?.loaded == true) getAppFilePath(item.file.fileName) else getLoadedFilePath(item.file)
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
        remoteHostId = null,
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
      back = {},
      info = {},
      showMemberInfo = { _, _ -> },
      loadPrevMessages = {},
      deleteMessage = { _, _ -> },
      deleteMessages = { _ -> },
      receiveFile = { _, _ -> },
      cancelFile = {},
      joinGroup = { _, _ -> },
      startCall = {},
      endCall = {},
      acceptCall = { _ -> },
      acceptFeature = { _, _, _ -> },
      openDirectChat = { _ -> },
      updateContactStats = { },
      updateMemberStats = { _, _ -> },
      syncContactConnection = { },
      syncMemberConnection = { _, _ -> },
      findModelChat = { null },
      findModelMember = { null },
      setReaction = { _, _, _, _ -> },
      showItemDetails = { _, _ -> },
      addMembers = { _ -> },
      openGroupLink = {},
      markRead = { _, _ -> },
      changeNtfsState = { _, _ -> },
      onSearchValueChanged = {},
      onComposed = {},
      developerTools = false,
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
        remoteHostId = null,
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
      back = {},
      info = {},
      showMemberInfo = { _, _ -> },
      loadPrevMessages = {},
      deleteMessage = { _, _ -> },
      deleteMessages = {},
      receiveFile = { _, _ -> },
      cancelFile = {},
      joinGroup = { _, _ -> },
      startCall = {},
      endCall = {},
      acceptCall = { _ -> },
      acceptFeature = { _, _, _ -> },
      openDirectChat = { _ -> },
      updateContactStats = { },
      updateMemberStats = { _, _ -> },
      syncContactConnection = { },
      syncMemberConnection = { _, _ -> },
      findModelChat = { null },
      findModelMember = { null },
      setReaction = { _, _, _, _ -> },
      showItemDetails = { _, _ -> },
      addMembers = { _ -> },
      openGroupLink = {},
      markRead = { _, _ -> },
      changeNtfsState = { _, _ -> },
      onSearchValueChanged = {},
      onComposed = {},
      developerTools = false,
    )
  }
}
