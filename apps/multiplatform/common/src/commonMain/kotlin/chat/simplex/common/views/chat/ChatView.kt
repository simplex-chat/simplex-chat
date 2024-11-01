package chat.simplex.common.views.chat

import androidx.compose.animation.*
import androidx.compose.animation.core.*
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.gestures.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.*
import androidx.compose.ui.draw.*
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.layer.GraphicsLayer
import androidx.compose.ui.layout.layoutId
import androidx.compose.ui.layout.onSizeChanged
import androidx.compose.ui.platform.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.intl.Locale
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.text.*
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.model.CIDirection.GroupRcv
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.model.ChatModel.withChats
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.call.*
import chat.simplex.common.views.chat.group.*
import chat.simplex.common.views.chat.item.*
import chat.simplex.common.views.chatlist.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.GroupInfo
import chat.simplex.common.platform.*
import chat.simplex.common.platform.AudioPlayer
import chat.simplex.common.views.newchat.ContactConnectionInfoView
import chat.simplex.res.MR
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*
import kotlinx.datetime.*
import java.io.File
import java.net.URI
import kotlin.math.*

data class ItemSeparation(val timestamp: Boolean, val largeGap: Boolean, val date: Instant?)

@Composable
// staleChatId means the id that was before chatModel.chatId becomes null. It's needed for Android only to make transition from chat
// to chat list smooth. Otherwise, chat view will become blank right before the transition starts
fun ChatView(staleChatId: State<String?>, onComposed: suspend (chatId: String) -> Unit) {
  val remoteHostId = remember { derivedStateOf { chatModel.chats.value.firstOrNull { chat -> chat.chatInfo.id == staleChatId.value }?.remoteHostId } }
  val showSearch = rememberSaveable { mutableStateOf(false) }
  val activeChatInfo = remember { derivedStateOf { chatModel.chats.value.firstOrNull { chat -> chat.chatInfo.id == staleChatId.value }?.chatInfo } }
  val user = chatModel.currentUser.value
  val chatInfo = activeChatInfo.value
  if (chatInfo == null || user == null) {
    LaunchedEffect(Unit) {
      chatModel.chatId.value = null
      ModalManager.end.closeModals()
    }
  } else {
    val searchText = rememberSaveable { mutableStateOf("") }
    val useLinkPreviews = chatModel.controller.appPrefs.privacyLinkPreviews.get()
    val composeState = rememberSaveable(saver = ComposeState.saver()) {
      val draft = chatModel.draft.value
      val sharedContent = chatModel.sharedContent.value
      mutableStateOf(
        if (chatModel.draftChatId.value == staleChatId.value && draft != null && (sharedContent !is SharedContent.Forward || sharedContent.fromChatInfo.id == staleChatId.value)) {
          draft
        } else {
          ComposeState(useLinkPreviews = useLinkPreviews)
        }
      )
    }
    val attachmentOption = rememberSaveable { mutableStateOf<AttachmentOption?>(null) }
    val attachmentBottomSheetState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden)
    val scope = rememberCoroutineScope()
    val selectedChatItems = rememberSaveable { mutableStateOf(null as Set<Long>?) }
    LaunchedEffect(Unit) {
      // snapshotFlow here is because it reacts much faster on changes in chatModel.chatId.value.
      // With LaunchedEffect(chatModel.chatId.value) there is a noticeable delay before reconstruction of the view
      launch {
        snapshotFlow { chatModel.chatId.value }
          .distinctUntilChanged()
          .filterNotNull()
          .collect { chatId ->
            markUnreadChatAsRead(chatId)
            showSearch.value = false
            searchText.value = ""
            selectedChatItems.value = null
          }
      }
    }
    val view = LocalMultiplatformView()
    val chatRh = remoteHostId.value
    // We need to have real unreadCount value for displaying it inside top right button
    // Having activeChat reloaded on every change in it is inefficient (UI lags)
    val unreadCount = remember {
      derivedStateOf {
        chatModel.chats.value.firstOrNull { chat -> chat.chatInfo.id == activeChatInfo.value?.id }?.chatStats?.unreadCount ?: 0
      }
    }
    val clipboard = LocalClipboardManager.current
    CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler(chatInfo.id, keyboardCoversBar = false)) {
    when (chatInfo) {
      is ChatInfo.Direct, is ChatInfo.Group, is ChatInfo.Local -> {
        val perChatTheme = remember(chatInfo, CurrentColors.value.base) { if (chatInfo is ChatInfo.Direct) chatInfo.contact.uiThemes?.preferredMode(!CurrentColors.value.colors.isLight) else if (chatInfo is ChatInfo.Group) chatInfo.groupInfo.uiThemes?.preferredMode(!CurrentColors.value.colors.isLight) else null }
        val overrides = if (perChatTheme != null) ThemeManager.currentColors(null, perChatTheme, chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get()) else null
        val fullDeleteAllowed = remember(chatInfo) { chatInfo.featureEnabled(ChatFeature.FullDelete) }
        SimpleXThemeOverride(overrides ?: CurrentColors.collectAsState().value) {
          ChatLayout(
            remoteHostId = remoteHostId,
            chatInfo = activeChatInfo,
            unreadCount,
            composeState,
            composeView = {
              if (selectedChatItems.value == null) {
                Column(
                  Modifier.fillMaxWidth(),
                  horizontalAlignment = Alignment.CenterHorizontally
                ) {
                  if (
                    chatInfo is ChatInfo.Direct
                    && !chatInfo.contact.sndReady
                    && chatInfo.contact.active
                    && !chatInfo.contact.nextSendGrpInv
                  ) {
                    Text(
                      generalGetString(MR.strings.contact_connection_pending),
                      Modifier.padding(top = 4.dp),
                      fontSize = 14.sp,
                      color = MaterialTheme.colors.secondary
                    )
                  }
                  ComposeView(
                    chatModel, Chat(remoteHostId = chatRh, chatInfo = chatInfo, chatItems = emptyList()), composeState, attachmentOption,
                    showChooseAttachment = { scope.launch { attachmentBottomSheetState.show() } }
                  )
                }
              } else {
                SelectedItemsBottomToolbar(
                  chatItems = remember { chatModel.chatItems }.value,
                  selectedChatItems = selectedChatItems,
                  chatInfo = chatInfo,
                  deleteItems = { canDeleteForAll ->
                    val itemIds = selectedChatItems.value
                    val questionText =
                      if (!canDeleteForAll || fullDeleteAllowed || chatInfo is ChatInfo.Local)
                        generalGetString(MR.strings.delete_messages_cannot_be_undone_warning)
                      else
                        generalGetString(MR.strings.delete_messages_mark_deleted_warning)
                    if (itemIds != null) {
                      deleteMessagesAlertDialog(
                        itemIds.sorted(),
                        questionText = questionText,
                        forAll = canDeleteForAll,
                        deleteMessages = { ids, forAll ->
                          deleteMessages(chatRh, chatInfo, ids, forAll, moderate = false) {
                            selectedChatItems.value = null
                          }
                        }
                      )
                    }
                  },
                  moderateItems = {
                    if (chatInfo is ChatInfo.Group) {
                      val itemIds = selectedChatItems.value
                      if (itemIds != null) {
                        moderateMessagesAlertDialog(itemIds.sorted(), moderateMessageQuestionText(chatInfo.featureEnabled(ChatFeature.FullDelete), itemIds.size), deleteMessages = { ids ->
                          deleteMessages(chatRh, chatInfo, ids, true, moderate = true) {
                            selectedChatItems.value = null
                          }
                        })
                      }
                    }
                  },
                  forwardItems = {
                    val itemIds = selectedChatItems.value

                    if (itemIds != null) {
                      withBGApi {
                        val chatItemIds = itemIds.toList()
                        val forwardPlan = controller.apiPlanForwardChatItems(
                          rh = chatRh,
                          fromChatType = chatInfo.chatType,
                          fromChatId = chatInfo.apiId,
                          chatItemIds = chatItemIds
                        )

                        if (forwardPlan != null) {
                          if (forwardPlan.chatItemIds.count() < chatItemIds.count() || forwardPlan.forwardConfirmation != null) {
                            handleForwardConfirmation(chatRh, forwardPlan, chatInfo)
                          } else {
                            forwardContent(forwardPlan.chatItemIds, chatInfo)
                          }
                        }
                      }
                    }
                  },
                )
              }
            },
            attachmentOption,
            attachmentBottomSheetState,
            searchText,
            useLinkPreviews = useLinkPreviews,
            linkMode = chatModel.simplexLinkMode.value,
            selectedChatItems = selectedChatItems,
            back = {
              hideKeyboard(view)
              AudioPlayer.stop()
              chatModel.chatId.value = null
              chatModel.groupMembers.clear()
              chatModel.groupMembersIndexes.clear()
            },
            info = {
              if (ModalManager.end.hasModalsOpen()) {
                ModalManager.end.closeModals()
                return@ChatLayout
              }
              hideKeyboard(view)
              withBGApi {
                // The idea is to preload information before showing a modal because large groups can take time to load all members
                var preloadedContactInfo: Pair<ConnectionStats?, Profile?>? = null
                var preloadedCode: String? = null
                var preloadedLink: Pair<String, GroupMemberRole>? = null
                if (chatInfo is ChatInfo.Direct) {
                  preloadedContactInfo = chatModel.controller.apiContactInfo(chatRh, chatInfo.apiId)
                  preloadedCode = chatModel.controller.apiGetContactCode(chatRh, chatInfo.apiId)?.second
                } else if (chatInfo is ChatInfo.Group) {
                  setGroupMembers(chatRh, chatInfo.groupInfo, chatModel)
                  preloadedLink = chatModel.controller.apiGetGroupLink(chatRh, chatInfo.groupInfo.groupId)
                }
                ModalManager.end.showModalCloseable(true) { close ->
                  val chatInfo = remember { activeChatInfo }.value
                  if (chatInfo is ChatInfo.Direct) {
                    var contactInfo: Pair<ConnectionStats?, Profile?>? by remember { mutableStateOf(preloadedContactInfo) }
                    var code: String? by remember { mutableStateOf(preloadedCode) }
                    KeyChangeEffect(chatInfo.id, ChatModel.networkStatuses.toMap()) {
                      contactInfo = chatModel.controller.apiContactInfo(chatRh, chatInfo.apiId)
                      preloadedContactInfo = contactInfo
                      code = chatModel.controller.apiGetContactCode(chatRh, chatInfo.apiId)?.second
                      preloadedCode = code
                    }
                    ChatInfoView(chatModel, chatInfo.contact, contactInfo?.first, contactInfo?.second, chatInfo.localAlias, code, close) {
                      showSearch.value = true
                    }
                  } else if (chatInfo is ChatInfo.Group) {
                    var link: Pair<String, GroupMemberRole>? by remember(chatInfo.id) { mutableStateOf(preloadedLink) }
                    KeyChangeEffect(chatInfo.id) {
                      setGroupMembers(chatRh, chatInfo.groupInfo, chatModel)
                      link = chatModel.controller.apiGetGroupLink(chatRh, chatInfo.groupInfo.groupId)
                      preloadedLink = link
                    }
                    GroupChatInfoView(chatModel, chatRh, chatInfo.id, link?.first, link?.second, {
                      link = it
                      preloadedLink = it
                    }, close, { showSearch.value = true })
                  } else {
                    LaunchedEffect(Unit) {
                      close()
                    }
                  }
                }
              }
            },
            showMemberInfo = { groupInfo: GroupInfo, member: GroupMember ->
              hideKeyboard(view)
              withBGApi {
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
            loadMessages = { chatId, scrollDirection, (itemId, idx, area) ->
              val c = chatModel.getChat(chatId)
              if (chatModel.chatId.value != chatId) return@ChatLayout
              when (scrollDirection) {
                ScrollDirection.Up -> {
                  if (c != null) {
                    withBGApi {
                      val chatSectionLoader = ChatSectionLoader(idx, area)
                      apiLoadMessages(
                        rhId = c.remoteHostId,
                        chatInfo = c.chatInfo,
                        chatModel = chatModel,
                        itemId = itemId,
                        search = "",
                        chatSectionLoader = chatSectionLoader,
                      )
                    }
                  }
                }
                ScrollDirection.Down -> {
                  if (c != null) {
                    withBGApi {
                      val chatSectionLoader = ChatSectionLoader(idx + 1, area)
                      apiLoadMessages(
                        rhId = c.remoteHostId,
                        chatInfo = c.chatInfo,
                        chatModel = chatModel,
                        itemId = itemId,
                        search = "",
                        chatSectionLoader = chatSectionLoader,
                        pagination = ChatPagination.After(itemId, ChatPagination.PRELOAD_COUNT)
                      )
                    }
                  }
                }
                else -> {}
              }
            },
            deleteMessage = { itemId, mode ->
              withBGApi {
                val toDeleteItem = chatModel.chatItems.value.firstOrNull { it.id == itemId }
                val toModerate = toDeleteItem?.memberToModerate(chatInfo)
                val groupInfo = toModerate?.first
                val groupMember = toModerate?.second
                val deletedChatItem: ChatItem?
                val toChatItem: ChatItem?
                val r = if (mode == CIDeleteMode.cidmBroadcast && groupInfo != null && groupMember != null) {
                  chatModel.controller.apiDeleteMemberChatItems(
                    chatRh,
                    groupId = groupInfo.groupId,
                    itemIds = listOf(itemId)
                  )
                } else {
                  chatModel.controller.apiDeleteChatItems(
                    chatRh,
                    type = chatInfo.chatType,
                    id = chatInfo.apiId,
                    itemIds = listOf(itemId),
                    mode = mode
                  )
                }
                val deleted = r?.firstOrNull()
                if (deleted != null) {
                  deletedChatItem = deleted.deletedChatItem.chatItem
                  toChatItem = deleted.toChatItem?.chatItem
                  withChats {
                    if (toChatItem != null) {
                      upsertChatItem(chatRh, chatInfo, toChatItem)
                    } else {
                      removeChatItem(chatRh, chatInfo, deletedChatItem)
                    }
                  }
                }
              }
            },
            deleteMessages = { itemIds -> deleteMessages(chatRh, chatInfo, itemIds, false, moderate = false) },
            receiveFile = { fileId ->
              withBGApi { chatModel.controller.receiveFile(chatRh, user, fileId) }
            },
            cancelFile = { fileId ->
              withBGApi { chatModel.controller.cancelFile(chatRh, user, fileId) }
            },
            joinGroup = { groupId, onComplete ->
              withBGApi {
                chatModel.controller.apiJoinGroup(chatRh, groupId)
                onComplete.invoke()
              }
            },
            startCall = out@{ media -> startChatCall(chatRh, chatInfo, media) },
            endCall = {
              val call = chatModel.activeCall.value
              if (call != null) withBGApi { chatModel.callManager.endCall(call) }
            },
            acceptCall = { contact ->
              hideKeyboard(view)
              withBGApi {
                val invitation = chatModel.callInvitations.remove(contact.id)
                  ?: controller.apiGetCallInvitations(chatModel.remoteHostId()).firstOrNull { it.contact.id == contact.id }
                if (invitation == null) {
                  AlertManager.shared.showAlertMsg(generalGetString(MR.strings.call_already_ended))
                } else {
                  chatModel.callManager.acceptIncomingCall(invitation = invitation)
                }
              }
            },
            acceptFeature = { contact, feature, param ->
              withBGApi {
                chatModel.controller.allowFeatureToContact(chatRh, contact, feature, param)
              }
            },
            openDirectChat = { contactId ->
              scope.launch {
                openDirectChat(chatRh, contactId, chatModel)
              }
            },
            forwardItem = { cInfo, cItem ->
              chatModel.chatId.value = null
              chatModel.sharedContent.value = SharedContent.Forward(listOf(cItem), cInfo)
            },
            updateContactStats = { contact ->
              withBGApi {
                val r = chatModel.controller.apiContactInfo(chatRh, chatInfo.apiId)
                if (r != null) {
                  val contactStats = r.first
                  if (contactStats != null)
                    withChats {
                      updateContactConnectionStats(chatRh, contact, contactStats)
                    }
                }
              }
            },
            updateMemberStats = { groupInfo, member ->
              withBGApi {
                val r = chatModel.controller.apiGroupMemberInfo(chatRh, groupInfo.groupId, member.groupMemberId)
                if (r != null) {
                  val memStats = r.second
                  if (memStats != null) {
                    withChats {
                      updateGroupMemberConnectionStats(chatRh, groupInfo, r.first, memStats)
                    }
                  }
                }
              }
            },
            syncContactConnection = { contact ->
              withBGApi {
                val cStats = chatModel.controller.apiSyncContactRatchet(chatRh, contact.contactId, force = false)
                if (cStats != null) {
                  withChats {
                    updateContactConnectionStats(chatRh, contact, cStats)
                  }
                }
              }
            },
            syncMemberConnection = { groupInfo, member ->
              withBGApi {
                val r = chatModel.controller.apiSyncGroupMemberRatchet(chatRh, groupInfo.apiId, member.groupMemberId, force = false)
                if (r != null) {
                  withChats {
                    updateGroupMemberConnectionStats(chatRh, groupInfo, r.first, r.second)
                  }
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
              withBGApi {
                val updatedCI = chatModel.controller.apiChatItemReaction(
                  rh = chatRh,
                  type = cInfo.chatType,
                  id = cInfo.apiId,
                  itemId = cItem.id,
                  add = add,
                  reaction = reaction
                )
                if (updatedCI != null) {
                  withChats {
                    updateChatItem(cInfo, updatedCI)
                  }
                }
              }
            },
            showItemDetails = { cInfo, cItem ->
              suspend fun loadChatItemInfo(): ChatItemInfo? {
                val ciInfo = chatModel.controller.apiGetChatItemInfo(chatRh, cInfo.chatType, cInfo.apiId, cItem.id)
                if (ciInfo != null) {
                  if (chatInfo is ChatInfo.Group) {
                    setGroupMembers(chatRh, chatInfo.groupInfo, chatModel)
                  }
                }
                return ciInfo
              }
              withBGApi {
                var initialCiInfo = loadChatItemInfo() ?: return@withBGApi
                ModalManager.end.closeModals()
                ModalManager.end.showModalCloseable(endButtons = {
                  ShareButton {
                    clipboard.shareText(itemInfoShareText(chatModel, cItem, initialCiInfo, chatModel.controller.appPrefs.developerTools.get()))
                  }
                }) { close ->
                  var ciInfo by remember(cItem.id) { mutableStateOf(initialCiInfo) }
                  ChatItemInfoView(chatRh, cItem, ciInfo, devTools = chatModel.controller.appPrefs.developerTools.get())
                  LaunchedEffect(cItem.id) {
                    withContext(Dispatchers.Default) {
                      for (apiResp in controller.messagesChannel) {
                        val msg = apiResp.resp
                        if (apiResp.remoteHostId == chatRh &&
                          msg is CR.ChatItemsStatusesUpdated &&
                          msg.chatItems.any { it.chatItem.id == cItem.id }
                        ) {
                          ciInfo = loadChatItemInfo() ?: return@withContext
                          initialCiInfo = ciInfo
                        }
                      }
                    }
                  }
                  KeyChangeEffect(chatModel.chatId.value) {
                    close()
                  }
                }
              }
            },
            addMembers = { groupInfo -> addGroupMembers(view = view, groupInfo = groupInfo, rhId = chatRh, close = { ModalManager.end.closeModals() }) },
            openGroupLink = { groupInfo -> openGroupLink(view = view, groupInfo = groupInfo, rhId = chatRh, close = { ModalManager.end.closeModals() }) },
            markRead = { range, unreadCountAfter ->
              withBGApi {
                withChats {
                  // It's important to call it on Main thread. Otherwise, composable crash occurs from time-to-time without useful stacktrace
                  withContext(Dispatchers.Main) {
                    markChatItemsRead(chatRh, chatInfo, range, unreadCountAfter)
                  }
                  ntfManager.cancelNotificationsForChat(chatInfo.id)
                  chatModel.controller.apiChatRead(
                    chatRh,
                    chatInfo.chatType,
                    chatInfo.apiId,
                    range
                  )
                }
              }
            },
            changeNtfsState = { enabled, currentValue -> toggleNotifications(chatRh, chatInfo, enabled, chatModel, currentValue) },
            onSearchValueChanged = { value ->
              if (searchText.value == value) return@ChatLayout
              val c = chatModel.getChat(chatInfo.id) ?: return@ChatLayout
              if (chatModel.chatId.value != chatInfo.id) return@ChatLayout
              withBGApi {
                apiFindMessages(c, chatModel, value)
                searchText.value = value
              }
            },
            onComposed,
            developerTools = chatModel.controller.appPrefs.developerTools.get(),
            showViaProxy = chatModel.controller.appPrefs.showSentViaProxy.get(),
            showSearch = showSearch
          )
        }
      }
      is ChatInfo.ContactConnection -> {
        val close = { chatModel.chatId.value = null }
          ModalView(close, showClose = appPlatform.isAndroid, content = {
            ContactConnectionInfoView(chatModel, chatRh, chatInfo.contactConnection.connReqInv, chatInfo.contactConnection, false, close)
          })
          LaunchedEffect(chatInfo.id) {
            onComposed(chatInfo.id)
            ModalManager.end.closeModals()
            chatModel.chatItems.clear()
          }
      }
      is ChatInfo.InvalidJSON -> {
        val close = { chatModel.chatId.value = null }
          ModalView(close, showClose = appPlatform.isAndroid, endButtons = { ShareButton { clipboard.shareText(chatInfo.json) } }, content = {
            InvalidJSONView(chatInfo.json)
          })
          LaunchedEffect(chatInfo.id) {
            onComposed(chatInfo.id)
            ModalManager.end.closeModals()
            chatModel.chatItems.clear()
          }
      }
      else -> {}
    }
    }
  }
}

fun startChatCall(remoteHostId: Long?, chatInfo: ChatInfo, media: CallMediaType) {
  withBGApi {
    if (chatInfo is ChatInfo.Direct) {
      val contactInfo = chatModel.controller.apiContactInfo(remoteHostId, chatInfo.contact.contactId)
      val profile = contactInfo?.second ?: chatModel.currentUser.value?.profile?.toProfile() ?: return@withBGApi
      chatModel.activeCall.value = Call(remoteHostId = remoteHostId, contact = chatInfo.contact, callUUID = null, callState = CallState.WaitCapabilities, initialCallType = media, userProfile = profile)
      chatModel.showCallView.value = true
      chatModel.callCommand.add(WCallCommand.Capabilities(media))
    }
  }
}

@Composable
fun ChatLayout(
  remoteHostId: State<Long?>,
  chatInfo: State<ChatInfo?>,
  unreadCount: State<Int>,
  composeState: MutableState<ComposeState>,
  composeView: (@Composable () -> Unit),
  attachmentOption: MutableState<AttachmentOption?>,
  attachmentBottomSheetState: ModalBottomSheetState,
  searchValue: State<String>,
  useLinkPreviews: Boolean,
  linkMode: SimplexLinkMode,
  selectedChatItems: MutableState<Set<Long>?>,
  back: () -> Unit,
  info: () -> Unit,
  showMemberInfo: (GroupInfo, GroupMember) -> Unit,
  loadMessages: (ChatId, ScrollDirection, Triple<Long, Int, ChatSectionArea>) -> Unit,
  deleteMessage: (Long, CIDeleteMode) -> Unit,
  deleteMessages: (List<Long>) -> Unit,
  receiveFile: (Long) -> Unit,
  cancelFile: (Long) -> Unit,
  joinGroup: (Long, () -> Unit) -> Unit,
  startCall: (CallMediaType) -> Unit,
  endCall: () -> Unit,
  acceptCall: (Contact) -> Unit,
  acceptFeature: (Contact, ChatFeature, Int?) -> Unit,
  openDirectChat: (Long) -> Unit,
  forwardItem: (ChatInfo, ChatItem) -> Unit,
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
  showViaProxy: Boolean,
  showSearch: MutableState<Boolean>,
) {
  val scope = rememberCoroutineScope()
  val attachmentDisabled = remember { derivedStateOf { composeState.value.attachmentDisabled } }
  Box(
    Modifier
      .fillMaxWidth()
      .desktopOnExternalDrag(
        enabled = remember(attachmentDisabled.value, chatInfo.value?.userCanSend) { mutableStateOf(!attachmentDisabled.value && chatInfo.value?.userCanSend == true) }.value,
        onFiles = { paths -> composeState.onFilesAttached(paths.map { it.toURI() }) },
        onImage = { file -> CoroutineScope(Dispatchers.IO).launch { composeState.processPickedMedia(listOf(file.toURI()), null) } },
        onText = {
          // Need to parse HTML in order to correctly display the content
          //composeState.value = composeState.value.copy(message = composeState.value.message + it)
        },
      )
  ) {
    ModalBottomSheetLayout(
      scrimColor = Color.Black.copy(alpha = 0.12F),
      sheetElevation = 0.dp,
      sheetContent = {
        ChooseAttachmentView(
          attachmentOption,
          hide = { scope.launch { attachmentBottomSheetState.hide() } }
        )
      },
      sheetState = attachmentBottomSheetState,
      sheetShape = RoundedCornerShape(topStart = 18.dp, topEnd = 18.dp)
    ) {
      val composeViewHeight = remember { mutableStateOf(0.dp) }
      Box(Modifier.fillMaxSize().chatViewBackgroundModifier(MaterialTheme.colors, MaterialTheme.wallpaper, LocalAppBarHandler.current?.backgroundGraphicsLayerSize, LocalAppBarHandler.current?.backgroundGraphicsLayer)) {
        val remoteHostId = remember { remoteHostId }.value
        val chatInfo = remember { chatInfo }.value
        AdaptingBottomPaddingLayout(Modifier, CHAT_COMPOSE_LAYOUT_ID, composeViewHeight) {
          if (chatInfo != null) {
            Box(Modifier.fillMaxSize()) {
              ChatItemsList(
                remoteHostId, chatInfo, unreadCount, composeState, composeViewHeight, searchValue,
                useLinkPreviews, linkMode, selectedChatItems, showMemberInfo, loadMessages, deleteMessage, deleteMessages,
                receiveFile, cancelFile, joinGroup, acceptCall, acceptFeature, openDirectChat, forwardItem,
                updateContactStats, updateMemberStats, syncContactConnection, syncMemberConnection, findModelChat, findModelMember,
                setReaction, showItemDetails, markRead, remember { { onComposed(it) } }, developerTools, showViaProxy
              )
            }
          }
          val oneHandUI = remember { appPrefs.oneHandUI.state }
          Box(
            Modifier
              .layoutId(CHAT_COMPOSE_LAYOUT_ID)
              .align(Alignment.BottomCenter)
              .imePadding()
              .navigationBarsPadding()
              .then(if (oneHandUI.value) Modifier.padding(bottom = AppBarHeight * fontSizeSqrtMultiplier) else Modifier)
          ) {
            composeView()
          }
        }
        val oneHandUI = remember { appPrefs.oneHandUI.state }
        if (oneHandUI.value) {
          StatusBarBackground()
        } else {
          NavigationBarBackground(true, oneHandUI.value, noAlpha = true)
        }
        Box(if (oneHandUI.value) Modifier.align(Alignment.BottomStart).imePadding() else Modifier) {
          if (selectedChatItems.value == null) {
            if (chatInfo != null) {
              ChatInfoToolbar(chatInfo, back, info, startCall, endCall, addMembers, openGroupLink, changeNtfsState, onSearchValueChanged, showSearch)
            }
          } else {
            SelectedItemsTopToolbar(selectedChatItems)
          }
        }
      }
    }
  }
}

@Composable
fun BoxScope.ChatInfoToolbar(
  chatInfo: ChatInfo,
  back: () -> Unit,
  info: () -> Unit,
  startCall: (CallMediaType) -> Unit,
  endCall: () -> Unit,
  addMembers: (GroupInfo) -> Unit,
  openGroupLink: (GroupInfo) -> Unit,
  changeNtfsState: (Boolean, currentValue: MutableState<Boolean>) -> Unit,
  onSearchValueChanged: (String) -> Unit,
  showSearch: MutableState<Boolean>
) {
  val scope = rememberCoroutineScope()
  val showMenu = rememberSaveable { mutableStateOf(false) }

  val onBackClicked = {
    if (!showSearch.value) {
      back()
    } else {
      onSearchValueChanged("")
      showSearch.value = false
    }
  }
  if (appPlatform.isAndroid) {
    BackHandler(onBack = onBackClicked)
  }
  val barButtons = arrayListOf<@Composable RowScope.() -> Unit>()
  val menuItems = arrayListOf<@Composable () -> Unit>()
  val activeCall by remember { chatModel.activeCall }
  if (chatInfo is ChatInfo.Local) {
    barButtons.add {
      IconButton(
        {
          showMenu.value = false
          showSearch.value = true
        }, enabled = chatInfo.noteFolder.ready
      ) {
        Icon(
          painterResource(MR.images.ic_search),
          stringResource(MR.strings.search_verb).capitalize(Locale.current),
          tint = if (chatInfo.noteFolder.ready) MaterialTheme.colors.primary else MaterialTheme.colors.secondary
        )
      }
    }
  } else {
    menuItems.add {
      ItemAction(stringResource(MR.strings.search_verb), painterResource(MR.images.ic_search), onClick = {
        showMenu.value = false
        showSearch.value = true
      })
    }
  }

  if (chatInfo is ChatInfo.Direct && chatInfo.contact.mergedPreferences.calls.enabled.forUser) {
    if (activeCall == null) {
      barButtons.add {
        IconButton({
          showMenu.value = false
          startCall(CallMediaType.Audio)
        }, enabled = chatInfo.contact.ready && chatInfo.contact.active
        ) {
          Icon(
            painterResource(MR.images.ic_call_500),
            stringResource(MR.strings.icon_descr_audio_call).capitalize(Locale.current),
            tint = if (chatInfo.contact.ready && chatInfo.contact.active) MaterialTheme.colors.primary else MaterialTheme.colors.secondary
          )
        }
      }
    } else if (activeCall?.contact?.id == chatInfo.id && appPlatform.isDesktop) {
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
    if (chatInfo.contact.ready && chatInfo.contact.active && activeCall == null) {
      menuItems.add {
        ItemAction(stringResource(MR.strings.icon_descr_video_call).capitalize(Locale.current), painterResource(MR.images.ic_videocam), onClick = {
          showMenu.value = false
          startCall(CallMediaType.Video)
        })
      }
    }
  } else if (chatInfo is ChatInfo.Group && chatInfo.groupInfo.canAddMembers) {
    if (!chatInfo.incognito) {
      barButtons.add {
        IconButton({
          showMenu.value = false
          addMembers(chatInfo.groupInfo)
        }) {
          Icon(painterResource(MR.images.ic_person_add_500), stringResource(MR.strings.icon_descr_add_members), tint = MaterialTheme.colors.primary)
        }
      }
    } else {
      barButtons.add {
        IconButton({
          showMenu.value = false
          openGroupLink(chatInfo.groupInfo)
        }) {
          Icon(painterResource(MR.images.ic_add_link), stringResource(MR.strings.group_link), tint = MaterialTheme.colors.primary)
        }
      }
    }
  }

  if ((chatInfo is ChatInfo.Direct && chatInfo.contact.ready && chatInfo.contact.active) || chatInfo is ChatInfo.Group) {
    val ntfsEnabled = remember { mutableStateOf(chatInfo.ntfsEnabled) }
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

  if (menuItems.isNotEmpty()) {
    barButtons.add {
      IconButton({ showMenu.value = true }) {
        Icon(MoreVertFilled, stringResource(MR.strings.icon_descr_more_button), tint = MaterialTheme.colors.primary)
      }
    }
  }
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  DefaultAppBar(
    navigationButton = { if (appPlatform.isAndroid || showSearch.value) { NavigationButtonBack(onBackClicked) }  },
    title = { ChatInfoToolbarTitle(chatInfo) },
    onTitleClick = if (chatInfo is ChatInfo.Local) null else info,
    showSearch = showSearch.value,
    onTop = !oneHandUI.value,
    onSearchValueChanged = onSearchValueChanged,
    buttons = { barButtons.forEach { it() } }
  )
  Box(Modifier.fillMaxWidth().wrapContentSize(Alignment.TopEnd)) {
    val density = LocalDensity.current
    val width = remember { mutableStateOf(250.dp) }
    val height = remember { mutableStateOf(0.dp) }
    DefaultDropdownMenu(
      showMenu,
      modifier = Modifier.onSizeChanged { with(density) {
        width.value = it.width.toDp().coerceAtLeast(250.dp)
        if (oneHandUI.value && (appPlatform.isDesktop || (platform.androidApiLevel ?: 0) >= 30)) height.value = it.height.toDp()
      } },
      offset = DpOffset(-width.value, if (oneHandUI.value) -height.value else AppBarHeight)
    ) {
      if (oneHandUI.value) {
        menuItems.asReversed().forEach { it() }
      } else {
        menuItems.forEach { it() }
      }
    }
  }
}

@Composable
fun ChatInfoToolbarTitle(cInfo: ChatInfo, imageSize: Dp = 40.dp, iconColor: Color = MaterialTheme.colors.secondaryVariant.mixWith(MaterialTheme.colors.onBackground, 0.97f)) {
  Row(
    horizontalArrangement = Arrangement.Center,
    verticalAlignment = Alignment.CenterVertically
  ) {
    if (cInfo.incognito) {
      IncognitoImage(size = 36.dp * fontSizeSqrtMultiplier, Indigo)
    }
    ChatInfoImage(cInfo, size = imageSize * fontSizeSqrtMultiplier, iconColor)
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
  Icon(painterResource(MR.images.ic_verified_user), null, Modifier.size(18.dp * fontSizeSqrtMultiplier).padding(end = 3.dp, top = 1.dp), tint = MaterialTheme.colors.secondary)
}

@Composable
fun BoxScope.ChatItemsList(
  remoteHostId: Long?,
  chatInfo: ChatInfo,
  unreadCount: State<Int>,
  composeState: MutableState<ComposeState>,
  composeViewHeight: State<Dp>,
  searchValue: State<String>,
  useLinkPreviews: Boolean,
  linkMode: SimplexLinkMode,
  selectedChatItems: MutableState<Set<Long>?>,
  showMemberInfo: (GroupInfo, GroupMember) -> Unit,
  loadMessages: (ChatId, ScrollDirection, Triple<Long, Int, ChatSectionArea>) -> Unit,
  deleteMessage: (Long, CIDeleteMode) -> Unit,
  deleteMessages: (List<Long>) -> Unit,
  receiveFile: (Long) -> Unit,
  cancelFile: (Long) -> Unit,
  joinGroup: (Long, () -> Unit) -> Unit,
  acceptCall: (Contact) -> Unit,
  acceptFeature: (Contact, ChatFeature, Int?) -> Unit,
  openDirectChat: (Long) -> Unit,
  forwardItem: (ChatInfo, ChatItem) -> Unit,
  updateContactStats: (Contact) -> Unit,
  updateMemberStats: (GroupInfo, GroupMember) -> Unit,
  syncContactConnection: (Contact) -> Unit,
  syncMemberConnection: (GroupInfo, GroupMember) -> Unit,
  findModelChat: (String) -> Chat?,
  findModelMember: (String) -> GroupMember?,
  setReaction: (ChatInfo, ChatItem, Boolean, MsgReaction) -> Unit,
  showItemDetails: (ChatInfo, ChatItem) -> Unit,
  markRead: (CC.ItemRange, unreadCountAfter: Int?) -> Unit,
  onComposed: suspend (chatId: String) -> Unit,
  developerTools: Boolean,
  showViaProxy: Boolean,
) {
  val listState = rememberLazyListState()
  val scope = rememberCoroutineScope()
  val scrollAdjustmentEnabled = remember { mutableStateOf(true) }
  ScrollToBottom(chatInfo.id, listState, chatModel.chatItems, scrollAdjustmentEnabled)
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

  Spacer(Modifier.size(8.dp))
  val reversedChatItems = remember { derivedStateOf { chatModel.chatItems.asReversed() } }
  val revealedItems = rememberSaveable { mutableStateOf(setOf<Long>()) }
  val sections = remember { derivedStateOf { reversedChatItems.value.putIntoSections(revealedItems.value) } }
  val preloadItemsEnabled = remember { mutableStateOf(true) }
  val boundaries = remember { derivedStateOf { sections.value.map { it.boundary } } }
  val scrollPosition: State<(Int) -> Int> = remember { mutableStateOf({ idx -> min(sections.value.revealedItemCount() - 1, idx + 1 ) }) }

  PreloadItems(chatInfo.id, listState, ChatPagination.UNTIL_PRELOAD_COUNT, preloadItemsEnabled, boundaries, loadMessages)

  val topPaddingToContentPx = rememberUpdatedState(with(LocalDensity.current) { topPaddingToContent().roundToPx() })
  val maxHeight = remember { derivedStateOf { listState.layoutInfo.viewportEndOffset - topPaddingToContentPx.value } }
  val chatInfoUpdated = rememberUpdatedState(chatInfo)

  LaunchedEffect(Unit) {
    launch {
      snapshotFlow { chatInfoUpdated.value.id }
        .distinctUntilChanged()
        .collect {
          if (revealedItems.value.isNotEmpty()) {
            revealedItems.value = setOf()
          }
          preloadItemsEnabled.value = true
          val firstUnreadItem = reversedChatItems.value.findLast { it.isRcvNew }
          if (firstUnreadItem != null) {
            val firstUnreadItemIndexIdx = sections.value.chatItemPosition(firstUnreadItem.id)
            if (firstUnreadItemIndexIdx != null) {
              listState.scrollToItem(scrollPosition.value(firstUnreadItemIndexIdx), -maxHeight.value)
            }

            if (chatModel.chatItemsSectionArea[firstUnreadItem.id] != ChatSectionArea.Bottom) {
              withBGApi {
                scrollAdjustmentEnabled.value = false
                try {
                  apiLoadBottomSection(chatInfoUpdated.value, remoteHostId)
                } finally {
                  delay(600)
                  scrollAdjustmentEnabled.value = true
                }
              }
            }
          }
        }
    }
  }
  val scrollToItem: State<(Long) -> Unit> = remember {
    mutableStateOf({ itemId: Long ->
      val index = sections.value.chatItemPosition(itemId)
      preloadItemsEnabled.value = false
      
      if (index != null) {
        scope.launch {
          listState.animateScrollToItem(scrollPosition.value(index), -maxHeight.value)
          preloadItemsEnabled.value = true
        }
      } else {
        withBGApi {
          try {
            val destinationSection = sections.value.find { it.boundary.area == ChatSectionArea.Destination }
            val itemsToDrop = destinationSection?.items?.flatMap { it.items }?.toList()
            withContext(Dispatchers.Main) {
              itemsToDrop?.forEach {
                chatModel.chatItemsSectionArea[it.id] = ChatSectionArea.Current
              }
            }
            val chatSectionLoader = ChatSectionLoader(0, ChatSectionArea.Destination)
            apiLoadMessages(
              rhId = remoteHostId,
              chatInfo = chatInfoUpdated.value,
              chatModel = chatModel,
              itemId = itemId,
              search = "",
              chatSectionLoader = chatSectionLoader,
              pagination = ChatPagination.Around(itemId, ChatPagination.PRELOAD_COUNT * 2)
            )
            val idx = sections.value.chatItemPosition(itemId)
            scope.launch {
              if (idx != null) {
                listState.animateScrollToItem(scrollPosition.value(idx), -maxHeight.value)
                withContext(Dispatchers.Main) {
                  if (!itemsToDrop.isNullOrEmpty()) {
                    itemsToDrop.forEach {
                      chatModel.chatItemsSectionArea.remove(it.id)
                    }
                    chatModel.chatItems.removeAll { chatModel.chatItemsSectionArea[it.id] == null }
                    val newIdx = reversedChatItems.value.indexOfFirst { it.id == itemId }
                    listState.scrollToItem(scrollPosition.value(newIdx), -maxHeight.value)
                  }
                }
              }
              preloadItemsEnabled.value = true
            }
          } catch (ex: Exception) {
            preloadItemsEnabled.value = true
          }
        }
      }
    })
  }
  // TODO: Having this block on desktop makes ChatItemsList() to recompose twice on chatModel.chatId update instead of once
  LaunchedEffect(chatInfo.id) {
    var stopListening = false
    snapshotFlow { listState.layoutInfo.visibleItemsInfo.lastIndex }
      .distinctUntilChanged()
      .filter { !stopListening }
      .collect {
        onComposed(chatInfo.id)
        stopListening = true
      }
  }
  DisposableEffectOnGone(
    whenGone = {
      VideoPlayerHolder.releaseAll()
    }
  )
  @Composable
  fun ChatViewListItem(i: Int, range: IntRange?, showAvatar: Boolean, cItem: ChatItem, prevItem: ChatItem?, nextItem: ChatItem?) {
      CompositionLocalProvider(
        // Makes horizontal and vertical scrolling to coexist nicely.
        // With default touchSlop when you scroll LazyColumn, you can unintentionally open reply view
        LocalViewConfiguration provides LocalViewConfiguration.current.bigTouchSlop()
      ) {
        val itemScope = rememberCoroutineScope()
        val provider = {
          providerForGallery(i, chatModel.chatItems.value, cItem.id) { indexInReversed ->
            itemScope.launch {
              listState.scrollToItem(
                kotlin.math.min(reversedChatItems.value.lastIndex, indexInReversed + 1),
                -maxHeight.value
              )
            }
          }
        }

      val revealed = remember { mutableStateOf(revealedItems.value.contains(cItem.id)) }

      KeyChangeEffect(revealed.value) {
        val revealIds = if (range == null) setOf(cItem.id) else reversedChatItems.value.subList(range.first, range.last + 1).map { it.id }.toSet()

        if (revealIds.isNotEmpty()) {
          if (revealed.value) {
            revealedItems.value = revealedItems.value.toMutableSet().apply { addAll(revealIds) }
          } else {
            revealedItems.value = revealedItems.value.toMutableSet().apply { removeAll(revealIds) }
          }
        }
      }

      @Composable
      fun ChatItemViewShortHand(cItem: ChatItem, itemSeparation: ItemSeparation, range: IntRange?, fillMaxWidth: Boolean = true) {
        tryOrShowError("${cItem.id}ChatItem", error = {
          CIBrokenComposableView(if (cItem.chatDir.sent) Alignment.CenterEnd else Alignment.CenterStart)
        }) {
          ChatItemView(remoteHostId, chatInfo, cItem, composeState, provider, useLinkPreviews = useLinkPreviews, linkMode = linkMode, revealed = revealed, range = range, fillMaxWidth = fillMaxWidth, selectedChatItems = selectedChatItems, selectChatItem = { selectUnselectChatItem(true, cItem, revealed, selectedChatItems) }, deleteMessage = deleteMessage, deleteMessages = deleteMessages, receiveFile = receiveFile, cancelFile = cancelFile, joinGroup = joinGroup, acceptCall = acceptCall, acceptFeature = acceptFeature, openDirectChat = openDirectChat, forwardItem = forwardItem, updateContactStats = updateContactStats, updateMemberStats = updateMemberStats, syncContactConnection = syncContactConnection, syncMemberConnection = syncMemberConnection, findModelChat = findModelChat, findModelMember = findModelMember, scrollToItem = scrollToItem.value, setReaction = setReaction, showItemDetails = showItemDetails, developerTools = developerTools, showViaProxy = showViaProxy, itemSeparation = itemSeparation, showTimestamp = itemSeparation.timestamp)
        }
      }

      @Composable
      fun ChatItemView(cItem: ChatItem, range: IntRange?, prevItem: ChatItem?, itemSeparation: ItemSeparation, previousItemSeparation: ItemSeparation?) {
        val dismissState = rememberDismissState(initialValue = DismissValue.Default) {
          if (it == DismissValue.DismissedToStart) {
            itemScope.launch {
              if ((cItem.content is CIContent.SndMsgContent || cItem.content is CIContent.RcvMsgContent) && chatInfo !is ChatInfo.Local) {
                if (composeState.value.editing) {
                  composeState.value = ComposeState(contextItem = ComposeContextItem.QuotedItem(cItem), useLinkPreviews = useLinkPreviews)
                } else if (cItem.id != ChatItem.TEMP_LIVE_CHAT_ITEM_ID) {
                  composeState.value = composeState.value.copy(contextItem = ComposeContextItem.QuotedItem(cItem))
                }
              }
            }
          }
          false
        }
        val swipeableModifier = SwipeToDismissModifier(
          state = dismissState,
          directions = setOf(DismissDirection.EndToStart),
          swipeDistance = with(LocalDensity.current) { 30.dp.toPx() },
        )
        val sent = cItem.chatDir.sent

        @Composable
        fun ChatItemBox(modifier: Modifier = Modifier, content: @Composable () -> Unit = { }) {
          Box(
            modifier = modifier.padding(
              bottom = if (itemSeparation.largeGap) {
                if (i == 0) {
                  8.dp
                } else {
                  4.dp
                }
              } else 1.dp, top = if (previousItemSeparation?.largeGap == true) 4.dp else 1.dp
            ),
            contentAlignment = Alignment.CenterStart
          ) {
            content()
          }
        }

        @Composable
        fun adjustTailPaddingOffset(originalPadding: Dp, start: Boolean): Dp {
          val chatItemTail = remember { appPreferences.chatItemTail.state }
          val style = shapeStyle(cItem, chatItemTail.value, itemSeparation.largeGap, true)
          val tailRendered = style is ShapeStyle.Bubble && style.tailVisible

          return originalPadding + (if (tailRendered) 0.dp else if (start) msgTailWidthDp * 2 else msgTailWidthDp)
        }

        Box {
          val voiceWithTransparentBack = cItem.content.msgContent is MsgContent.MCVoice && cItem.content.text.isEmpty() && cItem.quotedItem == null && cItem.meta.itemForwarded == null
          val selectionVisible = selectedChatItems.value != null && cItem.canBeDeletedForSelf
          val selectionOffset by animateDpAsState(if (selectionVisible && !sent) 4.dp + 22.dp * fontSizeMultiplier else 0.dp)
          val swipeableOrSelectionModifier = (if (selectionVisible) Modifier else swipeableModifier).graphicsLayer { translationX = selectionOffset.toPx() }
          if (chatInfo is ChatInfo.Group) {
            if (cItem.chatDir is CIDirection.GroupRcv) {
              val member = cItem.chatDir.groupMember
              val (prevMember, memCount) =
                if (range != null) {
                  chatModel.getPrevHiddenMember(member, range)
                } else {
                  null to 1
                }

              if (showMemberImage(member, prevItem) || showAvatar) {
                Column(
                  Modifier
                    .padding(top = 8.dp)
                    .padding(start = 8.dp, end = if (voiceWithTransparentBack) 12.dp else adjustTailPaddingOffset(66.dp, start = false))
                    .fillMaxWidth()
                    .then(swipeableModifier),
                  verticalArrangement = Arrangement.spacedBy(4.dp),
                  horizontalAlignment = Alignment.Start
                ) {
                  @Composable
                  fun MemberNameAndRole() {
                    Row(Modifier.padding(bottom = 2.dp).graphicsLayer { translationX = selectionOffset.toPx() }, horizontalArrangement = Arrangement.SpaceBetween) {
                      Text(
                        memberNames(member, prevMember, memCount),
                        Modifier
                          .padding(start = (MEMBER_IMAGE_SIZE * fontSizeSqrtMultiplier) + DEFAULT_PADDING_HALF)
                          .weight(1f, false),
                        fontSize = 13.5.sp,
                        color = MaterialTheme.colors.secondary,
                        overflow = TextOverflow.Ellipsis,
                        maxLines = 1
                      )
                      if (memCount == 1 && member.memberRole > GroupMemberRole.Member) {
                        val chatItemTail = remember { appPreferences.chatItemTail.state }
                        val style = shapeStyle(cItem, chatItemTail.value, itemSeparation.largeGap, true)
                        val tailRendered = style is ShapeStyle.Bubble && style.tailVisible

                        Text(
                          member.memberRole.text,
                          Modifier.padding(start = DEFAULT_PADDING_HALF * 1.5f, end = DEFAULT_PADDING_HALF + if (tailRendered) msgTailWidthDp else 0.dp),
                          fontSize = 13.5.sp,
                          fontWeight = FontWeight.Medium,
                          color = MaterialTheme.colors.secondary,
                          maxLines = 1
                        )
                      }
                    }
                  }

                  @Composable
                  fun Item() {
                    ChatItemBox(Modifier.layoutId(CHAT_BUBBLE_LAYOUT_ID)) {
                      androidx.compose.animation.AnimatedVisibility(selectionVisible, enter = fadeIn(), exit = fadeOut()) {
                        SelectedChatItem(Modifier, cItem.id, selectedChatItems)
                      }
                      Row(Modifier.graphicsLayer { translationX = selectionOffset.toPx() }) {
                        Box(Modifier.clickable { showMemberInfo(chatInfo.groupInfo, member) }) {
                          MemberImage(member)
                        }
                        Box(modifier = Modifier.padding(top = 2.dp, start = 4.dp).chatItemOffset(cItem, itemSeparation.largeGap, revealed = revealed.value)) {
                          ChatItemViewShortHand(cItem, itemSeparation, range, false)
                        }
                      }
                    }
                  }
                  if (cItem.content.showMemberName) {
                    DependentLayout(Modifier, CHAT_BUBBLE_LAYOUT_ID) {
                      MemberNameAndRole()
                      Item()
                    }
                  } else {
                    Item()
                  }
                }
              } else {
                ChatItemBox {
                  AnimatedVisibility(selectionVisible, enter = fadeIn(), exit = fadeOut()) {
                    SelectedChatItem(Modifier.padding(start = 8.dp), cItem.id, selectedChatItems)
                  }
                  Row(
                    Modifier
                      .padding(start = 8.dp + (MEMBER_IMAGE_SIZE * fontSizeSqrtMultiplier) + 4.dp, end = if (voiceWithTransparentBack) 12.dp else adjustTailPaddingOffset(66.dp, start = false))
                      .chatItemOffset(cItem, itemSeparation.largeGap, revealed = revealed.value)
                      .then(swipeableOrSelectionModifier)
                  ) {
                    ChatItemViewShortHand(cItem, itemSeparation, range)
                  }
                }
              }
            } else {
              ChatItemBox {
                AnimatedVisibility(selectionVisible, enter = fadeIn(), exit = fadeOut()) {
                  SelectedChatItem(Modifier.padding(start = 8.dp), cItem.id, selectedChatItems)
                }
                Box(
                  Modifier
                    .padding(start = if (voiceWithTransparentBack) 12.dp else adjustTailPaddingOffset(104.dp, start = true), end = 12.dp)
                    .chatItemOffset(cItem, itemSeparation.largeGap, revealed = revealed.value)
                    .then(if (selectionVisible) Modifier else swipeableModifier)
                ) {
                  ChatItemViewShortHand(cItem, itemSeparation, range)
                }
              }
            }
          } else { // direct message
            ChatItemBox {
              AnimatedVisibility(selectionVisible, enter = fadeIn(), exit = fadeOut()) {
                SelectedChatItem(Modifier.padding(start = 8.dp), cItem.id, selectedChatItems)
              }

              Box(
                Modifier.padding(
                  start = if (sent && !voiceWithTransparentBack) adjustTailPaddingOffset(76.dp, start = true) else 12.dp,
                  end = if (sent || voiceWithTransparentBack) 12.dp else adjustTailPaddingOffset(76.dp, start = false),
                )
                  .chatItemOffset(cItem, itemSeparation.largeGap, revealed = revealed.value)
                  .then(if (!selectionVisible || !sent) swipeableOrSelectionModifier else Modifier)
              ) {
                ChatItemViewShortHand(cItem, itemSeparation, range)
              }
            }
          }
          if (selectionVisible) {
            Box(Modifier.matchParentSize().clickable {
              val checked = selectedChatItems.value?.contains(cItem.id) == true
              selectUnselectChatItem(select = !checked, cItem, revealed, selectedChatItems)
            })
          }
        }
      }

      if (cItem.isRcvNew && chatInfo.id == ChatModel.chatId.value) {
        LaunchedEffect(cItem.id) {
          itemScope.launch {
            delay(600)
            val itemRange = if (range != null) {
              val firstItem = reversedChatItems.value.getOrNull(range.first)
              val lastItem = reversedChatItems.value.getOrNull(range.last)
              if (lastItem != null && firstItem != null) {
                CC.ItemRange(lastItem.id, firstItem.id)
              } else {
                null
              }
            } else {
              CC.ItemRange(cItem.id, cItem.id)
            }
            if (itemRange != null) {
              markRead(itemRange, null)
            }
          }
        }
      }

      val itemSeparation = getItemSeparation(cItem, nextItem)
      val previousItemSeparation = if (prevItem != null) getItemSeparation(prevItem, cItem) else null

      if (itemSeparation.date != null) {
        DateSeparator(itemSeparation.date)
      }

      ChatItemView(cItem, range, prevItem, itemSeparation, previousItemSeparation)
    }
  }
  LazyColumnWithScrollBar(
    Modifier.align(Alignment.BottomCenter),
    state = listState,
    reverseLayout = true,
    contentPadding = PaddingValues(
      top = topPaddingToContent(),
      bottom = composeViewHeight.value
    ),
    additionalBarOffset = composeViewHeight
  ) {
    for (area in sections.value) {
      for ((sIdx, section) in area.items.withIndex()) {
        if (section.revealed) {
          itemsIndexed(section.items, key = { _, item -> (item.id to item.meta.createdAt.toEpochMilliseconds()).toString() }) { i, cItem ->
            // index here is just temporary, should be removed at all or put in the section items
            val prevItem = area.getPreviousShownItem(sIdx, i)
            val nextItem = area.getNextShownItem(sIdx, i)
            ChatViewListItem(area.itemPositions[cItem.id] ?: -1, section.originalItemsRange.takeIf { cItem.mergeCategory != null }, section.showAvatar.contains(cItem.id), cItem, prevItem, nextItem, )
          }
        } else {
          val item = section.items.first()
          item(key = { (item.id to item.meta.createdAt.toEpochMilliseconds()).toString() }) {
            // here you make one collapsed item from multiple items (should be already in section items)
            val prevItem = area.getPreviousShownItem(sIdx, section.items.lastIndex)
            val nextItem = area.getNextShownItem(sIdx, section.items.lastIndex)
            ChatViewListItem(area.itemPositions[item.id] ?: -1, section.originalItemsRange, section.showAvatar.contains(item.id), item, prevItem, nextItem)
          }
        }
      }
    }
    if (reversedChatItems.value.isNotEmpty()) {
      item {
        DateSeparator(reversedChatItems.value.last().meta.itemTs)
      }
    }
  }

  FloatingButtons(chatModel.chatItems, unreadCount, composeViewHeight, remoteHostId, chatInfo, searchValue, markRead, listState) {
    preloadItemsEnabled.value = false
    scope.launch {
      listState.animateScrollToItem(0)
      preloadItemsEnabled.value = true
      sections.value.dropTemporarySections()
    }
  }

  FloatingDate(
    Modifier.padding(top = 10.dp + topPaddingToContent()).align(Alignment.TopCenter),
    listState,
  )

  LaunchedEffect(Unit) {
    snapshotFlow { listState.isScrollInProgress }
      .collect {
        chatViewScrollState.value = it
      }
  }
}

@Composable
private fun ScrollToBottom(chatId: ChatId, listState: LazyListState, chatItems: State<List<ChatItem>>, enabled: State<Boolean>) {
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
    snapshotFlow { chatItems.value.lastOrNull()?.id }
      .distinctUntilChanged()
      .filter { listState.layoutInfo.visibleItemsInfo.firstOrNull()?.key != it && enabled.value }
      .collect {
        try {
          if (listState.firstVisibleItemIndex == 0 || (listState.firstVisibleItemIndex == 1 && listState.layoutInfo.totalItemsCount == chatItems.value.size)) {
            if (appPlatform.isAndroid) listState.animateScrollToItem(0) else listState.scrollToItem(0)
          } else {
            if (appPlatform.isAndroid) listState.animateScrollBy(scrollDistance) else listState.scrollBy(scrollDistance)
          }
        } catch (e: CancellationException) {
          /**
           * When you tap and hold a finger on a lazy column with chatItems, and then you receive a message,
           * this coroutine will be canceled with the message "Current mutation had a higher priority" because of animatedScroll.
           * Which breaks auto-scrolling to bottom. So just ignoring the exception
           * */
        } catch (e: IllegalArgumentException) {
          Log.e(TAG, "Failed to scroll: ${e.stackTraceToString()}")
        }
      }
  }
}

@Composable
fun BoxScope.FloatingButtons(
  chatItems: State<List<ChatItem>>,
  unreadCount: State<Int>,
  composeViewHeight: State<Dp>,
  remoteHostId: Long?,
  chatInfo: ChatInfo,
  searchValue: State<String>,
  markRead: (CC.ItemRange, unreadCountAfter: Int?) -> Unit,
  listState: LazyListState,
  scrollToLatestItem: () -> Unit
) {
  val scope = rememberCoroutineScope()
  val maxHeight = remember { derivedStateOf { listState.layoutInfo.viewportSize.height } }
  val bottomUnreadCount = remember {
    derivedStateOf {
      if (unreadCount.value == 0) return@derivedStateOf 0
      val items = chatItems.value
      val from = items.lastIndex - listState.firstVisibleItemIndex - listState.layoutInfo.visibleItemsInfo.lastIndex
      if (items.size <= from || from < 0) return@derivedStateOf 0

      items.subList(from, items.size).count { it.isRcvNew }
    }
  }

  val showBottomButtonWithCounter = remember { derivedStateOf { bottomUnreadCount.value > 0 && listState.firstVisibleItemIndex != 0 && searchValue.value.isEmpty() } }
  val showBottomButtonWithArrow = remember { derivedStateOf { !showBottomButtonWithCounter.value && listState.firstVisibleItemIndex != 0 } }
  BottomEndFloatingButton(
    bottomUnreadCount,
    showBottomButtonWithCounter,
    showBottomButtonWithArrow,
    composeViewHeight,
    onClickArrowDown = scrollToLatestItem,
    onClickCounter = {
      val firstVisibleOffset = (-maxHeight.value * 0.8).toInt()
      scope.launch { listState.animateScrollToItem(kotlin.math.max(0, bottomUnreadCount.value - 1), firstVisibleOffset) }
    }
  )
  // Don't show top FAB if is in search
  if (searchValue.value.isNotEmpty()) return
  val fabSize = 56.dp
  val topUnreadCount = remember { derivedStateOf { unreadCount.value - bottomUnreadCount.value } }
  val showDropDown = remember { mutableStateOf(false) }

  TopEndFloatingButton(
    Modifier.padding(end = DEFAULT_PADDING, top = 24.dp + topPaddingToContent()).align(Alignment.TopEnd),
    topUnreadCount,
    onClick = { scope.launch { listState.animateScrollBy(maxHeight.value.toFloat()) } },
    onLongClick = { showDropDown.value = true }
  )

  Box(Modifier.fillMaxWidth().wrapContentSize(Alignment.TopEnd)) {
    val density = LocalDensity.current
    val width = remember { mutableStateOf(250.dp) }
    DefaultDropdownMenu(
      showDropDown,
      modifier = Modifier.onSizeChanged { with(density) { width.value = it.width.toDp().coerceAtLeast(250.dp) } },
      offset = DpOffset(-DEFAULT_PADDING - width.value, 24.dp + fabSize + topPaddingToContent())
    ) {
      ItemAction(
        generalGetString(MR.strings.mark_read),
        painterResource(MR.images.ic_check),
        onClick = {
          val minUnreadItemId = chatModel.chats.value.firstOrNull { it.remoteHostId == remoteHostId && it.id == chatInfo.id }?.chatStats?.minUnreadItemId ?: return@ItemAction
          markRead(
            CC.ItemRange(minUnreadItemId, chatItems.value[chatItems.value.size - listState.layoutInfo.visibleItemsInfo.lastIndex - 1].id - 1),
            bottomUnreadCount.value
          )
          showDropDown.value = false
        })
    }
  }
}

@Composable
fun PreloadItems(
  chatId: String,
  listState: LazyListState,
  remaining: Int = 10,
  enabled: State<Boolean>,
  boundaries: State<List<ChatSectionAreaBoundary>>,
  onLoadMore: (ChatId, ScrollDirection, Triple<Long, Int, ChatSectionArea>) -> Unit,
) {
  // Prevent situation when initial load and load more happens one after another after selecting a chat with long scroll position from previous selection
  val allowLoad = remember { mutableStateOf(false) }
  val chatId = rememberUpdatedState(chatId)
  val onLoadMore = rememberUpdatedState(onLoadMore)
  var scrollDirection by remember { mutableStateOf(ScrollDirection.Idle) }
  var previousIndex by remember { mutableStateOf(0) }
  var previousScrollOffset by remember { mutableStateOf(0) }

  LaunchedEffect(listState.firstVisibleItemIndex, listState.firstVisibleItemScrollOffset) {
    val currentIndex = listState.firstVisibleItemIndex
    val currentScrollOffset = listState.firstVisibleItemScrollOffset
    val threshold = 25

    scrollDirection = when {
      currentIndex > previousIndex -> ScrollDirection.Up
      currentIndex < previousIndex -> ScrollDirection.Down
      currentScrollOffset > previousScrollOffset + threshold -> ScrollDirection.Up
      currentScrollOffset < previousScrollOffset - threshold -> ScrollDirection.Down
      currentScrollOffset == previousScrollOffset -> ScrollDirection.Idle
      else -> scrollDirection
    }

    previousIndex = currentIndex
    previousScrollOffset = currentScrollOffset
  }
  LaunchedEffect(Unit) {
    snapshotFlow { chatId.value }
      .filterNotNull()
      .collect {
        allowLoad.value = listState.layoutInfo.totalItemsCount == listState.layoutInfo.visibleItemsInfo.size
        delay(500)
        allowLoad.value = true
      }
  }
  KeyChangeEffect(allowLoad.value, enabled.value) {
    snapshotFlow {
      val lInfo = listState.layoutInfo
      val totalItemsNumber = lInfo.totalItemsCount
      val lastVisibleItemIndex = (lInfo.visibleItemsInfo.lastOrNull()?.index ?: 0) + 1
      val section = if (scrollDirection == ScrollDirection.Up) {
        boundaries.value.find { lastVisibleItemIndex in it.minIndex..it.maxIndex }
      } else if (scrollDirection == ScrollDirection.Down) {
        boundaries.value.find { listState.firstVisibleItemIndex in it.minIndex..it.maxIndex }
      } else {
        null
      }

      val request = if (allowLoad.value && section != null && enabled.value) {
        val numberOfItemsInSection = section.maxIndex - section.minIndex + 1
        val itemIdx = when {
          scrollDirection == ScrollDirection.Up && lastVisibleItemIndex > (section.maxIndex - remaining) && numberOfItemsInSection >= ChatPagination.INITIAL_COUNT -> {
            chatModel.chatItems.size - 1 - section.maxIndex
          }
          scrollDirection == ScrollDirection.Down && listState.firstVisibleItemIndex < (section.minIndex + remaining) && totalItemsNumber > remaining -> {
            chatModel.chatItems.size - 1 - section.minIndex
          }
          else -> null
        }
        val itemId = itemIdx?.let { chatModel.chatItems.value.getOrNull(it)?.id }
        itemId?.let { Triple(it, itemIdx, section.area) }
      } else {
        null
      }

      request
    }
      .distinctUntilChanged()
      .filterNotNull()
      .collect {
         onLoadMore.value(chatId.value, scrollDirection, it)
      }
  }
}

private fun showMemberImage(member: GroupMember, prevItem: ChatItem?): Boolean =
  when (val dir = prevItem?.chatDir) {
    is CIDirection.GroupSnd -> true
    is CIDirection.GroupRcv -> dir.groupMember.groupMemberId != member.groupMemberId
    else -> false
  }

val MEMBER_IMAGE_SIZE: Dp = 37.dp

@Composable
fun MemberImage(member: GroupMember) {
  MemberProfileImage(MEMBER_IMAGE_SIZE * fontSizeSqrtMultiplier, member, backgroundColor = MaterialTheme.colors.background)
}

@Composable
private fun TopEndFloatingButton(
  modifier: Modifier = Modifier,
  unreadCount: State<Int>,
  onClick: () -> Unit,
  onLongClick: () -> Unit
) = when {
  unreadCount.value > 0 -> {
    val interactionSource = interactionSourceWithDetection(onClick, onLongClick)
    FloatingActionButton(
      {}, // no action here
      modifier.size(48.dp).onRightClick(onLongClick),
      backgroundColor = MaterialTheme.colors.secondaryVariant,
      elevation = FloatingActionButtonDefaults.elevation(0.dp, 0.dp),
      interactionSource = interactionSource,
    ) {
      Text(
        unreadCountStr(unreadCount.value),
        color = MaterialTheme.colors.primary,
        fontSize = 14.sp,
      )
    }
  }
  else -> {
  }
}

@Composable
fun topPaddingToContent(): Dp {
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  return if (oneHandUI.value) {
    WindowInsets.statusBars.asPaddingValues().calculateTopPadding()
  } else {
    AppBarHeight * fontSizeSqrtMultiplier + WindowInsets.statusBars.asPaddingValues().calculateTopPadding()
  }
}

@Composable
private fun FloatingDate(
  modifier: Modifier,
  listState: LazyListState,
) {
  var nearBottomIndex by remember { mutableStateOf(-1) }
  var isNearBottom by remember { mutableStateOf(true) }
  val lastVisibleItemDate = remember {
    derivedStateOf {
      if (listState.layoutInfo.visibleItemsInfo.lastIndex >= 0) {
        val lastFullyVisibleOffset = listState.layoutInfo.viewportEndOffset
        val lastVisibleChatItemIndex = chatModel.chatItems.value.lastIndex - (listState.layoutInfo.visibleItemsInfo.lastOrNull { item -> item.offset + item.size <= lastFullyVisibleOffset && item.size > 0 }?.index ?: 0)
        val item = chatModel.chatItems.value.getOrNull(lastVisibleChatItemIndex)
        val timeZone = TimeZone.currentSystemDefault()
        item?.meta?.itemTs?.toLocalDateTime(timeZone)?.date?.atStartOfDayIn(timeZone)
      } else {
        null
      }
    }
  }
  val showDate = remember { mutableStateOf(false) }
  LaunchedEffect(Unit) {
    launch {
      snapshotFlow { chatModel.chatId.value }
        .distinctUntilChanged()
        .collect {
          showDate.value = false
          isNearBottom = true
          nearBottomIndex = -1
        }
    }
  }

  LaunchedEffect(Unit) {
    snapshotFlow { listState.layoutInfo.visibleItemsInfo }
      .collect { visibleItemsInfo ->
        if (visibleItemsInfo.find { it.index == 0 } != null) {
          var elapsedOffset = 0

          for (it in visibleItemsInfo) {
            if (elapsedOffset >= listState.layoutInfo.viewportSize.height / 2.5) {
              nearBottomIndex = it.index
              break;
            }
            elapsedOffset += it.size
          }
        }

        isNearBottom = if (nearBottomIndex == -1) true else (visibleItemsInfo.firstOrNull()?.index ?: 0) <= nearBottomIndex
      }
  }

  fun setDateVisibility(isVisible: Boolean) {
    if (isVisible) {
      val now = Clock.System.now()
      val date = lastVisibleItemDate.value
      if (!isNearBottom && !showDate.value && date != null && getTimestampDateText(date) != getTimestampDateText(now)) {
        showDate.value = true
      }
    } else if (showDate.value) {
      showDate.value = false
    }
  }

  LaunchedEffect(Unit) {
    var hideDateWhenNotScrolling: Job = Job()
    snapshotFlow { listState.firstVisibleItemScrollOffset }
      .collect {
        setDateVisibility(true)
        hideDateWhenNotScrolling.cancel()
        hideDateWhenNotScrolling = launch {
          delay(1000)
          setDateVisibility(false)
        }
      }
  }

  AnimatedVisibility(
    modifier = modifier,
    visible = showDate.value,
    enter = fadeIn(tween(durationMillis = 350)),
    exit = fadeOut(tween(durationMillis = 350))
  ) {
    val date = lastVisibleItemDate.value
    Column {
      Text(
        text = if (date != null) getTimestampDateText(date) else "",
        Modifier
          .background(
            color = MaterialTheme.colors.secondaryVariant,
            RoundedCornerShape(25.dp)
          )
          .padding(vertical = 4.dp, horizontal = 8.dp)
          .clip(RoundedCornerShape(25.dp)),
        fontSize = 14.sp,
        fontWeight = FontWeight.Medium,
        textAlign = TextAlign.Center,
        color = MaterialTheme.colors.secondary
      )
    }
  }
}

@Composable
private fun DownloadFilesButton(
  forwardConfirmation: ForwardConfirmation.FilesNotAccepted,
  rhId: Long?,
  modifier: Modifier = Modifier,
  contentPadding: PaddingValues = ButtonDefaults.TextButtonContentPadding
) {
  val user = chatModel.currentUser.value

  if (user != null) {
    TextButton(
      contentPadding = contentPadding,
      modifier = modifier,
      onClick = {
        AlertManager.shared.hideAlert()

        withBGApi {
          controller.receiveFiles(
            rhId = rhId,
            fileIds = forwardConfirmation.fileIds,
            user = user
          )
        }
      }
    ) {
      Text(stringResource(MR.strings.forward_files_not_accepted_receive_files), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
    }
  }
}

@Composable
private fun ForwardButton(
  forwardPlan: CR.ForwardPlan,
  chatInfo: ChatInfo,
  modifier: Modifier = Modifier,
  contentPadding: PaddingValues = ButtonDefaults.TextButtonContentPadding
) {
  TextButton(
    onClick = {
      forwardContent(forwardPlan.chatItemIds, chatInfo)
      AlertManager.shared.hideAlert()
    },
    modifier = modifier,
    contentPadding = contentPadding
  ) {
    Text(stringResource(MR.strings.forward_chat_item), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
  }
}

@Composable
private fun ButtonRow(horizontalArrangement: Arrangement.Horizontal, content: @Composable() (RowScope.() -> Unit)) {
  Row(
    Modifier.fillMaxWidth().padding(horizontal = DEFAULT_PADDING),
    horizontalArrangement = horizontalArrangement
  ) {
    content()
  }
}

@Composable
private fun DateSeparator(date: Instant) {
  Text(
    text = getTimestampDateText(date),
    Modifier.padding(vertical = DEFAULT_PADDING_HALF + 4.dp, horizontal = DEFAULT_PADDING_HALF).fillMaxWidth(),
    fontSize = 14.sp,
    fontWeight = FontWeight.Medium,
    textAlign = TextAlign.Center,
    color = MaterialTheme.colors.secondary
  )
}

val chatViewScrollState = MutableStateFlow(false)

fun addGroupMembers(groupInfo: GroupInfo, rhId: Long?, view: Any? = null, close: (() -> Unit)? = null) {
  hideKeyboard(view)
  withBGApi {
    setGroupMembers(rhId, groupInfo, chatModel)
    close?.invoke()
    ModalManager.end.showModalCloseable(true) { close ->
      AddGroupMembersView(rhId, groupInfo, false, chatModel, close)
    }
  }
}

fun openGroupLink(groupInfo: GroupInfo, rhId: Long?, view: Any? = null, close: (() -> Unit)? = null) {
  hideKeyboard(view)
  withBGApi {
    val link = chatModel.controller.apiGetGroupLink(rhId, groupInfo.groupId)
    close?.invoke()
    ModalManager.end.showModalCloseable(true) {
      GroupLinkView(chatModel, rhId, groupInfo, link?.first, link?.second, onGroupLinkUpdated = null)
    }
  }
}

@Composable
private fun BoxScope.BottomEndFloatingButton(
  unreadCount: State<Int>,
  showButtonWithCounter: State<Boolean>,
  showButtonWithArrow: State<Boolean>,
  composeViewHeight: State<Dp>,
  onClickArrowDown: () -> Unit,
  onClickCounter: () -> Unit
) = when {
  showButtonWithCounter.value -> {
    FloatingActionButton(
      onClick = onClickCounter,
      elevation = FloatingActionButtonDefaults.elevation(0.dp, 0.dp, 0.dp, 0.dp),
      modifier = Modifier.padding(end = DEFAULT_PADDING, bottom = DEFAULT_PADDING + composeViewHeight.value).align(Alignment.BottomEnd).size(48.dp),
      backgroundColor = MaterialTheme.colors.secondaryVariant,
    ) {
      Text(
        unreadCountStr(unreadCount.value),
        color = MaterialTheme.colors.primary,
        fontSize = 14.sp,
      )
    }
  }
  showButtonWithArrow.value -> {
    FloatingActionButton(
      onClick = onClickArrowDown,
      elevation = FloatingActionButtonDefaults.elevation(0.dp, 0.dp, 0.dp, 0.dp),
      modifier = Modifier.padding(end = DEFAULT_PADDING, bottom = DEFAULT_PADDING + composeViewHeight.value).align(Alignment.BottomEnd).size(48.dp),
      backgroundColor = MaterialTheme.colors.secondaryVariant,
    ) {
      Icon(
        painter = painterResource(MR.images.ic_keyboard_arrow_down),
        contentDescription = null,
        tint = MaterialTheme.colors.primary
      )
    }
  }
  else -> {}
}

@Composable
private fun SelectedChatItem(
  modifier: Modifier,
  ciId: Long,
  selectedChatItems: State<Set<Long>?>,
) {
  val checked = remember { derivedStateOf { selectedChatItems.value?.contains(ciId) == true } }
  Icon(
    painterResource(if (checked.value) MR.images.ic_check_circle_filled else MR.images.ic_radio_button_unchecked),
    null,
    modifier.size(22.dp * fontSizeMultiplier),
    tint = if (checked.value) {
      MaterialTheme.colors.primary
    } else if (isInDarkTheme()) {
      // .tertiaryLabel instead of .secondary
      Color(red = 235f / 255f, 235f / 255f, 245f / 255f, 76f / 255f)
    } else {
      // .tertiaryLabel instead of .secondary
      Color(red = 60f / 255f, 60f / 255f, 67f / 255f, 76f / 255f)
    }
  )
}

private fun selectUnselectChatItem(select: Boolean, ci: ChatItem, revealed: State<Boolean>, selectedChatItems: MutableState<Set<Long>?>) {
  val itemIds = mutableSetOf<Long>()
  if (!revealed.value) {
    val currIndex = chatModel.getChatItemIndexOrNull(ci)
    val ciCategory = ci.mergeCategory
    if (currIndex != null && ciCategory != null) {
      val (prevHidden, _) = chatModel.getPrevShownChatItem(currIndex, ciCategory)
      val range = chatViewItemsRange(currIndex, prevHidden)
      if (range != null) {
        val reversedChatItems = chatModel.chatItems.asReversed()
        for (i in range) {
          itemIds.add(reversedChatItems[i].id)
        }
      } else {
        itemIds.add(ci.id)
      }
    } else {
      itemIds.add(ci.id)
    }
  } else {
    itemIds.add(ci.id)
  }
  if (select) {
    val sel = selectedChatItems.value ?: setOf()
    selectedChatItems.value = sel.union(itemIds)
  } else {
    val sel = (selectedChatItems.value ?: setOf()).toMutableSet()
    sel.removeAll(itemIds)
    selectedChatItems.value = sel
  }
}

private fun deleteMessages(chatRh: Long?, chatInfo: ChatInfo, itemIds: List<Long>, forAll: Boolean, moderate: Boolean, onSuccess: () -> Unit = {}) {
  if (itemIds.isNotEmpty()) {
    withBGApi {
      val deleted = if (chatInfo is ChatInfo.Group && forAll && moderate) {
        chatModel.controller.apiDeleteMemberChatItems(
          chatRh,
          groupId = chatInfo.groupInfo.groupId,
          itemIds = itemIds
        )
      } else {
        chatModel.controller.apiDeleteChatItems(
          chatRh,
          type = chatInfo.chatType,
          id = chatInfo.apiId,
          itemIds = itemIds,
          mode = if (forAll) CIDeleteMode.cidmBroadcast else CIDeleteMode.cidmInternal
        )
      }
      if (deleted != null) {
        withChats {
          for (di in deleted) {
            val toChatItem = di.toChatItem?.chatItem
            if (toChatItem != null) {
              upsertChatItem(chatRh, chatInfo, toChatItem)
            } else {
              removeChatItem(chatRh, chatInfo, di.deletedChatItem.chatItem)
            }
          }
        }
        onSuccess()
      }
    }
  }
}

private fun markUnreadChatAsRead(chatId: String) {
  val chat = chatModel.chats.value.firstOrNull { it.id == chatId }
  if (chat?.chatStats?.unreadChat != true) return
  withApi {
    val chatRh = chat.remoteHostId
    val success = chatModel.controller.apiChatUnread(
      chatRh,
      chat.chatInfo.chatType,
      chat.chatInfo.apiId,
      false
    )
    if (success) {
      withChats {
        replaceChat(chatRh, chat.id, chat.copy(chatStats = chat.chatStats.copy(unreadChat = false)))
      }
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

fun Modifier.chatViewBackgroundModifier(
  colors: Colors,
  wallpaper: AppWallpaper,
  backgroundGraphicsLayerSize: MutableState<IntSize>?,
  backgroundGraphicsLayer: GraphicsLayer?
): Modifier {
  val wallpaperImage = wallpaper.type.image
  val wallpaperType = wallpaper.type
  val backgroundColor = wallpaper.background ?: wallpaperType.defaultBackgroundColor(CurrentColors.value.base, colors.background)
  val tintColor = wallpaper.tint ?: wallpaperType.defaultTintColor(CurrentColors.value.base)

  return this
    .then(if (wallpaperImage != null)
      Modifier.drawWithCache { chatViewBackground(wallpaperImage, wallpaperType, backgroundColor, tintColor, backgroundGraphicsLayerSize, backgroundGraphicsLayer) }
    else
      Modifier.drawWithCache { onDrawBehind { copyBackgroundToAppBar(backgroundGraphicsLayerSize, backgroundGraphicsLayer) { drawRect(backgroundColor) } } }
    )
}

fun chatViewItemsRange(currIndex: Int?, prevHidden: Int?): IntRange? =
  if (currIndex != null && prevHidden != null && prevHidden > currIndex) {
    currIndex..prevHidden
  } else {
    null
  }


sealed class ProviderMedia {
  data class Image(val data: ByteArray, val image: ImageBitmap): ProviderMedia()
  data class Video(val uri: URI, val fileSource: CryptoFile?, val preview: String): ProviderMedia()
}

fun providerForGallery(
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
    // The first was deleted or moderated
    if (indexOfFirst == -1) return null
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

  // Pager has a bug with overflowing when total pages is around Int.MAX_VALUE. Using smaller value
  var initialIndex = 10000 / 2
  var initialChatId = cItemId
  return object: ImageGalleryProvider {
    override val initialIndex: Int = initialIndex
    override val totalMediaSize = mutableStateOf(10000)
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
            ProviderMedia.Video(uri, item.file?.fileSource, (item.content.msgContent as MsgContent.MCVideo).image)
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
      initialChatId = chatItems.firstOrNull { canShowMedia(it) }?.id ?: return
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

private fun forwardContent(chatItemsIds: List<Long>, chatInfo: ChatInfo) {
  chatModel.chatId.value = null
  chatModel.sharedContent.value = SharedContent.Forward(
    chatModel.chatItems.value.filter { chatItemsIds.contains(it.id) },
    chatInfo
  )
}

private fun forwardConfirmationAlertDescription(forwardConfirmation: ForwardConfirmation): String {
  return when (forwardConfirmation) {
    is ForwardConfirmation.FilesNotAccepted -> String.format(generalGetString(MR.strings.forward_files_not_accepted_desc), forwardConfirmation.fileIds.count())
    is ForwardConfirmation.FilesInProgress -> String.format(generalGetString(MR.strings.forward_files_in_progress_desc), forwardConfirmation.filesCount)
    is ForwardConfirmation.FilesFailed -> String.format(generalGetString(MR.strings.forward_files_failed_to_receive_desc), forwardConfirmation.filesCount)
    is ForwardConfirmation.FilesMissing -> String.format(generalGetString(MR.strings.forward_files_missing_desc), forwardConfirmation.filesCount)
  }
}

private fun handleForwardConfirmation(
  rhId: Long?,
  forwardPlan: CR.ForwardPlan,
  chatInfo: ChatInfo
) {
  var alertDescription = if (forwardPlan.forwardConfirmation != null) forwardConfirmationAlertDescription(forwardPlan.forwardConfirmation) else ""

  if (forwardPlan.chatItemIds.isNotEmpty()) {
    alertDescription += "\n${generalGetString(MR.strings.forward_alert_forward_messages_without_files)}"
  }

  AlertManager.shared.showAlertDialogButtonsColumn(
    title = if (forwardPlan.chatItemIds.isNotEmpty())
      String.format(generalGetString(MR.strings.forward_alert_title_messages_to_forward), forwardPlan.chatItemIds.count()) else
        generalGetString(MR.strings.forward_alert_title_nothing_to_forward),
    text = alertDescription,
    buttons = {
      if (forwardPlan.chatItemIds.isNotEmpty()) {
        when (val confirmation = forwardPlan.forwardConfirmation) {
          is ForwardConfirmation.FilesNotAccepted -> {
            val fillMaxWidthModifier = Modifier.fillMaxWidth()
            val contentPadding = PaddingValues(vertical = DEFAULT_MIN_SECTION_ITEM_PADDING_VERTICAL)
            Column {
              ForwardButton(forwardPlan, chatInfo, fillMaxWidthModifier, contentPadding)
              DownloadFilesButton(confirmation, rhId, fillMaxWidthModifier, contentPadding)
              TextButton(onClick = { AlertManager.shared.hideAlert() }, modifier = fillMaxWidthModifier, contentPadding = contentPadding) {
                Text(stringResource(MR.strings.cancel_verb), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
              }
            }
          }
          else -> {
            ButtonRow(Arrangement.SpaceBetween) {
              TextButton(onClick = { AlertManager.shared.hideAlert() }) {
                Text(stringResource(MR.strings.cancel_verb), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
              }
              ForwardButton(forwardPlan, chatInfo)
            }
          }
        }
      } else {
        when (val confirmation = forwardPlan.forwardConfirmation) {
          is ForwardConfirmation.FilesNotAccepted -> {
            ButtonRow(Arrangement.SpaceBetween) {
              TextButton(onClick = { AlertManager.shared.hideAlert() }) {
                Text(stringResource(MR.strings.cancel_verb), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
              }
              DownloadFilesButton(confirmation, rhId)
            }
          }
          else -> ButtonRow(Arrangement.Center) {
            TextButton(onClick = { AlertManager.shared.hideAlert() }) {
              Text(stringResource(MR.strings.ok), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
            }
          }
        }
      }
    }
  )
}

private fun getItemSeparation(chatItem: ChatItem, nextItem: ChatItem?): ItemSeparation {
  if (nextItem == null) {
    return ItemSeparation(timestamp = true, largeGap = true, date = null)
  }

  val sameMemberAndDirection = if (nextItem.chatDir is GroupRcv && chatItem.chatDir is GroupRcv) {
    chatItem.chatDir.groupMember.groupMemberId == nextItem.chatDir.groupMember.groupMemberId
  } else chatItem.chatDir.sent == nextItem.chatDir.sent
  val largeGap = !sameMemberAndDirection || (abs(nextItem.meta.createdAt.epochSeconds - chatItem.meta.createdAt.epochSeconds) >= 60)

  return ItemSeparation(
    timestamp = largeGap || nextItem.meta.timestampText != chatItem.meta.timestampText,
    largeGap = largeGap,
    date = if (getTimestampDateText(chatItem.meta.itemTs) == getTimestampDateText(nextItem.meta.itemTs)) null else nextItem.meta.itemTs
  )
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
      remoteHostId = remember { mutableStateOf(null) },
      chatInfo = remember { mutableStateOf(ChatInfo.Direct.sampleData) },
      unreadCount = unreadCount,
      composeState = remember { mutableStateOf(ComposeState(useLinkPreviews = true)) },
      composeView = {},
      attachmentOption = remember { mutableStateOf<AttachmentOption?>(null) },
      attachmentBottomSheetState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden),
      searchValue,
      useLinkPreviews = true,
      linkMode = SimplexLinkMode.DESCRIPTION,
      selectedChatItems = remember { mutableStateOf(setOf()) },
      back = {},
      info = {},
      showMemberInfo = { _, _ -> },
      loadMessages = { _, _, _ -> },
      deleteMessage = { _, _ -> },
      deleteMessages = { _ -> },
      receiveFile = { _ -> },
      cancelFile = {},
      joinGroup = { _, _ -> },
      startCall = {},
      endCall = {},
      acceptCall = { _ -> },
      acceptFeature = { _, _, _ -> },
      openDirectChat = { _ -> },
      forwardItem = { _, _ -> },
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
      showViaProxy = false,
      showSearch =  remember { mutableStateOf(false) }
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
      remoteHostId = remember { mutableStateOf(null) },
      chatInfo = remember { mutableStateOf(ChatInfo.Direct.sampleData) },
      unreadCount = unreadCount,
      composeState = remember { mutableStateOf(ComposeState(useLinkPreviews = true)) },
      composeView = {},
      attachmentOption = remember { mutableStateOf<AttachmentOption?>(null) },
      attachmentBottomSheetState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden),
      searchValue,
      useLinkPreviews = true,
      linkMode = SimplexLinkMode.DESCRIPTION,
      selectedChatItems = remember { mutableStateOf(setOf()) },
      back = {},
      info = {},
      showMemberInfo = { _, _ -> },
      loadMessages = { _, _, _ -> },
      deleteMessage = { _, _ -> },
      deleteMessages = {},
      receiveFile = { _ -> },
      cancelFile = {},
      joinGroup = { _, _ -> },
      startCall = {},
      endCall = {},
      acceptCall = { _ -> },
      acceptFeature = { _, _, _ -> },
      openDirectChat = { _ -> },
      forwardItem = { _, _ -> },
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
      showViaProxy = false,
      showSearch =  remember { mutableStateOf(false) }
    )
  }
}
