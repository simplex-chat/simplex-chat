package chat.simplex.common.views.chatlist

import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.interaction.collectIsHoveredAsState
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.focus.*
import androidx.compose.ui.graphics.*
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.platform.*
import androidx.compose.ui.text.TextRange
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.unit.*
import chat.simplex.common.SettingsViewState
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatController.stopRemoteHostAndReloadHosts
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.WhatsNewView
import chat.simplex.common.views.onboarding.shouldShowWhatsNew
import chat.simplex.common.platform.*
import chat.simplex.common.views.call.Call
import chat.simplex.common.views.chat.item.CIFileViewScope
import chat.simplex.common.views.newchat.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.res.MR
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.serialization.json.Json
import java.net.URI
import kotlin.time.Duration.Companion.seconds

private fun showNewChatSheet(oneHandUI: State<Boolean>) {
  ModalManager.start.closeModals()
  ModalManager.end.closeModals()
  chatModel.newChatSheetVisible.value = true
  ModalManager.start.showCustomModal { close ->
    val close = {
      // It will set it faster than in onDispose. It's important to catch the actual state before
      // closing modal for reacting with status bar changes in [App]
      chatModel.newChatSheetVisible.value = false
      close()
    }
    ModalView(close, closeOnTop = !oneHandUI.value) {
      if (appPlatform.isAndroid) {
        BackHandler {
          close()
        }
      }
      NewChatSheet(rh = chatModel.currentRemoteHost.value, close)
      DisposableEffect(Unit) {
        onDispose {
          chatModel.newChatSheetVisible.value = false
        }
      }
    }
  }
}

@Composable
fun ToggleChatListCard() {
  Column(
    modifier = Modifier
      .padding(16.dp)
      .clip(RoundedCornerShape(18.dp))
  ) {
    Box(
      modifier = Modifier
        .background(MaterialTheme.appColors.sentMessage)
    ) {
      Box(
        modifier = Modifier.fillMaxWidth().matchParentSize().padding(5.dp),
        contentAlignment = Alignment.TopEnd
      ) {
        IconButton(
          onClick = {
            appPrefs.oneHandUICardShown.set(true)
            AlertManager.shared.showAlertMsg(
              title = generalGetString(MR.strings.one_hand_ui),
              text = generalGetString(MR.strings.one_hand_ui_change_instruction),
            )
          }
        ) {
          Icon(
            painterResource(MR.images.ic_close), stringResource(MR.strings.back), tint = MaterialTheme.colors.secondary
          )
        }
      }
      Column(
        modifier = Modifier
          .padding(horizontal = DEFAULT_PADDING)
          .padding(top = DEFAULT_PADDING)
      ) {
        Row(
          horizontalArrangement = Arrangement.Start,
          verticalAlignment = Alignment.CenterVertically,
          modifier = Modifier.fillMaxWidth()
        ) {
          Text(stringResource(MR.strings.one_hand_ui_card_title), style = MaterialTheme.typography.h3)
        }
        Row(
          Modifier.fillMaxWidth().padding(top = 6.dp, bottom = 12.dp),
          verticalAlignment = Alignment.CenterVertically
        ) {
          Text(stringResource(MR.strings.one_hand_ui), Modifier.weight(10f), style = MaterialTheme.typography.body1)

          Spacer(Modifier.fillMaxWidth().weight(1f))

          SharedPreferenceToggle(
            appPrefs.oneHandUI,
            enabled = true,
            onChange = {
              val c = CurrentColors.value.colors
              platform.androidSetStatusAndNavBarColors(c.isLight, c.background, !appPrefs.oneHandUI.get(), appPrefs.oneHandUI.get())
            }
          )
        }
      }
    }
  }
}

@Composable
fun ChatListView(chatModel: ChatModel, settingsState: SettingsViewState, setPerformLA: (Boolean) -> Unit, stopped: Boolean) {
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  LaunchedEffect(Unit) {
    if (shouldShowWhatsNew(chatModel)) {
      delay(1000L)
      ModalManager.center.showCustomModal { close -> WhatsNewView(close = close) }
    }
  }

  if (appPlatform.isDesktop) {
    KeyChangeEffect(chatModel.chatId.value) {
      if (chatModel.chatId.value != null) {
        ModalManager.end.closeModalsExceptFirst()
      }
      AudioPlayer.stop()
      VideoPlayerHolder.stopAll()
    }
  }
  val endPadding = if (appPlatform.isDesktop) 56.dp else 0.dp
  val searchText = rememberSaveable(stateSaver = TextFieldValue.Saver) { mutableStateOf(TextFieldValue("")) }
  val scope = rememberCoroutineScope()
  val (userPickerState, scaffoldState ) = settingsState
  Scaffold(
    topBar = {
      if (!oneHandUI.value) {
        Column(Modifier.padding(end = endPadding)) {
          ChatListToolbar(
            scaffoldState.drawerState,
            userPickerState,
            stopped,
          )
          Divider()
        }
      }
    },
    bottomBar = {
      if (oneHandUI.value) {
        Column(Modifier.padding(end = endPadding)) {
          Divider()
          ChatListToolbar(
            scaffoldState.drawerState,
            userPickerState,
            stopped,
          )
        }
      }
    },
    scaffoldState = scaffoldState,
    drawerContent = {
      tryOrShowError("Settings", error = { ErrorSettingsView() }) {
        SettingsView(chatModel, setPerformLA, scaffoldState.drawerState)
      }
    },
    contentColor = LocalContentColor.current,
    drawerContentColor = LocalContentColor.current,
    drawerScrimColor = MaterialTheme.colors.onSurface.copy(alpha = if (isInDarkTheme()) 0.16f else 0.32f),
    drawerGesturesEnabled = appPlatform.isAndroid,
    floatingActionButton = {
      if (!oneHandUI.value && searchText.value.text.isEmpty() && !chatModel.desktopNoUserNoRemote && chatModel.chatRunning.value == true) {
        FloatingActionButton(
          onClick = {
            if (!stopped) {
              showNewChatSheet(oneHandUI)
            }
          },
          Modifier
            .padding(end = DEFAULT_PADDING - 16.dp + endPadding, bottom = DEFAULT_PADDING - 16.dp)
            .size(AppBarHeight * fontSizeSqrtMultiplier),
          elevation = FloatingActionButtonDefaults.elevation(
            defaultElevation = 0.dp,
            pressedElevation = 0.dp,
            hoveredElevation = 0.dp,
            focusedElevation = 0.dp,
          ),
          backgroundColor = if (!stopped) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
          contentColor = Color.White
        ) {
          Icon(painterResource(MR.images.ic_edit_filled), stringResource(MR.strings.add_contact_or_create_group), Modifier.size(24.dp * fontSizeSqrtMultiplier))
        }
      }
    }
  ) {
    Box(Modifier.padding(it).padding(end = endPadding)) {
      Box(
        modifier = Modifier
          .fillMaxSize()
      ) {
        if (!chatModel.desktopNoUserNoRemote) {
          ChatList(chatModel, searchText = searchText)
        }
        if (chatModel.chats.value.isEmpty() && !chatModel.switchingUsersAndHosts.value && !chatModel.desktopNoUserNoRemote) {
          Text(stringResource(
            if (chatModel.chatRunning.value == null) MR.strings.loading_chats else MR.strings.you_have_no_chats), Modifier.align(Alignment.Center), color = MaterialTheme.colors.secondary)
        }
      }
    }
  }
  if (searchText.value.text.isEmpty()) {
    if (appPlatform.isDesktop) {
      val call = remember { chatModel.activeCall }.value
      if (call != null) {
        ActiveCallInteractiveArea(call)
      }
    }
  }
  if (appPlatform.isAndroid) {
    tryOrShowError("UserPicker", error = {}) {
      UserPicker(
        chatModel = chatModel,
        userPickerState = userPickerState,
        contentAlignment = if (oneHandUI.value) Alignment.BottomStart else Alignment.TopStart
      ) {
        scope.launch { if (scaffoldState.drawerState.isOpen) scaffoldState.drawerState.close() else scaffoldState.drawerState.open() }
        userPickerState.value = AnimatedViewState.GONE
      }
    }
  }
}

@Composable
private fun ConnectButton(text: String, onClick: () -> Unit) {
  Button(
    onClick,
    shape = RoundedCornerShape(21.dp),
    colors = ButtonDefaults.textButtonColors(
      backgroundColor = MaterialTheme.colors.primaryVariant
    ),
    elevation = null,
    contentPadding = PaddingValues(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF),
    modifier = Modifier.height(42.dp)
  ) {
    Text(text, color = Color.White)
  }
}

@Composable
private fun ChatListToolbar(drawerState: DrawerState, userPickerState: MutableStateFlow<AnimatedViewState>, stopped: Boolean) {
  val serversSummary: MutableState<PresentedServersSummary?> = remember { mutableStateOf(null) }
  val barButtons = arrayListOf<@Composable RowScope.() -> Unit>()
  val updatingProgress = remember { chatModel.updatingProgress }.value
  val oneHandUI = remember { appPrefs.oneHandUI.state }

  if (oneHandUI.value) {
    val sp16 = with(LocalDensity.current) { 16.sp.toDp() }

    if (!stopped) {
      barButtons.add {
        IconButton(
          onClick = {
            showNewChatSheet(oneHandUI)
          },
        ) {
          Box(
            contentAlignment = Alignment.Center,
            modifier = Modifier
              .background(MaterialTheme.colors.primary, shape = CircleShape)
              .size(33.dp * fontSizeSqrtMultiplier)
          ) {
            Icon(
              painterResource(MR.images.ic_edit_filled),
              stringResource(MR.strings.add_contact_or_create_group),
              Modifier.size(sp16),
              tint = Color.White
            )
          }
        }
      }
    }
  }

  if (updatingProgress != null) {
    barButtons.add {
      val interactionSource = remember { MutableInteractionSource() }
      val hovered = interactionSource.collectIsHoveredAsState().value
      IconButton(onClick = {
        chatModel.updatingRequest?.close()
      }, Modifier.hoverable(interactionSource)) {
        if (hovered) {
          Icon(painterResource(MR.images.ic_close), null, tint = WarningOrange)
        } else if (updatingProgress == -1f) {
          CIFileViewScope.progressIndicator()
        } else {
          CIFileViewScope.progressCircle((updatingProgress * 100).toLong(), 100)
        }
      }
    }
  } else if (stopped) {
    barButtons.add {
      IconButton(onClick = {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.chat_is_stopped_indication),
          generalGetString(MR.strings.you_can_start_chat_via_setting_or_by_restarting_the_app)
        )
      }) {
        Icon(
          painterResource(MR.images.ic_report_filled),
          generalGetString(MR.strings.chat_is_stopped_indication),
          tint = Color.Red,
        )
      }
    }
  }
  val scope = rememberCoroutineScope()
  val clipboard = LocalClipboardManager.current
  DefaultTopAppBar(
    navigationButton = {
      if (chatModel.users.isEmpty() && !chatModel.desktopNoUserNoRemote) {
        NavigationButtonMenu { scope.launch { if (drawerState.isOpen) drawerState.close() else drawerState.open() } }
      } else {
        val users by remember { derivedStateOf { chatModel.users.filter { u -> u.user.activeUser || !u.user.hidden } } }
        val allRead = users
          .filter { u -> !u.user.activeUser && !u.user.hidden }
          .all { u -> u.unreadCount == 0 }
        UserProfileButton(chatModel.currentUser.value?.profile?.image, allRead) {
          if (users.size == 1 && chatModel.remoteHosts.isEmpty()) {
            scope.launch { drawerState.open() }
          } else {
            userPickerState.value = AnimatedViewState.VISIBLE
          }
        }
      }
    },
    title = {
      Row(verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(DEFAULT_SPACE_AFTER_ICON)) {
        Text(
          stringResource(MR.strings.your_chats),
          color = MaterialTheme.colors.onBackground,
          fontWeight = FontWeight.SemiBold,
        )
        SubscriptionStatusIndicator(
          click = {
            ModalManager.start.closeModals()
            ModalManager.start.showModalCloseable(
              endButtons = {
                val summary = serversSummary.value
                if (summary != null) {
                  ShareButton {
                    val json = Json {
                      prettyPrint = true
                    }

                    val text = json.encodeToString(PresentedServersSummary.serializer(), summary)
                    clipboard.shareText(text)
                  }
                }
              }
            ) { ServersSummaryView(chatModel.currentRemoteHost.value, serversSummary) }
          }
        )
      }
    },
    onTitleClick = null,
    showSearch = false,
    onSearchValueChanged = {},
    buttons = barButtons
  )
}

@Composable
fun SubscriptionStatusIndicator(click: (() -> Unit)) {
  var subs by remember { mutableStateOf(SMPServerSubs.newSMPServerSubs) }
  var hasSess by remember { mutableStateOf(false) }
  val scope = rememberCoroutineScope()

  suspend fun setSubsTotal() {
    if (chatModel.currentUser.value != null && chatModel.controller.hasChatCtrl() && chatModel.chatRunning.value == true) {
      val r = chatModel.controller.getAgentSubsTotal(chatModel.remoteHostId())
      if (r != null) {
        subs = r.first
        hasSess = r.second
      }
    }
  }

  LaunchedEffect(Unit) {
    setSubsTotal()
    scope.launch {
      while (isActive) {
        delay(1.seconds)
        if ((appPlatform.isDesktop || chatModel.chatId.value == null) && !ModalManager.start.hasModalsOpen() && !ModalManager.fullscreen.hasModalsOpen() && isAppVisibleAndFocused()) {
          setSubsTotal()
        }
      }
    }
  }

  SimpleButtonFrame(
    click = click,
    disabled = chatModel.chatRunning.value != true
  ) {
    SubscriptionStatusIndicatorView(subs = subs, hasSess = hasSess)
  }
}

@Composable
fun UserProfileButton(image: String?, allRead: Boolean, onButtonClicked: () -> Unit) {
  Row(verticalAlignment = Alignment.CenterVertically) {
    IconButton(onClick = onButtonClicked) {
      Box {
        ProfileImage(
          image = image,
          size = 37.dp * fontSizeSqrtMultiplier,
          color = MaterialTheme.colors.secondaryVariant.mixWith(MaterialTheme.colors.onBackground, 0.97f)
        )
        if (!allRead) {
          unreadBadge()
        }
      }
    }
    if (appPlatform.isDesktop) {
      val h by remember { chatModel.currentRemoteHost }
      if (h != null) {
        Spacer(Modifier.width(12.dp))
        HostDisconnectButton {
          stopRemoteHostAndReloadHosts(h!!, true)
        }
      }
    }
  }
}


@Composable
private fun BoxScope.unreadBadge(text: String? = "") {
  Text(
    text ?: "",
    color = MaterialTheme.colors.onPrimary,
    fontSize = 6.sp,
    modifier = Modifier
      .background(MaterialTheme.colors.primary, shape = CircleShape)
      .badgeLayout()
      .padding(horizontal = 3.dp)
      .padding(vertical = 1.dp)
      .align(Alignment.TopEnd)
  )
}

@Composable
private fun ToggleFilterEnabledButton() {
  val pref = remember { ChatController.appPrefs.showUnreadAndFavorites }
  IconButton(onClick = { pref.set(!pref.get()) }) {
    val sp16 = with(LocalDensity.current) { 16.sp.toDp() }
    Icon(
      painterResource(MR.images.ic_filter_list),
      null,
      tint = if (pref.state.value) MaterialTheme.colors.background else MaterialTheme.colors.secondary,
      modifier = Modifier
        .padding(3.dp)
        .background(color = if (pref.state.value) MaterialTheme.colors.primary else Color.Unspecified, shape = RoundedCornerShape(50))
        .border(width = 1.dp, color = if (pref.state.value) MaterialTheme.colors.primary else Color.Unspecified, shape = RoundedCornerShape(50))
        .padding(3.dp)
        .size(sp16)
    )
  }
}

@Composable
expect fun ActiveCallInteractiveArea(call: Call)

fun connectIfOpenedViaUri(rhId: Long?, uri: URI, chatModel: ChatModel) {
  Log.d(TAG, "connectIfOpenedViaUri: opened via link")
  if (chatModel.currentUser.value == null) {
    chatModel.appOpenUrl.value = rhId to uri
  } else {
    withBGApi {
      planAndConnect(rhId, uri, incognito = null, close = null)
    }
  }
}

@Composable
private fun ChatListSearchBar(listState: LazyListState, searchText: MutableState<TextFieldValue>, searchShowingSimplexLink: MutableState<Boolean>, searchChatFilteredBySimplexLink: MutableState<String?>) {
  Row(verticalAlignment = Alignment.CenterVertically, modifier = Modifier.fillMaxWidth()) {
    val focusRequester = remember { FocusRequester() }
    var focused by remember { mutableStateOf(false) }
    Icon(
      painterResource(MR.images.ic_search),
      contentDescription = null,
      Modifier.padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING_HALF).size(24.dp * fontSizeSqrtMultiplier),
      tint = MaterialTheme.colors.secondary
    )
    SearchTextField(
      Modifier.weight(1f).onFocusChanged { focused = it.hasFocus }.focusRequester(focusRequester),
      placeholder = stringResource(MR.strings.search_or_paste_simplex_link),
      alwaysVisible = true,
      searchText = searchText,
      enabled = !remember { searchShowingSimplexLink }.value,
      trailingContent = null,
    ) {
      searchText.value = searchText.value.copy(it)
    }
    val hasText = remember { derivedStateOf { searchText.value.text.isNotEmpty() } }
    if (hasText.value) {
      val hideSearchOnBack: () -> Unit = { searchText.value = TextFieldValue() }
      BackHandler(onBack = hideSearchOnBack)
      KeyChangeEffect(chatModel.currentRemoteHost.value) {
        hideSearchOnBack()
      }
    } else {
      val padding = if (appPlatform.isDesktop) 0.dp else 7.dp
      if (chatModel.chats.value.isNotEmpty()) {
        ToggleFilterEnabledButton() 
      }
      Spacer(Modifier.width(padding))
    }
    val focusManager = LocalFocusManager.current
    val keyboardState = getKeyboardState()
    LaunchedEffect(keyboardState.value) {
      if (keyboardState.value == KeyboardState.Closed && focused) {
        focusManager.clearFocus()
      }
    }
    val view = LocalMultiplatformView()
    LaunchedEffect(Unit) {
      snapshotFlow { searchText.value.text }
        .distinctUntilChanged()
        .collect {
          val link = strHasSingleSimplexLink(it.trim())
          if (link != null) {
            // if SimpleX link is pasted, show connection dialogue
            hideKeyboard(view)
            if (link.format is Format.SimplexLink) {
              val linkText = link.simplexLinkText(link.format.linkType, link.format.smpHosts)
              searchText.value = searchText.value.copy(linkText, selection = TextRange.Zero)
            }
            searchShowingSimplexLink.value = true
            searchChatFilteredBySimplexLink.value = null
            connect(link.text, searchChatFilteredBySimplexLink) { searchText.value = TextFieldValue() }
          } else if (!searchShowingSimplexLink.value || it.isEmpty()) {
            if (it.isNotEmpty()) {
              // if some other text is pasted, enter search mode
              focusRequester.requestFocus()
            } else if (listState.layoutInfo.totalItemsCount > 0) {
              listState.scrollToItem(0)
            }
            searchShowingSimplexLink.value = false
            searchChatFilteredBySimplexLink.value = null
          }
        }
    }
  }
}

private fun connect(link: String, searchChatFilteredBySimplexLink: MutableState<String?>, cleanup: (() -> Unit)?) {
  withBGApi {
    planAndConnect(
      chatModel.remoteHostId(),
      URI.create(link),
      incognito = null,
      filterKnownContact = { searchChatFilteredBySimplexLink.value = it.id },
      filterKnownGroup = { searchChatFilteredBySimplexLink.value = it.id },
      close = null,
      cleanup = cleanup,
    )
  }
}

@Composable
private fun ErrorSettingsView() {
  Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
    Text(generalGetString(MR.strings.error_showing_content), color = MaterialTheme.colors.error, fontStyle = FontStyle.Italic)
  }
}

private var lazyListState = 0 to 0

enum class ScrollDirection {
  Up, Down, Idle
}

@Composable
private fun ChatList(chatModel: ChatModel, searchText: MutableState<TextFieldValue>) {
  val listState = rememberLazyListState(lazyListState.first, lazyListState.second)
  var scrollDirection by remember { mutableStateOf(ScrollDirection.Idle) }
  var previousIndex by remember { mutableStateOf(0) }
  var previousScrollOffset by remember { mutableStateOf(0) }
  val keyboardState by getKeyboardState()
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  val oneHandUICardShown = remember { appPrefs.oneHandUICardShown.state }

  LaunchedEffect(listState.firstVisibleItemIndex, listState.firstVisibleItemScrollOffset) {
    val currentIndex = listState.firstVisibleItemIndex
    val currentScrollOffset = listState.firstVisibleItemScrollOffset
    val threshold = 25

    scrollDirection = when {
      currentIndex > previousIndex -> ScrollDirection.Down
      currentIndex < previousIndex -> ScrollDirection.Up
      currentScrollOffset > previousScrollOffset + threshold -> ScrollDirection.Down
      currentScrollOffset < previousScrollOffset - threshold -> ScrollDirection.Up
      currentScrollOffset == previousScrollOffset -> ScrollDirection.Idle
      else -> scrollDirection
    }

    previousIndex = currentIndex
    previousScrollOffset = currentScrollOffset
  }

  DisposableEffect(Unit) {
    onDispose { lazyListState = listState.firstVisibleItemIndex to listState.firstVisibleItemScrollOffset }
  }
  val showUnreadAndFavorites = remember { ChatController.appPrefs.showUnreadAndFavorites.state }.value
  val allChats = remember { chatModel.chats }
  // In some not always reproducible situations this code produce IndexOutOfBoundsException on Compose's side
  // which is related to [derivedStateOf]. Using safe alternative instead
  // val chats by remember(search, showUnreadAndFavorites) { derivedStateOf { filteredChats(showUnreadAndFavorites, search, allChats.toList()) } }
  val searchShowingSimplexLink = remember { mutableStateOf(false) }
  val searchChatFilteredBySimplexLink = remember { mutableStateOf<String?>(null) }
  val chats = filteredChats(showUnreadAndFavorites, searchShowingSimplexLink, searchChatFilteredBySimplexLink, searchText.value.text, allChats.value.toList())
  LazyColumnWithScrollBar(
    Modifier.fillMaxSize(),
    listState,
    reverseLayout = oneHandUI.value
  ) {
    stickyHeader {
      Column(
        Modifier
          .offset {
            val y = if (searchText.value.text.isEmpty()) {
              val offsetMultiplier = if (oneHandUI.value) 1 else -1
              if (
                (oneHandUI.value && scrollDirection == ScrollDirection.Up) ||
                (appPlatform.isAndroid && keyboardState == KeyboardState.Opened)
              ) {
                0
              } else if (listState.firstVisibleItemIndex == 0) offsetMultiplier * listState.firstVisibleItemScrollOffset else offsetMultiplier * 1000
            } else {
              0
            }
            IntOffset(0, y)
          }
          .background(MaterialTheme.colors.background),
        ) {
        if (oneHandUI.value) {
          Divider()
        }
        ChatListSearchBar(listState, searchText, searchShowingSimplexLink, searchChatFilteredBySimplexLink)
        if (!oneHandUI.value) {
          Divider()
        }
      }
    }
    if (appPlatform.isAndroid && !oneHandUICardShown.value && chats.count() > 1) {
      item {
        ToggleChatListCard()
      }
    }
    itemsIndexed(chats, key = { _, chat -> chat.remoteHostId to chat.id }) { index, chat ->
      val nextChatSelected = remember(chat.id, chats) { derivedStateOf {
        chatModel.chatId.value != null && chats.getOrNull(index + 1)?.id == chatModel.chatId.value
      } }
      ChatListNavLinkView(chat, nextChatSelected)
    }
    if (appPlatform.isAndroid && !oneHandUICardShown.value && chats.count() <= 1) {
      item {
        ToggleChatListCard()
      }
    }
  }
  if (chats.isEmpty() && chatModel.chats.value.isNotEmpty()) {
    Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
      Text(generalGetString(MR.strings.no_filtered_chats), color = MaterialTheme.colors.secondary)
    }
  }
}

fun filteredChats(
  showUnreadAndFavorites: Boolean,
  searchShowingSimplexLink: State<Boolean>,
  searchChatFilteredBySimplexLink: State<String?>,
  searchText: String,
  chats: List<Chat>
): List<Chat> {
  val linkChatId = searchChatFilteredBySimplexLink.value
  return if (linkChatId != null) {
    chats.filter { it.id == linkChatId }
  } else {
    val s = if (searchShowingSimplexLink.value) "" else searchText.trim().lowercase()
    if (s.isEmpty() && !showUnreadAndFavorites)
      chats.filter { chat -> !chat.chatInfo.chatDeleted && chatContactType(chat) != ContactType.CARD }
    else {
      chats.filter { chat ->
        when (val cInfo = chat.chatInfo) {
          is ChatInfo.Direct -> chatContactType(chat) != ContactType.CARD && !chat.chatInfo.chatDeleted && (
            if (s.isEmpty()) {
              chat.id == chatModel.chatId.value || filtered(chat)
            } else {
              cInfo.anyNameContains(s)
            })
          is ChatInfo.Group -> if (s.isEmpty()) {
            chat.id == chatModel.chatId.value || filtered(chat) || cInfo.groupInfo.membership.memberStatus == GroupMemberStatus.MemInvited
          } else {
            cInfo.anyNameContains(s)
          }
          is ChatInfo.Local -> s.isEmpty() || cInfo.anyNameContains(s)
          is ChatInfo.ContactRequest -> s.isEmpty() || cInfo.anyNameContains(s)
          is ChatInfo.ContactConnection -> (s.isNotEmpty() && cInfo.anyNameContains(s)) || (s.isEmpty() && chat.id == chatModel.chatId.value)
          is ChatInfo.InvalidJSON -> chat.id == chatModel.chatId.value
        }
      }
    }
  }
}

private fun filtered(chat: Chat): Boolean =
  (chat.chatInfo.chatSettings?.favorite ?: false) ||
      chat.chatStats.unreadChat ||
      (chat.chatInfo.ntfsEnabled && chat.chatStats.unreadCount > 0)
