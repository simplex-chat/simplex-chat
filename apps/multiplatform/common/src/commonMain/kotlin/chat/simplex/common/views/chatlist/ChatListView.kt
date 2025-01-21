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
import androidx.compose.ui.*
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
import chat.simplex.common.AppLock
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatController.setConditionsNotified
import chat.simplex.common.model.ChatController.stopRemoteHostAndReloadHosts
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.call.Call
import chat.simplex.common.views.chat.item.CIFileViewScope
import chat.simplex.common.views.chat.topPaddingToContent
import chat.simplex.common.views.mkValidName
import chat.simplex.common.views.newchat.*
import chat.simplex.common.views.onboarding.*
import chat.simplex.common.views.showInvalidNameAlert
import chat.simplex.common.views.usersettings.*
import chat.simplex.common.views.usersettings.networkAndServers.ConditionsLinkButton
import chat.simplex.common.views.usersettings.networkAndServers.UsageConditionsView
import chat.simplex.res.MR
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.serialization.json.Json
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
    ModalView(close, showAppBar = !oneHandUI.value) {
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
  ChatListCard(
    close = {
      appPrefs.oneHandUICardShown.set(true)
      AlertManager.shared.showAlertMsg(
        title = generalGetString(MR.strings.one_hand_ui),
        text = generalGetString(MR.strings.one_hand_ui_change_instruction),
      )
    }
  ) {
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
          enabled = true
        )
      }
    }
  }
}

@Composable
fun ChatListView(chatModel: ChatModel, userPickerState: MutableStateFlow<AnimatedViewState>, setPerformLA: (Boolean) -> Unit, stopped: Boolean) {
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  val rhId = chatModel.remoteHostId()

  LaunchedEffect(Unit) {
    val showWhatsNew = shouldShowWhatsNew(chatModel)
    val showUpdatedConditions = chatModel.conditions.value.conditionsAction?.shouldShowNotice ?: false
    if (showWhatsNew) {
      delay(1000L)
      ModalManager.center.showCustomModal { close -> WhatsNewView(close = close, updatedConditions = showUpdatedConditions) }
    } else if (showUpdatedConditions) {
      ModalManager.center.showModalCloseable(endButtons = { ConditionsLinkButton() }) { close ->
        LaunchedEffect(Unit) {
          val conditionsId = chatModel.conditions.value.currentConditions.conditionsId
          try {
            setConditionsNotified(rh = rhId, conditionsId = conditionsId)
          } catch (e: Exception) {
            Log.d(TAG, "UsageConditionsView setConditionsNotified error: ${e.message}")
          }
        }
        UsageConditionsView(userServers = mutableStateOf(emptyList()), currUserServers = mutableStateOf(emptyList()), close = close, rhId = rhId)
      }
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
  val searchText = rememberSaveable(stateSaver = TextFieldValue.Saver) { mutableStateOf(TextFieldValue("")) }
  val listState = rememberLazyListState(lazyListState.first, lazyListState.second)
  Box(Modifier.fillMaxSize()) {
    if (oneHandUI.value) {
      ChatListWithLoadingScreen(searchText, listState)
      Column(Modifier.align(Alignment.BottomCenter)) {
        ChatListToolbar(
          userPickerState,
          listState,
          stopped,
          setPerformLA,
        )
      }
    } else {
      ChatListWithLoadingScreen(searchText, listState)
      Column {
        ChatListToolbar(
          userPickerState,
          listState,
          stopped,
          setPerformLA,
        )
      }
      if (searchText.value.text.isEmpty() && !chatModel.desktopNoUserNoRemote && chatModel.chatRunning.value == true) {
        NewChatSheetFloatingButton(oneHandUI, stopped)
      }
    }
  }

  if (searchText.value.text.isEmpty()) {
    if (appPlatform.isDesktop && !oneHandUI.value) {
      val call = remember { chatModel.activeCall }.value
      if (call != null) {
        ActiveCallInteractiveArea(call)
      }
    }
  }
  if (appPlatform.isAndroid) {
    val wasAllowedToSetupNotifications = rememberSaveable { mutableStateOf(false) }
    val canEnableNotifications = remember { derivedStateOf { chatModel.chatRunning.value == true } }
    if (wasAllowedToSetupNotifications.value || canEnableNotifications.value) {
      SetNotificationsModeAdditions()
      LaunchedEffect(Unit) { wasAllowedToSetupNotifications.value = true }
    }
    tryOrShowError("UserPicker", error = {}) {
      UserPicker(
        chatModel = chatModel,
        userPickerState = userPickerState,
        setPerformLA = AppLock::setPerformLA
      )
    }
  }
}

@Composable
private fun ChatListCard(
  close: () -> Unit,
  onCardClick: (() -> Unit)? = null,
  content: @Composable BoxScope.() -> Unit
) {
  Column(
    modifier = Modifier.clip(RoundedCornerShape(18.dp))
  ) {
    Box(
      modifier = Modifier
        .background(MaterialTheme.appColors.sentMessage)
        .clickable {
          onCardClick?.invoke()
        }
    ) {
      Box(
        modifier = Modifier.fillMaxWidth().matchParentSize().padding(5.dp),
        contentAlignment = Alignment.TopEnd
      ) {
        IconButton(
          onClick = {
            close()
          }
        ) {
          Icon(
            painterResource(MR.images.ic_close), stringResource(MR.strings.back), tint = MaterialTheme.colors.secondary
          )
        }
      }
      content()
    }
  }
}

@Composable
private fun AddressCreationCard() {
  ChatListCard(
    close = {
      appPrefs.addressCreationCardShown.set(true)
      AlertManager.shared.showAlertMsg(
        title = generalGetString(MR.strings.simplex_address),
        text = generalGetString(MR.strings.address_creation_instruction),
      )
    },
    onCardClick = {
      ModalManager.start.showModal {
        UserAddressLearnMore(showCreateAddressButton = true)
      }
    }
  ) {
      Box(modifier = Modifier.matchParentSize().padding(end = (DEFAULT_PADDING_HALF + 2.dp) * fontSizeSqrtMultiplier, bottom = 2.dp), contentAlignment = Alignment.BottomEnd) {
      TextButton(
        onClick = {
          ModalManager.start.showModalCloseable { close ->
            UserAddressView(chatModel = chatModel, shareViaProfile = false, autoCreateAddress = true, close = close)
          }
        },
      ) {
        Text(stringResource(MR.strings.create_address_button), style = MaterialTheme.typography.body1)
      }
    }
    Row(
      Modifier
        .fillMaxWidth()
        .padding(DEFAULT_PADDING),
      verticalAlignment = Alignment.CenterVertically
    ) {
      Box(Modifier.padding(vertical = 4.dp)) {
        Box(Modifier.background(MaterialTheme.colors.primary, CircleShape).padding(12.dp)) {
          ProfileImage(size = 37.dp, null, icon = MR.images.ic_mail_filled, color = Color.White, backgroundColor = Color.Red)
        }
      }
      Column(modifier = Modifier.padding(start = DEFAULT_PADDING)) {
        Text(stringResource(MR.strings.your_simplex_contact_address), style = MaterialTheme.typography.h3)
        Spacer(Modifier.fillMaxWidth().padding(DEFAULT_PADDING_HALF))
        Row(verticalAlignment = Alignment.CenterVertically) {
          Text(stringResource(MR.strings.how_to_use_simplex_chat), Modifier.padding(end = DEFAULT_SPACE_AFTER_ICON), style = MaterialTheme.typography.body1)
          Icon(
            painterResource(MR.images.ic_info),
            null,
          )
        }
      }
    }
  }
}

@Composable
private fun BoxScope.ChatListWithLoadingScreen(searchText: MutableState<TextFieldValue>, listState: LazyListState) {
  if (!chatModel.desktopNoUserNoRemote) {
    ChatList(searchText = searchText, listState)
  }
  if (chatModel.chats.value.isEmpty() && !chatModel.switchingUsersAndHosts.value && !chatModel.desktopNoUserNoRemote) {
    Text(
      stringResource(
        if (chatModel.chatRunning.value == null) MR.strings.loading_chats else MR.strings.you_have_no_chats
      ), Modifier.align(Alignment.Center), color = MaterialTheme.colors.secondary
    )
  }
}

@Composable
private fun BoxScope.NewChatSheetFloatingButton(oneHandUI: State<Boolean>, stopped: Boolean) {
  FloatingActionButton(
    onClick = {
      if (!stopped) {
        showNewChatSheet(oneHandUI)
      }
    },
    Modifier
      .navigationBarsPadding()
      .padding(end = DEFAULT_PADDING, bottom = DEFAULT_PADDING)
      .align(Alignment.BottomEnd)
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
    Icon(painterResource(MR.images.ic_edit_filled), stringResource(MR.strings.add_contact_or_create_group), Modifier.size(22.dp * fontSizeSqrtMultiplier))
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
private fun ChatListToolbar(userPickerState: MutableStateFlow<AnimatedViewState>, listState: LazyListState, stopped: Boolean, setPerformLA: (Boolean) -> Unit) {
  val serversSummary: MutableState<PresentedServersSummary?> = remember { mutableStateOf(null) }
  val barButtons = arrayListOf<@Composable RowScope.() -> Unit>()
  val updatingProgress = remember { chatModel.updatingProgress }.value
  val oneHandUI = remember { appPrefs.oneHandUI.state }

  if (oneHandUI.value) {
    val sp16 = with(LocalDensity.current) { 16.sp.toDp() }

    if (appPlatform.isDesktop && oneHandUI.value) {
      val call = remember { chatModel.activeCall }
      if (call.value != null) {
        barButtons.add {
          val c = call.value
          if (c != null) {
            ActiveCallInteractiveArea(c)
            Spacer(Modifier.width(5.dp))
          }
        }
      }
    }
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
  val clipboard = LocalClipboardManager.current
  val scope = rememberCoroutineScope()
  val canScrollToZero = remember { derivedStateOf { listState.firstVisibleItemIndex != 0 || listState.firstVisibleItemScrollOffset != 0 } }
  DefaultAppBar(
    navigationButton = {
      if (chatModel.users.isEmpty() && !chatModel.desktopNoUserNoRemote) {
        NavigationButtonMenu {
          ModalManager.start.showModalCloseable { close ->
            SettingsView(chatModel, setPerformLA, close)
          }
        }
      } else {
        val users by remember { derivedStateOf { chatModel.users.filter { u -> u.user.activeUser || !u.user.hidden } } }
        val allRead = users
          .filter { u -> !u.user.activeUser && !u.user.hidden }
          .all { u -> u.unreadCount == 0 }
        UserProfileButton(chatModel.currentUser.value?.profile?.image, allRead) {
            userPickerState.value = AnimatedViewState.VISIBLE
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
            val summary = serversSummary.value
            ModalManager.start.showModalCloseable(
              endButtons = {
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
    onTitleClick = if (canScrollToZero.value) { { scrollToBottom(scope, listState) } } else null,
    onTop = !oneHandUI.value,
    onSearchValueChanged = {},
    buttons = { barButtons.forEach { it() } }
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

fun connectIfOpenedViaUri(rhId: Long?, uri: String, chatModel: ChatModel) {
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
  Box {
    Row(verticalAlignment = Alignment.CenterVertically, modifier = Modifier.fillMaxWidth()) {
      val focusRequester = remember { FocusRequester() }
      var focused by remember { mutableStateOf(false) }
      Icon(
        painterResource(MR.images.ic_search),
        contentDescription = null,
        Modifier.padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING_HALF).size(22.dp * fontSizeSqrtMultiplier),
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
    val oneHandUI = remember { appPrefs.oneHandUI.state }
    Divider(Modifier.align(if (oneHandUI.value) Alignment.TopStart else Alignment.BottomStart))
  }
}

private fun connect(link: String, searchChatFilteredBySimplexLink: MutableState<String?>, cleanup: (() -> Unit)?) {
  withBGApi {
    planAndConnect(
      chatModel.remoteHostId(),
      link,
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
fun BoxScope.StatusBarBackground() {
  if (appPlatform.isAndroid) {
    val finalColor = MaterialTheme.colors.background.copy(0.88f)
    Box(Modifier.fillMaxWidth().windowInsetsTopHeight(WindowInsets.statusBars).background(finalColor))
  }
}

@Composable
fun BoxScope.NavigationBarBackground(appBarOnBottom: Boolean = false, mixedColor: Boolean, noAlpha: Boolean = false) {
  if (appPlatform.isAndroid) {
    val barPadding = WindowInsets.navigationBars.asPaddingValues()
    val paddingBottom = barPadding.calculateBottomPadding()
    val color = if (mixedColor) MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.97f) else MaterialTheme.colors.background
    val finalColor = color.copy(if (noAlpha) 1f else if (appBarOnBottom) remember { appPrefs.inAppBarsAlpha.state }.value else 0.6f)
    Box(Modifier.align(Alignment.BottomStart).height(paddingBottom).fillMaxWidth().background(finalColor))
  }
}

@Composable
fun BoxScope.NavigationBarBackground(modifier: Modifier, color: Color = MaterialTheme.colors.background) {
  val keyboardState = getKeyboardState()
  if (appPlatform.isAndroid && keyboardState.value == KeyboardState.Closed) {
    val barPadding = WindowInsets.navigationBars.asPaddingValues()
    val paddingBottom = barPadding.calculateBottomPadding()
    val finalColor = color.copy(0.6f)
    Box(modifier.align(Alignment.BottomStart).height(paddingBottom).fillMaxWidth().background(finalColor))
  }
}

@Composable
private fun BoxScope.ChatList(searchText: MutableState<TextFieldValue>, listState: LazyListState) {
  var scrollDirection by remember { mutableStateOf(ScrollDirection.Idle) }
  var previousIndex by remember { mutableStateOf(0) }
  var previousScrollOffset by remember { mutableStateOf(0) }
  val keyboardState by getKeyboardState()
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  val oneHandUICardShown = remember { appPrefs.oneHandUICardShown.state }
  val addressCreationCardShown = remember { appPrefs.addressCreationCardShown.state }

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
  val topPaddingToContent = topPaddingToContent(false)
  val blankSpaceSize = if (oneHandUI.value) WindowInsets.navigationBars.asPaddingValues().calculateBottomPadding() + AppBarHeight * fontSizeSqrtMultiplier else topPaddingToContent
  LazyColumnWithScrollBar(
    if (!oneHandUI.value) Modifier.imePadding() else Modifier,
    listState,
    reverseLayout = oneHandUI.value
  ) {
    item { Spacer(Modifier.height(blankSpaceSize)) }
    stickyHeader {
      Column(
        Modifier
          .zIndex(1f)
          .offset {
            val offsetMultiplier = if (oneHandUI.value) 1 else -1
            val y = if (searchText.value.text.isNotEmpty() || (appPlatform.isAndroid && keyboardState == KeyboardState.Opened) || scrollDirection == ScrollDirection.Up) {
              if (listState.firstVisibleItemIndex == 0) -offsetMultiplier * listState.firstVisibleItemScrollOffset
              else -offsetMultiplier * blankSpaceSize.roundToPx()
            } else {
              when (listState.firstVisibleItemIndex) {
                0 -> 0
                1 -> offsetMultiplier * listState.firstVisibleItemScrollOffset
                else -> offsetMultiplier * 1000
              }
            }
            IntOffset(0, y)
          }
          .background(MaterialTheme.colors.background)
        ) {
        if (oneHandUI.value) {
          Column(Modifier.consumeWindowInsets(WindowInsets.navigationBars).consumeWindowInsets(PaddingValues(bottom = AppBarHeight))) {
            ChatListSearchBar(listState, searchText, searchShowingSimplexLink, searchChatFilteredBySimplexLink)
            Spacer(Modifier.windowInsetsBottomHeight(WindowInsets.ime))
          }
        } else {
          ChatListSearchBar(listState, searchText, searchShowingSimplexLink, searchChatFilteredBySimplexLink)
        }
      }
    }
    itemsIndexed(chats, key = { _, chat -> chat.remoteHostId to chat.id }) { index, chat ->
      val nextChatSelected = remember(chat.id, chats) { derivedStateOf {
        chatModel.chatId.value != null && chats.getOrNull(index + 1)?.id == chatModel.chatId.value
      } }
      ChatListNavLinkView(chat, nextChatSelected)
    }
    if (!oneHandUICardShown.value || !addressCreationCardShown.value) {
      item {
        ChatListFeatureCards()
      }
    }
    if (appPlatform.isAndroid) {
      item { Spacer(if (oneHandUI.value) Modifier.windowInsetsTopHeight(WindowInsets.statusBars) else Modifier.windowInsetsBottomHeight(WindowInsets.navigationBars)) }
    }
  }
  if (chats.isEmpty() && chatModel.chats.value.isNotEmpty()) {
    Box(Modifier.fillMaxSize().imePadding(), contentAlignment = Alignment.Center) {
      Text(generalGetString(MR.strings.no_filtered_chats), color = MaterialTheme.colors.secondary)
    }
  }
  if (oneHandUI.value) {
    StatusBarBackground()
  } else {
    NavigationBarBackground(oneHandUI.value, true)
  }
  if (!oneHandUICardShown.value) {
    LaunchedEffect(chats.size) {
      if (chats.size >= 3) {
        appPrefs.oneHandUICardShown.set(true)
      }
    }
  }

  if (!addressCreationCardShown.value) {
    LaunchedEffect(chatModel.userAddress.value) {
      if (chatModel.userAddress.value != null) {
        appPrefs.addressCreationCardShown.set(true)
      }
    }
  }
}

@Composable
private fun ChatListFeatureCards() {
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  val oneHandUICardShown = remember { appPrefs.oneHandUICardShown.state }
  val addressCreationCardShown = remember { appPrefs.addressCreationCardShown.state }

  Column(modifier = Modifier.padding(16.dp), verticalArrangement = Arrangement.spacedBy(16.dp)) {
    if (!oneHandUICardShown.value && !oneHandUI.value) {
      ToggleChatListCard()
    }
    if (!addressCreationCardShown.value) {
      AddressCreationCard()
    }
    if (!oneHandUICardShown.value && oneHandUI.value) {
      ToggleChatListCard()
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

fun scrollToBottom(scope: CoroutineScope, listState: LazyListState) {
  scope.launch { try { listState.animateScrollToItem(0) } catch (e: Exception) { Log.e(TAG, e.stackTraceToString()) } }
}
