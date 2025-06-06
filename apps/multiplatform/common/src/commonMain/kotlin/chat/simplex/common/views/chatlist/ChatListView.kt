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
import androidx.compose.ui.text.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import chat.simplex.common.AppLock
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatController.stopRemoteHostAndReloadHosts
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.call.Call
import chat.simplex.common.views.chat.item.*
import chat.simplex.common.views.chat.topPaddingToContent
import chat.simplex.common.views.newchat.*
import chat.simplex.common.views.onboarding.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.StringResource
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.serialization.json.Json
import kotlin.time.Duration.Companion.seconds

enum class PresetTagKind { GROUP_REPORTS, FAVORITES, CONTACTS, GROUPS, BUSINESS, NOTES }

sealed class ActiveFilter {
  data class PresetTag(val tag: PresetTagKind) : ActiveFilter()
  data class UserTag(val tag: ChatTag) : ActiveFilter()
  data object Unread: ActiveFilter()
}

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

  LaunchedEffect(Unit) {
    val showWhatsNew = shouldShowWhatsNew(chatModel)
    val showUpdatedConditions = chatModel.conditions.value.conditionsAction?.shouldShowNotice ?: false
    if (showWhatsNew || showUpdatedConditions) {
      delay(1000L)
      ModalManager.center.showCustomModal { close -> WhatsNewView(close = close, updatedConditions = showUpdatedConditions) }
    }
  }

  if (appPlatform.isDesktop) {
    KeyChangeEffect(chatModel.chatId.value) {
      if (chatModel.chatId.value != null && !ModalManager.end.isLastModalOpen(ModalViewId.SECONDARY_CHAT)) {
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
  val showUnread = remember { chatModel.activeChatTagFilter }.value == ActiveFilter.Unread

  IconButton(onClick = {
    if (showUnread) {
      chatModel.activeChatTagFilter.value = null
    } else {
      chatModel.activeChatTagFilter.value = ActiveFilter.Unread
    }
  }) {
    val sp16 = with(LocalDensity.current) { 16.sp.toDp() }
    Icon(
      painterResource(MR.images.ic_filter_list),
      null,
      tint = if (showUnread) MaterialTheme.colors.background else MaterialTheme.colors.secondary,
      modifier = Modifier
        .padding(3.dp)
        .background(color = if (showUnread) MaterialTheme.colors.primary else Color.Unspecified, shape = RoundedCornerShape(50))
        .border(width = 1.dp, color = if (showUnread) MaterialTheme.colors.primary else Color.Unspecified, shape = RoundedCornerShape(50))
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
  val activeFilter = remember { chatModel.activeChatTagFilter }

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
  val allChats = remember { chatModel.chats }
  // In some not always reproducible situations this code produce IndexOutOfBoundsException on Compose's side
  // which is related to [derivedStateOf]. Using safe alternative instead
  // val chats by remember(search, showUnreadAndFavorites) { derivedStateOf { filteredChats(showUnreadAndFavorites, search, allChats.toList()) } }
  val searchShowingSimplexLink = remember { mutableStateOf(false) }
  val searchChatFilteredBySimplexLink = remember { mutableStateOf<String?>(null) }
  val chats = filteredChats(searchShowingSimplexLink, searchChatFilteredBySimplexLink, searchText.value.text, allChats.value.toList(), activeFilter.value)
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
            Divider()
            TagsView(searchText)
            ChatListSearchBar(listState, searchText, searchShowingSimplexLink, searchChatFilteredBySimplexLink)
            Spacer(Modifier.windowInsetsBottomHeight(WindowInsets.ime))
          }
        } else {
          ChatListSearchBar(listState, searchText, searchShowingSimplexLink, searchChatFilteredBySimplexLink)
          TagsView(searchText)
          Divider()
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
    Box(Modifier.fillMaxSize().imePadding().padding(horizontal = DEFAULT_PADDING), contentAlignment = Alignment.Center) {
      NoChatsView(searchText = searchText)
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

  LaunchedEffect(activeFilter.value) {
    searchText.value = TextFieldValue("")
  }
}

@Composable
private fun NoChatsView(searchText: MutableState<TextFieldValue>) {
  val activeFilter = remember { chatModel.activeChatTagFilter }.value

  if (searchText.value.text.isBlank()) {
    when (activeFilter) {
      is ActiveFilter.PresetTag -> Text(generalGetString(MR.strings.no_filtered_chats), color = MaterialTheme.colors.secondary, textAlign = TextAlign.Center) // this should not happen
      is ActiveFilter.UserTag -> Text(String.format(generalGetString(MR.strings.no_chats_in_list), activeFilter.tag.chatTagText), color = MaterialTheme.colors.secondary, textAlign = TextAlign.Center)
      is ActiveFilter.Unread -> {
          Row(
            Modifier.clip(shape = CircleShape).clickable { chatModel.activeChatTagFilter.value = null }.padding(DEFAULT_PADDING_HALF),
            horizontalArrangement = Arrangement.spacedBy(4.dp),
            verticalAlignment = Alignment.CenterVertically
          ) {
            Icon(
              painterResource(MR.images.ic_filter_list),
              null,
              tint = MaterialTheme.colors.secondary
            )
            Text(generalGetString(MR.strings.no_unread_chats), color = MaterialTheme.colors.secondary, textAlign = TextAlign.Center)
          }
      }
      null -> {
        Text(generalGetString(MR.strings.no_chats), color = MaterialTheme.colors.secondary, textAlign = TextAlign.Center)
      }
    }
  } else {
    Text(generalGetString(MR.strings.no_chats_found), color = MaterialTheme.colors.secondary, textAlign = TextAlign.Center)
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

private val TAG_MIN_HEIGHT = 35.dp

@Composable
private fun TagsView(searchText: MutableState<TextFieldValue>) {
  val userTags = remember { chatModel.userTags }
  val presetTags = remember { chatModel.presetTags }
  val collapsiblePresetTags = presetTags.filter { presetCanBeCollapsed(it.key) && it.value > 0 }
  val alwaysShownPresetTags = presetTags.filter { !presetCanBeCollapsed(it.key) && it.value > 0 }
  val activeFilter = remember { chatModel.activeChatTagFilter }
  val unreadTags = remember { chatModel.unreadTags }
  val rhId = chatModel.remoteHostId()

  val rowSizeModifier = Modifier.sizeIn(minHeight = TAG_MIN_HEIGHT * fontSizeSqrtMultiplier)

  TagsRow {
    if (collapsiblePresetTags.size > 1) {
      if (collapsiblePresetTags.size + alwaysShownPresetTags.size + userTags.value.size <= 3) {
        PresetTagKind.entries.filter { t -> (presetTags[t] ?: 0) > 0 }.forEach { tag ->
          ExpandedTagFilterView(tag)
        }
      } else {
        CollapsedTagsFilterView(searchText)
        alwaysShownPresetTags.forEach { tag ->
          ExpandedTagFilterView(tag.key)
        }
      }
    }

    userTags.value.forEach { tag ->
      val current = when (val af = activeFilter.value) {
        is ActiveFilter.UserTag -> af.tag == tag
        else -> false
      }
      val interactionSource = remember { MutableInteractionSource() }
      val showMenu = rememberSaveable { mutableStateOf(false) }
      val saving = remember { mutableStateOf(false) }
      Box {
        Row(
          rowSizeModifier
            .clip(shape = CircleShape)
            .combinedClickable(
              onClick = {
                if (chatModel.activeChatTagFilter.value == ActiveFilter.UserTag(tag)) {
                  chatModel.activeChatTagFilter.value = null
                } else {
                  chatModel.activeChatTagFilter.value = ActiveFilter.UserTag(tag)
                }
              },
              onLongClick = { showMenu.value = true },
              interactionSource = interactionSource,
              indication = LocalIndication.current,
              enabled = !saving.value
            )
            .onRightClick { showMenu.value = true }
            .padding(4.dp),
          horizontalArrangement = Arrangement.Center,
          verticalAlignment = Alignment.CenterVertically
        ) {
          if (tag.chatTagEmoji != null) {
            ReactionIcon(tag.chatTagEmoji, fontSize = 14.sp)
          } else {
            Icon(
              painterResource(if (current) MR.images.ic_label_filled else MR.images.ic_label),
              null,
              Modifier.size(18.sp.toDp()),
              tint = if (current) MaterialTheme.colors.primary else MaterialTheme.colors.onBackground
            )
          }
          Spacer(Modifier.width(4.dp))
          Box {
            val badgeText = if ((unreadTags[tag.chatTagId] ?: 0) > 0) " ●" else ""
            val invisibleText = buildAnnotatedString {
              append(tag.chatTagText)
              withStyle(SpanStyle(fontSize = 12.sp, fontWeight = FontWeight.SemiBold)) {
                append(badgeText)
              }
            }
            Text(
              text = invisibleText,
              fontWeight = FontWeight.Medium,
              fontSize = 15.sp,
              color = Color.Transparent,
              maxLines = 1,
              overflow = TextOverflow.Ellipsis
            )
            // Visible text with styles
            val visibleText = buildAnnotatedString {
              append(tag.chatTagText)
              withStyle(SpanStyle(fontSize = 12.5.sp, color = MaterialTheme.colors.primary)) {
                append(badgeText)
              }
            }
            Text(
              text = visibleText,
              fontWeight = if (current) FontWeight.Medium else FontWeight.Normal,
              fontSize = 15.sp,
              color = if (current) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
              maxLines = 1,
              overflow = TextOverflow.Ellipsis
            )
          }
        }
        TagsDropdownMenu(rhId, tag, showMenu, saving)
      }
    }
    val plusClickModifier = Modifier
      .clickable {
        ModalManager.start.showModalCloseable { close ->
          TagListEditor(rhId = rhId, close = close)
        }
      }

    if (userTags.value.isEmpty()) {
      Row(rowSizeModifier.clip(shape = CircleShape).then(plusClickModifier).padding(start = 2.dp, top = 4.dp, end = 6.dp, bottom = 4.dp), verticalAlignment = Alignment.CenterVertically) {
        Icon(painterResource(MR.images.ic_add), stringResource(MR.strings.chat_list_add_list), Modifier.size(18.sp.toDp()), tint = MaterialTheme.colors.secondary)
        Spacer(Modifier.width(2.dp))
        Text(stringResource(MR.strings.chat_list_add_list), color = MaterialTheme.colors.secondary, fontSize = 15.sp)
      }
    } else {
      Box(rowSizeModifier, contentAlignment = Alignment.Center) {
        Icon(
          painterResource(MR.images.ic_add), stringResource(MR.strings.chat_list_add_list), Modifier.clip(shape = CircleShape).then(plusClickModifier).padding(2.dp), tint = MaterialTheme.colors.secondary
        )
      }
    }
  }
}

@Composable
expect fun TagsRow(content: @Composable() (() -> Unit))

@Composable
private fun ExpandedTagFilterView(tag: PresetTagKind) {
  val activeFilter = remember { chatModel.activeChatTagFilter }
  val active = when (val af = activeFilter.value) {
    is ActiveFilter.PresetTag -> af.tag == tag
    else -> false
  }
  val (icon, text) = presetTagLabel(tag, active)
  val color = if (active) MaterialTheme.colors.primary else MaterialTheme.colors.secondary

  Row(
    modifier = Modifier
      .sizeIn(minHeight = TAG_MIN_HEIGHT * fontSizeSqrtMultiplier)
      .clip(shape = CircleShape)
      .clickable {
        if (activeFilter.value == ActiveFilter.PresetTag(tag)) {
          chatModel.activeChatTagFilter.value = null
        } else {
          chatModel.activeChatTagFilter.value = ActiveFilter.PresetTag(tag)
        }
      }
      .padding(horizontal = 5.dp, vertical = 4.dp)
    ,
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.Center
  ) {
    Icon(
      painterResource(icon),
      stringResource(text),
      Modifier.size(18.sp.toDp()),
      tint = color
    )
    Spacer(Modifier.width(4.dp))
    Box {
      Text(
        stringResource(text),
        color = if (active) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
        fontWeight = if (active) FontWeight.Medium else FontWeight.Normal,
        fontSize = 15.sp
      )
      Text(
        stringResource(text),
        color = Color.Transparent,
        fontWeight = FontWeight.Medium,
        fontSize = 15.sp
      )
    }
  }
}


@Composable
private fun CollapsedTagsFilterView(searchText: MutableState<TextFieldValue>) {
  val activeFilter = remember { chatModel.activeChatTagFilter }
  val presetTags = remember { chatModel.presetTags }
  val showMenu = remember { mutableStateOf(false) }

  val selectedPresetTag = when (val af = activeFilter.value) {
    is ActiveFilter.PresetTag -> if (presetCanBeCollapsed(af.tag)) af.tag else null
    else -> null
  }

  Box(Modifier
    .clip(shape = CircleShape)
    .size(TAG_MIN_HEIGHT * fontSizeSqrtMultiplier)
    .clickable { showMenu.value = true },
    contentAlignment = Alignment.Center
  ) {
    if (selectedPresetTag != null) {
      val (icon, text) = presetTagLabel(selectedPresetTag, true)
      Icon(
        painterResource(icon),
        stringResource(text),
        Modifier.size(18.sp.toDp()),
        tint = MaterialTheme.colors.primary
      )
    } else {
      Icon(
        painterResource(MR.images.ic_menu),
        stringResource(MR.strings.chat_list_all),
        tint = MaterialTheme.colors.secondary
      )
    }

    val onCloseMenuAction = remember { mutableStateOf<(() -> Unit)>({}) }

    DefaultDropdownMenu(showMenu = showMenu, onClosed = onCloseMenuAction) {
      if (activeFilter.value != null || searchText.value.text.isNotBlank()) {
        ItemAction(
          stringResource(MR.strings.chat_list_all),
          painterResource(MR.images.ic_menu),
          onClick = {
            onCloseMenuAction.value = {
              searchText.value = TextFieldValue()
              chatModel.activeChatTagFilter.value = null
              onCloseMenuAction.value = {}
            }
            showMenu.value = false
          }
        )
      }
      PresetTagKind.entries.forEach { tag ->
        if ((presetTags[tag] ?: 0) > 0 && presetCanBeCollapsed(tag)) {
          ItemPresetFilterAction(tag, tag == selectedPresetTag, showMenu, onCloseMenuAction)
        }
      }
    }
  }
}

@Composable
fun ItemPresetFilterAction(
  presetTag: PresetTagKind,
  active: Boolean,
  showMenu: MutableState<Boolean>,
  onCloseMenuAction: MutableState<(() -> Unit)>
) {
  val (icon, text) = presetTagLabel(presetTag, active)
  ItemAction(
    stringResource(text),
    painterResource(icon),
    color = if (active) MaterialTheme.colors.primary else Color.Unspecified,
    onClick = {
      onCloseMenuAction.value = {
        chatModel.activeChatTagFilter.value = ActiveFilter.PresetTag(presetTag)
        onCloseMenuAction.value = {}
      }
      showMenu.value = false
    }
  )
}

fun filteredChats(
  searchShowingSimplexLink: State<Boolean>,
  searchChatFilteredBySimplexLink: State<String?>,
  searchText: String,
  chats: List<Chat>,
  activeFilter: ActiveFilter? = null,
): List<Chat> {
  val linkChatId = searchChatFilteredBySimplexLink.value
  return if (linkChatId != null) {
    chats.filter { it.id == linkChatId }
  } else {
    val s = if (searchShowingSimplexLink.value) "" else searchText.trim().lowercase()
    if (s.isEmpty())
      chats.filter { chat -> chat.id == chatModel.chatId.value || (!chat.chatInfo.chatDeleted && !chat.chatInfo.contactCard && filtered(chat, activeFilter)) }
    else {
      chats.filter { chat ->
        chat.id == chatModel.chatId.value ||
          when (val cInfo = chat.chatInfo) {
            is ChatInfo.Direct -> !cInfo.contact.chatDeleted && !chat.chatInfo.contactCard && cInfo.anyNameContains(s)
            is ChatInfo.Group -> cInfo.anyNameContains(s)
            is ChatInfo.Local -> cInfo.anyNameContains(s)
            is ChatInfo.ContactRequest -> cInfo.anyNameContains(s)
            is ChatInfo.ContactConnection -> cInfo.contactConnection.localAlias.lowercase().contains(s)
            is ChatInfo.InvalidJSON -> false
          }
      }
    }
  }
}

private fun filtered(chat: Chat, activeFilter: ActiveFilter?): Boolean =
  when (activeFilter) {
    is ActiveFilter.PresetTag -> presetTagMatchesChat(activeFilter.tag, chat.chatInfo, chat.chatStats)
    is ActiveFilter.UserTag -> chat.chatInfo.chatTags?.contains(activeFilter.tag.chatTagId) ?: false
    is ActiveFilter.Unread -> chat.unreadTag
    else -> true
  }

fun presetTagMatchesChat(tag: PresetTagKind, chatInfo: ChatInfo, chatStats: Chat.ChatStats): Boolean =
  when (tag) {
    PresetTagKind.GROUP_REPORTS -> chatStats.reportsCount > 0
    PresetTagKind.FAVORITES -> chatInfo.chatSettings?.favorite == true
    PresetTagKind.CONTACTS -> when (chatInfo) {
      is ChatInfo.Direct -> !(chatInfo.contact.activeConn == null && chatInfo.contact.profile.contactLink != null && chatInfo.contact.active) && !chatInfo.contact.chatDeleted
      is ChatInfo.ContactRequest -> true
      is ChatInfo.ContactConnection -> true
      is ChatInfo.Group -> chatInfo.groupInfo.businessChat?.chatType == BusinessChatType.Customer
      else -> false
    }
    PresetTagKind.GROUPS -> when (chatInfo) {
      is ChatInfo.Group -> chatInfo.groupInfo.businessChat == null
      else -> false
    }
    PresetTagKind.BUSINESS -> when (chatInfo) {
      is ChatInfo.Group -> chatInfo.groupInfo.businessChat?.chatType == BusinessChatType.Business
      else -> false
    }
    PresetTagKind.NOTES -> when (chatInfo) {
      is ChatInfo.Local -> !chatInfo.noteFolder.chatDeleted
      else -> false
    }
  }

private fun presetTagLabel(tag: PresetTagKind, active: Boolean): Pair<ImageResource, StringResource> =
  when (tag) {
    PresetTagKind.GROUP_REPORTS -> (if (active) MR.images.ic_flag_filled else MR.images.ic_flag) to MR.strings.chat_list_group_reports
    PresetTagKind.FAVORITES -> (if (active) MR.images.ic_star_filled else MR.images.ic_star) to MR.strings.chat_list_favorites
    PresetTagKind.CONTACTS -> (if (active) MR.images.ic_person_filled else MR.images.ic_person) to MR.strings.chat_list_contacts
    PresetTagKind.GROUPS -> (if (active) MR.images.ic_group_filled else MR.images.ic_group) to MR.strings.chat_list_groups
    PresetTagKind.BUSINESS -> (if (active) MR.images.ic_work_filled else MR.images.ic_work) to MR.strings.chat_list_businesses
    PresetTagKind.NOTES -> (if (active) MR.images.ic_folder_closed_filled else MR.images.ic_folder_closed) to MR.strings.chat_list_notes
  }

private fun presetCanBeCollapsed(tag: PresetTagKind): Boolean = when (tag) {
  PresetTagKind.GROUP_REPORTS -> false
  else -> true
}

fun scrollToBottom(scope: CoroutineScope, listState: LazyListState) {
  scope.launch { try { listState.animateScrollToItem(0) } catch (e: Exception) { Log.e(TAG, e.stackTraceToString()) } }
}
