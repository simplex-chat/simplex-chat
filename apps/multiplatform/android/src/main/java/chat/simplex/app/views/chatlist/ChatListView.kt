package chat.simplex.app.views.chatlist

import androidx.activity.compose.BackHandler
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.*
import androidx.compose.ui.platform.LocalUriHandler
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.capitalize
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.intl.Locale
import androidx.compose.ui.unit.*
import chat.simplex.app.*
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.newchat.NewChatSheet
import chat.simplex.app.views.onboarding.WhatsNewView
import chat.simplex.app.views.onboarding.shouldShowWhatsNew
import chat.simplex.app.views.usersettings.SettingsView
import chat.simplex.app.views.usersettings.simplexTeamUri
import chat.simplex.res.MR
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.launch

@Composable
fun ChatListView(chatModel: ChatModel, setPerformLA: (Boolean) -> Unit, stopped: Boolean) {
  val newChatSheetState by rememberSaveable(stateSaver = AnimatedViewState.saver()) { mutableStateOf(MutableStateFlow(AnimatedViewState.GONE)) }
  val userPickerState by rememberSaveable(stateSaver = AnimatedViewState.saver()) { mutableStateOf(MutableStateFlow(AnimatedViewState.GONE)) }
  val showNewChatSheet = {
    newChatSheetState.value = AnimatedViewState.VISIBLE
  }
  val hideNewChatSheet: (animated: Boolean) -> Unit = { animated ->
    if (animated) newChatSheetState.value = AnimatedViewState.HIDING
    else newChatSheetState.value = AnimatedViewState.GONE
  }
  LaunchedEffect(Unit) {
    if (shouldShowWhatsNew(chatModel)) {
      delay(1000L)
      ModalManager.shared.showCustomModal { close -> WhatsNewView(close = close) }
    }
  }
  LaunchedEffect(chatModel.clearOverlays.value) {
    if (chatModel.clearOverlays.value && newChatSheetState.value.isVisible()) hideNewChatSheet(false)
  }
  LaunchedEffect(chatModel.appOpenUrl.value) {
    val url = chatModel.appOpenUrl.value
    if (url != null) {
      chatModel.appOpenUrl.value = null
      connectIfOpenedViaUri(url, chatModel)
    }
  }
  var searchInList by rememberSaveable { mutableStateOf("") }
  val scaffoldState = rememberScaffoldState()
  val scope = rememberCoroutineScope()
  val switchingUsers = rememberSaveable { mutableStateOf(false) }
  Scaffold(topBar = { ChatListToolbar(chatModel, scaffoldState.drawerState, userPickerState, stopped) { searchInList = it.trim() } },
    scaffoldState = scaffoldState,
    drawerContent = { SettingsView(chatModel, setPerformLA) },
    drawerScrimColor = MaterialTheme.colors.onSurface.copy(alpha = if (isInDarkTheme()) 0.16f else 0.32f),
    floatingActionButton = {
      if (searchInList.isEmpty()) {
        FloatingActionButton(
          onClick = {
            if (!stopped) {
              if (newChatSheetState.value.isVisible()) hideNewChatSheet(true) else showNewChatSheet()
            }
          },
          Modifier.padding(end = DEFAULT_PADDING - 16.dp, bottom = DEFAULT_PADDING - 16.dp),
          elevation = FloatingActionButtonDefaults.elevation(
            defaultElevation = 0.dp,
            pressedElevation = 0.dp,
            hoveredElevation = 0.dp,
            focusedElevation = 0.dp,
          ),
          backgroundColor = if (!stopped) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
          contentColor = Color.White
        ) {
          Icon(if (!newChatSheetState.collectAsState().value.isVisible()) painterResource(MR.images.ic_edit_filled) else painterResource(MR.images.ic_close), stringResource(MR.strings.add_contact_or_create_group))
        }
      }
    }
  ) {
    Box(Modifier.padding(it)) {
      Column(
        modifier = Modifier
          .fillMaxSize()
      ) {
        if (chatModel.chats.isNotEmpty()) {
          ChatList(chatModel, search = searchInList)
        } else if (!switchingUsers.value) {
          Box(Modifier.fillMaxSize()) {
            if (!stopped && !newChatSheetState.collectAsState().value.isVisible()) {
              OnboardingButtons(showNewChatSheet)
            }
            Text(stringResource(MR.strings.you_have_no_chats), Modifier.align(Alignment.Center), color = MaterialTheme.colors.secondary)
          }
        }
      }
    }
  }
  if (searchInList.isEmpty()) {
    NewChatSheet(chatModel, newChatSheetState, stopped, hideNewChatSheet)
  }
  UserPicker(chatModel, userPickerState, switchingUsers) {
    scope.launch { if (scaffoldState.drawerState.isOpen) scaffoldState.drawerState.close() else scaffoldState.drawerState.open() }
  }
  if (switchingUsers.value) {
    Box(
      Modifier.fillMaxSize().clickable(enabled = false, onClick = {}),
      contentAlignment = Alignment.Center
    ) {
      ProgressIndicator()
    }
  }
}

@Composable
private fun OnboardingButtons(openNewChatSheet: () -> Unit) {
  Column(Modifier.fillMaxSize().padding(DEFAULT_PADDING), horizontalAlignment = Alignment.End, verticalArrangement = Arrangement.Bottom) {
    val uriHandler = LocalUriHandler.current
    ConnectButton(generalGetString(MR.strings.chat_with_developers)) {
      uriHandler.openUriCatching(simplexTeamUri)
    }
    Spacer(Modifier.height(DEFAULT_PADDING))
    ConnectButton(generalGetString(MR.strings.tap_to_start_new_chat), openNewChatSheet)
    val color = MaterialTheme.colors.primaryVariant
    Canvas(modifier = Modifier.width(40.dp).height(10.dp), onDraw = {
      val trianglePath = Path().apply {
        moveTo(0.dp.toPx(), 0f)
        lineTo(16.dp.toPx(), 0.dp.toPx())
        lineTo(8.dp.toPx(), 10.dp.toPx())
        lineTo(0.dp.toPx(), 0.dp.toPx())
      }
      drawPath(
        color = color,
        path = trianglePath
      )
    })
    Spacer(Modifier.height(62.dp))
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
private fun ChatListToolbar(chatModel: ChatModel, drawerState: DrawerState, userPickerState: MutableStateFlow<AnimatedViewState>, stopped: Boolean, onSearchValueChanged: (String) -> Unit) {
  var showSearch by rememberSaveable { mutableStateOf(false) }
  val hideSearchOnBack = { onSearchValueChanged(""); showSearch = false }
  if (showSearch) {
    BackHandler(onBack = hideSearchOnBack)
  }
  val barButtons = arrayListOf<@Composable RowScope.() -> Unit>()
  if (chatModel.chats.size > 0) {
    barButtons.add {
      IconButton({ showSearch = true }) {
        Icon(painterResource(MR.images.ic_search_500), stringResource(MR.strings.search_verb).capitalize(Locale.current), tint = MaterialTheme.colors.primary)
      }
    }
  }
  if (stopped) {
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
  DefaultTopAppBar(
    navigationButton = {
      if (showSearch) {
        NavigationButtonBack(hideSearchOnBack)
      } else if (chatModel.users.isEmpty()) {
        NavigationButtonMenu { scope.launch { if (drawerState.isOpen) drawerState.close() else drawerState.open() } }
      } else {
        val users by remember { derivedStateOf { chatModel.users.filter { u -> u.user.activeUser || !u.user.hidden } } }
        val allRead = users
          .filter { u -> !u.user.activeUser && !u.user.hidden }
          .all { u -> u.unreadCount == 0 }
        UserProfileButton(chatModel.currentUser.value?.profile?.image, allRead) {
          if (users.size == 1) {
            scope.launch { drawerState.open() }
          } else {
            userPickerState.value = AnimatedViewState.VISIBLE
          }
        }
      }
    },
    title = {
      Row(verticalAlignment = Alignment.CenterVertically) {
        if (chatModel.incognito.value) {
          Icon(
            painterResource(MR.images.ic_theater_comedy_filled),
            stringResource(MR.strings.incognito),
            tint = Indigo,
            modifier = Modifier.padding(10.dp).size(26.dp)
          )
        }
        Text(
          stringResource(MR.strings.your_chats),
          color = MaterialTheme.colors.onBackground,
          fontWeight = FontWeight.SemiBold,
        )
        if (chatModel.chats.size > 0) {
          ToggleFilterButton()
        }
      }
    },
    onTitleClick = null,
    showSearch = showSearch,
    onSearchValueChanged = onSearchValueChanged,
    buttons = barButtons
  )
  Divider(Modifier.padding(top = AppBarHeight))
}

@Composable
fun UserProfileButton(image: String?, allRead: Boolean, onButtonClicked: () -> Unit) {
  IconButton(onClick = onButtonClicked) {
    Box {
      ProfileImage(
        image = image,
        size = 37.dp
      )
      if (!allRead) {
        unreadBadge()
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
private fun ToggleFilterButton() {
  val pref = remember { SimplexApp.context.chatModel.controller.appPrefs.showUnreadAndFavorites }
  IconButton(onClick = { pref.set(!pref.get()) }) {
    Icon(
      painterResource(MR.images.ic_filter_list),
      null,
      tint = if (pref.state.value) MaterialTheme.colors.background else MaterialTheme.colors.primary,
      modifier = Modifier
        .padding(3.dp)
        .background(color = if (pref.state.value) MaterialTheme.colors.primary else MaterialTheme.colors.background, shape = RoundedCornerShape(50))
        .border(width = 1.dp, color = MaterialTheme.colors.primary, shape = RoundedCornerShape(50))
        .padding(3.dp)
        .size(16.dp)
    )
  }
}

@Composable
private fun ProgressIndicator() {
  CircularProgressIndicator(
    Modifier
      .padding(horizontal = 2.dp)
      .size(30.dp),
    color = MaterialTheme.colors.secondary,
    strokeWidth = 2.5.dp
  )
}

private var lazyListState = 0 to 0

@Composable
private fun ChatList(chatModel: ChatModel, search: String) {
  val listState = rememberLazyListState(lazyListState.first, lazyListState.second)
  DisposableEffect(Unit) {
    onDispose { lazyListState = listState.firstVisibleItemIndex to listState.firstVisibleItemScrollOffset }
  }
  val showUnreadAndFavorites = remember { chatModel.controller.appPrefs.showUnreadAndFavorites.state }.value
  val chats by remember(search, showUnreadAndFavorites) { derivedStateOf { filteredChats(showUnreadAndFavorites, search) } }
  LazyColumn(
    modifier = Modifier.fillMaxWidth(),
    listState
  ) {
    items(chats) { chat ->
      ChatListNavLinkView(chat, chatModel)
    }
  }
  if (chats.isEmpty() && !chatModel.chats.isEmpty()) {
    Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
      Text(generalGetString(MR.strings.no_filtered_chats), color = MaterialTheme.colors.secondary)
    }
  }
}

private fun filteredChats(showUnreadAndFavorites: Boolean, searchText: String): List<Chat> {
  val chatModel = SimplexApp.context.chatModel
  val s = searchText.trim().lowercase()
  return if (s.isEmpty() && !showUnreadAndFavorites)
    chatModel.chats
  else {
    chatModel.chats.filter { chat ->
      when (val cInfo = chat.chatInfo) {
        is ChatInfo.Direct -> if (s.isEmpty()) {
          filtered(chat)
        } else {
          (viewNameContains(cInfo, s) ||
              cInfo.contact.profile.displayName.lowercase().contains(s) ||
              cInfo.contact.fullName.lowercase().contains(s))
        }
        is ChatInfo.Group -> if (s.isEmpty()) {
          (filtered(chat) || cInfo.groupInfo.membership.memberStatus == GroupMemberStatus.MemInvited)
        } else {
          viewNameContains(cInfo, s)
        }
        is ChatInfo.ContactRequest -> s.isEmpty() || viewNameContains(cInfo, s)
        is ChatInfo.ContactConnection -> s.isNotEmpty() && cInfo.contactConnection.localAlias.lowercase().contains(s)
        is ChatInfo.InvalidJSON -> false
      }
    }
  }
}

private fun filtered(chat: Chat): Boolean =
  (chat.chatInfo.chatSettings?.favorite ?: false) || chat.chatStats.unreadCount > 0 || chat.chatStats.unreadChat

private fun viewNameContains(cInfo: ChatInfo, s: String): Boolean =
  cInfo.chatViewName.lowercase().contains(s.lowercase())

