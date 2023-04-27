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
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.capitalize
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.intl.Locale
import androidx.compose.ui.unit.*
import androidx.fragment.app.FragmentActivity
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.newchat.NewChatSheet
import chat.simplex.app.views.onboarding.WhatsNewView
import chat.simplex.app.views.onboarding.shouldShowWhatsNew
import chat.simplex.app.views.usersettings.SettingsView
import chat.simplex.app.views.usersettings.simplexTeamUri
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.launch

@Composable
fun ChatListView(chatModel: ChatModel, setPerformLA: (Boolean, FragmentActivity) -> Unit, stopped: Boolean) {
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
          Icon(if (!newChatSheetState.collectAsState().value.isVisible()) painterResource(R.drawable.ic_edit_filled) else painterResource(R.drawable.ic_close), stringResource(R.string.add_contact_or_create_group))
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
            Text(stringResource(R.string.you_have_no_chats), Modifier.align(Alignment.Center), color = MaterialTheme.colors.secondary)
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
    ConnectButton(generalGetString(R.string.chat_with_developers)) {
      uriHandler.openUriCatching(simplexTeamUri)
    }
    Spacer(Modifier.height(DEFAULT_PADDING))
    ConnectButton(generalGetString(R.string.tap_to_start_new_chat), openNewChatSheet)
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
  if (chatModel.chats.size >= 8) {
    barButtons.add {
      IconButton({ showSearch = true }) {
        Icon(painterResource(R.drawable.ic_search_500), stringResource(android.R.string.search_go).capitalize(Locale.current), tint = MaterialTheme.colors.primary)
      }
    }
  }
  if (stopped) {
    barButtons.add {
      IconButton(onClick = {
        AlertManager.shared.showAlertMsg(
          generalGetString(R.string.chat_is_stopped_indication),
          generalGetString(R.string.you_can_start_chat_via_setting_or_by_restarting_the_app)
        )
      }) {
        Icon(
          painterResource(R.drawable.ic_report_filled),
          generalGetString(R.string.chat_is_stopped_indication),
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
        Text(
          stringResource(R.string.your_chats),
          color = MaterialTheme.colors.onBackground,
          fontWeight = FontWeight.SemiBold,
        )
        if (chatModel.incognito.value) {
          Icon(
            painterResource(R.drawable.ic_theater_comedy_filled),
            stringResource(R.string.incognito),
            tint = Indigo,
            modifier = Modifier.padding(10.dp).size(26.dp)
          )
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
  val filter: (Chat) -> Boolean = { chat: Chat ->
    chat.chatInfo.chatViewName.lowercase().contains(search.lowercase())
  }
  val listState = rememberLazyListState(lazyListState.first, lazyListState.second)
  DisposableEffect(Unit) {
    onDispose { lazyListState = listState.firstVisibleItemIndex to listState.firstVisibleItemScrollOffset }
  }
  val chats by remember(search) { derivedStateOf { if (search.isEmpty()) chatModel.chats else chatModel.chats.filter(filter) } }
  LazyColumn(
    modifier = Modifier.fillMaxWidth(),
    listState
  ) {
    items(chats) { chat ->
      ChatListNavLinkView(chat, chatModel)
    }
  }
}
