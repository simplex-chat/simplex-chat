package chat.simplex.app.views.chatlist

import androidx.activity.compose.BackHandler
import androidx.compose.animation.animateColorAsState
import androidx.compose.animation.core.*
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.*
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.Path
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.capitalize
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.intl.Locale
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.newchat.NewChatSheet
import chat.simplex.app.views.usersettings.SettingsView
import chat.simplex.app.views.usersettings.simplexTeamUri
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch

@Composable
fun ChatListView(chatModel: ChatModel, setPerformLA: (Boolean) -> Unit, stopped: Boolean) {
  var newChatSheetState by rememberSaveable { mutableStateOf(NewChatSheetState.GONE) }
  val scope = rememberCoroutineScope()
  val showNewChatSheet = {
    newChatSheetState = NewChatSheetState.VISIBLE
  }
  val hideNewChatSheet: () -> Unit = {
    scope.launch {
      // Start hiding with animation
      newChatSheetState = NewChatSheetState.HIDING
      delay(250)
      // Remove from the screen completely
      newChatSheetState = NewChatSheetState.GONE
    }
  }
  LaunchedEffect(chatModel.clearOverlays.value) {
    if (chatModel.clearOverlays.value && newChatSheetState == NewChatSheetState.VISIBLE) hideNewChatSheet()
  }
  var searchInList by rememberSaveable { mutableStateOf("") }
  val scaffoldState = rememberScaffoldState()
  Scaffold (
    topBar = { ChatListToolbar(chatModel, scaffoldState.drawerState, stopped) { searchInList = it.trim() } },
    scaffoldState = scaffoldState,
    drawerContent = { SettingsView(chatModel, setPerformLA) },
    floatingActionButton = {
      if (searchInList.isEmpty()) {
        FloatingActionButton(
          onClick = {
            if (!stopped) {
              if (newChatSheetState == NewChatSheetState.VISIBLE) hideNewChatSheet() else showNewChatSheet()
            }
          },
          elevation = FloatingActionButtonDefaults.elevation(
            defaultElevation = 0.dp,
            pressedElevation = 0.dp,
            hoveredElevation = 0.dp,
            focusedElevation = 0.dp,
          ),
          backgroundColor = if (!stopped) MaterialTheme.colors.primary else HighOrLowlight,
          contentColor = Color.White
        ) {
          Icon(if (newChatSheetState != NewChatSheetState.VISIBLE) Icons.Default.Edit else Icons.Default.Close, stringResource(R.string.add_contact_or_create_group))
        }
      }
    }
  ) {
    Box(Modifier.padding(it)) {
      Column(
        modifier = Modifier
          .fillMaxSize()
          .background(MaterialTheme.colors.background)
      ) {
        if (chatModel.chats.isNotEmpty()) {
          ChatList(chatModel, search = searchInList)
        } else {
          Box(Modifier.fillMaxSize()) {
            if (!stopped && newChatSheetState != NewChatSheetState.VISIBLE) {
              OnboardingButtons(showNewChatSheet)
            }
            Text(stringResource(R.string.you_have_no_chats), Modifier.align(Alignment.Center), color = HighOrLowlight)
          }
        }
      }
    }
  }
  if (newChatSheetState != NewChatSheetState.GONE && searchInList.isEmpty()) {
    var startAnimate by remember { mutableStateOf(false) }
    val resultingColor = if (isInDarkTheme()) Color.Black.copy(0.64f) else DrawerDefaults.scrimColor
    val animatedColor by animateColorAsState(
      if (startAnimate && newChatSheetState == NewChatSheetState.VISIBLE) resultingColor else Color.Transparent,
      tween(200, 0, LinearEasing)
    )
    LaunchedEffect(Unit) {
      startAnimate = true
    }
    Surface(
      Modifier
        .fillMaxSize()
        .clickable(remember { MutableInteractionSource() }, indication = null) { hideNewChatSheet() },
      color = Color.Transparent
    ) {
      Column(
        Modifier.background(animatedColor),
        verticalArrangement = Arrangement.Bottom,
        horizontalAlignment = Alignment.End
      ) {
        NewChatSheet(chatModel, newChatSheetState == NewChatSheetState.VISIBLE, hideNewChatSheet)
        FloatingActionButton(
          onClick = {
            if (!stopped) hideNewChatSheet()
          },
          Modifier.padding(end = 16.dp, bottom = 16.dp),
          elevation = FloatingActionButtonDefaults.elevation(
            defaultElevation = 0.dp,
            pressedElevation = 0.dp,
            hoveredElevation = 0.dp,
            focusedElevation = 0.dp,
          ),
          backgroundColor = if (!stopped) MaterialTheme.colors.primary else HighOrLowlight,
          contentColor = Color.White
        ) {
          Icon(if (newChatSheetState != NewChatSheetState.VISIBLE) Icons.Default.Edit else Icons.Default.Close, stringResource(R.string.add_contact_or_create_group))
        }
      }
    }
  }
}

@Composable
private fun OnboardingButtons(openNewChatSheet: () -> Unit) {
  Column(Modifier.fillMaxSize().padding(DEFAULT_PADDING), horizontalAlignment = Alignment.End, verticalArrangement = Arrangement.Bottom) {
    val uriHandler = LocalUriHandler.current
    ConnectButton(generalGetString(R.string.chat_with_developers)) {
      uriHandler.openUri(simplexTeamUri)
    }
    Spacer(Modifier.height(DEFAULT_PADDING))
    ConnectButton(generalGetString(R.string.tap_to_start_new_chat), openNewChatSheet)
    val color = MaterialTheme.colors.primary
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
  Button(onClick,
    shape = RoundedCornerShape(21.dp),
    colors = ButtonDefaults.textButtonColors(
      backgroundColor = MaterialTheme.colors.primary
    ),
    elevation = null,
    contentPadding = PaddingValues(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF),
    modifier = Modifier.height(42.dp)
  ) {
    Text(text, color = Color.White)
  }
}

@Composable
private fun ChatListToolbar(chatModel: ChatModel, drawerState: DrawerState, stopped: Boolean, onSearchValueChanged: (String) -> Unit) {
  var showSearch by rememberSaveable { mutableStateOf(false) }
  val hideSearchOnBack = { onSearchValueChanged(""); showSearch = false }
  if (showSearch) {
    BackHandler(onBack = hideSearchOnBack)
  }
  val barButtons = arrayListOf<@Composable RowScope.() -> Unit>()
  if (chatModel.chats.size >= 8) {
    barButtons.add {
      IconButton({ showSearch = true }) {
        Icon(Icons.Outlined.Search, stringResource(android.R.string.search_go).capitalize(Locale.current), tint = MaterialTheme.colors.primary)
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
          Icons.Filled.Report,
          generalGetString(R.string.chat_is_stopped_indication),
          tint = Color.Red,
        )
      }
    }
  }

  val scope = rememberCoroutineScope()
  DefaultTopAppBar(
    navigationButton = { if (showSearch)
      NavigationButtonBack(hideSearchOnBack)
    else
      NavigationButtonMenu { scope.launch { if (drawerState.isOpen) drawerState.close() else drawerState.open() } } },
    title = {
      Row(verticalAlignment = Alignment.CenterVertically) {
        Text(
          stringResource(R.string.your_chats),
          color = MaterialTheme.colors.onBackground,
          fontWeight = FontWeight.SemiBold,
        )
        if (chatModel.incognito.value) {
          Icon(
            Icons.Filled.TheaterComedy,
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
  Divider()
}

@Composable
private fun ChatList(chatModel: ChatModel, search: String) {
  val filter: (Chat) -> Boolean = { chat: Chat ->
    chat.chatInfo.chatViewName.lowercase().contains(search.lowercase())
  }
  val chats by remember(search) { derivedStateOf { if (search.isEmpty()) chatModel.chats else chatModel.chats.filter(filter) } }
  LazyColumn(
    modifier = Modifier.fillMaxWidth()
  ) {
    items(chats) { chat ->
      ChatListNavLinkView(chat, chatModel)
    }
  }
}
