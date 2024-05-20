package chat.simplex.common.views.home


import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.*
import androidx.compose.ui.text.font.FontStyle
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.unit.*
import chat.simplex.common.SettingsViewState
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.stopRemoteHostAndReloadHosts
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.WhatsNewView
import chat.simplex.common.views.onboarding.shouldShowWhatsNew
import chat.simplex.common.views.usersettings.SettingsView
import chat.simplex.common.platform.*
import chat.simplex.common.views.call.Call
import chat.simplex.common.views.chatlist.*
import chat.simplex.common.views.newchat.*
import chat.simplex.res.MR
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.MutableStateFlow
import java.net.URI

@Composable
fun HomeView(chatModel: ChatModel, settingsState: SettingsViewState, setPerformLA: (Boolean) -> Unit, stopped: Boolean) {
  val newChatSheetState by rememberSaveable(stateSaver = AnimatedViewState.saver()) { mutableStateOf(MutableStateFlow(AnimatedViewState.GONE)) }
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
      ModalManager.center.showCustomModal { close -> WhatsNewView(close = close) }
    }
  }
  LaunchedEffect(chatModel.clearOverlays.value) {
    if (chatModel.clearOverlays.value && newChatSheetState.value.isVisible()) hideNewChatSheet(false)
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
    topBar = { Box(Modifier.padding(end = endPadding)) { HomeTopBar(stopped) } },
    bottomBar = { HomeBottomBar(scaffoldState.drawerState, userPickerState, stopped) },
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
      if (searchText.value.text.isEmpty() && !chatModel.desktopNoUserNoRemote && chatModel.chatRunning.value == true) {
        FloatingActionButton(
          onClick = {
            if (!stopped) {
              if (newChatSheetState.value.isVisible()) hideNewChatSheet(true) else showNewChatSheet()
            }
          },
          Modifier.padding(end = DEFAULT_PADDING - 16.dp + endPadding, bottom = DEFAULT_PADDING - 16.dp),
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
    Box(Modifier.padding(it).padding(end = endPadding)) {
      Box(
        modifier = Modifier
          .fillMaxSize()
      ) {
        if (!chatModel.desktopNoUserNoRemote) {
          ChatList(chatModel, searchText = searchText)
        }
        if (chatModel.chats.isEmpty() && !chatModel.switchingUsersAndHosts.value && !chatModel.desktopNoUserNoRemote) {
          Text(stringResource(
            if (chatModel.chatRunning.value == null) MR.strings.loading_chats else MR.strings.you_have_no_chats),
            Modifier.align(Alignment.Center), color = MaterialTheme.colors.secondary
          )
        }
      }
    }
  }
  if (searchText.value.text.isEmpty()) {
    if (appPlatform.isDesktop) {
      val call = remember { chatModel.activeCall }.value
      if (call != null) {
        ActiveCallInteractiveArea(call, newChatSheetState)
      }
    }
    // TODO disable this button and sheet for the duration of the switch
    tryOrShowError("NewChatSheet", error = {}) {
      NewChatSheet(chatModel, newChatSheetState, stopped, hideNewChatSheet)
    }
  }
  if (appPlatform.isAndroid) {
    tryOrShowError("UserPicker", error = {}) {
      UserPicker(chatModel, userPickerState) {
        scope.launch { if (scaffoldState.drawerState.isOpen) scaffoldState.drawerState.close() else scaffoldState.drawerState.open() }
        userPickerState.value = AnimatedViewState.GONE
      }
    }
  }
}

@Composable
private fun HomeTopBar(stopped: Boolean) {
  val barButtons = arrayListOf<@Composable RowScope.() -> Unit>()
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
  DefaultTopAppBar(
    title = {
      Row(verticalAlignment = Alignment.CenterVertically) {
        Text(
          stringResource(MR.strings.your_chats),
          color = MaterialTheme.colors.onBackground,
          fontWeight = FontWeight.SemiBold,
        )
      }
    },
    onTitleClick = null,
    showSearch = false,
    onSearchValueChanged = {},
    buttons = barButtons
  )
  Divider(Modifier.padding(top = AppBarHeight))
}

@Composable
private fun HomeBottomBar(drawerState: DrawerState, userPickerState: MutableStateFlow<AnimatedViewState>, stopped: Boolean) {
  Box(
    Modifier
      .fillMaxWidth()
      .height(AppBarHeight)
      .background(MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.97f))
      .padding(horizontal = 4.dp),
    contentAlignment = Alignment.CenterStart,
  ) {
    Row(
      Modifier
        .fillMaxHeight()
        .fillMaxWidth(),
      horizontalArrangement = Arrangement.SpaceEvenly,
      verticalAlignment = Alignment.CenterVertically,
    ) {
      SettingsButton(drawerState, userPickerState, stopped)

      Text("Chats")

      Text("Contacts")
    }
  }
}

@Composable
fun SettingsButton(drawerState: DrawerState, userPickerState: MutableStateFlow<AnimatedViewState>, stopped: Boolean) {
  val scope = rememberCoroutineScope()
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
}

@Composable
fun UserProfileButton(image: String?, allRead: Boolean, onButtonClicked: () -> Unit) {
  Row(verticalAlignment = Alignment.CenterVertically) {
    IconButton(onClick = onButtonClicked) {
      Box {
        ProfileImage(
          image = image,
          size = 52.dp,
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
expect fun ActiveCallInteractiveArea(call: Call, newChatSheetState: MutableStateFlow<AnimatedViewState>)

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
private fun ErrorSettingsView() {
  Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
    Text(generalGetString(MR.strings.error_showing_content), color = MaterialTheme.colors.error, fontStyle = FontStyle.Italic)
  }
}
