package chat.simplex.common.views.chatlist

import SectionItemView
import androidx.compose.animation.core.*
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.interaction.collectIsHoveredAsState
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.*
import androidx.compose.ui.draw.*
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.platform.LocalDensity
import dev.icerock.moko.resources.compose.painterResource
import androidx.compose.ui.text.capitalize
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.intl.Locale
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.stopRemoteHostAndReloadHosts
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.CreateProfile
import chat.simplex.common.views.remote.*
import chat.simplex.common.views.usersettings.doWithAuth
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*
import kotlin.math.roundToInt

@Composable
fun UserPicker(
  chatModel: ChatModel,
  userPickerState: MutableStateFlow<AnimatedViewState>,
  showSettings: Boolean = true,
  showCancel: Boolean = false,
  cancelClicked: () -> Unit = {},
  useFromDesktopClicked: () -> Unit = {},
  settingsClicked: () -> Unit = {},
) {
  val scope = rememberCoroutineScope()
  var newChat by remember { mutableStateOf(userPickerState.value) }
  if (newChat.isVisible()) {
    BackHandler {
      userPickerState.value = AnimatedViewState.HIDING
    }
  }
  val users by remember {
    derivedStateOf {
      chatModel.users
        .filter { u -> u.user.activeUser || !u.user.hidden }
        .sortedByDescending { it.user.activeUser }
    }
  }
  val remoteHosts by remember {
    derivedStateOf {
      chatModel.remoteHosts
        .sortedBy { it.hostDeviceName }
    }
  }
  val animatedFloat = remember { Animatable(if (newChat.isVisible()) 0f else 1f) }
  LaunchedEffect(Unit) {
    launch {
      userPickerState.collect {
        newChat = it
        launch {
          animatedFloat.animateTo(if (newChat.isVisible()) 1f else 0f, newChatSheetAnimSpec())
          if (newChat.isHiding()) userPickerState.value = AnimatedViewState.GONE
        }
      }
    }
  }
  LaunchedEffect(Unit) {
    snapshotFlow { newChat.isVisible() }
      .distinctUntilChanged()
      .filter { it }
      .collect {
        try {
          val updatedUsers = chatModel.controller.listUsers(chatModel.remoteHostId()).sortedByDescending { it.user.activeUser }
          var same = users.size == updatedUsers.size
          if (same) {
            for (i in 0 until minOf(users.size, updatedUsers.size)) {
              val prev = updatedUsers[i].user
              val next = users[i].user
              if (prev.userId != next.userId || prev.activeUser != next.activeUser || prev.chatViewName != next.chatViewName || prev.image != next.image) {
                same = false
                break
              }
            }
          }
          if (!same) {
            chatModel.users.clear()
            chatModel.users.addAll(updatedUsers)
          }
        } catch (e: Exception) {
          Log.e(TAG, "Error updating users ${e.stackTraceToString()}")
        }
        if (!appPlatform.isDesktop) return@collect
        try {
          val updatedHosts = chatModel.controller.listRemoteHosts()?.sortedBy { it.hostDeviceName } ?: emptyList()
          if (remoteHosts != updatedHosts) {
            chatModel.remoteHosts.clear()
            chatModel.remoteHosts.addAll(updatedHosts)
          }
        } catch (e: Exception) {
          Log.e(TAG, "Error updating remote hosts ${e.stackTraceToString()}")
        }
      }
  }
  LaunchedEffect(Unit) {
    // Controller.ctrl can be null when self-destructing activates
    if (controller.ctrl != null && controller.ctrl != -1L) {
      withBGApi {
        controller.reloadRemoteHosts()
      }
    }
  }
  val UsersView: @Composable ColumnScope.() -> Unit = {
    users.forEach { u ->
      UserProfilePickerItem(u.user, u.unreadCount, openSettings = settingsClicked) {
        userPickerState.value = AnimatedViewState.HIDING
        if (!u.user.activeUser) {
          withBGApi {
            controller.showProgressIfNeeded {
              ModalManager.closeAllModalsEverywhere()
              chatModel.controller.changeActiveUser(u.user.remoteHostId, u.user.userId, null)
            }
          }
        }
      }
      Divider(Modifier.requiredHeight(1.dp))
      if (u.user.activeUser) Divider(Modifier.requiredHeight(0.5.dp))
    }
  }
  val xOffset = with(LocalDensity.current) { 10.dp.roundToPx() }
  val maxWidth = with(LocalDensity.current) { windowWidth() * density }
  Box(Modifier
    .fillMaxSize()
    .offset { IntOffset(if (newChat.isGone()) -maxWidth.value.roundToInt() else xOffset, 0) }
    .clickable(interactionSource = remember { MutableInteractionSource() }, indication = null, onClick = { userPickerState.value = AnimatedViewState.HIDING })
    .padding(bottom = 10.dp, top = 10.dp)
    .graphicsLayer {
      alpha = animatedFloat.value
      translationY = (animatedFloat.value - 1) * xOffset
    }
  ) {
    Column(
      Modifier
        .widthIn(min = 260.dp)
        .width(IntrinsicSize.Min)
        .height(IntrinsicSize.Min)
        .shadow(8.dp, RoundedCornerShape(corner = CornerSize(25.dp)), clip = true)
        .background(MaterialTheme.colors.surface, RoundedCornerShape(corner = CornerSize(25.dp)))
        .clip(RoundedCornerShape(corner = CornerSize(25.dp)))
    ) {
      val currentRemoteHost = remember { chatModel.currentRemoteHost }.value
      Column(Modifier.weight(1f).verticalScroll(rememberScrollState())) {
        if (remoteHosts.isNotEmpty()) {
          if (currentRemoteHost == null && chatModel.localUserCreated.value == true) {
            LocalDevicePickerItem(true) {
              userPickerState.value = AnimatedViewState.HIDING
              switchToLocalDevice()
            }
            Divider(Modifier.requiredHeight(1.dp))
          } else if (currentRemoteHost != null) {
            val connecting = rememberSaveable { mutableStateOf(false) }
            RemoteHostPickerItem(currentRemoteHost,
              actionButtonClick = {
                userPickerState.value = AnimatedViewState.HIDING
                stopRemoteHostAndReloadHosts(currentRemoteHost, true)
              }) {
                userPickerState.value = AnimatedViewState.HIDING
                switchToRemoteHost(currentRemoteHost, connecting)
              }
            Divider(Modifier.requiredHeight(1.dp))
          }
        }

        UsersView()

        if (remoteHosts.isNotEmpty() && currentRemoteHost != null && chatModel.localUserCreated.value == true) {
          LocalDevicePickerItem(false) {
            userPickerState.value = AnimatedViewState.HIDING
            switchToLocalDevice()
          }
          Divider(Modifier.requiredHeight(1.dp))
        }
        remoteHosts.filter { !it.activeHost }.forEach { h ->
          val connecting = rememberSaveable { mutableStateOf(false) }
          RemoteHostPickerItem(h,
            actionButtonClick = {
              userPickerState.value = AnimatedViewState.HIDING
              stopRemoteHostAndReloadHosts(h, false)
            }) {
              userPickerState.value = AnimatedViewState.HIDING
              switchToRemoteHost(h, connecting)
          }
          Divider(Modifier.requiredHeight(1.dp))
        }
      }
      if (appPlatform.isAndroid) {
        UseFromDesktopPickerItem {
          ModalManager.start.showCustomModal { close ->
            ConnectDesktopView(close)
          }
          userPickerState.value = AnimatedViewState.GONE
        }
        Divider(Modifier.requiredHeight(1.dp))
      } else {
        if (remoteHosts.isEmpty()) {
          LinkAMobilePickerItem {
            ModalManager.start.showModal {
              ConnectMobileView()
            }
            userPickerState.value = AnimatedViewState.GONE
          }
          Divider(Modifier.requiredHeight(1.dp))
        }
        if (chatModel.desktopNoUserNoRemote) {
          CreateInitialProfile {
            doWithAuth(generalGetString(MR.strings.auth_open_chat_profiles), generalGetString(MR.strings.auth_log_in_using_credential)) {
              ModalManager.center.showModalCloseable { close ->
                LaunchedEffect(Unit) {
                  userPickerState.value = AnimatedViewState.HIDING
                }
                CreateProfile(chat.simplex.common.platform.chatModel, close)
              }
            }
          }
          Divider(Modifier.requiredHeight(1.dp))
        }
      }
      if (showSettings) {
        SettingsPickerItem(settingsClicked)
      }
      if (showCancel) {
        CancelPickerItem(cancelClicked)
      }
    }
  }
}

@Composable
fun UserProfilePickerItem(
  u: User,
  unreadCount: Int = 0,
  enabled: Boolean = chatModel.chatRunning.value == true || chatModel.connectedToRemote,
  onLongClick: () -> Unit = {},
  openSettings: () -> Unit = {},
  onClick: () -> Unit
) {
  Row(
    Modifier
      .fillMaxWidth()
      .sizeIn(minHeight = 46.dp)
      .combinedClickable(
        enabled = enabled,
        onClick = if (u.activeUser) openSettings else onClick,
        onLongClick = onLongClick,
        interactionSource = remember { MutableInteractionSource() },
        indication = if (!u.activeUser) LocalIndication.current else null
      )
      .onRightClick { if (enabled) onLongClick() }
      .padding(start = DEFAULT_PADDING_HALF, end = DEFAULT_PADDING),
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    UserProfileRow(u, enabled)
    if (u.activeUser) {
      Icon(painterResource(MR.images.ic_done_filled), null, Modifier.size(20.dp), tint = MaterialTheme.colors.onBackground)
    } else if (u.hidden) {
      Icon(painterResource(MR.images.ic_lock), null, Modifier.size(20.dp), tint = MaterialTheme.colors.secondary)
    } else if (unreadCount > 0) {
      Box(
        contentAlignment = Alignment.Center
      ) {
        Text(
          unreadCountStr(unreadCount),
          color = Color.White,
          fontSize = 11.sp,
          modifier = Modifier
            .background(MaterialTheme.colors.primaryVariant, shape = CircleShape)
            .padding(2.dp)
            .badgeLayout()
        )
      }
    } else if (!u.showNtfs) {
      Icon(painterResource(MR.images.ic_notifications_off), null, Modifier.size(20.dp), tint = MaterialTheme.colors.secondary)
    }  else {
      Box(Modifier.size(20.dp))
    }
  }
}

@Composable
fun UserProfileRow(u: User, enabled: Boolean = chatModel.chatRunning.value == true || chatModel.connectedToRemote) {
  Row(
    Modifier
      .widthIn(max = windowWidth() * 0.7f)
      .padding(vertical = 8.dp),
    verticalAlignment = Alignment.CenterVertically
  ) {
    ProfileImage(
      image = u.image,
      size = 54.dp * fontSizeSqrtMultiplier
    )
    Text(
      u.displayName,
      modifier = Modifier
        .padding(start = 10.dp, end = 8.dp),
      color = if (enabled) MenuTextColor else MaterialTheme.colors.secondary,
      fontWeight = if (u.activeUser) FontWeight.Medium else FontWeight.Normal
    )
  }
}

@Composable
fun RemoteHostPickerItem(h: RemoteHostInfo, onLongClick: () -> Unit = {}, actionButtonClick: () -> Unit = {}, onClick: () -> Unit) {
  Row(
    Modifier
      .fillMaxWidth()
      .background(color = if (h.activeHost) MaterialTheme.colors.surface.mixWith(MaterialTheme.colors.onBackground, 0.95f) else Color.Unspecified)
      .sizeIn(minHeight = 46.dp)
      .combinedClickable(
        onClick = onClick,
        onLongClick = onLongClick
      )
      .onRightClick { onLongClick() }
      .padding(start = DEFAULT_PADDING_HALF, end = DEFAULT_PADDING),
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    RemoteHostRow(h)
    if (h.sessionState is RemoteHostSessionState.Connected) {
      HostDisconnectButton(actionButtonClick)
    } else {
      Box(Modifier.size(20.dp))
    }
  }
}

@Composable
fun RemoteHostRow(h: RemoteHostInfo) {
  Row(
    Modifier
      .widthIn(max = windowWidth() * 0.7f)
      .padding(start = 17.dp),
    verticalAlignment = Alignment.CenterVertically
  ) {
    Icon(painterResource(MR.images.ic_smartphone_300), h.hostDeviceName, Modifier.size(20.dp * fontSizeSqrtMultiplier), tint = MaterialTheme.colors.onBackground)
    Text(
      h.hostDeviceName,
      modifier = Modifier.padding(start = 26.dp, end = 8.dp),
      color = if (h.activeHost) MaterialTheme.colors.onBackground else MenuTextColor,
      fontSize = 14.sp,
    )
  }
}

@Composable
fun LocalDevicePickerItem(active: Boolean, onLongClick: () -> Unit = {}, onClick: () -> Unit) {
  Row(
    Modifier
      .fillMaxWidth()
      .background(color = if (active) MaterialTheme.colors.surface.mixWith(MaterialTheme.colors.onBackground, 0.95f) else Color.Unspecified)
      .sizeIn(minHeight = 46.dp)
      .combinedClickable(
        onClick = if (active) {{}} else onClick,
        onLongClick = onLongClick,
        interactionSource = remember { MutableInteractionSource() },
        indication = if (!active) LocalIndication.current else null
      )
      .onRightClick { onLongClick() }
      .padding(start = DEFAULT_PADDING_HALF, end = DEFAULT_PADDING),
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    LocalDeviceRow(active)
    Box(Modifier.size(20.dp))
  }
}

@Composable
fun LocalDeviceRow(active: Boolean) {
  Row(
    Modifier
      .widthIn(max = windowWidth() * 0.7f)
      .padding(start = 17.dp, end = DEFAULT_PADDING),
    verticalAlignment = Alignment.CenterVertically
  ) {
    Icon(painterResource(MR.images.ic_desktop), stringResource(MR.strings.this_device), Modifier.size(20.dp * fontSizeSqrtMultiplier), tint = MaterialTheme.colors.onBackground)
    Text(
      stringResource(MR.strings.this_device),
      modifier = Modifier.padding(start = 26.dp, end = 8.dp),
      color = if (active) MaterialTheme.colors.onBackground else MenuTextColor,
      fontSize = 14.sp,
    )
  }
}

@Composable
private fun UseFromDesktopPickerItem(onClick: () -> Unit) {
  SectionItemView(onClick, padding = PaddingValues(start = DEFAULT_PADDING + 7.dp, end = DEFAULT_PADDING), minHeight = 68.dp) {
    val text = generalGetString(MR.strings.settings_section_title_use_from_desktop).lowercase().capitalize(Locale.current)
    Icon(painterResource(MR.images.ic_desktop), text, Modifier.size(20.dp * fontSizeSqrtMultiplier), tint = MaterialTheme.colors.onBackground)
    Spacer(Modifier.width(DEFAULT_PADDING + 6.dp))
    Text(text, color = MenuTextColor)
  }
}

@Composable
private fun LinkAMobilePickerItem(onClick: () -> Unit) {
  SectionItemView(onClick, padding = PaddingValues(start = DEFAULT_PADDING + 7.dp, end = DEFAULT_PADDING), minHeight = 68.dp) {
    val text = generalGetString(MR.strings.link_a_mobile)
    Icon(painterResource(MR.images.ic_smartphone_300), text, Modifier.size(20.dp * fontSizeSqrtMultiplier), tint = MaterialTheme.colors.onBackground)
    Spacer(Modifier.width(DEFAULT_PADDING + 6.dp))
    Text(text, color = MenuTextColor)
  }
}

@Composable
private fun CreateInitialProfile(onClick: () -> Unit) {
  SectionItemView(onClick, padding = PaddingValues(start = DEFAULT_PADDING + 7.dp, end = DEFAULT_PADDING), minHeight = 68.dp) {
    val text = generalGetString(MR.strings.create_chat_profile)
    Icon(painterResource(MR.images.ic_manage_accounts), text, Modifier.size(20.dp * fontSizeSqrtMultiplier), tint = MaterialTheme.colors.onBackground)
    Spacer(Modifier.width(DEFAULT_PADDING + 6.dp))
    Text(text, color = MenuTextColor)
  }
}

@Composable
private fun SettingsPickerItem(onClick: () -> Unit) {
  SectionItemView(onClick, padding = PaddingValues(start = DEFAULT_PADDING + 7.dp, end = DEFAULT_PADDING), minHeight = 68.dp) {
    val text = generalGetString(MR.strings.settings_section_title_settings).lowercase().capitalize(Locale.current)
    Icon(painterResource(MR.images.ic_settings), text, Modifier.size(20.dp * fontSizeSqrtMultiplier), tint = MaterialTheme.colors.onBackground)
    Spacer(Modifier.width(DEFAULT_PADDING + 6.dp))
    Text(text, color = MenuTextColor)
  }
}

@Composable
private fun CancelPickerItem(onClick: () -> Unit) {
  SectionItemView(onClick, padding = PaddingValues(start = DEFAULT_PADDING + 7.dp, end = DEFAULT_PADDING), minHeight = 68.dp) {
    val text = generalGetString(MR.strings.cancel_verb)
    Icon(painterResource(MR.images.ic_close), text, Modifier.size(20.dp * fontSizeSqrtMultiplier), tint = MaterialTheme.colors.onBackground)
    Spacer(Modifier.width(DEFAULT_PADDING + 6.dp))
    Text(text, color = MenuTextColor)
  }
}

@Composable
fun HostDisconnectButton(onClick: (() -> Unit)?) {
  val interactionSource = remember { MutableInteractionSource() }
  val hovered = interactionSource.collectIsHoveredAsState().value
  IconButton(onClick ?: {}, Modifier.requiredSize(20.dp * fontSizeSqrtMultiplier), enabled = onClick != null) {
    Icon(
      painterResource(if (onClick == null) MR.images.ic_desktop else if (hovered) MR.images.ic_wifi_off else MR.images.ic_wifi),
      null,
      Modifier.size(20.dp).hoverable(interactionSource),
      tint = if (hovered && onClick != null) WarningOrange else MaterialTheme.colors.onBackground
    )
  }
}

private fun switchToLocalDevice() {
  withBGApi {
    chatController.switchUIRemoteHost(null)
  }
}

private fun switchToRemoteHost(h: RemoteHostInfo, connecting: MutableState<Boolean>) {
  if (!h.activeHost()) {
    withBGApi {
      ModalManager.closeAllModalsEverywhere()
      if (h.sessionState != null) {
        chatModel.controller.switchUIRemoteHost(h.remoteHostId)
      } else {
        connectMobileDevice(h, connecting)
      }
    }
  } else {
    connectMobileDevice(h, connecting)
  }
}
