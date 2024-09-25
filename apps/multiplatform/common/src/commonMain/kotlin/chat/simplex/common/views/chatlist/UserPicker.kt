package chat.simplex.common.views.chatlist

import SectionItemView
import TextIconSpaced
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
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.text.TextStyle
import dev.icerock.moko.resources.compose.painterResource
import androidx.compose.ui.text.capitalize
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.intl.Locale
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.stopRemoteHostAndReloadHosts
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.CreateProfile
import chat.simplex.common.views.localauth.VerticalDivider
import chat.simplex.common.views.remote.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.common.views.usersettings.AppearanceScope.ColorModeSwitcher
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*
import kotlin.math.max

val USER_PICKER_ROW_PADDING = 16.dp
val USER_PICKER_SECTION_SPACING = 32.dp
val USER_PICKER_IMAGE_SIZE = 44.dp
val USER_PICKER_AVATAR_PADDING_MARGIN = USER_PICKER_IMAGE_SIZE / 12

@Composable
fun UserPicker(
  chatModel: ChatModel,
  userPickerState: MutableStateFlow<AnimatedViewState>,
  setPerformLA: (Boolean) -> Unit,
) {
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
        .sortedByDescending { it.user.activeOrder }
    }
  }
  val remoteHosts by remember {
    derivedStateOf {
      chatModel.remoteHosts
        .sortedBy { it.hostDeviceName }
    }
  }

  val view = LocalMultiplatformView()
  LaunchedEffect(Unit) {
    launch {
      userPickerState.collect {
        newChat = it
        if (it.isVisible()) {
          hideKeyboard(view)
        }
        launch {
          if (newChat.isHiding()) userPickerState.value = AnimatedViewState.GONE
        }
      }
    }
  }

  LaunchedEffect(Unit) {
    launch {
      snapshotFlow { ModalManager.start.modalCount.value }
        .filter { it > 0 }
        .collect {
          closePicker(userPickerState)
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

  PlatformUserPicker(
    modifier = Modifier
      .height(IntrinsicSize.Min)
      .fillMaxWidth()
      .then(if (newChat.isVisible()) Modifier.shadow(8.dp, clip = true) else Modifier)
      .background(MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.97f))
      .padding(vertical = USER_PICKER_SECTION_SPACING),
    pickerState = userPickerState
  ) {
    val showCustomModal: (@Composable() (ModalData.(ChatModel, () -> Unit) -> Unit)) -> () -> Unit = { modalView ->
      {
        ModalManager.start.showCustomModal { close -> modalView(chatModel, close) }
      }
    }
    val stopped = chatModel.chatRunning.value == false

    @Composable
    fun FirstSection() {
      if (remoteHosts.isNotEmpty()) {
        val currentRemoteHost = remember { chatModel.currentRemoteHost }.value
        val localDeviceActive = currentRemoteHost == null && chatModel.localUserCreated.value == true

        DevicePickerRow(
          localDeviceActive = localDeviceActive,
          remoteHosts = remoteHosts,
          onRemoteHostClick = { h, connecting ->
            userPickerState.value = AnimatedViewState.HIDING
            switchToRemoteHost(h, connecting)
          },
          onLocalDeviceClick = {
            userPickerState.value = AnimatedViewState.HIDING
            switchToLocalDevice()
          },
          onRemoteHostActionButtonClick = { h ->
            userPickerState.value = AnimatedViewState.HIDING
            stopRemoteHostAndReloadHosts(h, true)
          }
        )
      }
      UserPickerUsersSection(
        users = users,
        onUserClicked = { user ->
          if (!user.activeUser) {
            userPickerState.value = AnimatedViewState.HIDING
            withBGApi {
              controller.showProgressIfNeeded {
                ModalManager.closeAllModalsEverywhere()
                chatModel.controller.changeActiveUser(user.remoteHostId, user.userId, null)
              }
            }
          } else {
            showCustomModal { chatModel, close -> UserProfileView(chatModel, close) }()
            withBGApi {
              closePicker(userPickerState)
            }
          }
        },
        stopped = stopped
      )
    }

    @Composable
    fun SecondSection() {
      UserPickerOptionRow(
        painterResource(MR.images.ic_qr_code),
        if (chatModel.userAddress.value != null) generalGetString(MR.strings.your_simplex_contact_address) else generalGetString(MR.strings.create_simplex_address),
        showCustomModal { it, close -> UserAddressView(it, shareViaProfile = it.currentUser.value!!.addressShared, close = close) }, disabled = stopped
      )
      UserPickerOptionRow(
        painterResource(MR.images.ic_toggle_on),
        stringResource(MR.strings.chat_preferences),
        click = if (stopped) null else ({
          showCustomModal { m, close ->
            PreferencesView(m, m.currentUser.value ?: return@showCustomModal, close)
          }()
        }),
        disabled = stopped
      )
      if (chatModel.desktopNoUserNoRemote) {
        UserPickerOptionRow(
          painterResource(MR.images.ic_manage_accounts),
          generalGetString(MR.strings.create_chat_profile),
          {
            doWithAuth(generalGetString(MR.strings.auth_open_chat_profiles), generalGetString(MR.strings.auth_log_in_using_credential)) {
              ModalManager.center.showModalCloseable { close ->
                LaunchedEffect(Unit) {
                  userPickerState.value = AnimatedViewState.HIDING
                }
                CreateProfile(chat.simplex.common.platform.chatModel, close)
              }
            }
          }
        )
      } else {
        UserPickerOptionRow(
          painterResource(MR.images.ic_manage_accounts),
          stringResource(MR.strings.your_chat_profiles),
          {
            doWithAuth(
              generalGetString(MR.strings.auth_open_chat_profiles),
              generalGetString(MR.strings.auth_log_in_using_credential)
            ) {
              ModalManager.start.showCustomModal { close ->
                val search = rememberSaveable { mutableStateOf("") }
                val profileHidden = rememberSaveable { mutableStateOf(false) }
                ModalView(
                  { close() },
                  endButtons = {
                    SearchTextField(Modifier.fillMaxWidth(), placeholder = stringResource(MR.strings.search_verb), alwaysVisible = true) { search.value = it }
                  },
                  content = { UserProfilesView(chatModel, search, profileHidden) })
              }
            }
          }
        )
      }
    }

    if (appPlatform.isDesktop || windowOrientation() == WindowOrientation.PORTRAIT) {
      Column {
        FirstSection()
        Spacer(Modifier.padding(USER_PICKER_SECTION_SPACING - DEFAULT_MIN_SECTION_ITEM_PADDING_VERTICAL - 3.dp))
        SecondSection()
        GlobalSettingsSection(
          chatModel = chatModel,
          userPickerState = userPickerState,
          setPerformLA = setPerformLA,
        )
      }
    } else {
      Column {
        FirstSection()
        Spacer(Modifier.padding(USER_PICKER_SECTION_SPACING - DEFAULT_MIN_SECTION_ITEM_PADDING_VERTICAL - 3.dp))
        Row {
          Box(Modifier.weight(1f)) {
            Column {
              SecondSection()
            }
          }
          VerticalDivider()
          Box(Modifier.weight(1f)) {
            Column {
              GlobalSettingsSection(
                chatModel = chatModel,
                userPickerState = userPickerState,
                setPerformLA = setPerformLA,
              )
            }
          }
        }
      }
    }
  }
}

@Composable
private fun GlobalSettingsSection(
  chatModel: ChatModel,
  userPickerState: MutableStateFlow<AnimatedViewState>,
  setPerformLA: (Boolean) -> Unit,
) {
  val stopped = chatModel.chatRunning.value == false

  if (appPlatform.isAndroid) {
    val text = generalGetString(MR.strings.settings_section_title_use_from_desktop).lowercase().capitalize(Locale.current)

    UserPickerOptionRow(
      painterResource(MR.images.ic_desktop),
      text,
      click = {
        ModalManager.start.showCustomModal { close ->
          ConnectDesktopView(close)
        }
      }
    )
  } else {
    UserPickerOptionRow(
      icon = painterResource(MR.images.ic_smartphone_300),
      text = stringResource(if (remember { chat.simplex.common.platform.chatModel.remoteHosts }.isEmpty()) MR.strings.link_a_mobile else MR.strings.linked_mobiles),
      click = {
        userPickerState.value = AnimatedViewState.HIDING
        ModalManager.start.showModal {
          ConnectMobileView()
        }
      },
      disabled = stopped
    )
  }

  SectionItemView(
    click = {
      ModalManager.start.showModalCloseable { close ->
        SettingsView(chatModel, setPerformLA, close)
      }
    },
    padding = PaddingValues(start = DEFAULT_PADDING, end = DEFAULT_PADDING_HALF)
  ) {
    val text = generalGetString(MR.strings.settings_section_title_settings).lowercase().capitalize(Locale.current)
    Icon(painterResource(MR.images.ic_settings), text, tint = MaterialTheme.colors.secondary)
    TextIconSpaced()
    Text(text, color = Color.Unspecified)
    Spacer(Modifier.weight(1f))
    ColorModeSwitcher()
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
      .sizeIn(minHeight = DEFAULT_MIN_SECTION_ITEM_HEIGHT)
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
          fontSize = 10.sp,
          modifier = Modifier
            .background(MaterialTheme.colors.primaryVariant, shape = CircleShape)
            .padding(2.dp)
            .badgeLayout()
        )
      }
    } else if (!u.showNtfs) {
      Icon(painterResource(MR.images.ic_notifications_off), null, Modifier.size(20.dp), tint = MaterialTheme.colors.secondary)
    } else {
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
fun UserPickerOptionRow(icon: Painter, text: String, click: (() -> Unit)? = null, disabled: Boolean = false) {
  SectionItemView(click, disabled = disabled, extraPadding = false) {
    Icon(icon, text, tint = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.secondary)
    TextIconSpaced()
    Text(text = text, color = if (disabled) MaterialTheme.colors.secondary else Color.Unspecified)
  }
}

@Composable
fun UserPickerUserBox(
  userInfo: UserInfo,
  stopped: Boolean,
  size: Dp = USER_PICKER_IMAGE_SIZE,
  modifier: Modifier = Modifier,
  onClick: (user: User) -> Unit,
) {
  Row(
    modifier = modifier
      .userPickerBoxModifier()
      .clickable (
        onClick = { onClick(userInfo.user) },
        enabled = !stopped
      )
      .background(MaterialTheme.colors.surface)
      .padding(USER_PICKER_ROW_PADDING - USER_PICKER_AVATAR_PADDING_MARGIN),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.spacedBy(USER_PICKER_ROW_PADDING - USER_PICKER_AVATAR_PADDING_MARGIN)
  ) {
    Box {
      ProfileImage(size = size + USER_PICKER_AVATAR_PADDING_MARGIN, image = userInfo.user.profile.image, color = MaterialTheme.colors.secondaryVariant)

      if (userInfo.unreadCount > 0 && !userInfo.user.activeUser) {
        unreadBadge(userInfo.unreadCount, userInfo.user.showNtfs)
      }
    }
    val user = userInfo.user
    Text(
      user.displayName,
      fontWeight = if (user.activeUser) FontWeight.Bold else FontWeight.Normal,
      maxLines = 1,
      overflow = TextOverflow.Ellipsis,
    )
  }
}

@Composable
private fun Modifier.userPickerBoxModifier(): Modifier {
  val percent = remember { appPreferences.profileImageCornerRadius.state }
  val r = max(0f, percent.value)

  val cornerSize = when {
    r >= 50 -> 50
    r <= 0 -> 0
    else -> r.toInt()
  }

  val shape = RoundedCornerShape(CornerSize(cornerSize))
  return this.clip(shape).border(1.dp, MaterialTheme.colors.secondaryVariant, shape)
}

@OptIn(ExperimentalLayoutApi::class)
@Composable
private fun DevicePickerRow(
  localDeviceActive: Boolean,
  remoteHosts: List<RemoteHostInfo>,
  onLocalDeviceClick: () -> Unit,
  onRemoteHostClick: (rh: RemoteHostInfo, connecting: MutableState<Boolean>) -> Unit,
  onRemoteHostActionButtonClick: (rh: RemoteHostInfo) -> Unit,
) {
  FlowRow(
    Modifier
      .fillMaxWidth()
      .sizeIn(minHeight = DEFAULT_MIN_SECTION_ITEM_HEIGHT)
      .padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING, bottom = DEFAULT_PADDING, top = DEFAULT_MIN_SECTION_ITEM_PADDING_VERTICAL),
    horizontalArrangement = Arrangement.spacedBy(12.dp),
    verticalArrangement = Arrangement.spacedBy(12.dp)
  ) {
    val activeHost = remoteHosts.firstOrNull { h -> h.activeHost }

    if (activeHost != null) {
      val connecting = rememberSaveable { mutableStateOf(false) }

      DevicePill(
        active = true,
        icon = painterResource(MR.images.ic_smartphone_300),
        text = activeHost.hostDeviceName,
        actionButtonVisible = activeHost.sessionState is RemoteHostSessionState.Connected,
        onActionButtonClick = { onRemoteHostActionButtonClick(activeHost) }
      ) {
        onRemoteHostClick(activeHost, connecting)
      }
    }

    DevicePill(
      active = localDeviceActive,
      icon = painterResource(MR.images.ic_desktop),
      text = stringResource(MR.strings.this_device),
      actionButtonVisible = false
    ) {
      onLocalDeviceClick()
    }

    remoteHosts.filter { h -> h.sessionState is RemoteHostSessionState.Connected && !h.activeHost }.forEach { h ->
      val connecting = rememberSaveable { mutableStateOf(false) }

      DevicePill(
        active = h.activeHost,
        icon = painterResource(MR.images.ic_smartphone_300),
        text = h.hostDeviceName,
        actionButtonVisible = h.sessionState is RemoteHostSessionState.Connected,
        onActionButtonClick = { onRemoteHostActionButtonClick(h) }
      ) {
        onRemoteHostClick(h, connecting)
      }
    }
  }
}

@Composable
expect fun UserPickerUsersSection(
  users: List<UserInfo>,
  stopped: Boolean,
  onUserClicked: (user: User) -> Unit,
)

@Composable
expect fun PlatformUserPicker(
  modifier: Modifier,
  pickerState: MutableStateFlow<AnimatedViewState>,
  content: @Composable () -> Unit
)

@Composable
fun DevicePill(
  active: Boolean,
  icon: Painter,
  text: String,
  actionButtonVisible: Boolean,
  onActionButtonClick: (() -> Unit)? = null,
  onClick: () -> Unit) {
  Row(
    Modifier
      .clip(RoundedCornerShape(8.dp))
      .border(
        BorderStroke(1.dp, MaterialTheme.colors.secondaryVariant),
        shape = RoundedCornerShape(8.dp)
      )
      .background(if (active) MaterialTheme.colors.secondaryVariant else Color.Transparent)
      .clickable(
        enabled = !active,
        onClick = onClick,
        interactionSource = remember { MutableInteractionSource() },
        indication = LocalIndication.current
      ),
    verticalAlignment = Alignment.CenterVertically
  ) {
    Row(
      Modifier.padding(horizontal = 6.dp, vertical = 4.dp),
      verticalAlignment = Alignment.CenterVertically
    ) {
      Icon(
        icon,
        text,
        Modifier.size(16.dp * fontSizeSqrtMultiplier),
        tint = MaterialTheme.colors.onSurface
      )
      Spacer(Modifier.width(DEFAULT_SPACE_AFTER_ICON * fontSizeSqrtMultiplier))
      Text(
        text,
        color = MaterialTheme.colors.onSurface,
        fontSize = 12.sp,
        overflow = TextOverflow.Ellipsis,
        maxLines = 1,
        modifier = if (onActionButtonClick != null && actionButtonVisible) Modifier.widthIn(max = 300.dp * fontSizeSqrtMultiplier) else Modifier
      )
      if (onActionButtonClick != null && actionButtonVisible) {
        val interactionSource = remember { MutableInteractionSource() }
        val hovered = interactionSource.collectIsHoveredAsState().value
        Spacer(Modifier.width(DEFAULT_SPACE_AFTER_ICON * fontSizeSqrtMultiplier))
        IconButton(onActionButtonClick, Modifier.requiredSize(16.dp * fontSizeSqrtMultiplier)) {
          Icon(
            painterResource(if (hovered) MR.images.ic_wifi_off else MR.images.ic_wifi),
            null,
            Modifier.size(16.dp * fontSizeSqrtMultiplier).hoverable(interactionSource),
            tint = if (hovered) WarningOrange else MaterialTheme.colors.onBackground
          )
        }
      }
    }
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

@Composable
fun BoxScope.unreadBadge(unreadCount: Int, userMuted: Boolean) {
  Text(
    if (unreadCount > 0) unreadCountStr(unreadCount) else "",
    color = Color.White,
    fontSize = 10.sp,
    style = TextStyle(textAlign = TextAlign.Center),
    modifier = Modifier
      .offset(y = 3.sp.toDp())
      .background(if (userMuted) MaterialTheme.colors.primaryVariant else MaterialTheme.colors.secondary, shape = CircleShape)
      .badgeLayout()
      .padding(horizontal = 2.sp.toDp())
      .padding(vertical = 2.sp.toDp())
      .align(Alignment.TopEnd)
  )
}

private suspend fun closePicker(userPickerState: MutableStateFlow<AnimatedViewState>) {
  delay(500)
  userPickerState.value = AnimatedViewState.HIDING
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
