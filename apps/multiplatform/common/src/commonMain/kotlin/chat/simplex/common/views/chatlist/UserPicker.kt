package chat.simplex.common.views.chatlist

import SectionItemView
import SectionView
import TextIconSpaced
import androidx.compose.animation.*
import androidx.compose.animation.core.*
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.interaction.collectIsHoveredAsState
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.itemsIndexed
import androidx.compose.foundation.shape.*
import androidx.compose.material.*
import androidx.compose.material.DrawerDefaults.ScrimOpacity
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.*
import androidx.compose.ui.draw.*
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.painter.Painter
import dev.icerock.moko.resources.compose.painterResource
import androidx.compose.ui.text.capitalize
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.intl.Locale
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatController.stopRemoteHostAndReloadHosts
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.CreateProfile
import chat.simplex.common.views.newchat.*
import chat.simplex.common.views.onboarding.OnboardingStage
import chat.simplex.common.views.remote.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.common.views.usersettings.AppearanceScope.ColorModeSwitcher
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*

@Composable
private fun UserPickerOptionRow(icon: Painter, text: String, click: (() -> Unit)? = null, disabled: Boolean = false) {
  SectionItemView(click, disabled = disabled) {
    Icon(icon, text, tint = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.secondary)
    TextIconSpaced()
    Text(text = text, color = if (disabled) MaterialTheme.colors.secondary else Color.Unspecified)
  }
}

@Composable
fun ShareListUserPicker(
  chatModel: ChatModel,
  search: MutableState<String>,
  close: () -> Unit,
) {
  val switchingProfile = remember { mutableStateOf(false) }
  val searchTextOrPassword = rememberSaveable { search }
  val profiles = remember {
    chatModel.users.map { it.user }.sortedBy { !it.activeUser }
  }
  val filteredProfiles by remember {
    derivedStateOf { filteredProfiles(profiles, searchTextOrPassword.value) }
  }

  var progressByTimeout by rememberSaveable { mutableStateOf(false) }

  LaunchedEffect(switchingProfile.value) {
    progressByTimeout = if (switchingProfile.value) {
      delay(500)
      switchingProfile.value
    } else {
      false
    }
  }

  BoxWithConstraints {
    Column(
      Modifier
        .fillMaxSize()
        .alpha(if (progressByTimeout) 0.6f else 1f)
    ) {
      LazyColumnWithScrollBar(userScrollEnabled = !switchingProfile.value) {
        item {
          AppBarTitle(stringResource(MR.strings.select_chat_profile), hostDevice(chatModel.remoteHostId), bottomPadding = DEFAULT_PADDING)
        }

        itemsIndexed(filteredProfiles) { _, p ->
          ProfilePickerOption(
            title = p.chatViewName,
            disabled = switchingProfile.value || p.activeUser,
            image = { ProfileImage(size = 42.dp, image = p.image) },
            selected = p.activeUser,
            onSelected =  {
              switchingProfile.value = true
              withApi {
                try {
                  controller.changeActiveUser(
                    rhId = p.remoteHostId,
                    toUserId = p.userId,
                    viewPwd = if (p.hidden) searchTextOrPassword.value else null
                  )

                  if (chatModel.currentUser.value?.userId != p.userId) {
                    AlertManager.shared.showAlertMsg(
                      generalGetString(
                        MR.strings.switching_profile_error_title
                      ),
                      String.format(generalGetString(MR.strings.switching_profile_error_message), p.chatViewName)
                    )
                  }

                  close.invoke()
                } finally {
                  switchingProfile.value = false
                }
              }
            }
          )
        }
      }
    }
    if (progressByTimeout) {
      DefaultProgressView("")
    }
  }
}

@Composable
private fun UsersLayout(
  chatModel: ChatModel,
  onCurrentUserClick: () -> Unit,
  onUserClicked: (user: User) -> Unit,
  onShowAllProfilesClicked: () -> Unit
) {
  val currentUser = remember { chatModel.currentUser }.value
  val stopped = chatModel.chatRunning.value == false

  val users by remember {
    derivedStateOf {
      chatModel.users
        .filter { u -> !u.user.hidden && !u.user.activeUser }
    }
  }

  if (currentUser != null) {
    val mainColor = if (stopped) MaterialTheme.colors.secondary else Color.Unspecified
    val scrollState = rememberScrollState()

    SectionView(contentPadding = PaddingValues(bottom = DEFAULT_PADDING, end = DEFAULT_PADDING_HALF, top = if (appPlatform.isAndroid) DEFAULT_PADDING_HALF - 5.dp else 0.dp)) {
      Row {
        Column(modifier = Modifier.padding(start = DEFAULT_PADDING - 5.dp, top = 4.dp)) {
          IconButton(onClick = onCurrentUserClick, enabled = !stopped) {
              ProfileImage(
                image = currentUser.image,
                size = 54.dp * fontSizeSqrtMultiplier,
                color = MaterialTheme.colors.secondaryVariant.mixWith(MaterialTheme.colors.onBackground, 0.97f)
              )
          }
        }
        Box(
          contentAlignment = Alignment.TopStart
        ) {
          Row(
            horizontalArrangement = Arrangement.End,
            modifier = Modifier.padding(end = DEFAULT_PADDING_HALF).fillMaxWidth().horizontalScroll(scrollState)
          ) {
            Spacer(Modifier.width((93.dp + 8.dp) * fontSizeSqrtMultiplier))
            users.forEach { u ->
              IconButton(
                onClick = { onUserClicked(u.user) },
                enabled = !stopped
              ) {
                Box {
                  ProfileImage(size = 37.dp * fontSizeSqrtMultiplier, image = u.user.image)

                  if (u.unreadCount > 0) {
                    unreadBadge()
                  }
                }
              }
              Spacer(Modifier.width(8.dp))
            }
            Spacer(Modifier.width(37.dp * fontSizeSqrtMultiplier))
          }
          Box(
            modifier = Modifier
              .size(width = 37.dp, height = 37.dp)
              .background(
                brush = Brush.horizontalGradient(
                  colors = listOf(
                    MaterialTheme.colors.surface,
                    Color.Transparent
                  )
                )
              )
          )
          Box(modifier = Modifier.fillMaxWidth(), contentAlignment = Alignment.CenterEnd) {
            Box(
              modifier = Modifier.background(MaterialTheme.colors.surface).size(width = 30.dp, height = 37.dp)
            )
            Box(
              modifier = Modifier
                .padding(end = 27.dp)
                .size(37.dp)
                .background(
                  brush = Brush.horizontalGradient(
                    colors = listOf(
                      Color.Transparent,
                      MaterialTheme.colors.surface
                    )
                  )
                )
            )
            IconButton(
              onClick = onShowAllProfilesClicked,
              enabled = !stopped,
            ) {
              Box(
                contentAlignment = Alignment.Center,
                modifier = Modifier
                  .size(31.dp * fontSizeSqrtMultiplier)
                  .background(MaterialTheme.colors.surface, CircleShape)
                  .border(border = BorderStroke(2.dp, MaterialTheme.colors.secondary), shape = CircleShape),
                ) {
                Icon(
                  painterResource(MR.images.ic_chevron_right),
                  stringResource(MR.strings.your_chat_profiles),
                  tint = MaterialTheme.colors.secondary,
                )
              }
            }
          }
        }
      }
      Text(
        currentUser.displayName,
        style = MaterialTheme.typography.h3,
        fontWeight = FontWeight.Bold,
        color = mainColor,
        maxLines = 1,
        overflow = TextOverflow.Ellipsis,
        modifier = Modifier.padding(top = 12.dp, start = DEFAULT_PADDING)
      )
    }
  }
}

@Composable
private fun UserPickerUserSectionLayout (
  chatModel: ChatModel,
  userPickerState: MutableStateFlow<AnimatedViewState>,
  showCustomModal: (@Composable ModalData.(ChatModel, () -> Unit) -> Unit) -> (() -> Unit),
  showModalWithSearch: (@Composable (ChatModel, MutableState<String>) -> Unit) -> Unit,
  withAuth: (title: String, desc: String, block: () -> Unit) -> Unit,
  ) {
  val stopped = chatModel.chatRunning.value == false
  val profileHidden = rememberSaveable { mutableStateOf(false) }

  UsersLayout(
    chatModel = chatModel,
    onCurrentUserClick = showCustomModal { m, close -> UserProfileView(m, close) },
    onUserClicked = { user ->
      userPickerState.value = AnimatedViewState.HIDING
      if (!user.activeUser) {
        withBGApi {
          controller.showProgressIfNeeded {
            ModalManager.closeAllModalsEverywhere()
            chatModel.controller.changeActiveUser(user.remoteHostId, user.userId, null)
          }
        }
      }
    },
    onShowAllProfilesClicked = {
      withAuth(
        generalGetString(MR.strings.auth_open_chat_profiles),
        generalGetString(MR.strings.auth_log_in_using_credential)
      ) {
        showModalWithSearch { it, search -> UserProfilesView(it, search, profileHidden) }
      }
    }
  )

  SectionView {
    UserPickerOptionRow(
      painterResource(MR.images.ic_qr_code),
      if (chatModel.userAddress.value != null) generalGetString(MR.strings.your_public_contact_address) else generalGetString(MR.strings.create_public_contact_address),
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
    if (appPlatform.isAndroid) {
      UseFromDesktopPickerItem {
        showCustomModal { _, close ->
          ConnectDesktopView(close)
        }()
      }
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
    }
  }
}

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
        .sortedByDescending { it.user.activeUser }
    }
  }
  val remoteHosts by remember {
    derivedStateOf {
      chatModel.remoteHosts
        .sortedBy { it.hostDeviceName }
    }
  }
  val currentTheme by CurrentColors.collectAsState()
  val animatedFloat = remember { Animatable(if (newChat.isVisible()) 0f else 1f) }

  val resultingColor by remember {
    derivedStateOf {
        if (currentTheme.colors.isLight) currentTheme.colors.onSurface.copy(alpha = ScrimOpacity)else Color.Black.copy(0.64f)
    }
  }
  val animatedColor = remember {
    Animatable(
      if (newChat.isVisible()) Color.Transparent else resultingColor,
      Color.VectorConverter(resultingColor.colorSpace)
    )
  }

  LaunchedEffect(Unit) {
    snapshotFlow { currentTheme }
      .distinctUntilChanged()
      .collect { _ ->
        launch {
          userPickerState.collect {
            newChat = it
            val colors = CurrentColors.value.colors
            val toColor = if (colors.isLight) colors.onSurface.copy(alpha = ScrimOpacity) else Color.Black.copy(0.64f)

            animatedColor.animateTo(if (newChat.isVisible()) toColor else Color.Transparent, newChatSheetAnimSpec()) {
              if (newChat.isVisible()) {
                platform.androidSetDrawerStatusAndNavBarColor(
                  isLight = colors.isLight,
                  drawerShadingColor = animatedColor,
                  toolbarOnTop = !appPrefs.oneHandUI.get(),
                  navBarColor = colors.surface
                )
              } else if (newChat.isHiding()) {
                platform.androidSetDrawerStatusAndNavBarColor(
                  isLight = colors.isLight,
                  drawerShadingColor = animatedColor,
                  toolbarOnTop = !appPrefs.oneHandUI.get(),
                  navBarColor = (if (appPrefs.oneHandUI.get() && appPrefs.onboardingStage.get() == OnboardingStage.OnboardingComplete) {
                    colors.background.mixWith(CurrentColors.value.colors.onBackground, 0.97f)
                  } else {
                    colors.background
                  })
                )
              }
            }

            launch {
              animatedFloat.animateTo(if (newChat.isVisible()) 1f else 0f, newChatSheetAnimSpec())
              if (newChat.isHiding()) userPickerState.value = AnimatedViewState.GONE
            }
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

  Box(
    Modifier.drawBehind { drawRect(animatedColor.value) }
  ) {
    AnimatedVisibility(
      visible = newChat.isVisible(),
      enter = if (appPlatform.isAndroid) {
        slideInVertically(
          initialOffsetY = { it },
          animationSpec = userPickerAnimSpec()
        ) + fadeIn(animationSpec = userPickerAnimSpec())
      } else {
        fadeIn()
      },
      exit = if (appPlatform.isAndroid) {
        slideOutVertically(
          targetOffsetY = { it },
          animationSpec = userPickerAnimSpec()
        ) + fadeOut(animationSpec = userPickerAnimSpec())
      } else {
        fadeOut()
      }
    ) {
      Box(
        Modifier
          .fillMaxSize()

          .clickable(interactionSource = remember { MutableInteractionSource() }, indication = null, onClick = { userPickerState.value = AnimatedViewState.HIDING }),
        contentAlignment = if (appPlatform.isAndroid) Alignment.BottomStart else Alignment.TopStart
      ) {
        Column(
          Modifier
            .height(IntrinsicSize.Min)
            .then(if (appPlatform.isDesktop) Modifier.widthIn(max = 450.dp) else Modifier)
            .shadow(8.dp, clip = true)
            .fillMaxWidth()
            .background(MaterialTheme.colors.surface)
        ) {
          val currentRemoteHost = remember { chatModel.currentRemoteHost }.value
          Column(
            Modifier
              .padding(vertical = DEFAULT_PADDING_HALF)
          ) {
            if (remoteHosts.isNotEmpty()) {
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

            UserPickerUserSectionLayout(
              chatModel = chatModel,
              userPickerState = userPickerState,
              showCustomModal = { modalView ->
                {
                  if (appPlatform.isDesktop) {
                    userPickerState.value = AnimatedViewState.HIDING
                  }
                  ModalManager.start.showCustomModal { close -> modalView(chatModel, close) }
                }
              },
              withAuth = ::doWithAuth,
              showModalWithSearch = { modalView ->
                if (appPlatform.isDesktop) {
                  userPickerState.value = AnimatedViewState.HIDING
                }
                ModalManager.start.showCustomModal { close ->
                  val search = rememberSaveable { mutableStateOf("") }
                  ModalView(
                    { close() },
                    endButtons = {
                      SearchTextField(Modifier.fillMaxWidth(), placeholder = stringResource(MR.strings.search_verb), alwaysVisible = true) { search.value = it }
                    },
                    content = { modalView(chatModel, search) })
                }
              },
            )

            Divider(Modifier.padding(DEFAULT_PADDING))
            val text = generalGetString(MR.strings.settings_section_title_settings).lowercase().capitalize(Locale.current)
            SectionItemView(
              click = {
                ModalManager.start.showModalCloseable { close ->
                  SettingsView(chatModel, setPerformLA, close)
                }
              },
              padding = PaddingValues(start = DEFAULT_PADDING, end = DEFAULT_PADDING_HALF)
            ) {
              Icon(painterResource(MR.images.ic_settings), text, tint = MaterialTheme.colors.secondary)
              TextIconSpaced()
              Text(text, color = Color.Unspecified)
              Spacer(Modifier.weight(1f))
              ColorModeSwitcher()
            }

            Spacer(Modifier.height(DEFAULT_PADDING_HALF))
          }
        }
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
      .sizeIn(minHeight = DEFAULT_MIN_SECTION_ITEM_HEIGHT)
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
private fun DevicePickerRow(
  localDeviceActive: Boolean,
  remoteHosts: List<RemoteHostInfo>,
  onLocalDeviceClick: () -> Unit,
  onRemoteHostClick: (rh: RemoteHostInfo, connecting: MutableState<Boolean>) -> Unit,
  onRemoteHostActionButtonClick: (rh: RemoteHostInfo) -> Unit,
) {
  Row(
    Modifier
      .fillMaxWidth()
      .sizeIn(minHeight = DEFAULT_MIN_SECTION_ITEM_HEIGHT)
      .padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING),
    horizontalArrangement = Arrangement.spacedBy(12.dp),
    verticalAlignment = Alignment.CenterVertically
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
      .background(if (active) MaterialTheme.colors.secondaryVariant else MaterialTheme.colors.surface)
      .clickable(
        onClick = if (active) {{}} else onClick,
        interactionSource = remember { MutableInteractionSource() },
        indication = if (!active) LocalIndication.current else null
      ),
    verticalAlignment = Alignment.CenterVertically
  ) {
    Row(
      Modifier.padding(horizontal = 6.dp, vertical = 4.dp)
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
private fun UseFromDesktopPickerItem(onClick: () -> Unit) {
  val text = generalGetString(MR.strings.settings_section_title_use_from_desktop).lowercase().capitalize(Locale.current)

  UserPickerOptionRow(
    painterResource(MR.images.ic_desktop),
    text,
    onClick
  )
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
