package chat.simplex.common

import androidx.compose.animation.AnimatedContent
import androidx.compose.animation.SizeTransform
import androidx.compose.animation.core.Animatable
import androidx.compose.foundation.*
import androidx.compose.foundation.gestures.detectDragGestures
import androidx.compose.foundation.gestures.detectTapGestures
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.*
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.*
import androidx.compose.ui.input.key.*
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.platform.LocalLayoutDirection
import androidx.compose.ui.semantics.ProgressBarRangeInfo
import androidx.compose.ui.semantics.contentDescription
import androidx.compose.ui.semantics.progressBarRangeInfo
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.semantics.setProgress
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.LayoutDirection
import androidx.compose.ui.unit.dp
import androidx.compose.ui.zIndex
import chat.simplex.common.views.usersettings.SetDeliveryReceiptsView
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.CreateFirstProfile
import chat.simplex.common.views.helpers.SimpleButton
import chat.simplex.common.views.SplashView
import chat.simplex.common.views.call.*
import chat.simplex.common.views.chat.ChatView
import chat.simplex.common.views.chatlist.*
import chat.simplex.common.views.database.DatabaseErrorView
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.helpers.ModalManager.Companion.fromEndToStartTransition
import chat.simplex.common.views.helpers.ModalManager.Companion.fromStartToEndTransition
import chat.simplex.common.views.localauth.VerticalDivider
import chat.simplex.common.views.newchat.*
import chat.simplex.common.views.onboarding.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*

// Spec: spec/client/navigation.md#AppScreen
@Composable
fun AppScreen() {
  AppBarHandler.appBarMaxHeightPx = with(LocalDensity.current) { AppBarHeight.roundToPx() }
  SimpleXTheme {
    Surface(color = if (macOSWindowVibrancyAvailable) Color.Transparent else MaterialTheme.colors.background, contentColor = LocalContentColor.current) {
      // This padding applies to landscape view only taking care of navigation bar and holes in screen in status bar area
      // (because nav bar and holes located on vertical sides of screen in landscape view)
      val direction = LocalLayoutDirection.current
      val safePadding = WindowInsets.safeDrawing.asPaddingValues()
      val cutout = WindowInsets.displayCutout.asPaddingValues()
      val cutoutStart = cutout.calculateStartPadding(direction)
      val cutoutEnd = cutout.calculateEndPadding(direction)
      val cutoutMax = maxOf(cutoutStart, cutoutEnd)
      val paddingStartUntouched = safePadding.calculateStartPadding(direction)
      val paddingStart = paddingStartUntouched - cutoutStart
      val paddingEndUntouched = safePadding.calculateEndPadding(direction)
      val paddingEnd = paddingEndUntouched - cutoutEnd
      // Such a strange layout is needed because the main content should be covered by solid color in order to hide overflow
      // of some elements that may have negative offset (so, can't use Row {}).
      // To check: go to developer settings of Android, choose Display cutout -> Punch hole, and rotate the phone to landscape, open any chat
      Box {
        val fullscreenGallery = remember { chatModel.fullscreenGalleryVisible }
        Box(Modifier.padding(start = paddingStart + cutoutMax, end = paddingEnd + cutoutMax).consumeWindowInsets(PaddingValues(start = paddingStartUntouched, end = paddingEndUntouched))) {
          Box(Modifier.drawBehind {
            if (fullscreenGallery.value) {
              drawRect(Color.Black,  topLeft = Offset(-(paddingStart + cutoutMax).toPx(), 0f), Size(size.width + (paddingStart + cutoutMax).toPx() + (paddingEnd + cutoutMax).toPx(), size.height))
            }
          }) {
            MainScreen()
          }
        }
      }
    }
  }
}

// Spec: spec/client/navigation.md#MainScreen
@Composable
fun MainScreen() {
  val chatModel = ChatModel
  var showChatDatabaseError by rememberSaveable {
    mutableStateOf(chatModel.chatDbStatus.value != DBMigrationResult.OK && chatModel.chatDbStatus.value != null)
  }
  LaunchedEffect(chatModel.chatDbStatus.value) {
    showChatDatabaseError = chatModel.chatDbStatus.value != DBMigrationResult.OK && chatModel.chatDbStatus.value != null
  }
  var showAdvertiseLAAlert by remember { mutableStateOf(false) }
  LaunchedEffect(showAdvertiseLAAlert) {
    if (
      !chatModel.controller.appPrefs.laNoticeShown.get()
      && !appPrefs.performLA.get()
      && showAdvertiseLAAlert
      && chatModel.controller.appPrefs.onboardingStage.get() == OnboardingStage.OnboardingComplete
      && chatModel.chats.size > 3
      && chatModel.activeCallInvitation.value == null
    ) {
      AppLock.showLANotice(ChatModel.controller.appPrefs.laNoticeShown) }
  }
  LaunchedEffect(chatModel.showAdvertiseLAUnavailableAlert.value) {
    if (chatModel.showAdvertiseLAUnavailableAlert.value) {
      laUnavailableInstructionAlert()
    }
  }
  platform.desktopShowAppUpdateNotice()
  LaunchedEffect(chatModel.clearOverlays.value) {
    if (chatModel.clearOverlays.value) {
      ModalManager.closeAllModalsEverywhere()
      chatModel.clearOverlays.value = false
    }
  }

  @Composable
  fun AuthView() {
    Surface(color = MaterialTheme.colors.background.copy(1f), contentColor = LocalContentColor.current) {
      Box(
        Modifier.fillMaxSize(),
        contentAlignment = Alignment.Center
      ) {
        SimpleButton(
          stringResource(MR.strings.auth_unlock),
          icon = painterResource(MR.images.ic_lock),
          click = {
            AppLock.laFailed.value = false
            AppLock.runAuthenticate()
          }
        )
      }
    }
  }

  val unauthorized = remember { derivedStateOf { AppLock.userAuthorized.value != true } }
  val onboarding by remember { chatModel.controller.appPrefs.onboardingStage.state }
  val localUserCreated = chatModel.localUserCreated.value
  var showInitializationView by remember { mutableStateOf(false) }
  val desktopChatVisible = appPlatform.isDesktop &&
      onboarding == OnboardingStage.OnboardingComplete &&
      chatModel.chatDbStatus.value == DBMigrationResult.OK &&
      remember { chatModel.chatDbEncrypted }.value != null &&
      localUserCreated != null
  val windowBackground = if (macOSWindowVibrancyAvailable && desktopChatVisible) {
    Color.Transparent
  } else {
    MaterialTheme.colors.background
  }

  Box(Modifier.fillMaxSize().background(windowBackground)) {
    when {
      onboarding == OnboardingStage.Step1_SimpleXInfo && chatModel.migrationState.value != null -> {
        // In migration process. Nothing should interrupt it, that's why it's the first branch in when()
        if (appPlatform.isDesktop) DesktopOnboarding(onboarding, chatModel)
        else SimpleXInfo(chatModel, onboarding = true)
      }
      chatModel.dbMigrationInProgress.value -> DefaultProgressView(stringResource(MR.strings.database_migration_in_progress))
      chatModel.chatDbStatus.value == null && showInitializationView -> DefaultProgressView(stringResource(MR.strings.opening_database))
      showChatDatabaseError -> {
        // Prevent showing keyboard on Android when: passcode enabled and database password not saved
        if (!unauthorized.value && chatModel.chatDbStatus.value != null) {
          DatabaseErrorView(chatModel.chatDbStatus, chatModel.controller.appPrefs)
        }
      }
      remember { chatModel.chatDbEncrypted }.value == null || localUserCreated == null -> SplashView()
      onboarding == OnboardingStage.OnboardingComplete -> {
        Box {
          showAdvertiseLAAlert = true
          val userPickerState by rememberSaveable(stateSaver = AnimatedViewState.saver()) { mutableStateOf(MutableStateFlow(if (chatModel.desktopNoUserNoRemote()) AnimatedViewState.VISIBLE else AnimatedViewState.GONE)) }
          KeyChangeEffect(chatModel.desktopNoUserNoRemote) {
            if (chatModel.desktopNoUserNoRemote() && !ModalManager.start.hasModalsOpen()) {
              userPickerState.value = AnimatedViewState.VISIBLE
            }
          }
          if (appPlatform.isAndroid) {
            AndroidWrapInCallLayout {
              AndroidScreen(userPickerState)
            }
          } else {
            DesktopScreen(userPickerState)
          }
        }
      }
      else -> {
        if (appPlatform.isDesktop) {
          DesktopOnboarding(onboarding, chatModel)
        } else {
          AnimatedContent(targetState = onboarding,
            transitionSpec = {
              if (targetState > initialState) {
                fromEndToStartTransition()
              } else {
                fromStartToEndTransition()
              }.using(SizeTransform(clip = false))
            }
          ) { state ->
            when (state) {
              OnboardingStage.OnboardingComplete -> {}
              OnboardingStage.Step1_SimpleXInfo -> SimpleXInfo(chatModel, onboarding = true)
              OnboardingStage.Step2_CreateProfile -> CreateFirstProfile(chatModel) {}
              OnboardingStage.LinkAMobile -> LinkAMobile()
              OnboardingStage.Step2_5_SetupDatabasePassphrase -> SetupDatabasePassphrase(chatModel)
              OnboardingStage.Step3_ChooseServerOperators,
              OnboardingStage.Step3_CreateSimpleXAddress,
              OnboardingStage.Step4_SetNotificationsMode -> YourNetworkView(chatModel)
              OnboardingStage.Step4_NetworkCommitments -> OnboardingConditionsView(chatModel)
            }
          }
        }
      }
    }
    if (appPlatform.isAndroid) {
      AndroidWrapInCallLayout {
        ModalManager.fullscreen.showInView()
      }
      SwitchingUsersView()
    }

    if (unauthorized.value && !(chatModel.activeCallViewIsVisible.value && chatModel.showCallView.value)) {
      LaunchedEffect(Unit) {
        // With these constrains when user presses back button while on ChatList, activity destroys and shows auth request
        // while the screen moves to a launcher. Detect it and prevent showing the auth
        if (!(androidIsFinishingMainActivity() && chatModel.controller.appPrefs.laMode.get() == LAMode.SYSTEM)) {
          AppLock.runAuthenticate()
        }
      }
      if (chatModel.controller.appPrefs.performLA.get() && AppLock.laFailed.value) {
        AuthView()
      } else {
        SplashView(true)
        ModalManager.fullscreen.showPasscodeInView()
      }
    } else {
      if (chatModel.showCallView.value) {
        if (appPlatform.isAndroid) {
          LaunchedEffect(Unit) {
            // This if prevents running the activity in the following condition:
            // - the activity already started before and was destroyed by collapsing active call (start audio call, press back button, go to a launcher)
            if (!chatModel.activeCallViewIsCollapsed.value) {
              platform.androidStartCallActivity(false)
            }
          }
        } else {
          ActiveCallView()
        }
      }
      ModalManager.fullscreen.showOneTimePasscodeInView()
      AlertManager.privacySensitive.showInView()
      if (onboarding == OnboardingStage.OnboardingComplete) {
        LaunchedEffect(chatModel.chatRunning.value, chatModel.currentUser.value, chatModel.appOpenUrl.value) {
          val (rhId, url) = chatModel.appOpenUrl.value ?: (null to null)
          if (url != null && chatModel.chatRunning.value == true) {
            chatModel.appOpenUrl.value = null
            connectIfOpenedViaUri(rhId, url, chatModel)
          }
        }
      }
    }
    val invitation = chatModel.activeCallInvitation.value
    if (invitation != null) IncomingCallAlertView(invitation, chatModel)
    AlertManager.shared.showInView()

    LaunchedEffect(Unit) {
      delay(1000)
      if (chatModel.chatDbStatus.value == null) {
        showInitializationView = true
      }
    }
  }

  DisposableEffectOnRotate {
    // When using lock delay = 0 and screen rotates, the app will be locked which is not useful.
    // Let's prolong the unlocked period to 3 sec for screen rotation to take place
    if (chatModel.controller.appPrefs.laLockDelay.get() == 0) {
      AppLock.enteredBackground.value = AppLock.elapsedRealtime() + 3000
    }
  }
}

@Composable
private fun DesktopOnboarding(onboarding: OnboardingStage, chatModel: ChatModel) {
  if (onboarding == OnboardingStage.LinkAMobile) {
    LinkAMobile()
    ModalManager.fullscreen.showInView()
  } else {
    DesktopOnboardingShell(onboarding) {
      when (onboarding) {
        OnboardingStage.Step1_SimpleXInfo -> SimpleXInfo(chatModel, onboarding = true)
        OnboardingStage.Step2_CreateProfile -> CreateFirstProfile(chatModel) {}
        OnboardingStage.Step2_5_SetupDatabasePassphrase -> SetupDatabasePassphrase(chatModel)
        OnboardingStage.Step3_ChooseServerOperators,
        OnboardingStage.Step3_CreateSimpleXAddress,
        OnboardingStage.Step4_SetNotificationsMode -> YourNetworkView(chatModel)
        OnboardingStage.Step4_NetworkCommitments -> OnboardingConditionsView(chatModel)
        else -> {}
      }
    }
  }
}

val ANDROID_CALL_TOP_PADDING = 40.dp

@Composable
fun AndroidWrapInCallLayout(content: @Composable () -> Unit) {
  val call = remember { chatModel.activeCall}.value
  val showCallArea = call != null && call.callState != CallState.WaitCapabilities && call.callState != CallState.InvitationAccepted
  Box {
    Box(Modifier.padding(top = if (showCallArea) ANDROID_CALL_TOP_PADDING else 0.dp)) {
      content()
    }
    if (call != null && showCallArea) {
      ActiveCallInteractiveArea(call)
    }
  }
}

// Spec: spec/client/navigation.md#AndroidScreen
@Composable
fun AndroidScreen(userPickerState: MutableStateFlow<AnimatedViewState>) {
  BoxWithConstraints {
    val currentChatId = remember { mutableStateOf(chatModel.chatId.value) }
    val offset = remember { Animatable(if (chatModel.chatId.value == null) 0f else maxWidth.value) }
    val cutout = WindowInsets.displayCutout.only(WindowInsetsSides.Horizontal).asPaddingValues()
    val direction = LocalLayoutDirection.current
    val hasCutout = cutout.calculateStartPadding(direction) + cutout.calculateEndPadding(direction) > 0.dp
    Box(
      Modifier
        // clipping only for devices with cutout currently visible on sides. It prevents showing chat list with open chat view
        // In order cases it's not needed to use clip
        .then(if (hasCutout) Modifier.clip(RectangleShape) else Modifier)
        .graphicsLayer {
          // minOf thing is needed for devices with holes in screen while the user on ChatView rotates his phone from portrait to landscape
          // because in this case (at least in emulator) maxWidth changes in two steps: big first, smaller on next frame.
          // But offset is remembered already, so this is a better way than dropping a value of offset
          translationX = -minOf(offset.value.dp, maxWidth).toPx()
        }
    ) {
      StartPartOfScreen(userPickerState)
    }
    val scope = rememberCoroutineScope()
    val onComposed: suspend (chatId: String?) -> Unit = { chatId ->
      // coroutine, scope and join() because:
      // - it should be run from coroutine to wait until this function finishes
      // - without using scope.launch it throws CancellationException when changing user
      // - join allows to wait until completion
      scope.launch {
        offset.animateTo(
          if (chatId == null) 0f else maxWidth.value,
          chatListAnimationSpec()
        )
      }.join()
    }
    LaunchedEffect(Unit) {
      launch {
        snapshotFlow { chatModel.chatId.value }
          .distinctUntilChanged()
          .collect {
            if (it == null) onComposed(null)
            currentChatId.value = it
          }
      }
    }
    Box(Modifier
      .then(if (hasCutout) Modifier.clip(RectangleShape) else Modifier)
      .graphicsLayer { translationX = maxWidth.toPx() - minOf(offset.value.dp, maxWidth).toPx() }
    ) Box2@{
      currentChatId.value?.let {
        ChatView(chatsCtx = chatModel.chatsContext, currentChatId, onComposed = onComposed)
      }
    }
  }
}

@Composable
fun StartPartOfScreen(userPickerState: MutableStateFlow<AnimatedViewState>) {
  if (chatModel.setDeliveryReceipts.value) {
    CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
      SetDeliveryReceiptsView(chatModel)
    }
  } else {
    val stopped = chatModel.chatRunning.value == false
    if (chatModel.sharedContent.value == null) {
      CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
        ChatListView(chatModel, userPickerState, AppLock::setPerformLA, stopped)
      }
    } else {
      // LALAL initial load of view doesn't show blur. Focusing text field shows it
      CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler(keyboardCoversBar = false)) {
        ShareListView(chatModel, stopped)
      }
    }
  }
}

@Composable
fun CenterPartOfScreen() {
  val currentChatId = remember { ChatModel.chatId }
  LaunchedEffect(Unit) {
    snapshotFlow { currentChatId.value }
      .distinctUntilChanged()
      .collect {
        if (it != null) {
          ModalManager.center.closeModals()
        }
      }
  }
  when (currentChatId.value) {
    null -> {
      if (shouldShowOnboarding()) {
        ConnectOnboardingView()
      } else if (!rememberUpdatedState(ModalManager.center.hasModalsOpen()).value) {
        Box(
          Modifier
            .fillMaxSize()
            .background(MaterialTheme.colors.background),
          contentAlignment = Alignment.Center
        ) {
          Column(horizontalAlignment = Alignment.CenterHorizontally) {
            Surface(
              modifier = Modifier.size(64.dp),
              shape = RoundedCornerShape(18.dp),
              color = MaterialTheme.colors.onBackground.copy(alpha = 0.06f),
              elevation = 0.dp,
            ) {
              Box(contentAlignment = Alignment.Center) {
                Icon(
                  painterResource(MR.images.ic_chat),
                  contentDescription = null,
                  modifier = Modifier.size(30.dp),
                  tint = MaterialTheme.colors.secondary.copy(alpha = 0.72f),
                )
              }
            }
            Spacer(Modifier.height(18.dp))
            Text(
              stringResource(if (chatModel.desktopNoUserNoRemote) MR.strings.no_connected_mobile else MR.strings.no_selected_chat),
              style = MaterialTheme.typography.h3,
              fontWeight = FontWeight.Medium,
              color = MaterialTheme.colors.secondary,
            )
          }
        }
      } else {
        ModalManager.center.showInView()
      }
    }
    else -> ChatView(chatsCtx = chatModel.chatsContext, currentChatId) {}
  }
}

@Composable
fun EndPartOfScreen() {
  ModalManager.end.showInView()
}

// Spec: spec/client/navigation.md#DesktopScreen
@Composable
fun DesktopScreen(userPickerState: MutableStateFlow<AnimatedViewState>) {
  BoxWithConstraints(Modifier.fillMaxSize()) {
    val density = LocalDensity.current
    val direction = LocalLayoutDirection.current
    val savedSidebarWidth = DesktopLayoutState.sidebarWidth.floatValue.dp
    val maxSidebarWidth = minOf(
      DesktopLayoutState.MAX_SIDEBAR_WIDTH.dp,
      (maxWidth - 520.dp).coerceAtLeast(DesktopLayoutState.MIN_SIDEBAR_WIDTH.dp)
    )
    val sidebarWidth = savedSidebarWidth.coerceIn(DesktopLayoutState.MIN_SIDEBAR_WIDTH.dp, maxSidebarWidth)
    val autoCollapsed = maxWidth < DesktopLayoutState.MIN_SIDEBAR_WIDTH.dp + 520.dp
    val sidebarVisible = !DesktopLayoutState.sidebarCollapsed.value && !autoCollapsed
    val visibleSidebarWidth = if (sidebarVisible) sidebarWidth else 0.dp

    if (sidebarVisible) {
      Box(Modifier.width(sidebarWidth).fillMaxHeight()) {
        Box(Modifier.fillMaxSize().padding(top = if (macOSWindowVibrancyAvailable) 28.dp else 0.dp)) {
          StartPartOfScreen(userPickerState)
          tryOrShowError("UserPicker", error = {}) {
            UserPicker(chatModel, userPickerState, setPerformLA = AppLock::setPerformLA)
          }
        }
      }
      Box(Modifier.widthIn(max = sidebarWidth).fillMaxHeight()) {
        ModalManager.start.showInView()
        SwitchingUsersView()
      }
    }

    Row(
      Modifier
        .padding(start = visibleSidebarWidth)
        .fillMaxSize()
        .clipToBounds()
        .background(MaterialTheme.colors.background)
    ) {
      Box(Modifier.widthIn(min = 520.dp).weight(1f)) {
        CenterPartOfScreen()
      }
      if (ModalManager.end.hasModalsOpen()) {
        VerticalDivider()
      }
      Box(Modifier.widthIn(max = DEFAULT_END_MODAL_WIDTH * fontSizeSqrtMultiplier).clipToBounds()) {
        EndPartOfScreen()
      }
    }

    if (sidebarVisible) {
      Box(
        Modifier
          .offset(x = sidebarWidth - 0.5.dp)
          .width(1.dp)
          .fillMaxHeight()
          .zIndex(3f)
          .background(MaterialTheme.colors.onBackground.copy(alpha = 0.12f))
      )
      val updateSidebarWidth: (Float) -> Boolean = { requested ->
        val clamped = requested.coerceIn(DesktopLayoutState.MIN_SIDEBAR_WIDTH, maxSidebarWidth.value)
        DesktopLayoutState.setSidebarWidth(clamped)
        true
      }
      Box(
        Modifier
          .offset(x = sidebarWidth - 3.dp)
          .width(6.dp)
          .fillMaxHeight()
          .zIndex(4f)
          .desktopPointerHoverIconResize()
          .pointerInput(maxSidebarWidth, direction) {
            detectDragGestures { change, dragAmount ->
              change.consume()
              val dragDp = with(density) { dragAmount.x.toDp().value }
              val signedDrag = if (direction == LayoutDirection.Ltr) dragDp else -dragDp
              updateSidebarWidth(DesktopLayoutState.sidebarWidth.floatValue + signedDrag)
            }
          }
          .pointerInput(Unit) {
            detectTapGestures(onDoubleTap = { DesktopLayoutState.resetSidebarWidth() })
          }
          .focusable()
          .onPreviewKeyEvent { event ->
            if (event.type != KeyEventType.KeyDown) return@onPreviewKeyEvent false
            val delta = when (event.key) {
              Key.DirectionLeft -> if (direction == LayoutDirection.Ltr) -8f else 8f
              Key.DirectionRight -> if (direction == LayoutDirection.Ltr) 8f else -8f
              Key.Enter, Key.NumPadEnter -> {
                DesktopLayoutState.resetSidebarWidth()
                return@onPreviewKeyEvent true
              }
              else -> return@onPreviewKeyEvent false
            }
            updateSidebarWidth(DesktopLayoutState.sidebarWidth.floatValue + delta)
          }
          .semantics {
            contentDescription = "Resize conversations sidebar"
            progressBarRangeInfo = ProgressBarRangeInfo(
              DesktopLayoutState.sidebarWidth.floatValue,
              DesktopLayoutState.MIN_SIDEBAR_WIDTH..maxSidebarWidth.value
            )
            setProgress { updateSidebarWidth(it) }
          }
      )
    }

    if (userPickerState.collectAsState().value.isVisible() || (ModalManager.start.hasModalsOpen && !ModalManager.center.hasModalsOpen)) {
      Box(
        Modifier
          .fillMaxSize()
          .padding(start = visibleSidebarWidth)
          .clickable(interactionSource = remember { MutableInteractionSource() }, indication = null, onClick = {
            if (chatModel.centerPanelBackgroundClickHandler == null || chatModel.centerPanelBackgroundClickHandler?.invoke() == false) {
              ModalManager.start.closeModals()
              userPickerState.value = AnimatedViewState.HIDING
            }
          })
      )
    }
    ModalManager.fullscreen.showInView()
  }
}

@Composable
private fun SwitchingUsersView() {
  if (remember { chatModel.switchingUsersAndHosts }.value) {
    Box(
      Modifier.fillMaxSize().clickable(enabled = false, onClick = {}),
      contentAlignment = Alignment.Center
    ) {
      ProgressIndicator()
    }
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
