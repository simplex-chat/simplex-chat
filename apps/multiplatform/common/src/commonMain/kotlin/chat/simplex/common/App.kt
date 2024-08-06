package chat.simplex.common

import androidx.compose.animation.core.Animatable
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clipToBounds
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.unit.dp
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
import chat.simplex.common.views.localauth.VerticalDivider
import chat.simplex.common.views.onboarding.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*

data class SettingsViewState(
  val userPickerState: MutableStateFlow<AnimatedViewState>,
  val scaffoldState: ScaffoldState
)

@Composable
fun AppScreen() {
  SimpleXTheme {
    ProvideWindowInsets(windowInsetsAnimationsEnabled = true) {
      Surface(color = MaterialTheme.colors.background, contentColor = LocalContentColor.current) {
        MainScreen()
      }
    }
  }
}

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
      && showAdvertiseLAAlert
      && chatModel.controller.appPrefs.onboardingStage.get() == OnboardingStage.OnboardingComplete
      && chatModel.chats.size > 2
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
    Surface(color = MaterialTheme.colors.background, contentColor = LocalContentColor.current) {
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

  Box {
    val unauthorized = remember { derivedStateOf { AppLock.userAuthorized.value != true } }
    val onboarding by remember { chatModel.controller.appPrefs.onboardingStage.state }
    val localUserCreated = chatModel.localUserCreated.value
    var showInitializationView by remember { mutableStateOf(false) }
    when {
      onboarding == OnboardingStage.Step1_SimpleXInfo && chatModel.migrationState.value != null -> {
        // In migration process. Nothing should interrupt it, that's why it's the first branch in when()
        SimpleXInfo(chatModel, onboarding = true)
        if (appPlatform.isDesktop) {
          ModalManager.fullscreen.showInView()
        }
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
          val scaffoldState = rememberScaffoldState()
          val settingsState = remember { SettingsViewState(userPickerState, scaffoldState) }
          SetupClipboardListener()
          if (appPlatform.isAndroid) {
            AndroidScreen(settingsState)
          } else {
            DesktopScreen(settingsState)
          }
        }
      }
      onboarding == OnboardingStage.Step1_SimpleXInfo -> {
        SimpleXInfo(chatModel, onboarding = true)
        if (appPlatform.isDesktop) {
          ModalManager.fullscreen.showInView()
        }
      }
      onboarding == OnboardingStage.Step2_CreateProfile -> CreateFirstProfile(chatModel) {}
      onboarding == OnboardingStage.LinkAMobile -> LinkAMobile()
      onboarding == OnboardingStage.Step2_5_SetupDatabasePassphrase -> SetupDatabasePassphrase(chatModel)
      onboarding == OnboardingStage.Step3_CreateSimpleXAddress -> CreateSimpleXAddress(chatModel, null)
      onboarding == OnboardingStage.Step4_SetNotificationsMode -> SetNotificationsMode(chatModel)
    }
    if (appPlatform.isAndroid) {
      ModalManager.fullscreen.showInView()
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
        SplashView()
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
      } else {
        // It's needed for privacy settings toggle, so it can be shown even if the app is passcode unlocked
        ModalManager.fullscreen.showPasscodeInView()
      }
      AlertManager.privacySensitive.showInView()
      if (onboarding == OnboardingStage.OnboardingComplete) {
        LaunchedEffect(chatModel.currentUser.value, chatModel.appOpenUrl.value) {
          val (rhId, url) = chatModel.appOpenUrl.value ?: (null to null)
          if (url != null) {
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

val ANDROID_CALL_TOP_PADDING = 40.dp

@Composable
fun AndroidScreen(settingsState: SettingsViewState) {
  BoxWithConstraints {
    val call = remember { chatModel.activeCall} .value
    val showCallArea = call != null && call.callState != CallState.WaitCapabilities && call.callState != CallState.InvitationAccepted
    val currentChatId = remember { mutableStateOf(chatModel.chatId.value) }
    val offset = remember { Animatable(if (chatModel.chatId.value == null) 0f else maxWidth.value) }
    Box(
      Modifier
        .graphicsLayer {
          translationX = -offset.value.dp.toPx()
        }
        .padding(top = if (showCallArea) ANDROID_CALL_TOP_PADDING else 0.dp)
    ) {
      StartPartOfScreen(settingsState)
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
            if (it == null) {
              platform.androidSetStatusAndNavBarColors(CurrentColors.value.colors.isLight, CurrentColors.value.colors.background, !appPrefs.oneHandUI.get(), appPrefs.oneHandUI.get())
              onComposed(null)
            }
            currentChatId.value = it
          }
      }
    }
    LaunchedEffect(Unit) {
      snapshotFlow { ModalManager.center.modalCount.value > 0 }
        .filter { chatModel.chatId.value == null }
        .collect { modalBackground ->
          if (modalBackground && !chatModel.newChatSheetVisible.value) {
            platform.androidSetStatusAndNavBarColors(CurrentColors.value.colors.isLight, CurrentColors.value.colors.background, false, false)
          } else {
            platform.androidSetStatusAndNavBarColors(CurrentColors.value.colors.isLight, CurrentColors.value.colors.background, !appPrefs.oneHandUI.get(), appPrefs.oneHandUI.get())
          }
        }
    }
    Box(Modifier
      .graphicsLayer { translationX = maxWidth.toPx() - offset.value.dp.toPx() }
      .padding(top = if (showCallArea) ANDROID_CALL_TOP_PADDING else 0.dp)
    ) Box2@{
      currentChatId.value?.let {
        ChatView(currentChatId, onComposed)
      }
    }
    if (call != null && showCallArea) {
      ActiveCallInteractiveArea(call)
    }
  }
}

@Composable
fun StartPartOfScreen(settingsState: SettingsViewState) {
  if (chatModel.setDeliveryReceipts.value) {
    SetDeliveryReceiptsView(chatModel)
  } else {
    val stopped = chatModel.chatRunning.value == false
    if (chatModel.sharedContent.value == null)
      ChatListView(chatModel, settingsState, AppLock::setPerformLA, stopped)
    else
      ShareListView(chatModel, settingsState, stopped)
  }
}

@Composable
fun CenterPartOfScreen() {
  val currentChatId = remember { ChatModel.chatId }
  LaunchedEffect(Unit) {
    snapshotFlow { currentChatId }
      .distinctUntilChanged()
      .collect {
        if (it != null) {
          ModalManager.center.closeModals()
        }
      }
  }
  when (currentChatId.value) {
    null -> {
      if (!rememberUpdatedState(ModalManager.center.hasModalsOpen()).value) {
        Box(
          Modifier
            .fillMaxSize()
            .background(MaterialTheme.colors.background),
          contentAlignment = Alignment.Center
        ) {
          Text(stringResource(if (chatModel.desktopNoUserNoRemote) MR.strings.no_connected_mobile else MR.strings.no_selected_chat))
        }
      } else {
        ModalManager.center.showInView()
      }
    }
    else -> ChatView(currentChatId) {}
  }
}

@Composable
fun EndPartOfScreen() {
  ModalManager.end.showInView()
}

@Composable
fun DesktopScreen(settingsState: SettingsViewState) {
  Box {
    // 56.dp is a size of unused space of settings drawer
    Box(Modifier.width(DEFAULT_START_MODAL_WIDTH * fontSizeSqrtMultiplier + 56.dp)) {
      StartPartOfScreen(settingsState)
    }
    Box(Modifier.widthIn(max = DEFAULT_START_MODAL_WIDTH * fontSizeSqrtMultiplier)) {
      ModalManager.start.showInView()
      SwitchingUsersView()
    }
    Row(Modifier.padding(start = DEFAULT_START_MODAL_WIDTH * fontSizeSqrtMultiplier).clipToBounds()) {
      Box(Modifier.widthIn(min = DEFAULT_MIN_CENTER_MODAL_WIDTH).weight(1f)) {
        CenterPartOfScreen()
      }
      if (ModalManager.end.hasModalsOpen()) {
        VerticalDivider()
      }
      Box(Modifier.widthIn(max = DEFAULT_END_MODAL_WIDTH * fontSizeSqrtMultiplier).clipToBounds()) {
        EndPartOfScreen()
      }
    }
    val (userPickerState, scaffoldState ) = settingsState
    val scope = rememberCoroutineScope()
    if (scaffoldState.drawerState.isOpen || (ModalManager.start.hasModalsOpen && !ModalManager.center.hasModalsOpen)) {
      Box(
        Modifier
          .fillMaxSize()
          .padding(start = DEFAULT_START_MODAL_WIDTH * fontSizeSqrtMultiplier)
          .clickable(interactionSource = remember { MutableInteractionSource() }, indication = null, onClick = {
            ModalManager.start.closeModals()
            scope.launch { settingsState.scaffoldState.drawerState.close() }
          })
      )
    }
    VerticalDivider(Modifier.padding(start = DEFAULT_START_MODAL_WIDTH * fontSizeSqrtMultiplier))
    tryOrShowError("UserPicker", error = {}) {
      UserPicker(chatModel, userPickerState) {
        scope.launch { if (scaffoldState.drawerState.isOpen) scaffoldState.drawerState.close() else scaffoldState.drawerState.open() }
        userPickerState.value = AnimatedViewState.GONE
      }
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
