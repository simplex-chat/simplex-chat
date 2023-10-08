package chat.simplex.common

import androidx.compose.animation.core.Animatable
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
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
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.SimpleButton
import chat.simplex.common.views.SplashView
import chat.simplex.common.views.call.ActiveCallView
import chat.simplex.common.views.call.IncomingCallAlertView
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
  val scaffoldState: ScaffoldState,
  val switchingUsers: MutableState<Boolean>
)

@Composable
fun AppScreen() {
  ProvideWindowInsets(windowInsetsAnimationsEnabled = true) {
    Surface(color = MaterialTheme.colors.background) {
      MainScreen()
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
      && chatModel.chats.isNotEmpty()
      && chatModel.activeCallInvitation.value == null
    ) {
      AppLock.showLANotice(ChatModel.controller.appPrefs.laNoticeShown) }
  }
  LaunchedEffect(chatModel.showAdvertiseLAUnavailableAlert.value) {
    if (chatModel.showAdvertiseLAUnavailableAlert.value) {
      laUnavailableInstructionAlert()
    }
  }
  LaunchedEffect(chatModel.clearOverlays.value) {
    if (chatModel.clearOverlays.value) {
      ModalManager.closeAllModalsEverywhere()
      chatModel.clearOverlays.value = false
    }
  }

  @Composable
  fun AuthView() {
    Surface(color = MaterialTheme.colors.background) {
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
    var onboarding by remember { mutableStateOf(chatModel.controller.appPrefs.onboardingStage.get()) }
    LaunchedEffect(Unit) {
      snapshotFlow { chatModel.controller.appPrefs.onboardingStage.state.value }.distinctUntilChanged().collect { onboarding = it }
    }
    val userCreated = chatModel.userCreated.value
    var showInitializationView by remember { mutableStateOf(false) }
    when {
      chatModel.chatDbStatus.value == null && showInitializationView -> InitializationView()
      showChatDatabaseError -> {
        chatModel.chatDbStatus.value?.let {
          DatabaseErrorView(chatModel.chatDbStatus, chatModel.controller.appPrefs)
        }
      }
      remember { chatModel.chatDbEncrypted }.value == null || userCreated == null -> SplashView()
      onboarding == OnboardingStage.OnboardingComplete && userCreated -> {
        Box {
          showAdvertiseLAAlert = true
          val userPickerState by rememberSaveable(stateSaver = AnimatedViewState.saver()) { mutableStateOf(MutableStateFlow(AnimatedViewState.GONE)) }
          val scaffoldState = rememberScaffoldState()
          val switchingUsers = rememberSaveable { mutableStateOf(false) }
          val settingsState = remember { SettingsViewState(userPickerState, scaffoldState, switchingUsers) }
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
      onboarding == OnboardingStage.Step2_CreateProfile -> CreateProfile(chatModel) {}
      onboarding == OnboardingStage.Step2_5_SetupDatabasePassphrase -> SetupDatabasePassphrase(chatModel)
      onboarding == OnboardingStage.Step3_CreateSimpleXAddress -> CreateSimpleXAddress(chatModel)
      onboarding == OnboardingStage.Step4_SetNotificationsMode -> SetNotificationsMode(chatModel)
    }
    if (appPlatform.isAndroid) {
      ModalManager.fullscreen.showInView()
    }

    val unauthorized = remember { derivedStateOf { AppLock.userAuthorized.value != true } }
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
      }
    } else if (chatModel.showCallView.value) {
      ActiveCallView()
    }
    ModalManager.fullscreen.showPasscodeInView()
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
fun AndroidScreen(settingsState: SettingsViewState) {
  BoxWithConstraints {
    var currentChatId by rememberSaveable { mutableStateOf(chatModel.chatId.value) }
    val offset = remember { Animatable(if (chatModel.chatId.value == null) 0f else maxWidth.value) }
    Box(
      Modifier
        .graphicsLayer {
          translationX = -offset.value.dp.toPx()
        }
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
            if (it == null) onComposed(null)
            currentChatId = it
          }
      }
    }
    Box(Modifier.graphicsLayer { translationX = maxWidth.toPx() - offset.value.dp.toPx() }) Box2@{
      currentChatId?.let {
        ChatView(it, chatModel, onComposed)
      }
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
  val currentChatId by remember { ChatModel.chatId }
  LaunchedEffect(Unit) {
    snapshotFlow { currentChatId }
      .distinctUntilChanged()
      .collect {
        if (it != null) {
          ModalManager.center.closeModals()
        }
      }
  }
  when (val id = currentChatId) {
    null -> {
      if (!rememberUpdatedState(ModalManager.center.hasModalsOpen()).value) {
        Box(
          Modifier
            .fillMaxSize()
            .background(MaterialTheme.colors.background),
          contentAlignment = Alignment.Center
        ) {
          Text(stringResource(MR.strings.no_selected_chat))
        }
      } else {
        ModalManager.center.showInView()
      }
    }
    else -> ChatView(id, chatModel) {}
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
    Box(Modifier.width(DEFAULT_START_MODAL_WIDTH + 56.dp)) {
      StartPartOfScreen(settingsState)
    }
    Box(Modifier.widthIn(max = DEFAULT_START_MODAL_WIDTH)) {
      ModalManager.start.showInView()
    }
    Row(Modifier.padding(start = DEFAULT_START_MODAL_WIDTH).clipToBounds()) {
      Box(Modifier.widthIn(min = DEFAULT_MIN_CENTER_MODAL_WIDTH).weight(1f)) {
        CenterPartOfScreen()
      }
      if (ModalManager.end.hasModalsOpen()) {
        VerticalDivider()
      }
      Box(Modifier.widthIn(max = DEFAULT_END_MODAL_WIDTH).clipToBounds()) {
        EndPartOfScreen()
      }
    }
    val (userPickerState, scaffoldState, switchingUsers ) = settingsState
    val scope = rememberCoroutineScope()
    if (scaffoldState.drawerState.isOpen) {
      Box(
        Modifier
          .fillMaxSize()
          .padding(start = DEFAULT_START_MODAL_WIDTH)
          .clickable(interactionSource = remember { MutableInteractionSource() }, indication = null, onClick = {
            ModalManager.start.closeModals()
            scope.launch { settingsState.scaffoldState.drawerState.close() }
          })
      )
    }
    VerticalDivider(Modifier.padding(start = DEFAULT_START_MODAL_WIDTH))
    UserPicker(chatModel, userPickerState, switchingUsers) {
      scope.launch { if (scaffoldState.drawerState.isOpen) scaffoldState.drawerState.close() else scaffoldState.drawerState.open() }
    }
    ModalManager.fullscreen.showInView()
  }
}

@Composable
fun InitializationView() {
  Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
    Column(horizontalAlignment = Alignment.CenterHorizontally) {
      CircularProgressIndicator(
        Modifier
          .padding(bottom = DEFAULT_PADDING)
          .size(30.dp),
        color = MaterialTheme.colors.secondary,
        strokeWidth = 2.5.dp
      )
      Text(stringResource(MR.strings.opening_database))
    }
  }
}
