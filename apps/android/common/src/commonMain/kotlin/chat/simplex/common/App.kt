package chat.simplex.common

import androidx.compose.animation.core.Animatable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.unit.dp
import chat.simplex.common.AppLock.destroyedAfterBackPress
import chat.simplex.common.AppLock.elapsedRealtime
import chat.simplex.common.AppLock.enteredBackground
import chat.simplex.common.AppLock.laFailed
import chat.simplex.common.AppLock.runAuthenticate
import chat.simplex.common.AppLock.showLANotice
import chat.simplex.common.AppLock.userAuthorized
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.SplashView
import chat.simplex.common.views.call.ActiveCallView
import chat.simplex.common.views.call.IncomingCallAlertView
import chat.simplex.common.views.chat.ChatView
import chat.simplex.common.views.chatlist.ChatListView
import chat.simplex.common.views.chatlist.ShareListView
import chat.simplex.common.views.database.DatabaseErrorView
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.*
import chat.simplex.common.views.usersettings.*
import com.icerockdev.library.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.distinctUntilChanged

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
      && chatModel.onboardingStage.value == OnboardingStage.OnboardingComplete
      && chatModel.chats.isNotEmpty()
      && chatModel.activeCallInvitation.value == null
    ) {
      showLANotice(ChatModel.controller.appPrefs.laNoticeShown) }
  }
  LaunchedEffect(chatModel.showAdvertiseLAUnavailableAlert.value) {
    if (chatModel.showAdvertiseLAUnavailableAlert.value) {
      laUnavailableInstructionAlert()
    }
  }
  LaunchedEffect(chatModel.clearOverlays.value) {
    if (chatModel.clearOverlays.value) {
      ModalManager.shared.closeModals()
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
            laFailed.value = false
            runAuthenticate()
          }
        )
      }
    }
  }

  Box {
    val onboarding = chatModel.onboardingStage.value
    val userCreated = chatModel.userCreated.value
    var showInitializationView by remember { mutableStateOf(false) }
    when {
      chatModel.chatDbStatus.value == null && showInitializationView -> InitializationView().also { println("LALAL 1") }
      showChatDatabaseError -> {
        chatModel.chatDbStatus.value?.let {
          DatabaseErrorView(chatModel.chatDbStatus, chatModel.controller.appPrefs).also { println("LALAL 2") }
        }
      }
      onboarding == null || userCreated == null -> SplashView().also { println("LALAL 3") }
      onboarding == OnboardingStage.OnboardingComplete && userCreated -> {
        Box {
          showAdvertiseLAAlert = true
          BoxWithConstraints {
            var currentChatId by rememberSaveable { mutableStateOf(chatModel.chatId.value) }
            val offset = remember { Animatable(if (chatModel.chatId.value == null) 0f else maxWidth.value) }
            Box(
              Modifier
                .graphicsLayer {
                  translationX = -offset.value.dp.toPx()
                }
            ) {
              val stopped = chatModel.chatRunning.value == false
              if (chatModel.sharedContent.value == null)
                ChatListView(chatModel, AppLock::setPerformLA, stopped)
              else
                ShareListView(chatModel, stopped)
            }
            val scope = rememberCoroutineScope()
            val onComposed: () -> Unit = {
              scope.launch {
                offset.animateTo(
                  if (chatModel.chatId.value == null) 0f else maxWidth.value,
                  chatListAnimationSpec()
                )
                if (offset.value == 0f) {
                  currentChatId = null
                }
              }
            }
            LaunchedEffect(Unit) {
              launch {
                snapshotFlow { chatModel.chatId.value }
                  .distinctUntilChanged()
                  .collect {
                    if (it != null) currentChatId = it
                    else onComposed()
                  }
              }
            }
            Box(Modifier.graphicsLayer { translationX = maxWidth.toPx() - offset.value.dp.toPx() }) Box2@{
              currentChatId?.let {
                ChatView(it, chatModel, onComposed)
              }
            }
          }
        }.also { println("LALAL 4") }
      }
      onboarding == OnboardingStage.Step1_SimpleXInfo -> SimpleXInfo(chatModel, onboarding = true).also { println("LALAL 5") }
      onboarding == OnboardingStage.Step2_CreateProfile -> CreateProfile(chatModel) {}.also { println("LALAL 6") }
      onboarding == OnboardingStage.Step3_CreateSimpleXAddress -> CreateSimpleXAddress(chatModel).also { println("LALAL 7") }
      onboarding == OnboardingStage.Step4_SetNotificationsMode -> SetNotificationsMode(chatModel).also { println("LALAL 8") }
    }
    ModalManager.shared.showInView()

    val unauthorized = remember { derivedStateOf { userAuthorized.value != true } }
    if (unauthorized.value && !(chatModel.activeCallViewIsVisible.value && chatModel.showCallView.value)) {
      LaunchedEffect(Unit) {
        // With these constrains when user presses back button while on ChatList, activity destroys and shows auth request
        // while the screen moves to a launcher. Detect it and prevent showing the auth
        if (!(destroyedAfterBackPress.value && chatModel.controller.appPrefs.laMode.get() == LAMode.SYSTEM)) {
          runAuthenticate()
        }
      }
      if (chatModel.controller.appPrefs.performLA.get() && laFailed.value) {
        AuthView()
      } else {
        SplashView()
      }
    } else if (chatModel.showCallView.value) {
      ActiveCallView()
    }
    ModalManager.shared.showPasscodeInView()
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
      enteredBackground.value = elapsedRealtime() + 3000
    }
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
