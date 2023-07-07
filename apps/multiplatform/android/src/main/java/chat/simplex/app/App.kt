package chat.simplex.app

import android.os.SystemClock
import androidx.compose.animation.core.Animatable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.DEFAULT_PADDING
import chat.simplex.app.ui.theme.SimpleButton
import chat.simplex.app.views.SplashView
import chat.simplex.app.views.call.ActiveCallView
import chat.simplex.app.views.call.IncomingCallAlertView
import chat.simplex.app.views.chat.ChatView
import chat.simplex.app.views.chatlist.ChatListView
import chat.simplex.app.views.chatlist.ShareListView
import chat.simplex.app.views.database.DatabaseErrorView
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.onboarding.*
import chat.simplex.app.views.usersettings.LAMode
import chat.simplex.app.views.usersettings.laUnavailableInstructionAlert
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.coroutines.launch

@Composable
fun MainPage(
  chatModel: ChatModel,
  userAuthorized: MutableState<Boolean?>,
  laFailed: MutableState<Boolean>,
  destroyedAfterBackPress: MutableState<Boolean>,
  runAuthenticate: () -> Unit,
  setPerformLA: (Boolean) -> Unit,
  showLANotice: () -> Unit
) {
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
      showLANotice()
    }
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
      chatModel.chatDbStatus.value == null && showInitializationView -> InitializationView()
      showChatDatabaseError -> {
        chatModel.chatDbStatus.value?.let {
          DatabaseErrorView(chatModel.chatDbStatus, chatModel.controller.appPrefs)
        }
      }
      onboarding == null || userCreated == null -> SplashView()
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
                ChatListView(chatModel, setPerformLA, stopped)
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
            Box (Modifier.graphicsLayer { translationX = maxWidth.toPx() - offset.value.dp.toPx() }) Box2@ {
              currentChatId?.let {
                ChatView(it, chatModel, onComposed)
              }
            }
          }
        }
      }
      onboarding == OnboardingStage.Step1_SimpleXInfo -> SimpleXInfo(chatModel, onboarding = true)
      onboarding == OnboardingStage.Step2_CreateProfile -> CreateProfile(chatModel) {}
      onboarding == OnboardingStage.Step3_CreateSimpleXAddress -> CreateSimpleXAddress(chatModel)
      onboarding == OnboardingStage.Step4_SetNotificationsMode -> SetNotificationsMode(chatModel)
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
      ActiveCallView(chatModel)
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
      AppLock.enteredBackground.value = SystemClock.elapsedRealtime() + 3000
    }
  }
}

@Composable
private fun InitializationView() {
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
