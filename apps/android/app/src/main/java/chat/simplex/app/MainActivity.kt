package chat.simplex.app

import android.app.Application
import android.content.Intent
import android.net.Uri
import android.os.*
import android.os.SystemClock.elapsedRealtime
import android.util.Log
import android.view.WindowManager
import androidx.activity.compose.setContent
import androidx.activity.viewModels
import androidx.compose.animation.core.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import androidx.fragment.app.FragmentActivity
import androidx.lifecycle.*
import chat.simplex.app.MainActivity.Companion.enteredBackground
import chat.simplex.app.model.*
import chat.simplex.app.model.NtfManager.Companion.getUserIdFromIntent
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.SplashView
import chat.simplex.app.views.call.ActiveCallView
import chat.simplex.app.views.call.IncomingCallAlertView
import chat.simplex.app.views.chat.ChatView
import chat.simplex.app.views.chat.group.ProgressIndicator
import chat.simplex.app.views.chatlist.*
import chat.simplex.app.views.database.DatabaseErrorView
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.helpers.DatabaseUtils.ksAppPassword
import chat.simplex.app.views.helpers.DatabaseUtils.ksSelfDestructPassword
import chat.simplex.app.views.localauth.SetAppPasscodeView
import chat.simplex.app.views.newchat.*
import chat.simplex.app.views.onboarding.*
import chat.simplex.app.views.usersettings.LAMode
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.distinctUntilChanged

class MainActivity: FragmentActivity() {
  companion object {
    /**
     * We don't want these values to be bound to Activity lifecycle since activities are changed often, for example, when a user
     * clicks on new message in notification. In this case savedInstanceState will be null (this prevents restoring the values)
     * See [SimplexService.onTaskRemoved] for another part of the logic which nullifies the values when app closed by the user
     * */
    val userAuthorized = mutableStateOf<Boolean?>(null)
    val enteredBackground = mutableStateOf<Long?>(null)
    // Remember result and show it after orientation change
    private val laFailed = mutableStateOf(false)

    fun clearAuthState() {
      userAuthorized.value = null
      enteredBackground.value = null
    }
  }
  private val vm by viewModels<SimplexViewModel>()
  private val destroyedAfterBackPress = mutableStateOf(false)

  override fun onCreate(savedInstanceState: Bundle?) {
    super.onCreate(savedInstanceState)
    // testJson()
    val m = vm.chatModel
    applyAppLocale(m.controller.appPrefs.appLanguage)
    // When call ended and orientation changes, it re-process old intent, it's unneeded.
    // Only needed to be processed on first creation of activity
    if (savedInstanceState == null) {
      processNotificationIntent(intent, m)
      processIntent(intent, m)
      processExternalIntent(intent, m)
    }
    if (m.controller.appPrefs.privacyProtectScreen.get()) {
      Log.d(TAG, "onCreate: set FLAG_SECURE")
      window.setFlags(
        WindowManager.LayoutParams.FLAG_SECURE,
        WindowManager.LayoutParams.FLAG_SECURE
      )
    }
    setContent {
      SimpleXTheme {
        Surface(color = MaterialTheme.colors.background) {
          MainPage(
            m,
            userAuthorized,
            laFailed,
            destroyedAfterBackPress,
            ::runAuthenticate,
            ::setPerformLA,
            showLANotice = { showLANotice(m.controller.appPrefs.laNoticeShown, this) }
          )
        }
      }
    }
    SimplexApp.context.schedulePeriodicServiceRestartWorker()
    SimplexApp.context.schedulePeriodicWakeUp()
  }

  override fun onNewIntent(intent: Intent?) {
    super.onNewIntent(intent)
    processIntent(intent, vm.chatModel)
    processExternalIntent(intent, vm.chatModel)
  }

  override fun onResume() {
    super.onResume()
    val enteredBackgroundVal = enteredBackground.value
    val delay = vm.chatModel.controller.appPrefs.laLockDelay.get()
    if (enteredBackgroundVal == null || elapsedRealtime() - enteredBackgroundVal >= delay * 1000) {
      if (userAuthorized.value != false) {
        /** [runAuthenticate] will be called in [MainPage] if needed. Making like this prevents double showing of passcode on start */
        setAuthState()
      } else if (!vm.chatModel.activeCallViewIsVisible.value) {
        runAuthenticate()
      }
    }
  }

  override fun onPause() {
    super.onPause()
    /**
    * When new activity is created after a click on notification, the old one receives onPause before
    * recreation but receives onStop after recreation. So using both (onPause and onStop) to prevent
    * unwanted multiple auth dialogs from [runAuthenticate]
    * */
    enteredBackground.value = elapsedRealtime()
  }

  override fun onStop() {
    super.onStop()
    VideoPlayer.stopAll()
    enteredBackground.value = elapsedRealtime()
  }

  override fun onBackPressed() {
    if (
      onBackPressedDispatcher.hasEnabledCallbacks() // Has something to do in a backstack
      || Build.VERSION.SDK_INT >= Build.VERSION_CODES.R // Android 11 or above
      || isTaskRoot // there are still other tasks after we reach the main (home) activity
    ) {
      // https://medium.com/mobile-app-development-publication/the-risk-of-android-strandhogg-security-issue-and-how-it-can-be-mitigated-80d2ddb4af06
      super.onBackPressed()
    }

    if (!onBackPressedDispatcher.hasEnabledCallbacks() && vm.chatModel.controller.appPrefs.performLA.get()) {
      // When pressed Back and there is no one wants to process the back event, clear auth state to force re-auth on launch
      clearAuthState()
      laFailed.value = true
      destroyedAfterBackPress.value = true
    }
    if (!onBackPressedDispatcher.hasEnabledCallbacks()) {
      // Drop shared content
      SimplexApp.context.chatModel.sharedContent.value = null
    }
  }

  private fun setAuthState() {
    userAuthorized.value = !vm.chatModel.controller.appPrefs.performLA.get()
  }

  private fun runAuthenticate() {
    val m = vm.chatModel
    setAuthState()
    if (userAuthorized.value == false) {
      // To make Main thread free in order to allow to Compose to show blank view that hiding content underneath of it faster on slow devices
      CoroutineScope(Dispatchers.Default).launch {
        delay(50)
        withContext(Dispatchers.Main) {
          authenticate(
            if (m.controller.appPrefs.laMode.get() == LAMode.SYSTEM)
              generalGetString(R.string.auth_unlock)
            else
              generalGetString(R.string.la_enter_app_passcode),
            if (m.controller.appPrefs.laMode.get() == LAMode.SYSTEM)
              generalGetString(R.string.auth_log_in_using_credential)
            else
              generalGetString(R.string.auth_unlock),
            selfDestruct = true,
            this@MainActivity,
            completed = { laResult ->
              when (laResult) {
                LAResult.Success ->
                  userAuthorized.value = true
                is LAResult.Failed -> { /* Can be called multiple times on every failure */ }
                is LAResult.Error -> {
                  laFailed.value = true
                  if (m.controller.appPrefs.laMode.get() == LAMode.PASSCODE) {
                    laFailedAlert()
                  }
                }
                is LAResult.Unavailable -> {
                  userAuthorized.value = true
                  m.performLA.value = false
                  m.controller.appPrefs.performLA.set(false)
                  laUnavailableTurningOffAlert()
                }
              }
            }
          )
        }
      }
    }
  }

  private fun showLANotice(laNoticeShown: SharedPreference<Boolean>, activity: FragmentActivity) {
    Log.d(TAG, "showLANotice")
    if (!laNoticeShown.get()) {
      laNoticeShown.set(true)
      AlertManager.shared.showAlertDialog(
        title = generalGetString(R.string.la_notice_title_simplex_lock),
        text = generalGetString(R.string.la_notice_to_protect_your_information_turn_on_simplex_lock_you_will_be_prompted_to_complete_authentication_before_this_feature_is_enabled),
        confirmText = generalGetString(R.string.la_notice_turn_on),
        onConfirm = {
          withBGApi { // to remove this call, change ordering of onConfirm call in AlertManager
            showChooseLAMode(laNoticeShown, activity)
          }
        }
      )
    }
  }

  private fun showChooseLAMode(laNoticeShown: SharedPreference<Boolean>, activity: FragmentActivity) {
    Log.d(TAG, "showLANotice")
    laNoticeShown.set(true)
    AlertManager.shared.showAlertDialogStacked(
      title = generalGetString(R.string.la_lock_mode),
      text = null,
      confirmText = generalGetString(R.string.la_lock_mode_passcode),
      dismissText = generalGetString(R.string.la_lock_mode_system),
      onConfirm = {
        AlertManager.shared.hideAlert()
        setPasscode()
      },
      onDismiss = {
        AlertManager.shared.hideAlert()
        initialEnableLA(activity)
      }
    )
  }

  private fun initialEnableLA(activity: FragmentActivity) {
    val m = vm.chatModel
    val appPrefs = m.controller.appPrefs
    m.controller.appPrefs.laMode.set(LAMode.SYSTEM)
    authenticate(
      generalGetString(R.string.auth_enable_simplex_lock),
      generalGetString(R.string.auth_confirm_credential),
      activity = activity,
      completed = { laResult ->
        when (laResult) {
          LAResult.Success -> {
            m.performLA.value = true
            appPrefs.performLA.set(true)
            laTurnedOnAlert()
          }
          is LAResult.Failed -> { /* Can be called multiple times on every failure */ }
          is LAResult.Error -> {
            m.performLA.value = false
            appPrefs.performLA.set(false)
            laFailedAlert()
          }
          is LAResult.Unavailable -> {
            m.performLA.value = false
            appPrefs.performLA.set(false)
            m.showAdvertiseLAUnavailableAlert.value = true
          }
        }
      }
    )
  }

  private fun setPasscode() {
    val chatModel = vm.chatModel
    val appPrefs = chatModel.controller.appPrefs
    ModalManager.shared.showCustomModal { close ->
      Surface(Modifier.fillMaxSize(), color = MaterialTheme.colors.background) {
        SetAppPasscodeView(
          submit = {
            chatModel.performLA.value = true
            appPrefs.performLA.set(true)
            appPrefs.laMode.set(LAMode.PASSCODE)
            laTurnedOnAlert()
          },
          cancel = {
            chatModel.performLA.value = false
            appPrefs.performLA.set(false)
            laPasscodeNotSetAlert()
          },
          close = close
        )
      }
    }
  }

  private fun setPerformLA(on: Boolean, activity: FragmentActivity) {
    vm.chatModel.controller.appPrefs.laNoticeShown.set(true)
    if (on) {
      enableLA(activity)
    } else {
      disableLA(activity)
    }
  }

  private fun enableLA(activity: FragmentActivity) {
    val m = vm.chatModel
    authenticate(
      if (m.controller.appPrefs.laMode.get() == LAMode.SYSTEM)
        generalGetString(R.string.auth_enable_simplex_lock)
      else
        generalGetString(R.string.new_passcode),
      if (m.controller.appPrefs.laMode.get() == LAMode.SYSTEM)
        generalGetString(R.string.auth_confirm_credential)
      else
        "",
      activity = activity,
      completed = { laResult ->
        val prefPerformLA = m.controller.appPrefs.performLA
        when (laResult) {
          LAResult.Success -> {
            m.performLA.value = true
            prefPerformLA.set(true)
            laTurnedOnAlert()
          }
          is LAResult.Failed -> { /* Can be called multiple times on every failure */ }
          is LAResult.Error -> {
            m.performLA.value = false
            prefPerformLA.set(false)
            laFailedAlert()
          }
          is LAResult.Unavailable -> {
            m.performLA.value = false
            prefPerformLA.set(false)
            laUnavailableInstructionAlert()
          }
        }
      }
    )
  }

  private fun disableLA(activity: FragmentActivity) {
    val m = vm.chatModel
    authenticate(
      if (m.controller.appPrefs.laMode.get() == LAMode.SYSTEM)
        generalGetString(R.string.auth_disable_simplex_lock)
      else
        generalGetString(R.string.la_enter_app_passcode),
      if (m.controller.appPrefs.laMode.get() == LAMode.SYSTEM)
        generalGetString(R.string.auth_confirm_credential)
      else
        generalGetString(R.string.auth_disable_simplex_lock),
      activity = activity,
      completed = { laResult ->
        val prefPerformLA = m.controller.appPrefs.performLA
        val selfDestructPref = m.controller.appPrefs.selfDestruct
        when (laResult) {
          LAResult.Success -> {
            m.performLA.value = false
            prefPerformLA.set(false)
            ksAppPassword.remove()
            selfDestructPref.set(false)
            ksSelfDestructPassword.remove()
          }
          is LAResult.Failed -> { /* Can be called multiple times on every failure */ }
          is LAResult.Error -> {
            m.performLA.value = true
            prefPerformLA.set(true)
            laFailedAlert()
          }
          is LAResult.Unavailable -> {
            m.performLA.value = false
            prefPerformLA.set(false)
            laUnavailableTurningOffAlert()
          }
        }
      }
    )
  }
}

class SimplexViewModel(application: Application): AndroidViewModel(application) {
  val app = getApplication<SimplexApp>()
  val chatModel = app.chatModel
}

@Composable
fun MainPage(
  chatModel: ChatModel,
  userAuthorized: MutableState<Boolean?>,
  laFailed: MutableState<Boolean>,
  destroyedAfterBackPress: MutableState<Boolean>,
  runAuthenticate: () -> Unit,
  setPerformLA: (Boolean, FragmentActivity) -> Unit,
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
          stringResource(R.string.auth_unlock),
          icon = painterResource(R.drawable.ic_lock),
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
      enteredBackground.value = elapsedRealtime() + 3000
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
      Text(stringResource(R.string.opening_database))
    }
  }
}

fun processNotificationIntent(intent: Intent?, chatModel: ChatModel) {
  val userId = getUserIdFromIntent(intent)
  when (intent?.action) {
    NtfManager.OpenChatAction -> {
      val chatId = intent.getStringExtra("chatId")
      Log.d(TAG, "processNotificationIntent: OpenChatAction $chatId")
      if (chatId != null) {
        withBGApi {
          awaitChatStartedIfNeeded(chatModel)
          if (userId != null && userId != chatModel.currentUser.value?.userId && chatModel.currentUser.value != null) {
            chatModel.controller.changeActiveUser(userId, null)
          }
          val cInfo = chatModel.getChat(chatId)?.chatInfo
          chatModel.clearOverlays.value = true
          if (cInfo != null) openChat(cInfo, chatModel)
        }
      }
    }
    NtfManager.ShowChatsAction -> {
      Log.d(TAG, "processNotificationIntent: ShowChatsAction")
      withBGApi {
        awaitChatStartedIfNeeded(chatModel)
        if (userId != null && userId != chatModel.currentUser.value?.userId && chatModel.currentUser.value != null) {
          chatModel.controller.changeActiveUser(userId, null)
        }
        chatModel.chatId.value = null
        chatModel.clearOverlays.value = true
      }
    }
    NtfManager.AcceptCallAction -> {
      val chatId = intent.getStringExtra("chatId")
      if (chatId == null || chatId == "") return
      Log.d(TAG, "processNotificationIntent: AcceptCallAction $chatId")
      chatModel.clearOverlays.value = true
      val invitation = chatModel.callInvitations[chatId]
      if (invitation == null) {
        AlertManager.shared.showAlertMsg(generalGetString(R.string.call_already_ended))
      } else {
        chatModel.callManager.acceptIncomingCall(invitation = invitation)
      }
    }
  }
}

fun processIntent(intent: Intent?, chatModel: ChatModel) {
  when (intent?.action) {
    "android.intent.action.VIEW" -> {
      val uri = intent.data
      if (uri != null) connectIfOpenedViaUri(uri, chatModel)
    }
  }
}

fun processExternalIntent(intent: Intent?, chatModel: ChatModel) {
  when (intent?.action) {
    Intent.ACTION_SEND -> {
      // Close active chat and show a list of chats
      chatModel.chatId.value = null
      chatModel.clearOverlays.value = true
      when {
        intent.type == "text/plain" -> {
          val text = intent.getStringExtra(Intent.EXTRA_TEXT)
          if (text != null) {
            chatModel.sharedContent.value = SharedContent.Text(text)
          }
        }
        isMediaIntent(intent) -> {
          val uri = intent.getParcelableExtra<Parcelable>(Intent.EXTRA_STREAM) as? Uri
          if (uri != null) {
            chatModel.sharedContent.value = SharedContent.Media(intent.getStringExtra(Intent.EXTRA_TEXT) ?: "", listOf(uri))
          } // All other mime types
        }
        else -> {
          val uri = intent.getParcelableExtra<Parcelable>(Intent.EXTRA_STREAM) as? Uri
          if (uri != null) {
            chatModel.sharedContent.value = SharedContent.File(intent.getStringExtra(Intent.EXTRA_TEXT) ?: "", uri)
          }
        }
      }
    }
    Intent.ACTION_SEND_MULTIPLE -> {
      // Close active chat and show a list of chats
      chatModel.chatId.value = null
      chatModel.clearOverlays.value = true
      Log.e(TAG, "ACTION_SEND_MULTIPLE ${intent.type}")
      when {
        isMediaIntent(intent) -> {
          val uris = intent.getParcelableArrayListExtra<Parcelable>(Intent.EXTRA_STREAM) as? List<Uri>
          if (uris != null) {
            chatModel.sharedContent.value = SharedContent.Media(intent.getStringExtra(Intent.EXTRA_TEXT) ?: "", uris)
          } // All other mime types
        }
        else -> {}
      }
    }
  }
}

fun isMediaIntent(intent: Intent): Boolean =
  intent.type?.startsWith("image/") == true || intent.type?.startsWith("video/") == true

fun connectIfOpenedViaUri(uri: Uri, chatModel: ChatModel) {
  Log.d(TAG, "connectIfOpenedViaUri: opened via link")
  if (chatModel.currentUser.value == null) {
    chatModel.appOpenUrl.value = uri
  } else {
    withUriAction(uri) { linkType ->
      val title = when (linkType) {
        ConnectionLinkType.CONTACT -> generalGetString(R.string.connect_via_contact_link)
        ConnectionLinkType.INVITATION -> generalGetString(R.string.connect_via_invitation_link)
        ConnectionLinkType.GROUP -> generalGetString(R.string.connect_via_group_link)
      }
      AlertManager.shared.showAlertDialog(
        title = title,
        text = if (linkType == ConnectionLinkType.GROUP)
          generalGetString(R.string.you_will_join_group)
        else
          generalGetString(R.string.profile_will_be_sent_to_contact_sending_link),
        confirmText = generalGetString(R.string.connect_via_link_verb),
        onConfirm = {
          withApi {
            Log.d(TAG, "connectIfOpenedViaUri: connecting")
            connectViaUri(chatModel, linkType, uri)
          }
        }
      )
    }
  }
}

suspend fun awaitChatStartedIfNeeded(chatModel: ChatModel, timeout: Long = 30_000) {
  // Still decrypting database
  if (chatModel.chatRunning.value == null) {
    val step = 50L
    for (i in 0..(timeout / step)) {
      if (chatModel.chatRunning.value == true || chatModel.onboardingStage.value == OnboardingStage.Step1_SimpleXInfo) {
        break
      }
      delay(step)
    }
  }
}

//fun testJson() {
//  val str: String = """
//  """.trimIndent()
//
//  println(json.decodeFromString<APIResponse>(str))
//}
