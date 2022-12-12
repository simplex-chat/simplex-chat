package chat.simplex.app

import android.app.Application
import android.content.Intent
import android.net.Uri
import android.os.Build
import android.os.Bundle
import android.os.Parcelable
import android.os.SystemClock.elapsedRealtime
import android.util.Log
import android.view.WindowManager
import androidx.activity.compose.setContent
import androidx.activity.viewModels
import androidx.compose.animation.core.*
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Surface
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Lock
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import androidx.fragment.app.FragmentActivity
import androidx.lifecycle.*
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.NtfManager
import chat.simplex.app.ui.theme.SimpleButton
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.SplashView
import chat.simplex.app.views.call.ActiveCallView
import chat.simplex.app.views.call.IncomingCallAlertView
import chat.simplex.app.views.chat.ChatView
import chat.simplex.app.views.chatlist.*
import chat.simplex.app.views.database.DatabaseErrorView
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.newchat.*
import chat.simplex.app.views.onboarding.*
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.coroutines.flow.filter
import kotlinx.coroutines.launch

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

  override fun onCreate(savedInstanceState: Bundle?) {
    super.onCreate(savedInstanceState)
    // testJson()
    val m = vm.chatModel
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
        Surface(
          Modifier
            .background(MaterialTheme.colors.background)
            .fillMaxSize()
        ) {
          MainPage(
            m,
            userAuthorized,
            laFailed,
            ::runAuthenticate,
            ::setPerformLA,
            showLANotice = { m.controller.showLANotice(this) }
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

  override fun onStart() {
    super.onStart()
    val enteredBackgroundVal = enteredBackground.value
    if (enteredBackgroundVal == null || elapsedRealtime() - enteredBackgroundVal >= 30_000) {
      runAuthenticate()
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
    }
    if (!onBackPressedDispatcher.hasEnabledCallbacks()) {
      // Drop shared content
      SimplexApp.context.chatModel.sharedContent.value = null
    }
  }

  private fun runAuthenticate() {
    val m = vm.chatModel
    if (!m.controller.appPrefs.performLA.get()) {
      userAuthorized.value = true
    } else {
      userAuthorized.value = false
      ModalManager.shared.closeModals()
      authenticate(
        generalGetString(R.string.auth_unlock),
        generalGetString(R.string.auth_log_in_using_credential),
        this@MainActivity,
        completed = { laResult ->
          when (laResult) {
            LAResult.Success ->
              userAuthorized.value = true
            is LAResult.Error, LAResult.Failed ->
              laFailed.value = true
            LAResult.Unavailable -> {
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

  private fun setPerformLA(on: Boolean) {
    vm.chatModel.controller.appPrefs.laNoticeShown.set(true)
    if (on) {
      enableLA()
    } else {
      disableLA()
    }
  }

  private fun enableLA() {
    val m = vm.chatModel
    authenticate(
      generalGetString(R.string.auth_enable_simplex_lock),
      generalGetString(R.string.auth_confirm_credential),
      this@MainActivity,
      completed = { laResult ->
        val prefPerformLA = m.controller.appPrefs.performLA
        when (laResult) {
          LAResult.Success -> {
            m.performLA.value = true
            prefPerformLA.set(true)
            laTurnedOnAlert()
          }
          is LAResult.Error, LAResult.Failed -> {
            m.performLA.value = false
            prefPerformLA.set(false)
          }
          LAResult.Unavailable -> {
            m.performLA.value = false
            prefPerformLA.set(false)
            laUnavailableInstructionAlert()
          }
        }
      }
    )
  }

  private fun disableLA() {
    val m = vm.chatModel
    authenticate(
      generalGetString(R.string.auth_disable_simplex_lock),
      generalGetString(R.string.auth_confirm_credential),
      this@MainActivity,
      completed = { laResult ->
        val prefPerformLA = m.controller.appPrefs.performLA
        when (laResult) {
          LAResult.Success -> {
            m.performLA.value = false
            prefPerformLA.set(false)
          }
          is LAResult.Error, LAResult.Failed -> {
            m.performLA.value = true
            prefPerformLA.set(true)
          }
          LAResult.Unavailable -> {
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
  runAuthenticate: () -> Unit,
  setPerformLA: (Boolean) -> Unit,
  showLANotice: () -> Unit
) {
  // this with LaunchedEffect(userAuthorized.value) fixes bottom sheet visibly collapsing after authentication
  var chatsAccessAuthorized by rememberSaveable { mutableStateOf(false) }
  LaunchedEffect(userAuthorized.value) {
    if (chatModel.controller.appPrefs.performLA.get()) {
      delay(500L)
    }
    chatsAccessAuthorized = userAuthorized.value == true
  }
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
  fun authView() {
    Box(
      Modifier.fillMaxSize(),
      contentAlignment = Alignment.Center
    ) {
      SimpleButton(
        stringResource(R.string.auth_unlock),
        icon = Icons.Outlined.Lock,
        click = {
          laFailed.value = false
          runAuthenticate()
        }
      )
    }
  }

  Box {
    val onboarding = chatModel.onboardingStage.value
    val userCreated = chatModel.userCreated.value
    when {
      showChatDatabaseError -> {
        chatModel.chatDbStatus.value?.let {
          DatabaseErrorView(chatModel.chatDbStatus, chatModel.controller.appPrefs)
        }
      }
      onboarding == null || userCreated == null -> SplashView()
      !chatsAccessAuthorized -> {
        if (chatModel.controller.appPrefs.performLA.get() && laFailed.value) {
          authView()
        } else {
          SplashView()
        }
      }
      onboarding == OnboardingStage.OnboardingComplete && userCreated -> {
        Box {
          if (chatModel.showCallView.value) ActiveCallView(chatModel)
          else {
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

                      // Deletes files that were not sent but already stored in files directory.
                      // Currently, it's voice records only
                      if (it == null && chatModel.filesToDelete.isNotEmpty()) {
                        chatModel.filesToDelete.forEach { it.delete() }
                        chatModel.filesToDelete.clear()
                      }
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
      }
      onboarding == OnboardingStage.Step1_SimpleXInfo -> SimpleXInfo(chatModel, onboarding = true)
      onboarding == OnboardingStage.Step2_CreateProfile -> CreateProfile(chatModel)
      onboarding == OnboardingStage.Step3_SetNotificationsMode -> SetNotificationsMode(chatModel)
    }
    ModalManager.shared.showInView()
    val invitation = chatModel.activeCallInvitation.value
    if (invitation != null) IncomingCallAlertView(invitation, chatModel)
    AlertManager.shared.showInView()
  }
}

fun processNotificationIntent(intent: Intent?, chatModel: ChatModel) {
  when (intent?.action) {
    NtfManager.OpenChatAction -> {
      val chatId = intent.getStringExtra("chatId")
      Log.d(TAG, "processNotificationIntent: OpenChatAction $chatId")
      if (chatId != null) {
        val cInfo = chatModel.getChat(chatId)?.chatInfo
        chatModel.clearOverlays.value = true
        if (cInfo != null) withApi { openChat(cInfo, chatModel) }
      }
    }
    NtfManager.ShowChatsAction -> {
      Log.d(TAG, "processNotificationIntent: ShowChatsAction")
      chatModel.chatId.value = null
      chatModel.clearOverlays.value = true
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
        "text/plain" == intent.type -> intent.getStringExtra(Intent.EXTRA_TEXT)?.let {
          chatModel.sharedContent.value = SharedContent.Text(it)
        }
        intent.type?.startsWith("image/") == true -> (intent.getParcelableExtra<Parcelable>(Intent.EXTRA_STREAM) as? Uri)?.let {
          chatModel.sharedContent.value = SharedContent.Images(intent.getStringExtra(Intent.EXTRA_TEXT) ?: "", listOf(it))
        } // All other mime types
        else -> (intent.getParcelableExtra<Parcelable>(Intent.EXTRA_STREAM) as? Uri)?.let {
          chatModel.sharedContent.value = SharedContent.File(intent.getStringExtra(Intent.EXTRA_TEXT) ?: "", it)
        }
      }
    }
    Intent.ACTION_SEND_MULTIPLE -> {
      // Close active chat and show a list of chats
      chatModel.chatId.value = null
      chatModel.clearOverlays.value = true
      when {
        intent.type?.startsWith("image/") == true -> (intent.getParcelableArrayListExtra<Parcelable>(Intent.EXTRA_STREAM) as? List<Uri>)?.let {
          chatModel.sharedContent.value = SharedContent.Images(intent.getStringExtra(Intent.EXTRA_TEXT) ?: "", it)
        } // All other mime types
        else -> {}
      }
    }
  }
}

fun connectIfOpenedViaUri(uri: Uri, chatModel: ChatModel) {
  Log.d(TAG, "connectIfOpenedViaUri: opened via link")
  if (chatModel.currentUser.value == null) {
    // TODO open from chat list view
    chatModel.appOpenUrl.value = uri
  } else {
    withUriAction(uri) { linkType ->
      val title = when (linkType) {
        ConnectionLinkType.CONTACT -> generalGetString(R.string.connect_via_contact_link)
        ConnectionLinkType.INVITATION -> generalGetString(R.string.connect_via_invitation_link)
        ConnectionLinkType.GROUP -> generalGetString(R.string.connect_via_group_link)
      }
      AlertManager.shared.showAlertMsg(
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
//fun testJson() {
//  val str: String = """
//  """.trimIndent()
//
//  println(json.decodeFromString<APIResponse>(str))
//}
