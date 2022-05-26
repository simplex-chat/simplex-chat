package chat.simplex.app

import android.app.Application
import android.content.Intent
import android.net.Uri
import android.os.Bundle
import android.util.Log
import androidx.activity.compose.setContent
import androidx.activity.viewModels
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Surface
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import androidx.fragment.app.FragmentActivity
import androidx.lifecycle.*
import androidx.work.*
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.NtfManager
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.SplashView
import chat.simplex.app.views.call.ActiveCallView
import chat.simplex.app.views.chat.ChatView
import chat.simplex.app.views.chatlist.ChatListView
import chat.simplex.app.views.chatlist.openChat
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.newchat.connectViaUri
import chat.simplex.app.views.newchat.withUriAction
import chat.simplex.app.views.onboarding.*
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import java.util.concurrent.TimeUnit

class MainActivity: FragmentActivity(), LifecycleEventObserver {
  private val vm by viewModels<SimplexViewModel>()
  private val chatController by lazy { (application as SimplexApp).chatController }
  private val chatShown = mutableStateOf(false)
  private val userAuthorized = mutableStateOf<Boolean?>(null)
  private val lastLA = mutableStateOf<Long?>(null)

  override fun onCreate(savedInstanceState: Bundle?) {
    super.onCreate(savedInstanceState)
    ProcessLifecycleOwner.get().lifecycle.addObserver(this)
//    testJson()
    val cm = vm.chatModel
    processNotificationIntent(intent, cm)
    setContent {
      SimpleXTheme {
        Surface(
          Modifier
            .background(MaterialTheme.colors.background)
            .fillMaxSize()
        ) {
          MainPage(cm, userAuthorized, ::setPerformLA, ::advertiseLA, chatShown)
          LaunchedEffect(cm.showAdvertiseLAUnavailableAlert.value) {
            if (cm.showAdvertiseLAUnavailableAlert.value) {
              laUnavailableInstructionAlert()
            }
          }
        }
      }
    }
    schedulePeriodicServiceRestartWorker()
  }

  override fun onNewIntent(intent: Intent?) {
    super.onNewIntent(intent)
    processIntent(intent, vm.chatModel)
  }

  override fun onStateChanged(source: LifecycleOwner, event: Lifecycle.Event) {
    withApi {
      when (event) {
        Lifecycle.Event.ON_START -> {
          // perform local authentication if needed
          val cm = vm.chatModel
          val lastLAVal = lastLA.value
          if (
            cm.controller.getPerformLA()
            && (lastLAVal == null || (System.nanoTime() - lastLAVal >= 30 * 1e+9))
          ) {
            userAuthorized.value = false
            authenticate(
              generalGetString(R.string.auth_access_chats),
              generalGetString(R.string.auth_log_in_using_credential),
              this@MainActivity,
              completed = { laResult ->
                when (laResult) {
                  LAResult.Success -> {
                    userAuthorized.value = true
                    lastLA.value = System.nanoTime()
                  }
                  is LAResult.Error -> laErrorToast(applicationContext, laResult.errString)
                  LAResult.Failed -> laFailedToast(applicationContext)
                  LAResult.Unavailable -> {
                    userAuthorized.value = true
                    cm.performLA.value = false
                    cm.controller.setPerformLA(false)
                    laUnavailableTurningOffAlert()
                  }
                }
              }
            )
          } else {
            userAuthorized.value = true
          }
        }
      }
    }
  }

  private fun schedulePeriodicServiceRestartWorker() {
    val workerVersion = chatController.getAutoRestartWorkerVersion()
    val workPolicy = if (workerVersion == SimplexService.SERVICE_START_WORKER_VERSION) {
      Log.d(TAG, "ServiceStartWorker version matches: choosing KEEP as existing work policy")
      ExistingPeriodicWorkPolicy.KEEP
    } else {
      Log.d(TAG, "ServiceStartWorker version DOES NOT MATCH: choosing REPLACE as existing work policy")
      chatController.setAutoRestartWorkerVersion(SimplexService.SERVICE_START_WORKER_VERSION)
      ExistingPeriodicWorkPolicy.REPLACE
    }
    val work = PeriodicWorkRequestBuilder<SimplexService.ServiceStartWorker>(SimplexService.SERVICE_START_WORKER_INTERVAL_MINUTES, TimeUnit.MINUTES)
      .addTag(SimplexService.TAG)
      .addTag(SimplexService.SERVICE_START_WORKER_WORK_NAME_PERIODIC)
      .build()
    Log.d(TAG, "ServiceStartWorker: Scheduling period work every ${SimplexService.SERVICE_START_WORKER_INTERVAL_MINUTES} minutes")
    WorkManager.getInstance(this)?.enqueueUniquePeriodicWork(SimplexService.SERVICE_START_WORKER_WORK_NAME_PERIODIC, workPolicy, work)
  }

  private fun setPerformLA(on: Boolean) {
    val cm = vm.chatModel
    if (on) {
      cm.controller.setLANoticeShown(true)
      authenticate(
        generalGetString(R.string.auth_enable),
        generalGetString(R.string.auth_confirm_credential),
        this@MainActivity,
        completed = { laResult ->
          when (laResult) {
            LAResult.Success -> {
              cm.performLA.value = true
              cm.controller.setPerformLA(true)
              userAuthorized.value = true
              lastLA.value = System.nanoTime()
              laTurnedOnAlert()
            }
            is LAResult.Error -> {
              cm.performLA.value = false
              cm.controller.setPerformLA(false)
              laErrorToast(applicationContext, laResult.errString)
            }
            LAResult.Failed -> {
              cm.performLA.value = false
              cm.controller.setPerformLA(false)
              laFailedToast(applicationContext)
            }
            LAResult.Unavailable -> {
              cm.performLA.value = false
              cm.controller.setPerformLA(false)
              laUnavailableInstructionAlert()
            }
          }
        }
      )
    } else {
      authenticate(
        generalGetString(R.string.auth_disable),
        generalGetString(R.string.auth_confirm_credential),
        this@MainActivity,
        completed = { laResult ->
          when (laResult) {
            LAResult.Success -> {
              cm.performLA.value = false
              cm.controller.setPerformLA(false)
            }
            is LAResult.Error -> {
              cm.performLA.value = true
              cm.controller.setPerformLA(true)
              laErrorToast(applicationContext, laResult.errString)
            }
            LAResult.Failed -> {
              cm.performLA.value = true
              cm.controller.setPerformLA(true)
              laFailedToast(applicationContext)
            }
            LAResult.Unavailable -> {
              cm.performLA.value = false
              cm.controller.setPerformLA(false)
              laUnavailableTurningOffAlert()
            }
          }
        }
      )
    }
  }

  private fun advertiseLA() {
    val cm = vm.chatModel
    if (
      !cm.controller.getLANoticeShown()
      && !chatShown.value
      && cm.chats.isNotEmpty()
    ) {
      cm.controller.showLANotice(this@MainActivity)
    }
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
  setPerformLA: (Boolean) -> Unit,
  advertiseLA: () -> Unit,
  chatShown: MutableState<Boolean>
) {
  // this with LaunchedEffect(userAuthorized.value) fixes bottom sheet visibly collapsing after authentication
  var chatsAccessAuthorized by remember { mutableStateOf<Boolean>(false) }
  LaunchedEffect(userAuthorized.value) {
    val userAuthorizedVal = userAuthorized.value
    launch {
      delay(500L)
      chatsAccessAuthorized = when {
        userAuthorizedVal != null && !userAuthorizedVal -> false
        else -> true
      }
    }
  }
  Box {
    val onboarding = chatModel.onboardingStage.value
    val userCreated = chatModel.userCreated.value
    when {
      onboarding == null || userCreated == null -> SplashView()
      !chatsAccessAuthorized -> SplashView()
      onboarding == OnboardingStage.OnboardingComplete && userCreated -> {
        if (chatModel.showCallView.value) ActiveCallView(chatModel)
        else {
          advertiseLA()
          chatShown.value = true
          if (chatModel.chatId.value == null) ChatListView(chatModel, setPerformLA = { setPerformLA(it) })
          else ChatView(chatModel)
        }
      }
      onboarding == OnboardingStage.Step1_SimpleXInfo ->
        Box(Modifier.padding(horizontal = 20.dp)) {
          SimpleXInfo(chatModel, onboarding = true)
        }
      onboarding == OnboardingStage.Step2_CreateProfile -> CreateProfile(chatModel)
    }
    ModalManager.shared.showInView()
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

fun connectIfOpenedViaUri(uri: Uri, chatModel: ChatModel) {
  Log.d(TAG, "connectIfOpenedViaUri: opened via link")
  if (chatModel.currentUser.value == null) {
    // TODO open from chat list view
    chatModel.appOpenUrl.value = uri
  } else {
    withUriAction(uri) { action ->
      val title = when (action) {
        "contact" -> generalGetString(R.string.connect_via_contact_link)
        "invitation" -> generalGetString(R.string.connect_via_invitation_link)
        else -> {
          Log.e(TAG, "URI has unexpected action. Alert shown.")
          action
        }
      }
      AlertManager.shared.showAlertMsg(
        title = title,
        text = generalGetString(R.string.profile_will_be_sent_to_contact_sending_link),
        confirmText = generalGetString(R.string.connect_via_link_verb),
        onConfirm = {
          withApi {
            Log.d(TAG, "connectIfOpenedViaUri: connecting")
            connectViaUri(chatModel, action, uri)
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
