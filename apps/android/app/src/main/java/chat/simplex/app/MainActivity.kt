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
    processNotificationIntent(intent, vm.chatModel)
    setContent {
      SimpleXTheme {
        Surface(
          Modifier
            .background(MaterialTheme.colors.background)
            .fillMaxSize()
        ) {
          MainPage()
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
            authenticate(this@MainActivity, applicationContext, onLAResult = { laResult ->
              when (laResult) {
                LAResult.Success -> {
                  userAuthorized.value = true
                  lastLA.value = System.nanoTime()
                }
                LAResult.Unavailable -> {
                  cm.controller.setPerformLA(false)
                }
                else -> {}
              }
            })
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

  @Composable
  fun MainPage() {
    Box {
      val cm = vm.chatModel
      val onboarding = cm.onboardingStage.value
      val userCreated = cm.userCreated.value
      val userAuthorized = userAuthorized.value
      when {
        userAuthorized != null && !userAuthorized -> SplashView() // TODO not authorized view
        onboarding == null || userCreated == null -> SplashView()
        onboarding == OnboardingStage.OnboardingComplete && userCreated -> {
          if (cm.showCallView.value) ActiveCallView(cm)
          else {
            // <-- advertise local authentication
            // not in Lifecycle.Event.ON_START because chat has to be started (startChat) so we can check onboarding stage and chats
            if (
              !cm.controller.getLANoticeShown()
              && !chatShown.value
              && cm.chats.isNotEmpty()
              && authenticationAvailable(this@MainActivity)
            ) {
              cm.controller.showLANotice(this@MainActivity)
            }
            // advertise local authentication -->
            chatShown.value = true
            if (cm.chatId.value == null) ChatListView(cm)
            else ChatView(cm)
          }
        }
        onboarding == OnboardingStage.Step1_SimpleXInfo ->
          Box(Modifier.padding(horizontal = 20.dp)) {
            SimpleXInfo(cm, onboarding = true)
          }
        onboarding == OnboardingStage.Step2_CreateProfile -> CreateProfile(cm)
      }
      ModalManager.shared.showInView()
      AlertManager.shared.showInView()
    }
  }
}

class SimplexViewModel(application: Application): AndroidViewModel(application) {
  val app = getApplication<SimplexApp>()
  val chatModel = app.chatModel
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
