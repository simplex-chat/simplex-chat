package chat.simplex.app

import android.app.Application
import android.content.Intent
import android.net.Uri
import android.os.Bundle
import android.util.Log
import android.widget.Toast
import androidx.activity.compose.setContent
import androidx.activity.viewModels
import androidx.biometric.BiometricManager
import androidx.biometric.BiometricManager.Authenticators.*
import androidx.biometric.BiometricPrompt
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Surface
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import androidx.core.content.ContextCompat
import androidx.fragment.app.FragmentActivity
import androidx.lifecycle.AndroidViewModel
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
import java.util.concurrent.Executor
import java.util.concurrent.TimeUnit

class MainActivity: FragmentActivity() {
  private val vm by viewModels<SimplexViewModel>()
  private val chatController by lazy { (application as SimplexApp).chatController }
  private val showChats = mutableStateOf(false)

  private lateinit var executor: Executor
  private lateinit var biometricPrompt: BiometricPrompt
  private lateinit var promptInfo: BiometricPrompt.PromptInfo

  override fun onCreate(savedInstanceState: Bundle?) {
    super.onCreate(savedInstanceState)
//    testJson()
    processNotificationIntent(intent, vm.chatModel)
    setContent {
      SimpleXTheme {
        Surface(
          Modifier
            .background(MaterialTheme.colors.background)
            .fillMaxSize()
        ) {
          MainPage(vm.chatModel)
        }
      }
    }
    schedulePeriodicServiceRestartWorker()
  }

  override fun onNewIntent(intent: Intent?) {
    super.onNewIntent(intent)
    processIntent(intent, vm.chatModel)
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

  private fun authenticate() {
    val biometricManager = BiometricManager.from(this)
    when (biometricManager.canAuthenticate(BIOMETRIC_STRONG or DEVICE_CREDENTIAL)) {
      BiometricManager.BIOMETRIC_SUCCESS -> {
        executor = ContextCompat.getMainExecutor(this)
        biometricPrompt = BiometricPrompt(
          this,
          executor,
          object: BiometricPrompt.AuthenticationCallback() {
            override fun onAuthenticationError(
              errorCode: Int,
              errString: CharSequence
            ) {
              super.onAuthenticationError(errorCode, errString)
              Toast.makeText(
                applicationContext,
                if (errString.isNotEmpty()) "Authentication error: $errString" else "Authentication error",
                Toast.LENGTH_SHORT
              ).show()
            }

            override fun onAuthenticationSucceeded(
              result: BiometricPrompt.AuthenticationResult
            ) {
              super.onAuthenticationSucceeded(result)
              showChats.value = true
            }

            override fun onAuthenticationFailed() {
              super.onAuthenticationFailed()
              Toast.makeText(
                applicationContext,
                "Authentication failed",
                Toast.LENGTH_SHORT
              ).show()
            }
          }
        )
        promptInfo = BiometricPrompt.PromptInfo.Builder()
          .setTitle("Access chats")
          .setSubtitle("Log in using your credential")
          .setAllowedAuthenticators(BIOMETRIC_STRONG or DEVICE_CREDENTIAL)
          .setConfirmationRequired(false)
          .build()
        biometricPrompt.authenticate(promptInfo)
      }
      else -> {
        AlertManager.shared.showAlertMsg(
          "Authentication unavailable",
          "Your device is not configured for authentication."
        )
        // TODO turn off preference
        showChats.value = true
      }
    }
  }

  @Composable
  fun MainPage(chatModel: ChatModel) {
    LaunchedEffect(chatModel.runAuthenticate.value) {
      // TODO authenticate only if onboarding is complete and preference is set
      authenticate()
      chatModel.runAuthenticate.value = false
    }
    Box {
      val onboarding = chatModel.onboardingStage.value
      val userCreated = chatModel.userCreated.value
      when {
        onboarding == null || userCreated == null -> SplashView()
        onboarding == OnboardingStage.OnboardingComplete && userCreated ->
          if (showChats.value) {
            if (chatModel.showCallView.value) ActiveCallView(chatModel)
            else if (chatModel.chatId.value == null) ChatListView(chatModel)
            else ChatView(chatModel)
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
  Log.e(TAG, "######################################### in processIntent")
  when (intent?.action) {
    "android.intent.action.VIEW" -> {
      Log.e(TAG, "######################################### in android.intent.action.VIEW")
      val uri = intent.data
      if (uri != null) connectIfOpenedViaUri(uri, chatModel)
    }
  }
}

fun connectIfOpenedViaUri(uri: Uri, chatModel: ChatModel) {
  Log.e(TAG, "######################################### in connectIfOpenedViaUri")
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
