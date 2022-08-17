package chat.simplex.app

import android.app.Application
import android.net.LocalServerSocket
import android.util.Log
import androidx.lifecycle.*
import androidx.work.*
import chat.simplex.app.model.*
import chat.simplex.app.views.helpers.getFilesDirectory
import chat.simplex.app.views.helpers.withApi
import chat.simplex.app.views.onboarding.OnboardingStage
import kotlinx.coroutines.*
import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.*
import java.util.concurrent.Semaphore
import java.util.concurrent.TimeUnit
import kotlin.concurrent.thread

const val TAG = "SIMPLEX"

// ghc's rts
external fun initHS()
// android-support
external fun pipeStdOutToSocket(socketName: String) : Int

// SimpleX API
typealias ChatCtrl = Long
external fun chatInit(path: String): ChatCtrl
external fun chatSendCmd(ctrl: ChatCtrl, msg: String): String
external fun chatRecvMsg(ctrl: ChatCtrl): String
external fun chatRecvMsgWait(ctrl: ChatCtrl, timeout: Int): String
external fun chatParseMarkdown(str: String): String

class SimplexApp: Application(), LifecycleEventObserver {
  val chatController: ChatController by lazy {
    val ctrl = chatInit(getFilesDirectory(applicationContext))
    ChatController(ctrl, ntfManager, applicationContext, appPreferences)
  }

  val chatModel: ChatModel by lazy {
    chatController.chatModel
  }

  private val ntfManager: NtfManager by lazy {
    NtfManager(applicationContext, appPreferences)
  }

  private val appPreferences: AppPreferences by lazy {
    AppPreferences(applicationContext)
  }

  override fun onCreate() {
    super.onCreate()
    context = this
    ProcessLifecycleOwner.get().lifecycle.addObserver(this)
    withApi {
      val user = chatController.apiGetActiveUser()
      if (user == null) {
        chatModel.onboardingStage.value = OnboardingStage.Step1_SimpleXInfo
      } else {
        chatController.startChat(user)
        SimplexService.start(applicationContext)
        chatController.showBackgroundServiceNoticeIfNeeded()
      }
    }
  }

  override fun onStateChanged(source: LifecycleOwner, event: Lifecycle.Event) {
    Log.d(TAG, "onStateChanged: $event")
    withApi {
      when (event) {
        Lifecycle.Event.ON_STOP ->
          if (!appPreferences.runServiceInBackground.get()) SimplexService.stop(applicationContext)
        Lifecycle.Event.ON_START ->
          if (chatModel.chatRunning.value != false)  SimplexService.start(applicationContext)
        Lifecycle.Event.ON_RESUME ->
          if (chatModel.onboardingStage.value == OnboardingStage.OnboardingComplete) {
            chatController.showBackgroundServiceNoticeIfNeeded()
          }
        else -> {}
      }
    }
  }

  fun allowToStartServiceAfterAppExit() = with(chatModel.controller) {
    appPrefs.runServiceInBackground.get() && isIgnoringBatteryOptimizations(chatModel.controller.appContext)
  }

  /*
  * It takes 1-10 milliseconds to process this function. Better to do it in a background thread
  * */
  fun schedulePeriodicServiceRestartWorker() = CoroutineScope(Dispatchers.Default).launch {
    if (!allowToStartServiceAfterAppExit()) {
      return@launch
    }
    val workerVersion = chatController.appPrefs.autoRestartWorkerVersion.get()
    val workPolicy = if (workerVersion == SimplexService.SERVICE_START_WORKER_VERSION) {
      Log.d(TAG, "ServiceStartWorker version matches: choosing KEEP as existing work policy")
      ExistingPeriodicWorkPolicy.KEEP
    } else {
      Log.d(TAG, "ServiceStartWorker version DOES NOT MATCH: choosing REPLACE as existing work policy")
      chatController.appPrefs.autoRestartWorkerVersion.set(SimplexService.SERVICE_START_WORKER_VERSION)
      ExistingPeriodicWorkPolicy.REPLACE
    }
    val work = PeriodicWorkRequestBuilder<SimplexService.ServiceStartWorker>(SimplexService.SERVICE_START_WORKER_INTERVAL_MINUTES, TimeUnit.MINUTES)
      .addTag(SimplexService.TAG)
      .addTag(SimplexService.SERVICE_START_WORKER_WORK_NAME_PERIODIC)
      .build()
    Log.d(TAG, "ServiceStartWorker: Scheduling period work every ${SimplexService.SERVICE_START_WORKER_INTERVAL_MINUTES} minutes")
    WorkManager.getInstance(context)?.enqueueUniquePeriodicWork(SimplexService.SERVICE_START_WORKER_WORK_NAME_PERIODIC, workPolicy, work)
  }

  companion object {
    lateinit var context: SimplexApp private set

    init {
      val socketName = BuildConfig.APPLICATION_ID + ".local.socket.address.listen.native.cmd2"
      val s = Semaphore(0)
      thread(name="stdout/stderr pipe") {
        Log.d(TAG, "starting server")
        val server = LocalServerSocket(socketName)
        Log.d(TAG, "started server")
        s.release()
        val receiver = server.accept()
        Log.d(TAG, "started receiver")
        val logbuffer = FifoQueue<String>(500)
        if (receiver != null) {
          val inStream = receiver.inputStream
          val inStreamReader = InputStreamReader(inStream)
          val input = BufferedReader(inStreamReader)
          Log.d(TAG, "starting receiver loop")
          while (true) {
            val line = input.readLine() ?: break
            Log.w("$TAG (stdout/stderr)", line)
            logbuffer.add(line)
          }
          Log.w(TAG, "exited receiver loop")
        }
      }

      System.loadLibrary("app-lib")

      s.acquire()
      pipeStdOutToSocket(socketName)

      initHS()
    }
  }
}

class FifoQueue<E>(private var capacity: Int) : LinkedList<E>() {
  override fun add(element: E): Boolean {
    if(size > capacity) removeFirst()
    return super.add(element)
  }
}
