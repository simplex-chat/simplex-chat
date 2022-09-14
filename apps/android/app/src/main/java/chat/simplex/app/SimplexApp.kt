package chat.simplex.app

import android.app.Application
import android.net.LocalServerSocket
import android.util.Log
import androidx.lifecycle.*
import androidx.work.*
import chat.simplex.app.model.*
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.onboarding.OnboardingStage
import chat.simplex.app.views.usersettings.NotificationsMode
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
external fun chatMigrateDB(dbPath: String, dbKey: String): String
external fun chatInitKey(dbPath: String, dbKey: String): ChatCtrl
external fun chatInit(dbPath: String): ChatCtrl
external fun chatSendCmd(ctrl: ChatCtrl, msg: String): String
external fun chatRecvMsg(ctrl: ChatCtrl): String
external fun chatRecvMsgWait(ctrl: ChatCtrl, timeout: Int): String
external fun chatParseMarkdown(str: String): String

class SimplexApp: Application(), LifecycleEventObserver {
  lateinit var chatController: ChatController

  fun initChatController(useKey: String? = null, startChat: Boolean = true) {
    val dbKey = useKey ?: DatabaseUtils.getDatabaseKey() ?: ""
    val res = DatabaseUtils.migrateChatDatabase(dbKey)
    val ctrl = if (res.second is DBMigrationResult.OK) {
      chatInitKey(getFilesDirectory(applicationContext), dbKey)
    } else null
    if (::chatController.isInitialized) {
      chatController.ctrl = ctrl
    } else {
      chatController = ChatController(ctrl, ntfManager, applicationContext, appPreferences)
    }
    chatModel.chatDbEncrypted.value = res.first
    chatModel.chatDbStatus.value = res.second
    if (res.second != DBMigrationResult.OK) {
      Log.d(TAG, "Unable to migrate successfully: ${res.second}")
    } else if (startChat) {
      withApi {
        val user = chatController.apiGetActiveUser()
        if (user == null) {
          chatModel.onboardingStage.value = OnboardingStage.Step1_SimpleXInfo
        } else {
          chatController.startChat(user)
        }
      }
    }
  }

  val chatModel: ChatModel
    get() = chatController.chatModel

  private val ntfManager: NtfManager by lazy {
    NtfManager(applicationContext, appPreferences)
  }

  private val appPreferences: AppPreferences by lazy {
    AppPreferences(applicationContext)
  }

  override fun onCreate() {
    super.onCreate()
    context = this
    initChatController()
    ProcessLifecycleOwner.get().lifecycle.addObserver(this)
  }

  override fun onStateChanged(source: LifecycleOwner, event: Lifecycle.Event) {
    Log.d(TAG, "onStateChanged: $event")
    withApi {
      when (event) {
        Lifecycle.Event.ON_RESUME -> {
          if (chatModel.onboardingStage.value == OnboardingStage.OnboardingComplete) {
            chatController.showBackgroundServiceNoticeIfNeeded()
          }
          /**
           * We're starting service here instead of in [Lifecycle.Event.ON_START] because
           * after calling [ChatController.showBackgroundServiceNoticeIfNeeded] notification mode in prefs can be changed.
           * It can happen when app was started and a user enables battery optimization while app in background
           * */
          if (chatModel.chatRunning.value != false && appPreferences.notificationsMode.get() == NotificationsMode.SERVICE.name)
            SimplexService.start(applicationContext)
        }
        else -> {}
      }
    }
  }

  fun allowToStartServiceAfterAppExit() = with(chatModel.controller) {
    appPrefs.notificationsMode.get() == NotificationsMode.SERVICE.name && isIgnoringBatteryOptimizations(chatModel.controller.appContext)
  }

  private fun allowToStartPeriodically() = chatModel.controller.appPrefs.notificationsMode.get() == NotificationsMode.PERIODIC.name

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

  fun schedulePeriodicWakeUp() = CoroutineScope(Dispatchers.Default).launch {
    if (!allowToStartPeriodically()) {
      return@launch
    }
    MessagesFetcherWorker.scheduleWork()
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
