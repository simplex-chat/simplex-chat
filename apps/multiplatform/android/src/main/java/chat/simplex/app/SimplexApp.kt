package chat.simplex.app

import android.app.Application
import android.net.LocalServerSocket
import android.util.Log
import androidx.lifecycle.*
import androidx.work.*
import chat.simplex.app.model.*
import chat.simplex.app.platform.*
import chat.simplex.app.ui.theme.DefaultTheme
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.onboarding.OnboardingStage
import com.jakewharton.processphoenix.ProcessPhoenix
import kotlinx.coroutines.*
import kotlinx.serialization.decodeFromString
import java.io.*
import java.lang.ref.WeakReference
import java.util.*
import java.util.concurrent.Semaphore
import java.util.concurrent.TimeUnit
import kotlin.concurrent.thread

const val TAG = "SIMPLEX"

class SimplexApp: Application(), LifecycleEventObserver {
  val chatModel: ChatModel
    get() = chatController.chatModel

  override fun onCreate() {
    super.onCreate()
    if (ProcessPhoenix.isPhoenixProcess(this)) {
      return;
    }
    context = this
    initHaskell()
    context.getDir("temp", MODE_PRIVATE).deleteRecursively()
    withBGApi {
      initChatController()
      runMigrations()
    }
    ProcessLifecycleOwner.get().lifecycle.addObserver(this@SimplexApp)
  }

  override fun onStateChanged(source: LifecycleOwner, event: Lifecycle.Event) {
    Log.d(TAG, "onStateChanged: $event")
    withApi {
      when (event) {
        Lifecycle.Event.ON_START -> {
          isAppOnForeground = true
          if (chatModel.chatRunning.value == true) {
            kotlin.runCatching {
              val currentUserId = chatModel.currentUser.value?.userId
              val chats = ArrayList(chatController.apiGetChats())
              /** Active user can be changed in background while [ChatController.apiGetChats] is executing */
              if (chatModel.currentUser.value?.userId == currentUserId) {
                val currentChatId = chatModel.chatId.value
                val oldStats = if (currentChatId != null) chatModel.getChat(currentChatId)?.chatStats else null
                if (oldStats != null) {
                  val indexOfCurrentChat = chats.indexOfFirst { it.id == currentChatId }
                  /** Pass old chatStats because unreadCounter can be changed already while [ChatController.apiGetChats] is executing */
                  if (indexOfCurrentChat >= 0) chats[indexOfCurrentChat] = chats[indexOfCurrentChat].copy(chatStats = oldStats)
                }
                chatModel.updateChats(chats)
              }
            }.onFailure { Log.e(TAG, it.stackTraceToString()) }
          }
        }
        Lifecycle.Event.ON_RESUME -> {
          isAppOnForeground = true
          if (chatModel.onboardingStage.value == OnboardingStage.OnboardingComplete) {
            SimplexService.showBackgroundServiceNoticeIfNeeded()
          }
          /**
           * We're starting service here instead of in [Lifecycle.Event.ON_START] because
           * after calling [ChatController.showBackgroundServiceNoticeIfNeeded] notification mode in prefs can be changed.
           * It can happen when app was started and a user enables battery optimization while app in background
           * */
          if (chatModel.chatRunning.value != false &&
            chatModel.onboardingStage.value == OnboardingStage.OnboardingComplete &&
            appPreferences.notificationsMode.get() == NotificationsMode.SERVICE.name
          ) {
            SimplexService.start()
          }
        }
        else -> isAppOnForeground = false
      }
    }
  }

  fun allowToStartServiceAfterAppExit() = with(chatModel.controller) {
    appPrefs.notificationsMode.get() == NotificationsMode.SERVICE.name &&
        (!NotificationsMode.SERVICE.requiresIgnoringBattery || SimplexService.isIgnoringBatteryOptimizations())
  }

  private fun allowToStartPeriodically() = with(chatModel.controller) {
    appPrefs.notificationsMode.get() == NotificationsMode.PERIODIC.name &&
        (!NotificationsMode.PERIODIC.requiresIgnoringBattery || SimplexService.isIgnoringBatteryOptimizations())
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

  fun schedulePeriodicWakeUp() = CoroutineScope(Dispatchers.Default).launch {
    if (!allowToStartPeriodically()) {
      return@launch
    }
    MessagesFetcherWorker.scheduleWork()
  }

  companion object {
    lateinit var context: SimplexApp private set
  }
}
