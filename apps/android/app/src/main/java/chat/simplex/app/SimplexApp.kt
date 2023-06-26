package chat.simplex.app

import android.app.Application
import chat.simplex.common.platform.Log
import androidx.lifecycle.*
import androidx.work.*
import chat.simplex.app.model.NtfManager
import chat.simplex.common.helpers.APPLICATION_ID
import chat.simplex.common.helpers.requiresIgnoringBattery
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.OnboardingStage
import chat.simplex.common.platform.*
import chat.simplex.common.views.call.RcvCallInvitation
import com.jakewharton.processphoenix.ProcessPhoenix
import kotlinx.coroutines.*
import java.io.*
import java.util.*
import java.util.concurrent.TimeUnit

const val TAG = "SIMPLEX"

class SimplexApp: Application(), LifecycleEventObserver {
  val chatModel: ChatModel
    get() = chatController.chatModel

  val chatController: ChatController = ChatController

  override fun onCreate() {
    super.onCreate()
    if (ProcessPhoenix.isPhoenixProcess(this)) {
      return;
    }
    context = this
    initHaskell()
    initMultiplatform()
    tmpDir.deleteRecursively()
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
            appPrefs.notificationsMode.get() == NotificationsMode.SERVICE
          ) {
            SimplexService.start()
          }
        }
        else -> isAppOnForeground = false
      }
    }
  }

  fun allowToStartServiceAfterAppExit() = with(chatModel.controller) {
    appPrefs.notificationsMode.get() == NotificationsMode.SERVICE &&
        (!NotificationsMode.SERVICE.requiresIgnoringBattery || SimplexService.isIgnoringBatteryOptimizations())
  }

  private fun allowToStartPeriodically() = with(chatModel.controller) {
    appPrefs.notificationsMode.get() == NotificationsMode.PERIODIC &&
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

  private fun initMultiplatform() {
    androidAppContext = this
    APPLICATION_ID = BuildConfig.APPLICATION_ID
    serviceStart = { SimplexService.start() }
    serviceSafeStop = { SimplexService.safeStopService() }
    ntfManager = object : chat.simplex.common.platform.NtfManager() {
      override fun notifyContactConnected(user: User, contact: Contact) = NtfManager.notifyContactConnected(user, contact)
      override fun notifyContactRequestReceived(user: User, cInfo: ChatInfo.ContactRequest) = NtfManager.notifyContactRequestReceived(user, cInfo)
      override fun notifyMessageReceived(user: User, cInfo: ChatInfo, cItem: ChatItem) = NtfManager.notifyMessageReceived(user, cInfo, cItem)
      override fun notifyCallInvitation(invitation: RcvCallInvitation) = NtfManager.notifyCallInvitation(invitation)
      override fun hasNotificationsForChat(chatId: String): Boolean = NtfManager.hasNotificationsForChat(chatId)
      override fun cancelNotificationsForChat(chatId: String) = NtfManager.cancelNotificationsForChat(chatId)
      override fun displayNotification(user: User, chatId: String, displayName: String, msgText: String, image: String?, actions: List<NotificationAction>) = NtfManager.displayNotification(user, chatId, displayName, msgText, image, actions)
      override fun createNtfChannelsMaybeShowAlert() = NtfManager.createNtfChannelsMaybeShowAlert()
      override fun cancelCallNotification() = NtfManager.cancelCallNotification()
      override fun cancelAllNotifications() = NtfManager.cancelAllNotifications()
    }
    notificationsModeChanged = { mode: NotificationsMode ->
      if (mode.requiresIgnoringBattery && !SimplexService.isIgnoringBatteryOptimizations()) {
        appPrefs.backgroundServiceNoticeShown.set(false)
      }
      SimplexService.StartReceiver.toggleReceiver(mode == NotificationsMode.SERVICE)
      CoroutineScope(Dispatchers.Default).launch {
        if (mode == NotificationsMode.SERVICE)
          SimplexService.start()
        else
          SimplexService.safeStopService()
      }

      if (mode != NotificationsMode.PERIODIC) {
        MessagesFetcherWorker.cancelAll()
      }
      SimplexService.showBackgroundServiceNoticeIfNeeded()
    }
    chatStartedAfterBeingOff = {
      SimplexService.cancelPassphraseNotification()
      when (appPrefs.notificationsMode.get()) {
        NotificationsMode.SERVICE -> CoroutineScope(Dispatchers.Default).launch { serviceStart() }
        NotificationsMode.PERIODIC -> SimplexApp.context.schedulePeriodicWakeUp()
        NotificationsMode.OFF -> {}
      }
    }
    chatStopped = {
      SimplexService.safeStopService()
      MessagesFetcherWorker.cancelAll()
    }
    chatInitializedAndStarted = {
      // Prevents from showing "Enable notifications" alert when onboarding wasn't complete yet
      if (chatModel.onboardingStage.value == OnboardingStage.OnboardingComplete) {
        SimplexService.showBackgroundServiceNoticeIfNeeded()
        if (appPrefs.notificationsMode.get() == NotificationsMode.SERVICE)
          withBGApi {
            serviceStart()
          }
      }
    }
  }
}
