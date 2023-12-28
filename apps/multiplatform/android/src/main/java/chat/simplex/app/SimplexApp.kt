package chat.simplex.app

import android.app.Application
import android.app.UiModeManager
import android.os.*
import androidx.lifecycle.*
import androidx.work.*
import chat.simplex.app.model.NtfManager
import chat.simplex.common.helpers.APPLICATION_ID
import chat.simplex.common.helpers.requiresIgnoringBattery
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatModel.updatingChatsMutex
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.CurrentColors
import chat.simplex.common.ui.theme.DefaultTheme
import chat.simplex.common.views.call.RcvCallInvitation
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.OnboardingStage
import com.jakewharton.processphoenix.ProcessPhoenix
import kotlinx.coroutines.*
import kotlinx.coroutines.sync.withLock
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
      return
    } else {
      registerGlobalErrorHandler()
      Handler(Looper.getMainLooper()).post {
        while (true) {
          try {
            Looper.loop()
          } catch (e: Throwable) {
            if (e.message != null && e.message!!.startsWith("Unable to start activity")) {
              android.os.Process.killProcess(android.os.Process.myPid())
              break
            } else {
              // Send it to our exception handled because it will not get the exception otherwise
              Thread.getDefaultUncaughtExceptionHandler()?.uncaughtException(Looper.getMainLooper().thread, e)
            }
          }
        }
      }
    }
    context = this
    initHaskell()
    initMultiplatform()
    tmpDir.deleteRecursively()
    tmpDir.mkdir()

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
            updatingChatsMutex.withLock {
              kotlin.runCatching {
                val currentUserId = chatModel.currentUser.value?.userId
                val chats = ArrayList(chatController.apiGetChats(chatModel.remoteHostId()))
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
        }
        Lifecycle.Event.ON_RESUME -> {
          isAppOnForeground = true
          if (chatModel.controller.appPrefs.onboardingStage.get() == OnboardingStage.OnboardingComplete && chatModel.currentUser.value != null) {
            SimplexService.showBackgroundServiceNoticeIfNeeded()
          }
          /**
           * We're starting service here instead of in [Lifecycle.Event.ON_START] because
           * after calling [ChatController.showBackgroundServiceNoticeIfNeeded] notification mode in prefs can be changed.
           * It can happen when app was started and a user enables battery optimization while app in background
           * */
          if (chatModel.chatRunning.value != false &&
            chatModel.controller.appPrefs.onboardingStage.get() == OnboardingStage.OnboardingComplete &&
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
        (!NotificationsMode.SERVICE.requiresIgnoringBattery || SimplexService.isBackgroundAllowed())
  }

  private fun allowToStartPeriodically() = with(chatModel.controller) {
    appPrefs.notificationsMode.get() == NotificationsMode.PERIODIC &&
        (!NotificationsMode.PERIODIC.requiresIgnoringBattery || SimplexService.isBackgroundAllowed())
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
    ntfManager = object : chat.simplex.common.platform.NtfManager() {
      override fun notifyCallInvitation(invitation: RcvCallInvitation) = NtfManager.notifyCallInvitation(invitation)
      override fun hasNotificationsForChat(chatId: String): Boolean = NtfManager.hasNotificationsForChat(chatId)
      override fun cancelNotificationsForChat(chatId: String) = NtfManager.cancelNotificationsForChat(chatId)
      override fun displayNotification(user: UserLike, chatId: String, displayName: String, msgText: String, image: String?, actions: List<Pair<NotificationAction, () -> Unit>>) = NtfManager.displayNotification(user, chatId, displayName, msgText, image, actions.map { it.first })
      override fun androidCreateNtfChannelsMaybeShowAlert() = NtfManager.createNtfChannelsMaybeShowAlert()
      override fun cancelCallNotification() = NtfManager.cancelCallNotification()
      override fun cancelAllNotifications() = NtfManager.cancelAllNotifications()
    }
    platform = object : PlatformInterface {
      override suspend fun androidServiceStart() {
        SimplexService.start()
      }

      override fun androidServiceSafeStop() {
        SimplexService.safeStopService()
      }

      override fun androidNotificationsModeChanged(mode: NotificationsMode) {
        if (mode.requiresIgnoringBattery && !SimplexService.isBackgroundAllowed()) {
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
        SimplexService.showBackgroundServiceNoticeIfNeeded(showOffAlert = false)
      }

      override fun androidChatStartedAfterBeingOff() {
        SimplexService.cancelPassphraseNotification()
        when (appPrefs.notificationsMode.get()) {
          NotificationsMode.SERVICE -> CoroutineScope(Dispatchers.Default).launch { platform.androidServiceStart() }
          NotificationsMode.PERIODIC -> SimplexApp.context.schedulePeriodicWakeUp()
          NotificationsMode.OFF -> {}
        }
      }

      override fun androidChatStopped() {
        SimplexService.safeStopService()
        MessagesFetcherWorker.cancelAll()
      }

      override fun androidChatInitializedAndStarted() {
        // Prevents from showing "Enable notifications" alert when onboarding wasn't complete yet
        if (chatModel.controller.appPrefs.onboardingStage.get() == OnboardingStage.OnboardingComplete) {
          SimplexService.showBackgroundServiceNoticeIfNeeded()
          if (appPrefs.notificationsMode.get() == NotificationsMode.SERVICE)
            withBGApi {
              platform.androidServiceStart()
            }
        }
      }

      override fun androidIsBackgroundCallAllowed(): Boolean = !SimplexService.isBackgroundRestricted()

      override fun androidSetNightModeIfSupported() {
        if (Build.VERSION.SDK_INT < 31) return

        val light = if (CurrentColors.value.name == DefaultTheme.SYSTEM.name) {
          null
        } else {
          CurrentColors.value.colors.isLight
        }
        val mode = when (light) {
          null -> UiModeManager.MODE_NIGHT_AUTO
          true -> UiModeManager.MODE_NIGHT_NO
          false -> UiModeManager.MODE_NIGHT_YES
        }
        val uiModeManager = androidAppContext.getSystemService(UI_MODE_SERVICE) as UiModeManager
        uiModeManager.setApplicationNightMode(mode)
      }

      override suspend fun androidAskToAllowBackgroundCalls(): Boolean {
        if (SimplexService.isBackgroundRestricted()) {
          val userChoice: CompletableDeferred<Boolean> = CompletableDeferred()
          SimplexService.showBGRestrictedInCall {
            userChoice.complete(it)
          }
          return userChoice.await()
        }
        return true
      }
    }
  }
}
