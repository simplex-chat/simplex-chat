package chat.simplex.app

import android.annotation.SuppressLint
import android.app.*
import android.content.Context
import chat.simplex.common.platform.Log
import android.content.Intent
import android.content.pm.ActivityInfo
import android.os.*
import android.view.View
import androidx.compose.runtime.Composable
import androidx.compose.runtime.DisposableEffect
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.toArgb
import androidx.compose.ui.platform.LocalContext
import androidx.core.view.ViewCompat
import androidx.lifecycle.*
import androidx.work.*
import chat.simplex.app.model.NtfManager
import chat.simplex.app.model.NtfManager.AcceptCallAction
import chat.simplex.app.views.call.CallActivity
import chat.simplex.common.helpers.*
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatModel.withChats
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.call.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.OnboardingStage
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
      return
    } else {
      registerGlobalErrorHandler()
      Handler(Looper.getMainLooper()).post {
        while (true) {
          try {
            Looper.loop()
          } catch (e: Throwable) {
            if (e is UnsatisfiedLinkError || e.message?.startsWith("Unable to start activity") == true) {
              Process.killProcess(Process.myPid())
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
    runMigrations()
    tmpDir.deleteRecursively()
    tmpDir.mkdir()

    // Present screen for continue migration if it wasn't finished yet
    if (chatModel.migrationState.value != null) {
      // It's important, otherwise, user may be locked in undefined state
      appPrefs.onboardingStage.set(OnboardingStage.Step1_SimpleXInfo)
    } else if (DatabaseUtils.ksAppPassword.get() == null || DatabaseUtils.ksSelfDestructPassword.get() == null) {
      initChatControllerOnStart()
    }
    ProcessLifecycleOwner.get().lifecycle.addObserver(this@SimplexApp)
  }

  override fun onStateChanged(source: LifecycleOwner, event: Lifecycle.Event) {
    Log.d(TAG, "onStateChanged: $event")
    withLongRunningApi {
      when (event) {
        Lifecycle.Event.ON_START -> {
          isAppOnForeground = true
          if (chatModel.chatRunning.value == true) {
            withChats {
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
                  updateChats(chats)
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
      override fun notifyCallInvitation(invitation: RcvCallInvitation): Boolean = NtfManager.notifyCallInvitation(invitation)
      override fun hasNotificationsForChat(chatId: String): Boolean = NtfManager.hasNotificationsForChat(chatId)
      override fun cancelNotificationsForChat(chatId: String) = NtfManager.cancelNotificationsForChat(chatId)
      override fun cancelNotificationsForUser(userId: Long) = NtfManager.cancelNotificationsForUser(userId)
      override fun displayNotification(user: UserLike, chatId: String, displayName: String, msgText: String, image: String?, actions: List<Pair<NotificationAction, () -> Unit>>) = NtfManager.displayNotification(user, chatId, displayName, msgText, image, actions.map { it.first })
      override fun androidCreateNtfChannelsMaybeShowAlert() = NtfManager.createNtfChannelsMaybeShowAlert()
      override fun cancelCallNotification() = NtfManager.cancelCallNotification()
      override fun cancelAllNotifications() = NtfManager.cancelAllNotifications()
      override fun showMessage(title: String, text: String) = NtfManager.showMessage(title, text)
    }
    platform = object : PlatformInterface {
      override suspend fun androidServiceStart() {
        SimplexService.start()
      }

      override fun androidServiceSafeStop() {
        SimplexService.safeStopService()
      }

      override fun androidCallServiceSafeStop() {
        CallService.stopService()
      }

      override fun androidNotificationsModeChanged(mode: NotificationsMode) {
        if (mode.requiresIgnoringBattery && !SimplexService.isBackgroundAllowed()) {
          appPrefs.backgroundServiceNoticeShown.set(false)
        }
        SimplexService.StartReceiver.toggleReceiver(mode == NotificationsMode.SERVICE)
        CoroutineScope(Dispatchers.Default).launch {
          if (mode == NotificationsMode.SERVICE) {
            SimplexService.start()
            // Sometimes, when we change modes fast from one to another, system destroys the service after start.
            // We can wait a little and restart the service, and it will work in 100% of cases
            delay(2000)
            if (!SimplexService.isServiceStarted && appPrefs.notificationsMode.get() == NotificationsMode.SERVICE) {
              Log.i(TAG, "Service tried to start but destroyed by system, repeating once more")
              SimplexService.start()
            }
          } else {
            SimplexService.safeStopService()
          }
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

        val light = if (CurrentColors.value.name == DefaultTheme.SYSTEM_THEME_NAME) {
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

      override fun androidSetStatusAndNavBarColors(isLight: Boolean, statusBackgroundColor: Color, navBackgroundColor: Color) {
        val window = mainActivity.get()?.window ?: return
        if (window.statusBarColor == statusBackgroundColor.toArgb()) return
        window.statusBarColor = statusBackgroundColor.toArgb()
        window.navigationBarColor = (if (appPrefs.oneHandUI.get()) {
          navBackgroundColor.mixWith(CurrentColors.value.colors.onBackground, 0.97f)
        } else {
          navBackgroundColor
        }).toArgb()
        @Suppress("DEPRECATION")
        val windowInsetController = ViewCompat.getWindowInsetsController(window.decorView)
        windowInsetController?.isAppearanceLightStatusBars = isLight
        windowInsetController?.isAppearanceLightNavigationBars = isLight
      }

      override fun androidStartCallActivity(acceptCall: Boolean, remoteHostId: Long?, chatId: ChatId?) {
        val context = mainActivity.get() ?: return
        val intent = Intent(context, CallActivity::class.java)
          .addFlags(Intent.FLAG_ACTIVITY_NO_ANIMATION)
        if (acceptCall) {
          intent.setAction(AcceptCallAction)
            .putExtra("remoteHostId", remoteHostId)
            .putExtra("chatId", chatId)
        }
        intent.flags += Intent.FLAG_ACTIVITY_BROUGHT_TO_FRONT
        context.startActivity(intent)
      }

      override fun androidPictureInPictureAllowed(): Boolean {
        val appOps = androidAppContext.getSystemService(Context.APP_OPS_SERVICE) as AppOpsManager
        return appOps.checkOpNoThrow(AppOpsManager.OPSTR_PICTURE_IN_PICTURE, Process.myUid(), packageName) == AppOpsManager.MODE_ALLOWED
      }

      override fun androidCallEnded() {
        activeCallDestroyWebView()
      }

      override fun androidRestartNetworkObserver() {
        NetworkObserver.shared.restartNetworkObserver()
      }

      @SuppressLint("SourceLockedOrientationActivity")
      @Composable
      override fun androidLockPortraitOrientation() {
        val context = LocalContext.current
        DisposableEffect(Unit) {
          val activity = context as? Activity ?: return@DisposableEffect onDispose {}
          // Lock orientation to portrait in order to have good experience with calls
          activity.requestedOrientation = ActivityInfo.SCREEN_ORIENTATION_PORTRAIT
          onDispose {
            // Unlock orientation
            activity.requestedOrientation = ActivityInfo.SCREEN_ORIENTATION_UNSPECIFIED
          }
        }
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
