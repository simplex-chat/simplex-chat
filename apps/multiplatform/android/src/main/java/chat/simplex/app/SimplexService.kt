package chat.simplex.app

import android.annotation.SuppressLint
import android.app.*
import android.content.*
import android.content.pm.PackageManager
import android.net.Uri
import android.os.*
import android.os.SystemClock
import android.provider.Settings
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.core.app.NotificationCompat
import androidx.core.content.ContextCompat
import androidx.work.*
import chat.simplex.common.AppLock
import chat.simplex.common.helpers.requiresIgnoringBattery
import chat.simplex.common.model.ChatController
import chat.simplex.common.model.NotificationsMode
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.*
import kotlinx.coroutines.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlin.system.exitProcess

// based on:
// https://robertohuertas.com/2019/06/29/android_foreground_services/
// https://github.com/binwiederhier/ntfy-android/blob/main/app/src/main/java/io/heckel/ntfy/service/SubscriberService.kt

class SimplexService: Service() {
  private var wakeLock: PowerManager.WakeLock? = null
  private var isCheckingNewMessages = false
  private var notificationManager: NotificationManager? = null
  private var serviceNotification: Notification? = null

  override fun onStartCommand(intent: Intent?, flags: Int, startId: Int): Int {
    Log.d(TAG, "onStartCommand startId: $startId")
    isServiceStarting = false
    if (intent != null) {
      val action = intent.action
      Log.d(TAG, "intent action $action")
      when (action) {
        Action.START.name -> startService()
        else -> Log.e(TAG, "No action in the intent")
      }
    } else {
      Log.d(TAG, "null intent. Probably restarted by the system.")
    }
    startForeground(SIMPLEX_SERVICE_ID, serviceNotification)
    return START_STICKY // to restart if killed
  }

  override fun onCreate() {
    super.onCreate()
    Log.d(TAG, "Simplex service created")
    val title = generalGetString(MR.strings.simplex_service_notification_title)
    val text = generalGetString(MR.strings.simplex_service_notification_text)
    notificationManager = createNotificationChannel()
    serviceNotification = createNotification(title, text)
    startForeground(SIMPLEX_SERVICE_ID, serviceNotification)
    /**
     * The reason [stopAfterStart] exists is because when the service is not called [startForeground] yet, and
     * we call [stopSelf] on the same service, [ForegroundServiceDidNotStartInTimeException] will be thrown.
     * To prevent that, we can call [stopSelf] only when the service made [startForeground] call
     * */
    if (stopAfterStart) {
      stopForeground(true)
      stopSelf()
    } else {
      isServiceStarting = false
      isServiceStarted = true
      // In case of self-destruct is enabled the initialization process will not start in SimplexApp, Let's start it here
      if (DatabaseUtils.ksSelfDestructPassword.get() != null && chatModel.chatDbStatus.value == null) {
        initChatControllerAndRunMigrations()
      }
    }
  }

  override fun onDestroy() {
    Log.d(TAG, "Simplex service destroyed")
    try {
      wakeLock?.let {
        while (it.isHeld) it.release() // release all, in case acquired more than once
      }
      wakeLock = null
    } catch (e: Exception) {
      Log.d(TAG, "Exception while releasing wakelock: ${e.message}")
    }
    isServiceStarting = false
    isServiceStarted = false
    stopAfterStart = false
    saveServiceState(this, ServiceState.STOPPED)

    // If notification service is enabled and battery optimization is disabled, restart the service
    if (SimplexApp.context.allowToStartServiceAfterAppExit())
      sendBroadcast(Intent(this, AutoRestartReceiver::class.java))
    super.onDestroy()
  }

  private fun startService() {
    Log.d(TAG, "SimplexService startService")
    if (wakeLock != null || isCheckingNewMessages) return
    val self = this
    isCheckingNewMessages = true
    withLongRunningApi {
      val chatController = ChatController
      waitDbMigrationEnds(chatController)
      try {
        Log.w(TAG, "Starting foreground service")
        val chatDbStatus = chatController.chatModel.chatDbStatus.value
        if (chatDbStatus != DBMigrationResult.OK) {
          Log.w(chat.simplex.app.TAG, "SimplexService: problem with the database: $chatDbStatus")
          showPassphraseNotification(chatDbStatus)
          safeStopService()
          return@withLongRunningApi
        }
        saveServiceState(self, ServiceState.STARTED)
        wakeLock = (getSystemService(Context.POWER_SERVICE) as PowerManager).run {
          newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, WAKE_LOCK_TAG).apply {
            acquire()
          }
        }
      } finally {
        isCheckingNewMessages = false
      }
    }
  }

  private fun createNotificationChannel(): NotificationManager? {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
      val notificationManager = getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
      val channel = NotificationChannel(NOTIFICATION_CHANNEL_ID, NOTIFICATION_CHANNEL_NAME, NotificationManager.IMPORTANCE_LOW).let {
        it.setShowBadge(false) // no long-press badge
        it
      }
      notificationManager.createNotificationChannel(channel)
      return notificationManager
    }
    return null
  }

  private fun createNotification(title: String, text: String): Notification {
    val pendingIntent: PendingIntent = Intent(this, MainActivity::class.java).let { notificationIntent ->
      PendingIntent.getActivity(this, 0, notificationIntent, PendingIntent.FLAG_IMMUTABLE)
    }

    val builder =  NotificationCompat.Builder(this, NOTIFICATION_CHANNEL_ID)
      .setSmallIcon(R.drawable.ntf_service_icon)
      .setColor(0x88FFFF)
      .setContentTitle(title)
      .setContentText(text)
      .setContentIntent(pendingIntent)
      .setSilent(true)
      .setShowWhen(false) // no date/time

    // Shows a button which opens notification channel settings
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
      val flags = PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_IMMUTABLE
      val setupIntent = Intent(Settings.ACTION_CHANNEL_NOTIFICATION_SETTINGS)
      setupIntent.putExtra(Settings.EXTRA_APP_PACKAGE, packageName)
      setupIntent.putExtra(Settings.EXTRA_CHANNEL_ID, NOTIFICATION_CHANNEL_ID)
      val setup = PendingIntent.getActivity(this, 0, setupIntent, flags)
      builder.addAction(0, generalGetString(MR.strings.hide_notification), setup)
    }

    return builder.build()
  }

  override fun onBind(intent: Intent): IBinder? {
    return null // no binding
  }

  // re-schedules the task when "Clear recent apps" is pressed
  override fun onTaskRemoved(rootIntent: Intent) {
    // Just to make sure that after restart of the app the user will need to re-authenticate
    AppLock.clearAuthState()

    if (appPreferences.chatStopped.get()) {
      stopSelf()
      exitProcess(0)
    }

    // If notification service isn't enabled or battery optimization isn't disabled, we shouldn't restart the service
    if (!SimplexApp.context.allowToStartServiceAfterAppExit()) {
      return
    }

    val restartServiceIntent = Intent(applicationContext, SimplexService::class.java).also {
      it.setPackage(packageName)
    };
    val restartServicePendingIntent: PendingIntent = PendingIntent.getService(this, 1, restartServiceIntent, PendingIntent.FLAG_ONE_SHOT or PendingIntent.FLAG_IMMUTABLE);
    val alarmService: AlarmManager = applicationContext.getSystemService(Context.ALARM_SERVICE) as AlarmManager;
    alarmService.set(AlarmManager.ELAPSED_REALTIME, SystemClock.elapsedRealtime() + 1000, restartServicePendingIntent);
  }

  // restart on reboot
  class StartReceiver: BroadcastReceiver() {
    override fun onReceive(context: Context, intent: Intent) {
      Log.d(TAG, "StartReceiver: onReceive called")
      scheduleStart(context)
    }
    companion object {
      fun toggleReceiver(enable: Boolean) {
        Log.d(TAG, "StartReceiver: toggleReceiver enabled: $enable")
        val component = ComponentName(BuildConfig.APPLICATION_ID, StartReceiver::class.java.name)
        SimplexApp.context.packageManager.setComponentEnabledSetting(
          component,
          if (enable) PackageManager.COMPONENT_ENABLED_STATE_ENABLED else PackageManager.COMPONENT_ENABLED_STATE_DISABLED,
          PackageManager.DONT_KILL_APP
        )
      }
    }
  }

  // restart on destruction
  class AutoRestartReceiver: BroadcastReceiver() {
    override fun onReceive(context: Context, intent: Intent) {
      Log.d(TAG, "AutoRestartReceiver: onReceive called")
      scheduleStart(context)
    }
  }

  class ServiceStartWorker(private val context: Context, params: WorkerParameters): CoroutineWorker(context, params) {
    override suspend fun doWork(): Result {
      val id = this.id
      if (context.applicationContext !is Application) {
        Log.d(TAG, "ServiceStartWorker: Failed, no application found (work ID: $id)")
        return Result.failure()
      }
      if (getServiceState(context) == ServiceState.STARTED) {
        Log.d(TAG, "ServiceStartWorker: Starting foreground service (work ID: $id)")
        start()
      }
      return Result.success()
    }
  }

  enum class Action {
    START,
  }

  enum class ServiceState {
    STARTED,
    STOPPED,
  }

  companion object {
    const val TAG = "SIMPLEX_SERVICE"
    const val NOTIFICATION_CHANNEL_ID = "chat.simplex.app.SIMPLEX_SERVICE_NOTIFICATION"
    const val NOTIFICATION_CHANNEL_NAME = "SimpleX Chat service"
    const val SIMPLEX_SERVICE_ID = 6789
    const val SERVICE_START_WORKER_VERSION = BuildConfig.VERSION_CODE
    const val SERVICE_START_WORKER_INTERVAL_MINUTES = 3 * 60L
    const val SERVICE_START_WORKER_WORK_NAME_PERIODIC = "SimplexAutoRestartWorkerPeriodic" // Do not change!

    private const val PASSPHRASE_NOTIFICATION_ID = 1535

    private const val WAKE_LOCK_TAG = "SimplexService::lock"
    private const val SHARED_PREFS_ID = "chat.simplex.app.SIMPLEX_SERVICE_PREFS"
    private const val SHARED_PREFS_SERVICE_STATE = "SIMPLEX_SERVICE_STATE"
    private const val WORK_NAME_ONCE = "ServiceStartWorkerOnce"

    var isServiceStarting = false
    var isServiceStarted = false
    private var stopAfterStart = false

    fun scheduleStart(context: Context) {
      Log.d(TAG, "Enqueuing work to start subscriber service")
      val workManager = WorkManager.getInstance(context)
      val startServiceRequest = OneTimeWorkRequest.Builder(ServiceStartWorker::class.java).build()
      workManager.enqueueUniqueWork(WORK_NAME_ONCE, ExistingWorkPolicy.KEEP, startServiceRequest) // Unique avoids races!
    }

    suspend fun start() = serviceAction(Action.START)

    /**
     * If there is a need to stop the service, use this function only. It makes sure that the service will be stopped without an
     * exception related to foreground services lifecycle
     * */
    fun safeStopService() {
      if (isServiceStarted) {
        androidAppContext.stopService(Intent(androidAppContext, SimplexService::class.java))
      } else if (isServiceStarting) {
        stopAfterStart = true
      }
    }

    private suspend fun serviceAction(action: Action) {
      Log.d(TAG, "SimplexService serviceAction: ${action.name}")
      withContext(Dispatchers.IO) {
        Intent(androidAppContext, SimplexService::class.java).also {
          it.action = action.name
          isServiceStarting = true
          ContextCompat.startForegroundService(androidAppContext, it)
        }
      }
    }

    fun restart(context: Context) {
      Intent(context, SimplexService::class.java).also { intent ->
        context.stopService(intent) // Service will auto-restart
      }
    }

    fun saveServiceState(context: Context, state: ServiceState) {
      getPreferences(context).edit()
        .putString(SHARED_PREFS_SERVICE_STATE, state.name)
        .apply()
    }

    fun getServiceState(context: Context): ServiceState {
      val value = getPreferences(context)
        .getString(SHARED_PREFS_SERVICE_STATE, ServiceState.STOPPED.name)
      return ServiceState.valueOf(value!!)
    }

    fun showPassphraseNotification(chatDbStatus: DBMigrationResult?) {
      val pendingIntent: PendingIntent = Intent(SimplexApp.context, MainActivity::class.java).let { notificationIntent ->
        PendingIntent.getActivity(SimplexApp.context, 0, notificationIntent, PendingIntent.FLAG_IMMUTABLE)
      }

      val title = when(chatDbStatus) {
        is DBMigrationResult.ErrorNotADatabase -> generalGetString(MR.strings.enter_passphrase_notification_title)
        is DBMigrationResult.OK -> return
        else -> generalGetString(MR.strings.database_initialization_error_title)
      }

      val description = when(chatDbStatus) {
        is DBMigrationResult.ErrorNotADatabase -> generalGetString(MR.strings.enter_passphrase_notification_desc)
        is DBMigrationResult.OK -> return
        else -> generalGetString(MR.strings.database_initialization_error_desc)
      }

      val builder =  NotificationCompat.Builder(SimplexApp.context, NOTIFICATION_CHANNEL_ID)
        .setSmallIcon(R.drawable.ntf_service_icon)
        .setColor(0x88FFFF)
        .setContentTitle(title)
        .setContentText(description)
        .setContentIntent(pendingIntent)
        .setSilent(true)
        .setShowWhen(false)

      val notificationManager = SimplexApp.context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
      notificationManager.notify(PASSPHRASE_NOTIFICATION_ID, builder.build())
    }

    fun cancelPassphraseNotification() {
      val notificationManager = SimplexApp.context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
      notificationManager.cancel(PASSPHRASE_NOTIFICATION_ID)
    }

    /*
    * When the app starts the database is in migration process. It can take from seconds to tens of seconds.
    * It happens in background thread, so other places that relies on database should wait til the end of migration
    * */
    suspend fun waitDbMigrationEnds(chatController: ChatController) {
      var maxWaitTime = 50_000
      while (chatController.chatModel.chatDbStatus.value == null && maxWaitTime > 0) {
        delay(50)
        maxWaitTime -= 50
      }
    }

    private fun getPreferences(context: Context): SharedPreferences = context.getSharedPreferences(SHARED_PREFS_ID, Context.MODE_PRIVATE)

    fun showBackgroundServiceNoticeIfNeeded(showOffAlert: Boolean = true) {
      val appPrefs = ChatController.appPrefs
      val mode = appPrefs.notificationsMode.get()
      Log.d(TAG, "showBackgroundServiceNoticeIfNeeded")
      // Nothing to do if mode is OFF. Can be selected on on-boarding stage
      if (mode == NotificationsMode.OFF) return

      if (!appPrefs.backgroundServiceNoticeShown.get()) {
        // the branch for the new users who have never seen service notice
        if (!mode.requiresIgnoringBattery || isBackgroundAllowed()) {
          showBGServiceNotice(mode)
        } else if (isBackgroundRestricted()) {
          showBGServiceNoticeSystemRestricted(mode, showOffAlert)
        } else if (!isIgnoringBatteryOptimizations()) {
          showBGServiceNoticeIgnoreOptimization(mode, showOffAlert)
        }
        // set both flags, so that if the user doesn't allow ignoring optimizations, the service will be disabled without additional notice
        appPrefs.backgroundServiceNoticeShown.set(true)
        appPrefs.backgroundServiceBatteryNoticeShown.set(true)
      } else if (mode.requiresIgnoringBattery && isBackgroundRestricted()) {
        // the branch for users who have app installed, and have seen the service notice,
        // but the service is running AND system background restriction is on OR the battery optimization for the app is in RESTRICTED state
        showBGServiceNoticeSystemRestricted(mode, showOffAlert)
        if (!appPrefs.backgroundServiceBatteryNoticeShown.get()) {
          appPrefs.backgroundServiceBatteryNoticeShown.set(true)
        }
      } else if (mode.requiresIgnoringBattery && !isIgnoringBatteryOptimizations()) {
        // the branch for users who have app installed, and have seen the service notice,
        // but the battery optimization for the app is in OPTIMIZED state (Android 12+) AND the service is running
        showBGServiceNoticeIgnoreOptimization(mode, showOffAlert)
        if (!appPrefs.backgroundServiceBatteryNoticeShown.get()) {
          appPrefs.backgroundServiceBatteryNoticeShown.set(true)
        }
      } else {
        // service or periodic mode was chosen and battery optimization is disabled
        SimplexApp.context.schedulePeriodicServiceRestartWorker()
        SimplexApp.context.schedulePeriodicWakeUp()
      }
    }

    private fun showBGServiceNotice(mode: NotificationsMode) = AlertManager.shared.showAlert {
      AlertDialog(
        onDismissRequest = AlertManager.shared::hideAlert,
        title = {
          Row {
            Icon(
              painterResource(MR.images.ic_bolt),
              contentDescription =
              if (mode == NotificationsMode.SERVICE) stringResource(MR.strings.icon_descr_instant_notifications) else stringResource(MR.strings.periodic_notifications),
            )
            Text(
              if (mode == NotificationsMode.SERVICE) stringResource(MR.strings.icon_descr_instant_notifications) else stringResource(MR.strings.periodic_notifications),
              fontWeight = FontWeight.Bold
            )
          }
        },
        text = {
          Column {
            Text(
              if (mode == NotificationsMode.SERVICE) annotatedStringResource(MR.strings.to_preserve_privacy_simplex_has_background_service_instead_of_push_notifications_it_uses_a_few_pc_battery) else annotatedStringResource(MR.strings.periodic_notifications_desc),
              Modifier.padding(bottom = 8.dp)
            )
            Text(
              annotatedStringResource(MR.strings.it_can_disabled_via_settings_notifications_still_shown)
            )
          }
        },
        confirmButton = {
          TextButton(onClick = AlertManager.shared::hideAlert) { Text(stringResource(MR.strings.ok)) }
        }
      )
    }

    private fun showBGServiceNoticeIgnoreOptimization(mode: NotificationsMode, showOffAlert: Boolean) = AlertManager.shared.showAlert {
      val ignoreOptimization = {
        AlertManager.shared.hideAlert()
        askAboutIgnoringBatteryOptimization()
      }
      val disableNotifications = {
        AlertManager.shared.hideAlert()
        disableNotifications(mode, showOffAlert)
      }
      AlertDialog(
        onDismissRequest = disableNotifications,
        title = {
          Row {
            Icon(
              painterResource(MR.images.ic_bolt),
              contentDescription =
              if (mode == NotificationsMode.SERVICE) stringResource(MR.strings.icon_descr_instant_notifications) else stringResource(MR.strings.periodic_notifications),
            )
            Text(
              if (mode == NotificationsMode.SERVICE) stringResource(MR.strings.service_notifications) else stringResource(MR.strings.periodic_notifications),
              fontWeight = FontWeight.Bold
            )
          }
        },
        text = {
          Column {
            Text(
              if (mode == NotificationsMode.SERVICE) annotatedStringResource(MR.strings.to_preserve_privacy_simplex_has_background_service_instead_of_push_notifications_it_uses_a_few_pc_battery) else annotatedStringResource(MR.strings.periodic_notifications_desc),
              Modifier.padding(bottom = 8.dp)
            )
            Text(annotatedStringResource(MR.strings.turn_off_battery_optimization))
          }
        },
        dismissButton = {
          TextButton(onClick = disableNotifications) { Text(stringResource(MR.strings.disable_notifications_button), color = MaterialTheme.colors.error) }
        },
        confirmButton = {
          TextButton(onClick = ignoreOptimization) { Text(stringResource(MR.strings.turn_off_battery_optimization_button)) }
        }
      )
    }

    private fun showBGServiceNoticeSystemRestricted(mode: NotificationsMode, showOffAlert: Boolean) = AlertManager.shared.showAlert {
      val unrestrict = {
        AlertManager.shared.hideAlert()
        askToUnrestrictBackground()
      }
      val disableNotifications = {
        AlertManager.shared.hideAlert()
        disableNotifications(mode, showOffAlert)
      }
      AlertDialog(
        onDismissRequest = disableNotifications,
        title = {
          Row {
            Icon(
              painterResource(MR.images.ic_bolt),
              contentDescription =
              if (mode == NotificationsMode.SERVICE) stringResource(MR.strings.icon_descr_instant_notifications) else stringResource(MR.strings.periodic_notifications),
            )
            Text(
              if (mode == NotificationsMode.SERVICE) stringResource(MR.strings.service_notifications) else stringResource(MR.strings.periodic_notifications),
              fontWeight = FontWeight.Bold
            )
          }
        },
        text = {
          Column {
            Text(
              annotatedStringResource(MR.strings.system_restricted_background_desc),
              Modifier.padding(bottom = 8.dp)
            )
            Text(annotatedStringResource(MR.strings.system_restricted_background_warn))
          }
        },
        dismissButton = {
          TextButton(onClick = disableNotifications) { Text(stringResource(MR.strings.disable_notifications_button), color = MaterialTheme.colors.error) }
        },
        confirmButton = {
          TextButton(onClick = unrestrict) { Text(stringResource(MR.strings.turn_off_system_restriction_button)) }
        }
      )
    }

    fun showBGRestrictedInCall(onDismiss: (allowedCall: Boolean) -> Unit) = AlertManager.shared.showAlert {
      val unrestrict = {
        askToUnrestrictBackground()
      }
      AlertDialog(
        onDismissRequest = AlertManager.shared::hideAlert,
        title = {
          Text(
            stringResource(MR.strings.system_restricted_background_in_call_title),
            fontWeight = FontWeight.Bold
          )
        },
        text = {
          Column {
            Text(
              annotatedStringResource(MR.strings.system_restricted_background_in_call_desc),
              Modifier.padding(bottom = 8.dp)
            )
            Text(annotatedStringResource(MR.strings.system_restricted_background_in_call_warn))
          }
        },
        confirmButton = {
          TextButton(onClick = unrestrict) { Text(stringResource(MR.strings.turn_off_system_restriction_button)) }
        }
      )
      val scope = rememberCoroutineScope()
      DisposableEffect(Unit) {
        scope.launch {
          repeat(10000) {
            delay(200)
            if (!isBackgroundRestricted()) {
              AlertManager.shared.hideAlert()
            }
          }
        }
        onDispose {
          onDismiss(!isBackgroundRestricted())
        }
      }
    }

    private fun showDisablingServiceNotice(mode: NotificationsMode) = AlertManager.shared.showAlert {
      AlertDialog(
        onDismissRequest = AlertManager.shared::hideAlert,
        title = {
          Row {
            Icon(
              painterResource(MR.images.ic_bolt),
              contentDescription =
              if (mode == NotificationsMode.SERVICE) stringResource(MR.strings.icon_descr_instant_notifications) else stringResource(MR.strings.periodic_notifications),
            )
            Text(
              if (mode == NotificationsMode.SERVICE) stringResource(MR.strings.service_notifications_disabled) else stringResource(MR.strings.periodic_notifications_disabled),
              fontWeight = FontWeight.Bold
            )
          }
        },
        text = {
          Column {
            Text(
              annotatedStringResource(MR.strings.turning_off_service_and_periodic),
              Modifier.padding(bottom = 8.dp)
            )
          }
        },
        confirmButton = {
          TextButton(onClick = AlertManager.shared::hideAlert) { Text(stringResource(MR.strings.ok)) }
        }
      )
    }

    fun isBackgroundAllowed(): Boolean = isIgnoringBatteryOptimizations() && !isBackgroundRestricted()

    fun isIgnoringBatteryOptimizations(): Boolean {
      val powerManager = androidAppContext.getSystemService(Application.POWER_SERVICE) as PowerManager
      return powerManager.isIgnoringBatteryOptimizations(androidAppContext.packageName)
    }

    fun isBackgroundRestricted(): Boolean {
      return if (Build.VERSION.SDK_INT >= 28) {
        val activityService = androidAppContext.getSystemService(ACTIVITY_SERVICE) as ActivityManager
        activityService.isBackgroundRestricted
      } else {
        false
      }
    }

    private fun askAboutIgnoringBatteryOptimization() {
      Intent().apply {
        @SuppressLint("BatteryLife")
        action = Settings.ACTION_REQUEST_IGNORE_BATTERY_OPTIMIZATIONS
        data = Uri.parse("package:${androidAppContext.packageName}")
        // This flag is needed when you start a new activity from non-Activity context
        addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
        androidAppContext.startActivity(this)
      }
    }

    private fun askToUnrestrictBackground() {
      Intent().apply {
        action = Settings.ACTION_APPLICATION_DETAILS_SETTINGS
        data = Uri.parse("package:${androidAppContext.packageName}")
        // This flag is needed when you start a new activity from non-Activity context
        addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
        try {
          androidAppContext.startActivity(this)
        } catch (e: ActivityNotFoundException) {
          Log.e(TAG, e.stackTraceToString())
        }
      }
    }

    private fun disableNotifications(mode: NotificationsMode, showOffAlert: Boolean) {
      if (showOffAlert) {
        showDisablingServiceNotice(mode)
      }
      ChatController.appPrefs.notificationsMode.set(NotificationsMode.OFF)
      StartReceiver.toggleReceiver(false)
      MessagesFetcherWorker.cancelAll()
      safeStopService()
    }
  }
}
