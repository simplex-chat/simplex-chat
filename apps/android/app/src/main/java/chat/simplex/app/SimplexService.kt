package chat.simplex.app

import android.app.*
import android.content.*
import android.os.*
import android.util.Log
import androidx.core.app.NotificationCompat
import androidx.core.content.ContextCompat
import androidx.work.*
import chat.simplex.app.views.helpers.withApi
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext

// based on:
// https://robertohuertas.com/2019/06/29/android_foreground_services/
// https://github.com/binwiederhier/ntfy-android/blob/main/app/src/main/java/io/heckel/ntfy/service/SubscriberService.kt

class SimplexService: Service() {
  private var wakeLock: PowerManager.WakeLock? = null
  private var isServiceStarted = false
  private var notificationManager: NotificationManager? = null
  private var serviceNotification: Notification? = null
  private val chatController by lazy { (application as SimplexApp).chatController }

  override fun onStartCommand(intent: Intent?, flags: Int, startId: Int): Int {
    Log.d(TAG, "onStartCommand startId: $startId")
    if (intent != null) {
      val action = intent.action
      Log.d(TAG, "intent action $action")
      when (action) {
        Action.START.name -> startService()
        Action.STOP.name -> stopService()
        else -> Log.e(TAG, "No action in the intent")
      }
    } else {
      Log.d(TAG, "null intent. Probably restarted by the system.")
    }
    return START_STICKY // to restart if killed
  }

  override fun onCreate() {
    super.onCreate()
    Log.d(TAG, "Simplex service created")
    val title = getString(R.string.simplex_service_notification_title)
    val text = getString(R.string.simplex_service_notification_text)
    notificationManager = createNotificationChannel()
    serviceNotification = createNotification(title, text)

    startForeground(SIMPLEX_SERVICE_ID, serviceNotification)
  }

  override fun onDestroy() {
    Log.d(TAG, "Simplex service destroyed")
    stopService()
    sendBroadcast(Intent(this, AutoRestartReceiver::class.java)) // Restart if necessary!
    super.onDestroy()
  }

  private fun startService() {
    if (isServiceStarted) return
    Log.d(TAG, "Starting foreground service")
    isServiceStarted = true
    saveServiceState(this, ServiceState.STARTED)
    wakeLock = (getSystemService(Context.POWER_SERVICE) as PowerManager).run {
      newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, WAKE_LOCK_TAG).apply {
        acquire()
      }
    }
    withApi {
      val user = chatController.apiGetActiveUser()
      if (user != null) {
        chatController.startChat(user)
        chatController.startReceiver()
      }
    }
  }

  private fun stopService() {
    Log.d(TAG, "Stopping foreground service")

// TODO stop simplex chat here?

    try {
      wakeLock?.let {
        while (it.isHeld) it.release() // release all, in case acquired more than once
      }
      wakeLock = null
      stopForeground(true)
      stopSelf()
    } catch (e: Exception) {
      Log.d(TAG, "Service stopped without being started: ${e.message}")
    }

    isServiceStarted = false
    saveServiceState(this, ServiceState.STOPPED)
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
    return NotificationCompat.Builder(this, NOTIFICATION_CHANNEL_ID)
      .setSmallIcon(R.drawable.ntf_icon)
      .setColor(0x88FFFF)
      .setContentTitle(title)
      .setContentText(text)
      .setContentIntent(pendingIntent)
      .setSound(null)
      .setShowWhen(false) // no date/time
      .build()
  }

  override fun onBind(intent: Intent): IBinder? {
    return null // no binding
  }

  // re-schedules the task when "Clear recent apps" is pressed
  override fun onTaskRemoved(rootIntent: Intent) {
    val restartServiceIntent = Intent(applicationContext, SimplexService::class.java).also {
      it.setPackage(packageName)
    };
    val restartServicePendingIntent: PendingIntent = PendingIntent.getService(this, 1, restartServiceIntent, PendingIntent.FLAG_ONE_SHOT or PendingIntent.FLAG_IMMUTABLE);
    applicationContext.getSystemService(Context.ALARM_SERVICE);
    val alarmService: AlarmManager = applicationContext.getSystemService(Context.ALARM_SERVICE) as AlarmManager;
    alarmService.set(AlarmManager.ELAPSED_REALTIME, SystemClock.elapsedRealtime() + 1000, restartServicePendingIntent);
  }

  // restart on reboot
  class StartReceiver: BroadcastReceiver() {
    override fun onReceive(context: Context, intent: Intent) {
      Log.d(TAG, "StartReceiver: onReceive called")
      scheduleStart(context)
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
      Log.d(TAG, "ServiceStartWorker: Starting foreground service (work ID: $id)")
      start(context)
      return Result.success()
    }
  }

  enum class Action {
    START,
    STOP
  }

  enum class ServiceState {
    STARTED,
    STOPPED,
  }

  companion object {
    const val TAG = "SIMPLEX_SERVICE"
    const val NOTIFICATION_CHANNEL_ID = "chat.simplex.app.service.SIMPLEX_SERVICE_NOTIFICATION"
    const val NOTIFICATION_CHANNEL_NAME = "SimpleX Chat service"
    const val SIMPLEX_SERVICE_ID = 6789

    private const val WAKE_LOCK_TAG = "SimplexService::lock"
    private const val SHARED_PREFS_ID = "chat.simplex.app.service.SIMPLEX_SERVICE_PREFS"
    private const val SHARED_PREFS_SERVICE_STATE = "SIMPLEX_SERVICE_STATE"
    private const val WORK_NAME_ONCE = "ServiceStartWorkerOnce"

    fun scheduleStart(context: Context) {
      Log.d(TAG, "Enqueuing work to start subscriber service")
      val workManager = WorkManager.getInstance(context)
      val startServiceRequest = OneTimeWorkRequest.Builder(ServiceStartWorker::class.java).build()
      workManager.enqueueUniqueWork(WORK_NAME_ONCE, ExistingWorkPolicy.KEEP, startServiceRequest) // Unique avoids races!
    }

    suspend fun start(context: Context) {
      withContext(Dispatchers.IO) {
        Intent(context, SimplexService::class.java).also {
          it.action = Action.START.name
          ContextCompat.startForegroundService(context, it)
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

    private fun getPreferences(context: Context): SharedPreferences = context.getSharedPreferences(SHARED_PREFS_ID, Context.MODE_PRIVATE)
  }
}