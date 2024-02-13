package chat.simplex.app

import android.app.*
import android.content.*
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.os.*
import androidx.compose.ui.graphics.asAndroidBitmap
import androidx.core.app.NotificationCompat
import androidx.core.content.ContextCompat
import chat.simplex.app.model.NtfManager.EndCallAction
import chat.simplex.app.views.call.CallActivity
import chat.simplex.common.model.NotificationPreviewMode
import chat.simplex.common.platform.*
import chat.simplex.common.views.call.CallState
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import kotlinx.datetime.Instant

class CallService: Service() {
  private var wakeLock: PowerManager.WakeLock? = null
  private var notificationManager: NotificationManager? = null
  private var serviceNotification: Notification? = null

  override fun onStartCommand(intent: Intent?, flags: Int, startId: Int): Int {
    Log.d(TAG, "onStartCommand startId: $startId")
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
    startForeground(CALL_SERVICE_ID, serviceNotification)
    return START_STICKY
  }

  override fun onCreate() {
    super.onCreate()
    Log.d(TAG, "Call service created")
    notificationManager = createNotificationChannel()
    updateNotification()
    startForeground(CALL_SERVICE_ID, serviceNotification)
  }

  override fun onDestroy() {
    Log.d(TAG, "Call service destroyed")
    try {
      wakeLock?.let {
        while (it.isHeld) it.release() // release all, in case acquired more than once
      }
      wakeLock = null
    } catch (e: Exception) {
      Log.d(TAG, "Exception while releasing wakelock: ${e.message}")
    }
    super.onDestroy()
  }

  private fun startService() {
    Log.d(TAG, "CallService startService")
    if (wakeLock != null) return
    wakeLock = (getSystemService(Context.POWER_SERVICE) as PowerManager).run {
      newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, WAKE_LOCK_TAG).apply {
        acquire()
      }
    }
  }

  fun updateNotification() {
    val call = chatModel.activeCall.value
    val previewMode = appPreferences.notificationPreviewMode.get()
    val title = if (previewMode == NotificationPreviewMode.HIDDEN.name)
      generalGetString(MR.strings.notification_preview_somebody)
    else
      call?.contact?.profile?.displayName ?: ""
    val text = generalGetString(if (call?.supportsVideo() == true) MR.strings.call_service_notification_video_call else MR.strings.call_service_notification_audio_call)
    val image = call?.contact?.image
    val largeIcon = if (image == null || previewMode == NotificationPreviewMode.HIDDEN.name)
      BitmapFactory.decodeResource(resources, R.drawable.icon)
    else
      base64ToBitmap(image).asAndroidBitmap()

    serviceNotification = createNotification(title, text, largeIcon, call?.connectedAt)
    startForeground(CALL_SERVICE_ID, serviceNotification)
  }

  private fun createNotificationChannel(): NotificationManager? {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
      val notificationManager = getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
      val channel = NotificationChannel(CALL_NOTIFICATION_CHANNEL_ID, CALL_NOTIFICATION_CHANNEL_NAME, NotificationManager.IMPORTANCE_DEFAULT)
      notificationManager.createNotificationChannel(channel)
      return notificationManager
    }
    return null
  }

  private fun createNotification(title: String, text: String, icon: Bitmap, connectedAt: Instant? = null): Notification {
    val pendingIntent: PendingIntent = Intent(this, CallActivity::class.java).let { notificationIntent ->
      PendingIntent.getActivity(this, 0, notificationIntent, PendingIntent.FLAG_IMMUTABLE)
    }

    val endCallPendingIntent: PendingIntent = Intent(this, CallActionReceiver::class.java).let { notificationIntent ->
      notificationIntent.setAction(EndCallAction)
      PendingIntent.getBroadcast(this, 1, notificationIntent, PendingIntent.FLAG_CANCEL_CURRENT or PendingIntent.FLAG_IMMUTABLE)
    }

    val builder =  NotificationCompat.Builder(this, CALL_NOTIFICATION_CHANNEL_ID)
      .setSmallIcon(R.drawable.ntf_icon)
      .setLargeIcon(icon.clipToCircle())
      .setColor(0x88FFFF)
      .setContentTitle(title)
      .setContentText(text)
      .setContentIntent(pendingIntent)
      .setSilent(true)
      .addAction(R.drawable.ntf_icon, generalGetString(MR.strings.call_service_notification_end_call), endCallPendingIntent)
    if (connectedAt != null) {
      builder.setUsesChronometer(true)
      builder.setWhen(connectedAt.epochSeconds * 1000)
    }

    return builder.build()
  }

  override fun onBind(intent: Intent): IBinder {
    return CallServiceBinder()
  }

  inner class CallServiceBinder : Binder() {
    fun getService() = this@CallService
  }

  enum class Action {
    START,
  }

  class CallActionReceiver: BroadcastReceiver() {
    override fun onReceive(context: Context?, intent: Intent?) {
      when (intent?.action) {
        EndCallAction -> {
          val call = chatModel.activeCall.value
          if (call != null) {
            withBGApi {
              chatModel.callManager.endCall(call)
            }
          }
        }
        else -> {
          Log.e(TAG, "Unknown action. Make sure you provided an action")
        }
      }
    }
  }

companion object {
    const val TAG = "CALL_SERVICE"
    const val CALL_NOTIFICATION_CHANNEL_ID = "chat.simplex.app.CALL_SERVICE_NOTIFICATION"
    const val CALL_NOTIFICATION_CHANNEL_NAME = "SimpleX Chat call service"
    const val CALL_SERVICE_ID = 6788
    const val WAKE_LOCK_TAG = "CallService::lock"

    fun startService(): Intent {
      Log.d(TAG, "CallService start")
      return Intent(androidAppContext, CallService::class.java).also {
        it.action = Action.START.name
        ContextCompat.startForegroundService(androidAppContext, it)
      }
    }

    fun stopService() {
      androidAppContext.stopService(Intent(androidAppContext, CallService::class.java))
    }
  }
}
