package chat.simplex.common.platform

import android.annotation.SuppressLint
import android.content.Context
import android.net.LocalServerSocket
import android.util.Log
import androidx.activity.ComponentActivity
import androidx.fragment.app.FragmentActivity
import androidx.work.Configuration
import androidx.work.WorkManager
import java.io.*
import java.lang.ref.WeakReference
import java.util.*
import java.util.concurrent.Semaphore
import kotlin.concurrent.thread
import kotlin.random.Random

actual val appPlatform = AppPlatform.ANDROID

actual val deviceName = android.os.Build.MODEL

var isAppOnForeground: Boolean = false

@Suppress("ConstantLocale")
val defaultLocale: Locale = Locale.getDefault()

actual fun isAppVisibleAndFocused(): Boolean = isAppOnForeground

@SuppressLint("StaticFieldLeak")
lateinit var androidAppContext: Context
var mainActivity: WeakReference<FragmentActivity> = WeakReference(null)
var callActivity: WeakReference<ComponentActivity> = WeakReference(null)

fun initHaskell(packageName: String) {
  val s = Semaphore(0)
  thread(name="stdout/stderr pipe") {
    Log.d(TAG, "starting server")
    val server: LocalServerSocket
    try {
      server = LocalServerSocket(packageName)
    } catch (e: IOException) {
      Log.e(TAG, e.stackTraceToString())
      Log.e(TAG, "Unable to setup local server socket. Contact developers")
      s.release()
      // Will not have logs from backend
      return@thread
    }
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
        Log.w(TAG, "(stdout/stderr) $line")
        logbuffer.add(line)
      }
      Log.w(TAG, "exited receiver loop")
    }
  }

  System.loadLibrary("app-lib")

  s.acquire()
  pipeStdOutToSocket(packageName)

  initHS()
}

fun Context.getWorkManagerInstance(): WorkManager {
  // https://github.com/OneSignal/OneSignal-Android-SDK/pull/2052/files
  // https://github.com/OneSignal/OneSignal-Android-SDK/issues/1672
  if (!WorkManager.isInitialized()) {
    try {
      WorkManager.initialize(this, Configuration.Builder().build())
    } catch (e: IllegalStateException) {
      Log.e(TAG, "Error initializing WorkManager: ${e.stackTraceToString()}")
    }
  }
  return WorkManager.getInstance(this)
}
