package chat.simplex.common.platform

import android.annotation.SuppressLint
import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import android.content.pm.PackageInfo
import android.content.pm.PackageInstaller
import android.content.pm.PackageManager
import android.net.LocalServerSocket
import android.net.Uri
import android.os.Build
import android.util.Log
import androidx.activity.ComponentActivity
import androidx.core.content.FileProvider
import androidx.fragment.app.FragmentActivity
import chat.simplex.common.helpers.APPLICATION_ID
import chat.simplex.common.views.helpers.*
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

@SuppressLint("StaticFieldLeak")
lateinit var androidAppContext: Context
var mainActivity: WeakReference<FragmentActivity> = WeakReference(null)
var callActivity: WeakReference<ComponentActivity> = WeakReference(null)

fun initHaskell() {
  val socketName = "chat.simplex.app.local.socket.address.listen.native.cmd2" + Random.nextLong(100000)
  val s = Semaphore(0)
  thread(name="stdout/stderr pipe") {
    Log.d(TAG, "starting server")
    var server: LocalServerSocket? = null
    for (i in 0..100) {
      try {
        server = LocalServerSocket(socketName + i)
        break
      } catch (e: IOException) {
        Log.e(TAG, e.stackTraceToString())
      }
    }
    if (server == null) {
      throw Error("Unable to setup local server socket. Contact developers")
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

actual fun chooseGitHubReleaseAssets(release: GitHubRelease): List<GitHubAsset> {
  val preferredArmeabi = Build.SUPPORTED_ABIS[0] == "armeabi-v7a" || Build.SUPPORTED_ABIS[0] == "armeabi-v7a-hard"
  val preferredAbi = if (preferredArmeabi) "simplex-armv7a.apk" else "simplex.apk"
  return release.assets.filter { it.name == preferredAbi }
}

actual fun installAppUpdate(file: File) {
  val context = mainActivity.get() ?: return
  val apkUri: Uri = FileProvider.getUriForFile(context, "$APPLICATION_ID.provider", file)
  @Suppress("DEPRECATION")
  val intent = Intent(Intent.ACTION_INSTALL_PACKAGE)
  intent.setData(apkUri)
  intent.setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION)
  context.startActivity(intent)
//  val packageInstaller = context.packageManager.packageInstaller
//  val params = PackageInstaller.SessionParams(PackageInstaller.SessionParams.MODE_FULL_INSTALL)
//  val sessionId = packageInstaller.createSession(params)
//  val session = packageInstaller.openSession(sessionId)
//  withBGApi {
//    try {
//      val packageInStream: OutputStream = session.openWrite("package", 0, -1)
//      val apkUri: Uri = FileProvider.getUriForFile(context, "$APPLICATION_ID.provider", file)
//      val input = context.contentResolver.openInputStream(apkUri) ?: return@withBGApi
//      val buffer = ByteArray(16384)
//      var n: Int
//      while ((input.read(buffer).also { n = it }) >= 0) {
//        packageInStream.write(buffer, 0, n)
//      }
//      packageInStream.close()
//      input.close()
//      val pendingIntent = PendingIntent.getBroadcast(context, sessionId, Intent("$APPLICATION_ID.INSTALL_COMPLETE"), PendingIntent.FLAG_IMMUTABLE)
//      session.commit(pendingIntent.intentSender)
//      Log.d(TAG, "Installed the update")
//    } catch (e: Exception) {
//      Log.d(TAG, "Error installing apk: ${e.stackTraceToString()}")
//    }
//  }
}