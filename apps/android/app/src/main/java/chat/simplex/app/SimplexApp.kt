package chat.simplex.app

import android.app.Application
import android.content.Context
import android.net.*
import android.util.Log
import androidx.lifecycle.*
import androidx.work.*
import chat.simplex.app.model.*
import chat.simplex.app.views.helpers.withApi
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
external fun chatInit(path: String): ChatCtrl
external fun chatSendCmd(ctrl: ChatCtrl, msg: String) : String
external fun chatRecvMsg(ctrl: ChatCtrl) : String

class SimplexApp: Application(), LifecycleEventObserver {
  private lateinit var controller: ChatController
  lateinit var chatModel: ChatModel
  private lateinit var ntfManager: NtfManager

  fun initiateBackgroundWork() {
    val backgroundConstraints = Constraints.Builder()
      .setRequiredNetworkType(NetworkType.CONNECTED)
      .build()
    val request = OneTimeWorkRequestBuilder<BackgroundAPIWorker>()
      .setInitialDelay(5, TimeUnit.MINUTES)
      .setConstraints(backgroundConstraints)
      .build()
    WorkManager.getInstance(applicationContext)
      .enqueue(request)
  }

  override fun onCreate() {
    super.onCreate()
    ProcessLifecycleOwner.get().lifecycle.addObserver(this)
    registerNetworkCallback()
    ntfManager = NtfManager(applicationContext)
    val ctrl = chatInit(applicationContext.filesDir.toString())
    controller = ChatController(ctrl, ntfManager, applicationContext)
    chatModel = controller.chatModel
    withApi {
      val user = controller.apiGetActiveUser()
      if (user != null) controller.startChat(user)
    }
  }

  override fun onStateChanged(source: LifecycleOwner, event: Lifecycle.Event) {
    Log.d(TAG, "onStateChanged: $event")
  }

  private fun registerNetworkCallback() {
    val connectivityManager = getSystemService(ConnectivityManager::class.java)
    connectivityManager.registerDefaultNetworkCallback(object : ConnectivityManager.NetworkCallback() {
      override fun onAvailable(network: Network) {
        Log.e(TAG, "The default network is now: " + network)
      }

      override fun onLost(network: Network) {
        Log.e(TAG, "The application no longer has a default network. The last default network was " + network)
      }

      override fun onCapabilitiesChanged(network: Network, networkCapabilities: NetworkCapabilities) {
        Log.e(TAG, "The default network changed capabilities: " + networkCapabilities)
      }

      override fun onLinkPropertiesChanged(network: Network, linkProperties: LinkProperties) {
        Log.e(TAG, "The default network changed link properties: " + linkProperties)
      }
    })
  }

  companion object {
    init {
      val socketName = "local.socket.address.listen.native.cmd2"

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

          while(true) {
            val line = input.readLine() ?: break
            Log.w("$TAG (stdout/stderr)", line)
            logbuffer.add(line)
          }
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
