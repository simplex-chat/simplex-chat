package chat.simplex.app

import android.app.Application
import android.net.LocalServerSocket
import android.util.Log
import chat.simplex.app.model.*
import chat.simplex.app.views.helpers.withApi
import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.*
import java.util.concurrent.Semaphore
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

//class SimplexApp: Application(), LifecycleEventObserver {
class SimplexApp: Application() {
  private lateinit var controller: ChatController
  lateinit var chatModel: ChatModel
  private lateinit var ntfManager: NtfManager

  override fun onCreate() {
    super.onCreate()
//    ProcessLifecycleOwner.get().lifecycle.addObserver(this)
    ntfManager = NtfManager(applicationContext)
    val ctrl = chatInit(applicationContext.filesDir.toString())
    controller = ChatController(ctrl, ntfManager, applicationContext)
    chatModel = controller.chatModel
    withApi {
      val user = controller.apiGetActiveUser()
      if (user != null) controller.startChat(user)
    }
  }

//  override fun onStateChanged(source: LifecycleOwner, event: Lifecycle.Event) {
//    Log.d(TAG, "onStateChanged: $event")
//    if (event == Lifecycle.Event.ON_STOP) {
//      Log.e(TAG, "BGManager schedule ${Clock.System.now()}")
//      BGManager.schedule(applicationContext)
//    }
//  }

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
