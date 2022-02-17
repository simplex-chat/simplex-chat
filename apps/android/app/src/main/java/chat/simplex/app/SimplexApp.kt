package chat.simplex.app

import android.app.Application
import android.net.LocalServerSocket
import android.util.Log
import androidx.compose.runtime.mutableStateListOf
import androidx.compose.runtime.snapshots.SnapshotStateList
import chat.simplex.app.model.*
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.GlobalScope
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import java.io.BufferedReader
import java.io.InputStreamReader
import java.lang.ref.WeakReference
import java.util.*
import java.util.concurrent.Semaphore
import kotlin.concurrent.thread

// ghc's rts
external fun initHS()
// android-support
external fun pipeStdOutToSocket(socketName: String) : Int

// SimpleX API
typealias ChatCtrl = Long
external fun chatInit(path: String): ChatCtrl
external fun chatSendCmd(ctrl: ChatCtrl, msg: String) : String
external fun chatRecvMsg(ctrl: ChatCtrl) : String

class SimplexApp: Application() {
  private lateinit var controller: ChatController

  override fun onCreate() {
    super.onCreate()
    controller = ChatController(chatInit(applicationContext.filesDir.toString()))
    GlobalScope.launch {
      withContext(Dispatchers.Main) {
        var user = controller.apiGetActiveUser()
        if (user != null) controller.startChat(user)
      }
    }
  }

  val chatModel by lazy {
    val m = ChatModel(controller)
    controller.setModel(m)
    controller.startReceiver()
    m
  }

  companion object {
    lateinit var weakActivity: WeakReference<MainActivity>
    init {
      val socketName = "local.socket.address.listen.native.cmd2"

      val s = Semaphore(0)
      thread(name="stdout/stderr pipe") {
        Log.d("SIMPLEX", "starting server")
        val server = LocalServerSocket(socketName)
        Log.d("SIMPLEX", "started server")
        s.release()
        val receiver = server.accept()
        Log.d("SIMPLEX", "started receiver")
        val logbuffer = FifoQueue<String>(500)
        if (receiver != null) {
          val inStream = receiver.inputStream
          val inStreamReader = InputStreamReader(inStream)
          val input = BufferedReader(inStreamReader)

          while(true) {
            val line = input.readLine() ?: break
            Log.d("SIMPLEX (stdout/stderr)", line)
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
