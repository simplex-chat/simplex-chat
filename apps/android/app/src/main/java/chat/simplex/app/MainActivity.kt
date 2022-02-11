package chat.simplex.app

import android.net.LocalServerSocket
import android.os.Bundle
import android.util.Log
import android.view.inputmethod.EditorInfo
import android.widget.ScrollView
import android.widget.TextView
import androidx.appcompat.app.AppCompatActivity
import androidx.appcompat.widget.AppCompatEditText
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

// simplex-chat
typealias Store = Long
typealias Controller = Long
external fun chatInit(filesDir: String): Store
external fun chatGetUser(controller: Store) : String
external fun chatCreateUser(controller: Store, data: String) : String
external fun chatStart(controller: Store) : Controller
external fun chatSendCmd(controller: Controller, msg: String) : String
external fun chatRecvMsg(controller: Controller) : String

class MainActivity : AppCompatActivity() {
  override fun onCreate(savedInstanceState: Bundle?) {
    weakActivity = WeakReference(this)

    super.onCreate(savedInstanceState)
    setContentView(R.layout.activity_main)

    val store : Store = chatInit(this.applicationContext.filesDir.toString())
    // create user if needed
    if(chatGetUser(store) == "{}") {
      chatCreateUser(store, """
                    {"displayName": "test", "fullName": "android test"}
                    """.trimIndent())
    }
    Log.d("SIMPLEX (user)", chatGetUser(store))

    val controller = chatStart(store)

    val cmdinput = this.findViewById<AppCompatEditText>(R.id.cmdInput)

    cmdinput.setOnEditorActionListener { _, actionId, _ ->
      when (actionId) {
        EditorInfo.IME_ACTION_SEND -> {
          Log.d("SIMPLEX SEND", chatSendCmd(controller, cmdinput.text.toString()))
          cmdinput.text?.clear()
          true
        }
        else -> false
      }
    }

    thread(name="receiver") {
      val chatlog = FifoQueue<String>(500)
      while(true) {
        val msg = chatRecvMsg(controller)
        Log.d("SIMPLEX RECV", msg)
        chatlog.add(msg)
        val currentText = chatlog.joinToString("\n")
        weakActivity.get()?.runOnUiThread {
          val log = weakActivity.get()?.findViewById<TextView>(R.id.chatlog)
          val scroll = weakActivity.get()?.findViewById<ScrollView>(R.id.scroller)
          log?.text = currentText
          scroll?.scrollTo(0, scroll.getChildAt(0).height)
        }
      }
    }
  }

  companion object {
    lateinit var weakActivity : WeakReference<MainActivity>
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
