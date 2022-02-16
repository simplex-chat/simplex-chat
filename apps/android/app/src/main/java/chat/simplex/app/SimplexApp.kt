package chat.simplex.app

import android.app.Application
import android.net.LocalServerSocket
import android.util.Log
import chat.simplex.app.model.ChatController
import chat.simplex.app.model.ChatModel
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
typealias Controller = Long
typealias Store = Long
external fun chatInit(filesDir: String): Store
external fun chatGetUser(controller: Store) : String
external fun chatCreateUser(controller: Store, data: String) : String
external fun chatStart(controller: Store) : Controller
external fun chatSendCmd(controller: Controller, msg: String) : String
external fun chatRecvMsg(controller: Controller) : String

class SimplexApp: Application() {
    private lateinit var controller: ChatController

    override fun onCreate() {
        super.onCreate()
        val store: Store = chatInit(applicationContext.filesDir.toString())
        // create user if needed
        if (chatGetUser(store) == "{}") {
            createUser(store, "test", "android test")
        }
        Log.d("SIMPLEX (user)", chatGetUser(store))
        controller = ChatController(chatStart(store))
    }

    fun createUser(store: Store, displayName: String, fullName: String?){
        chatCreateUser(store, "{\"displayName:\"$displayName\", \"fullName\":\"$fullName\"}")
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
