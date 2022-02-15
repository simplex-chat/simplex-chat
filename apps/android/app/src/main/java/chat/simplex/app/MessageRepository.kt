package chat.simplex.app

import android.util.Log
import android.widget.ScrollView
import android.widget.TextView
import androidx.compose.runtime.mutableStateListOf
import androidx.lifecycle.LiveData
import androidx.lifecycle.MutableLiveData
import chat.simplex.app.model.ChatController
import kotlin.concurrent.thread

class MessageRepository(val controller: ChatController) {
    var terminalLog = mutableStateListOf<String>()
     private set
//    private val store = chatInit(filesDir)
//    private val controller: Controller by lazy { // Maybe this shouldn't be lazy
//        this.maybeCreateUser()
//        chatStart(store)
//    }

//    fun maybeCreateUser() {
//        // create user if needed
//        if(chatGetUser(store) == "{}") {
//            chatCreateUser(store, """
//                    {"displayName": "test", "fullName": "android test"}
//                    """.trimIndent())
//        }
//        Log.d("SIMPLEX (user)", chatGetUser(store))
//    }

    fun sendCmd(cmd: String) {
        val response = chatSendCmd(controller.ctrl, cmd)
        Log.d("SIMPLEX SEND", response)
        terminalLog.add(response)
    }

    fun startReceiver() {
        thread(name="receiver") {
//            val chatlog = FifoQueue<String>(500)
            while(true) {
                val msg = chatRecvMsg(controller.ctrl)
                Log.d("SIMPLEX RECV", msg)
                terminalLog.add(msg)
//                val currentText = chatlog.joinToString("\n")
//                weakActivity.get()?.runOnUiThread {
//                    val log = weakActivity.get()?.findViewById<TextView>(R.id.chatlog)
//                    val scroll = weakActivity.get()?.findViewById<ScrollView>(R.id.scroller)
//                    log?.text = currentText
//                    scroll?.scrollTo(0, scroll.getChildAt(0).height)
//                }
            }
        }
    }
}
